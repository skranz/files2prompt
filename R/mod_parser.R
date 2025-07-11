# FILE: R/mod_parser.R
#' @importFrom RcppTOML parseTOML
#' @importFrom stringi stri_split_fixed stri_trim_both stri_match_all_regex
#' @keywords internal
parse_ai_response <- function(text) {
  restore.point("parse_ai_response")
  # Use regex to find all blocks between !MODIFICATION and !END_MODIFICATION
  pattern <- "(?s)!MODIFICATION.*?\\n(.*?)\\n!END_MODIFICATION"
  matches <- stri_match_all_regex(text, pattern)[[1]]

  if (nrow(matches) == 0) {
    return(list())
  }

  blocks <- matches[, 2] # Extract the captured group

  i = 1
  parsed_mods <- lapply(seq_along(blocks), function(i) {

    block <- blocks[i]
    parts <- stri_split_fixed(block, "\n---\n", n = 2)[[1]]

    if (length(parts) != 2) {
      warning("Block ", i, " is malformed (missing '---' separator). Skipping.")
      return(NULL)
    }

    meta_str <- parts[1]
    payload_str <- parts[2]

    meta <- tryCatch({
      RcppTOML::parseTOML(meta_str,fromFile = FALSE)
    }, error = function(e) {
      warning("Failed to parse TOML in block ", i, ": ", e$message, ". Skipping.")
      NULL
    })

    # --- Validate metadata based on the new spec ---
    if (is.null(meta) || is.null(meta$scope) || is.null(meta$file)) {
      warning("Block ", i, " is missing required metadata (scope, file). Skipping.")
      return(NULL)
    }

    # Extract code from the markdown fence
    payload_match <- regexpr("(?s)^```[a-zA-Z]*\\n(.*?)\\n```\\s*$", payload_str, perl = TRUE)
    if (payload_match == -1) {
      if (stri_trim_both(payload_str) == "```\n```") {
        payload <- ""
      } else {
        warning("Could not extract code payload from block ", i, ". Skipping.")
        return(NULL)
      }
    } else {
      start <- attr(payload_match, "capture.start")[1]
      len <- attr(payload_match, "capture.length")[1]
      payload <- substr(payload_str, start, start + len - 1)
    }

    list(meta = meta, payload = payload)
  })

  Filter(Negate(is.null), parsed_mods)
}
