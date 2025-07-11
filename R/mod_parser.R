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

  parsed_mods <- list()
  for (i in seq_along(blocks)) {
    block <- blocks[i]
    parts <- stri_split_fixed(block, "\n---\n", n = 2)[[1]]

    if (length(parts) != 2) {
      stop("Modification block ", i, " is malformed (missing '---' separator).")
    }

    meta_str <- parts[1]
    payload_str <- parts[2]

    meta <- tryCatch({
      RcppTOML::parseTOML(meta_str, fromFile = FALSE)
    }, error = function(e) {
      stop("Failed to parse TOML in modification block ", i, ": ", e$message)
    })

    if (is.null(meta$scope) || is.null(meta$file)) {
      stop("Modification block ", i, " is missing required metadata (scope, file).")
    }

    # Extract code from the markdown fence
    payload_match <- regexpr("(?s)^```[a-zA-Z]*\\n(.*?)\\n```\\s*$", payload_str, perl = TRUE)
    if (payload_match != -1) {
      start <- attr(payload_match, "capture.start")[1]
      len <- attr(payload_match, "capture.length")[1]
      payload <- substr(payload_str, start, start + len - 1)
    } else if (stri_trim_both(payload_str) == "```\n```") {
      # Handle empty code block
      payload <- ""
    } else {
      # If no markdown fence, assume the whole payload is the code
      payload <- payload_str
      warning("Payload in block ", i, " was not wrapped in a markdown code fence. Using entire block as payload.")
    }

    parsed_mods[[i]] <- list(meta = meta, payload = payload)
  }

  return(parsed_mods)
}

