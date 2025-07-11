#' @importFrom RcppTOML parseTOML
#' @importFrom stringi stri_split_fixed stri_trim_both
#' @keywords internal
parse_ai_response <- function(text) {
  # Split the entire response into modification blocks
  blocks <- stri_split_fixed(text, "\n!MODIFICATION", omit_empty = TRUE)[[1]]

  parsed_mods <- lapply(seq_along(blocks), function(i) {
    block <- blocks[[i]]
    # Each block is composed of TOML metadata, a '---' separator, and a code payload
    parts <- stri_split_fixed(block, "\n---\n", n = 2)[[1]]

    if (length(parts) != 2) {
      warning("Block ", i, " is malformed (missing '---' separator). Skipping.")
      return(NULL)
    }

    meta_str <- parts[1]
    payload_str <- parts[2]

    # Parse TOML metadata
    meta <- tryCatch({
      parseTOML(textConnection(meta_str), escape = FALSE)
    }, error = function(e) {
      warning("Failed to parse TOML in block ", i, ": ", e$message, ". Skipping.")
      NULL
    })

    if (is.null(meta) || is.null(meta$operation) || is.null(meta$file)) {
      warning("Block ", i, " has invalid or missing TOML metadata (operation, file). Skipping.")
      return(NULL)
    }

    # Extract code from the markdown fence
    # This regex captures content between ```...```, ignoring the language specifier
    payload_match <- regexpr("(?s)^```[a-zA-Z]*\\n(.*?)\\n```\\s*$", payload_str, perl = TRUE)
    if (payload_match == -1) {
       # Handle empty payload for deletions
      if (stri_trim_both(payload_str) == "```\n```") {
        payload <- ""
      } else {
        warning("Could not extract code from payload in block ", i, ". Skipping.")
        return(NULL)
      }
    } else {
        start <- attr(payload_match, "capture.start")[1]
        length <- attr(payload_match, "capture.length")[1]
        payload <- substr(payload_str, start, start + length - 1)
    }

    # Cleanup potential end-of-file carriage returns from payload
    payload <- gsub("\r$", "", payload)


    list(meta = meta, payload = payload)
  })

  # Remove NULLs from list (from skipped blocks)
  parsed_mods <- Filter(Negate(is.null), parsed_mods)
  return(parsed_mods)
}
