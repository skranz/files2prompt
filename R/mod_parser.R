#' Normalize AI response text to fix common formatting errors.
#'
#' @param text The raw text from the AI response.
#' @return The normalized text.
#' @keywords internal
f2p_normalize_ai_response <- function(text) {
  # Add a newline before a markdown fence if it's not at the start of a line.
  # This handles cases where the AI might write "description...'''r" on one line.
  text <- gsub("([^\n])```", "\\1\n```", text, perl = TRUE)
  return(text)
}


#' @importFrom RcppTOML parseTOML
#' @importFrom stringi stri_split_fixed stri_trim_both stri_match_all_regex
#' @keywords internal
parse_ai_response <- function(text) {
  restore.point("parse_ai_response")
  # First, normalize the response to fix common formatting issues
  text <- f2p_normalize_ai_response(text)

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
      meta_obj <- RcppTOML::parseTOML(meta_str, fromFile = FALSE)
      if (is.null(meta_obj$scope) || is.null(meta_obj$file)) {
        stop("Modification block is missing required metadata (scope, file).")
      }
      meta_obj
    }, error = function(e) {
      warning("Failed to parse metadata in modification block ", i, ": ", e$message)
      return(list(
        parse_error = TRUE,
        parse_error_message = e$message,
        raw_toml = meta_str,
        scope = "parse_error",
        file = "unknown",
        description = "Metadata parsing failed."
      ))
    })

    # NEW, more robust payload extraction logic.
    # It handles surrounding whitespace in the regex and uses a non-greedy capture.
    pattern <- "(?s)\\s*```[a-zA-Z]*\\n(.*?)\\n```\\s*"
    match <- stri_match_all_regex(payload_str, pattern)[[1]]

    if (nrow(match) > 0) {
      # The second column of the match matrix contains the first capture group.
      payload <- match[1, 2]
    } else {
      # If no markdown fence, assume the whole payload is the code.
      # Here, we trim whitespace as it's not part of a fence.
      payload <- stri_trim_both(payload_str)
      warning("Payload in block ", i, " was not wrapped in a markdown code fence. Using entire block as payload.")
    }


    parsed_mods[[i]] <- list(meta = meta, payload = payload)
  }

  return(parsed_mods)
}
