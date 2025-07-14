#' Normalise AI response text to fix common formatting errors.
#'
#' Inserts a newline before a Markdown fence (```),
#' except when the fence is already at the start of the line
#' **or** when that line contains a double quote before **and** after the fence
#' (heuristic for “inside a string literal”).
#'
#' @param text Character scalar: raw AI response.
#' @return Character scalar: normalised text.
#' @keywords internal
f2p_normalize_ai_response <- function(text) {
  stopifnot(is.character(text), length(text) == 1)

  lines = stringi::stri_split_lines(text, omit_empty = FALSE)[[1]]

  has_fence      = stringi::stri_detect_fixed(lines, "```")
  fence_at_start = stringi::stri_detect_regex(lines, "^```")
  fence_in_quote = stringi::stri_detect_regex(lines, "\".*```.*\"")

  target = has_fence & !fence_at_start & !fence_in_quote

  lines[target] = stringi::stri_replace_all_regex(
    lines[target],
    "([^\\n])```",   # non-newline immediately followed by ```
    "$1\n```"
  )

  stringi::stri_join(lines, collapse = "\n")
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
