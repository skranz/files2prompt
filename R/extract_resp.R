#' Defines standard marker pairs for identifying file blocks in a text.
#'
#' This function provides a default set of marker pairs that can be used to
#' identify file blocks in a text, like an AI response. Each row in the returned
#' data frame represents one pair of start and end markers.
#'
#' @return A data frame with `start` and `end` columns.
#' @export
#' @examples
#' f2p_resp_file_markers()
f2p_resp_file_markers = function() {
  as.data.frame(rbind(
    # Example block structure this marker pair would match:
    # !FILE src/code.R
    # ```r
    # print("hello world")
    # ```
    # !END_FILE src/code.R
    c(start="!FILE", end="!END_FILE"),
    # A simple XML-like alternative
    c(start="<FILE>", end="</FILE>")
  ))
}

#' Extract file contents from a text based on file markers.
#'
#' Parses a text, typically a response from an AI, and extracts blocks of text
#' that are delimited by specified start and end markers. It returns a data frame
#' with file names and their corresponding content.
#'
#' The function iterates through marker pairs defined in `file_markers`. For each
#' pair, it finds all occurrences of text blocks. The file name is expected to be
#' on the same line as the start marker, separated by whitespace.
#'
#' If a matching block's content contains a markdown code fence (```...```), only the
#' content inside the fence is extracted. Otherwise, the whole content between
#' the markers (with leading/trailing whitespace trimmed) is taken.
#'
#' If multiple marker pairs match blocks for the same file name, only the first one
#' encountered (based on the order in `file_markers`) will be kept in the final
#' output.
#'
#' @param text A character vector or a single string containing the text to parse.
#' @param file_markers A data frame with `start` and `end` columns, defining
#'   the delimiters for file blocks. Defaults to `f2p_resp_file_markers()`.
#' @return A data frame with two columns: `file` (character) and `content`
#'   (character). Each row corresponds to an extracted file.
#' @importFrom restorepoint restore.point
#' @importFrom stringi stri_trim_both stri_match_all_regex stri_match_first_regex stri_replace_all_regex
#' @export
#' @examples
#' response_text <- "
#' Here is the new file you requested:
#'
#' !FILE new_script.R
#' ```r
#' # A new R script
#' x <- 1:10
#' print(mean(x))
#' ```
#' !END_FILE
#'
#' I have also updated the documentation.
#'
#' <FILE> README.md
#' # Project Title
#'
#' This is a sample project.
#' </FILE>
#' "
#'
#' extracted <- f2p_resp_extract_files(response_text)
#' print(extracted)
#'
f2p_resp_extract_files <- function(text, file_markers = f2p_resp_file_markers()) {
  restore.point("f2p_resp_extract_files")
  text_content <- paste0(text, collapse="\n")

  files_li <- list()

  # Helper to escape special regex characters from marker strings
  escape_regex <- function(string) {
    stringi::stri_replace_all_regex(string, "([\\\\\\^\\$\\.\\*\\+\\?\\(\\)\\[\\]\\{\\}\\|])", "\\\\$1")
  }

  for (i in seq_len(nrow(file_markers))) {
    start_marker <- escape_regex(file_markers$start[i])
    end_marker <- escape_regex(file_markers$end[i])

    # Regex to find all file blocks for the current marker pair.
    # It captures the filename (group 1) and the block's content (group 2).
    # The (?s) flag makes "." match newlines.
    block_pattern <- paste0(
      "(?s)",
      start_marker,
      "\\s+([^\\n\\r]+)",  # 1: filename (up to newline)
      "[\\n\\r]+",
      "(.*?)",              # 2: content (non-greedy)
      "[\\n\\r]+\\s*",
      end_marker
    )

    matches <- stringi::stri_match_all_regex(text_content, block_pattern, omit_no_match = TRUE)[[1]]

    if (nrow(matches) > 0) {
      for (j in seq_len(nrow(matches))) {
        file_name <- stringi::stri_trim_both(matches[j, 2])
        raw_content <- matches[j, 3]

        # Regex to find a markdown code block inside the content.
        # It captures the code itself (group 1).
        code_pattern <- "(?s)```[a-zA-Z0-9_]*[\\n\\r]+(.*?)[\\n\\r]*```"
        code_match <- stringi::stri_match_first_regex(raw_content, code_pattern)

        content <- if (!is.na(code_match[1, 1])) {
          # Markdown block found, use its content.
          code_match[1, 2]
        } else {
          # No markdown block, use the raw content, trimmed.
          stringi::stri_trim_both(raw_content)
        }

        files_li[[length(files_li) + 1]] <- list(file = file_name, content = content)
      }
    }
  }

  if (length(files_li) == 0) {
    return(data.frame(file = character(0), content = character(0)))
  }

  # Convert list of lists to a data frame
  files <- vapply(files_li, `[[`, "file", FUN.VALUE = character(1))
  contents <- vapply(files_li, `[[`, "content", FUN.VALUE = character(1))

  res_df <- data.frame(file = files, content = contents, stringsAsFactors = FALSE)

  # Remove duplicated files, keeping the first occurrence. This can happen
  # if different marker pairs match the same file block. The order of markers
  # in file_markers() determines precedence.
  res_df[!duplicated(res_df$file), ]
}
