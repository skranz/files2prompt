# FILE: R/mod_undo.R

#' Create an empty undo state object
#'
#' This is used to initialize or clear the undo buffer in the addin's
#' reactive values.
#'
#' @return A list with NULL fields for the undo state.
#' @keywords internal
create_undo_state <- function() {
  list(
    mod_index = NULL,
    file_path = NULL,
    original_content = NULL,
    start_line = NULL,
    end_line = NULL
  )
}

#' Prepare an undo state before applying a modification
#'
#' Captures the content that is about to be replaced or the location
#' where an insertion will occur.
#'
#' @param mod_index The index of the modification being applied.
#' @param target_file The path to the file being modified.
#' @param start_line The starting line of the modification range.
#' @param end_line The ending line of the modification range (can be less than
#'   start_line for an insertion).
#' @return An undo state list populated with the necessary information.
#' @keywords internal
prepare_undo_state <- function(mod_index, target_file, start_line, end_line) {
  if (!file.exists(target_file)) {
     # If the file doesn't exist, it's a new file.
     # The original content is nothing.
     original_content <- ""
  } else {
     original_lines <- readLines(target_file, warn = FALSE)
     if (end_line < start_line) { # This signifies an insertion
        original_content <- ""
     } else {
        original_content <- paste(original_lines[start_line:end_line], collapse = "\n")
     }
  }

  list(
    mod_index = mod_index,
    file_path = target_file,
    original_content = original_content,
    start_line = start_line,
    end_line = end_line
  )
}
