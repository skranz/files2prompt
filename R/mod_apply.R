# FILE: R/mod_apply.R
#' @importFrom restorepoint restore.point
#' @keywords internal

#' Apply a single, pre-located modification using the rstudioapi
#'
#' This function assumes the modification object `mod` has already been
#' populated with `file_path`, `start_line`, and `end_line` by `mod_locate_target`.
#'
#' @param mod The modification object.
#' @return Invisibly TRUE on success.
apply_modification_via_api <- function(mod) {
  restore.point("apply_modification_via_api")

  # Create directory if it doesn't exist (for new files)
  dir.create(dirname(mod$meta$file_path), showWarnings = FALSE, recursive = TRUE)

  # The generic rstudio document modifier
  modify_rstudio_doc(
    file = mod$meta$file_path,
    start_line = mod$meta$start_line,
    end_line = mod$meta$end_line,
    text = mod$payload
  )

  cat("Successfully applied '", mod$meta$scope, "' to '", mod$meta$file, "'.\n", sep = "")
  return(invisible(TRUE))
}

#' Apply a content restoration (for undo) using the rstudioapi
#'
#' @param file The full path to the file.
#' @param start_line The start line of the range to replace.
#' @param end_line The end line of the range to replace.
#' @param text The original text to restore.
#' @return Invisibly TRUE on success.
apply_restoration_via_api <- function(file, start_line, end_line, text) {
  modify_rstudio_doc(file, start_line, end_line, text)
  return(invisible(TRUE))
}


#' Use rstudioapi to perform a modification on a document
#'
#' This is the core function that interacts with the RStudio editor. It can
#' replace a range of lines or insert text.
#'
#' @param file The full path to the file.
#' @param start_line The start line of the range.
#' @param end_line The end line of the range. For an insertion, this should be
#'   `start_line - 1`.
#' @param text The text to insert.
#' @keywords internal
modify_rstudio_doc <- function(file, start_line, end_line, text) {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("This operation requires the RStudio API.")
  }

  # Ensure the target file is active for the user to see the change.
  rstudioapi::navigateToFile(file, line = start_line)

  # The API's modifyRange function deletes content in the specified range
  # and then inserts the new text.
  # For replacement of lines 5-7, range is from start of line 5 to start of line 8.
  # This corresponds to start_line=5, end_line=7. The range end position is end_line+1.
  # For insertion at line 5, we want to replace a zero-length range at (5,1).
  # This corresponds to start_line=5, end_line=4. The range end position is end_line+1 = 5.
  range <- rstudioapi::document_range(
    rstudioapi::document_position(start_line, 1),
    rstudioapi::document_position(end_line + 1, 1)
  )
  rstudioapi::modifyRange(location = range, text = text)
}
