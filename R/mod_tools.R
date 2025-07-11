#' Recursively find a file in a project directory by its basename.
#' @param file The basename of the file to find (e.g., "utils.R").
#' @param root_dir The project's root directory.
#' @return The full, normalized path to the file, or NULL if not found.
#'         Issues a warning if multiple files with the same name exist.
#' @keywords internal
find_project_file <- function(file, root_dir) {
  # Normalize file to use forward slashes for consistent matching
  file_norm <- gsub("\\\\", "/", file)

  # If the provided path is already absolute or exists relative to root, use it.
  if (is_absolute_path(file) && file.exists(file)) {
    return(normalizePath(file, winslash = "/"))
  }

  # Check if the file exists as-is relative to the root_dir
  full_path <- file.path(root_dir, file)
  if (file.exists(full_path)) {
     return(normalizePath(full_path, winslash = "/"))
  }

  # Fallback to searching by basename
  fname <- basename(file_norm)

  all_files <- list.files(
    path = root_dir,
    pattern = paste0("^", fname, "$"),
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(all_files) == 0) {
    return(NULL)
  }

  if (length(all_files) > 1) {
    warning("Found multiple files named '", fname, "'. Using the first one found: ", all_files[1])
  }

  normalizePath(all_files[1], winslash = "/")
}


#' Locate the AI response file for the modification add-in.
#'
#' Follows a search hierarchy to find the response file:
#' 1. The currently active file in RStudio, if it ends with `.ai_resp.txt`.
#' 2. A file named `ai_resp.txt` in the project root.
#' 3. The first file matching `*.ai_resp.txt` in the project root.
#'
#' @return The path to the AI response file.
#' @keywords internal
find_ai_response_file <- function() {
  # 1. Active editor file
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable("1.1.287")) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx$path) && nzchar(ctx$path) && grepl("\\.ai_resp\\.txt$", ctx$path, ignore.case = TRUE)) {
      return(normalizePath(ctx$path, winslash = "/"))
    }
  }

  proj_dir <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  if (is.null(proj_dir)) proj_dir <- getwd()

  # 2. `ai_resp.txt` in project root
  standard_file <- file.path(proj_dir, "ai_resp.txt")
  if (file.exists(standard_file)) {
    return(normalizePath(standard_file, winslash = "/"))
  }

  # 3. First `*.ai_resp.txt` in project root
  pattern_files <- list.files(proj_dir, pattern = "\\.ai_resp\\.txt$", full.names = TRUE, ignore.case = TRUE)
  if (length(pattern_files) > 0) {
    return(normalizePath(sort(pattern_files)[1], winslash = "/"))
  }

  stop("Could not find an AI response file (e.g., 'ai_resp.txt' or '*.ai_resp.txt') in the project root or active editor.")
}
