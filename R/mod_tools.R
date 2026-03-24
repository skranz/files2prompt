#' Recursively find a file in a project directory by its basename.
#' @param file The basename of the file to find (e.g., "utils.R").
#' @param root_dir The project's root directory.
#' @return The full, normalized path to the file, or NULL if not found.
#'         Issues a warning if multiple files with the same name exist.
#' @keywords internal
find_project_file = function(file, root_dir) {
  file_norm = gsub("\\\\", "/", file)

  if (is_absolute_path(file) && file.exists(file)) {
    return(normalizePath(file, winslash = "/"))
  }

  full_path = file.path(root_dir, file)
  if (file.exists(full_path)) {
    return(normalizePath(full_path, winslash = "/"))
  }

  has_path_sep = grepl("[/\\\\]", file_norm)
  if (has_path_sep) {
    return(NULL)
  }

  fname = basename(file_norm)

  all_files = list.files(
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
#' 1. The currently active file in RStudio, if it ends with `.ai_resp.md`.
#' 2. A file named `ai_resp.md` in the project root.
#' 3. The first file matching `*.ai_resp.md` in the project root.
#'
#' @return The path to the AI response file.
#' @keywords internal
find_ai_response_file <- function() {
  # 1. Active editor file
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable("1.1.287")) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx$path) && nzchar(ctx$path) && grepl("\\.ai_resp\\.md$", ctx$path, ignore.case = TRUE)) {
      return(normalizePath(ctx$path, winslash = "/"))
    }
  }

  proj_dir <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  if (is.null(proj_dir)) proj_dir <- getwd()

  # 2. `ai_resp.md` in project root
  standard_file <- file.path(proj_dir, "ai_resp.md")
  if (file.exists(standard_file)) {
    return(normalizePath(standard_file, winslash = "/"))
  }

  # 3. First `*.ai_resp.md` in project root
  pattern_files <- list.files(proj_dir, pattern = "\\.ai_resp\\.md$", full.names = TRUE, ignore.case = TRUE)
  if (length(pattern_files) > 0) {
    return(normalizePath(sort(pattern_files)[1], winslash = "/"))
  }

  stop("Could not find an AI response file (e.g., 'ai_resp.md' or '*.ai_resp.md') in the project root or active editor.")
}

#' Read or write the last-prompt file index for mod2
#'
#' @param project_dir Project root directory.
#' @param files Optional character vector of relative paths (one per line) when action = "write".
#' @param action "read" or "write".
#' @return For "read": character vector of relative paths (may be empty).
#' @keywords internal
f2p_last_prompt_index <- function(project_dir, files = NULL, action = c("read", "write")) {
  action = match.arg(action)
  project_dir = normalizePath(project_dir, mustWork = FALSE)
  path = file.path(project_dir, "f2p_last_prompt_files.txt")

  if (action == "write") {
    if (is.null(files)) files = character(0)
    files = unique(as.character(files))
    # Keep only simple relative paths
    files = files[nzchar(files)]
    dir.create(project_dir, showWarnings = FALSE, recursive = TRUE)
    con <- file(path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(files, con, sep = "\n", useBytes = TRUE)
    return(invisible(path))
  } else {
    if (!file.exists(path)) return(character(0))
    out <- readLines(path, warn = FALSE)
    out <- out[nzchar(out)]
    out
  }
}

