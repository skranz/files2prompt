# FILE: R/mod_apply.R
#' @importFrom restorepoint restore.point
#' @keywords internal
apply_modification <- function(mod, project_dir) {
  restore.point("apply_modification")
  scope <- mod$meta$scope

  # --- Find target file ---
  target_file <- find_project_file(mod$meta$file, project_dir)
  is_new <- is.null(target_file)
  if (is_new && !isTRUE(mod$meta$is_new_file)) {
      stop("Could not find file '", mod$meta$file, "' and it's not marked as a new file.")
  }
  if (!is_new && isTRUE(mod$meta$is_new_file)) {
      warning("File '", mod$meta$file, "' already exists but is_new_file is true. It will be overwritten.")
  }
  if (is_new) {
    target_file <- file.path(project_dir, mod$meta$file)
  }

  # --- Dispatch based on scope ---
  switch(scope,
    "file" = apply_file_scope(mod, target_file, is_new),
    "function" = apply_function_scope(mod, target_file),
    "lines" = apply_lines_scope(mod, target_file),
    stop("Unknown modification scope: '", scope, "'")
  )

  cat("Successfully applied '", scope, "' to '", mod$meta$file, "'.\n", sep = "")
  return(invisible(TRUE))
}


# --- SCOPE-SPECIFIC HANDLERS ---

#' Handle scope = "file"
#' @keywords internal
apply_file_scope <- function(mod, target_file, is_new) {
  if (!isTRUE(mod$meta$is_new_file) && !file.exists(target_file)) {
    stop("File '", target_file, "' to be rewritten does not exist.")
  }
  dir.create(dirname(target_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(mod$payload, target_file)

  # If in RStudio, open the newly created/modified file
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::navigateToFile(target_file)
  }
}

#' Handle scope = "function"
#' @keywords internal
apply_function_scope <- function(mod, target_file) {
  restore.point("apply_function_scope")
  if (!file.exists(target_file)) stop("File '", target_file, "' does not exist.")
  original_lines <- readLines(target_file, warn = FALSE)
  all_funs <- fp_get_all_function_locations(target_file)

  # --- Case 1: Replace an existing function ---
  if (!is.null(mod$meta$function_name)) {
    loc <- all_funs[[mod$meta$function_name]]
    if (is.null(loc)) {
      stop("Function '", mod$meta$function_name, "' not found in '", basename(target_file), "'.")
    }
    modify_rstudio_doc(target_file, loc$start_line, loc$end_line, mod$payload)
    return()
  }

  # --- Case 2: Insert a new function ---
  insert_line <- get_insertion_line(mod$meta, original_lines, all_funs)
  modify_rstudio_doc(target_file, insert_line, insert_line - 1, mod$payload)
}

#' Handle scope = "lines"
#' @keywords internal
apply_lines_scope <- function(mod, target_file) {
  if (!file.exists(target_file)) stop("File '", target_file, "' does not exist.")
  original_lines <- readLines(target_file, warn = FALSE)

  # --- Case 1: Replace lines ---
  if (!is.null(mod$meta$replace_lines)) {
    lines_to_replace <- strsplit(mod$meta$replace_lines, "\n")[[1]]
    loc <- find_line_sequence(original_lines, lines_to_replace)
    if (is.null(loc)) {
      stop("Could not find the 'replace_lines' sequence in '", basename(target_file), "'.")
    }
    modify_rstudio_doc(target_file, loc$start, loc$end, mod$payload)
    return()
  }
  # --- Case 2: Insert lines ---
  insert_after <- strsplit(mod$meta$insert_after_lines %||% "", "\n")[[1]]
  if(length(insert_after) > 0 && nchar(insert_after[1]) > 0) {
      loc <- find_line_sequence(original_lines, insert_after)
      if(is.null(loc)) stop("Could not find 'insert_after_lines' sequence.")
      insert_line <- loc$end + 1
  } else {
      all_funs <- if (grepl("\\.R$", target_file, ignore.case = TRUE)) fp_get_all_function_locations(target_file) else list()
      insert_line <- get_insertion_line(mod$meta, original_lines, all_funs)
  }
  modify_rstudio_doc(target_file, insert_line, insert_line - 1, mod$payload)
}


# --- HELPERS ---

#' Use rstudioapi to perform the modification
#' @keywords internal
modify_rstudio_doc <- function(file, start_line, end_line, text) {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("This operation requires the RStudio API.")
  }
  rstudioapi::navigateToFile(file, line = start_line)

  # The API needs a document range
  # For insertion, end_line will be less than start_line, creating a zero-length range
  range <- rstudioapi::document_range(
    rstudioapi::document_position(start_line, 1),
    rstudioapi::document_position(end_line + 1, 1)
  )
  rstudioapi::modifyRange(range, text)
}

#' Determine the line number for an insertion
#' @keywords internal
get_insertion_line <- function(meta, lines, fun_locs) {
  if (isTRUE(meta$insert_top)) return(1)
  if (isTRUE(meta$insert_bottom)) return(length(lines) + 1)

  if (!is.null(meta$insert_after_fun)) {
    loc <- fun_locs[[meta$insert_after_fun]]
    if (is.null(loc)) stop("Function '", meta$insert_after_fun, "' not found for insertion.")
    return(loc$end_line + 1)
  }
  if (!is.null(meta$insert_before_fun)) {
    loc <- fun_locs[[meta$insert_before_fun]]
    if (is.null(loc)) stop("Function '", meta$insert_before_fun, "' not found for insertion.")
    return(loc$start_line)
  }
  stop("Invalid insertion criteria specified.")
}

#' Find a sequence of lines within a vector of lines
#' @return A list with start and end line numbers, or NULL
#' @keywords internal
find_line_sequence <- function(source_lines, sequence_to_find) {
  if (length(sequence_to_find) == 0) return(NULL)
  len_seq <- length(sequence_to_find)
  len_src <- length(source_lines)
  if (len_seq > len_src) return(NULL)

  for (i in 1:(len_src - len_seq + 1)) {
    chunk <- source_lines[i:(i + len_seq - 1)]
    if (all(chunk == sequence_to_find)) {
      return(list(start = i, end = i + len_seq - 1))
    }
  }
  return(NULL)
}
