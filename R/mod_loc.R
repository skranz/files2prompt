# FILE: R/mod_loc.R

#' @importFrom utils getParseData adist
#' @keywords internal

# --- Main Dispatcher ---

#' Locate the target for a modification
#'
#' This is the main dispatcher function. It takes a `mod` object, finds its
#' full file path, and determines the exact start and end lines for the edit.
#' It populates `mod$meta` with `file_path`, `start_line`, `end_line`, and
#' several status fields: `location_found`, `location_is_fuzzy`, `location_error`.
#'
#' @param mod A single parsed modification object.
#' @param project_dir The root directory of the project.
#' @return The modified `mod` object with location information.
mod_locate_target <- function(mod, project_dir) {
  restore.point("mod_locate_target")
  scope <- mod$meta$scope

  # Initialize meta fields for location status
  mod$meta$location_found <- FALSE
  mod$meta$location_is_fuzzy <- FALSE
  mod$meta$location_error <- NULL

  # 1. Find target file
  target_file <- find_project_file(mod$meta$file, project_dir)
  is_new <- is.null(target_file)

  if (is_new && !isTRUE(mod$meta$is_new_file)) {
    mod$meta$location_error <- paste0("Could not find file '", mod$meta$file, "' and it's not marked as a new file.")
    mod$meta$start_line <- 1
    mod$meta$end_line <- 0
    return(mod)
  }
  if (!is_new && isTRUE(mod$meta$is_new_file)) {
    warning("File '", mod$meta$file, "' already exists but is_new_file is true. It may be overwritten.")
  }
  if (is_new) {
    target_file <- file.path(project_dir, mod$meta$file)
  }
  mod$meta$file_path <- target_file

  # 2. Dispatch to find lines based on scope
  loc <- switch(scope,
    "file" = locate_scope_file(mod),
    "function" = locate_scope_function(mod),
    "lines" = locate_scope_lines(mod),
    list(start = 1, end = 0, found = FALSE, is_fuzzy = FALSE,
         error_msg = paste("Unknown modification scope:", scope))
  )

  mod$meta$start_line <- loc$start
  mod$meta$end_line <- loc$end
  mod$meta$location_found <- isTRUE(loc$found)
  mod$meta$location_is_fuzzy <- isTRUE(loc$is_fuzzy)
  mod$meta$location_error <- loc$error_msg
  mod
}


# --- Scope-specific Location Finders ---

locate_scope_file <- function(mod) {
  restore.point("locate_scope_file")
  # For `scope="file"`, we replace the whole file.
  if (!file.exists(mod$meta$file_path)) { # New file
    return(list(start = 1, end = 0, found = TRUE, is_fuzzy = FALSE)) # Insertion at line 1
  }
  line_count <- length(readLines(mod$meta$file_path, warn = FALSE))
  list(start = 1, end = line_count, found = TRUE, is_fuzzy = FALSE)
}

locate_scope_function <- function(mod) {
  restore.point("locate_scope_function")
  target_file <- mod$meta$file_path
  if (!file.exists(target_file)) {
    # This case is for new files, handled by file scope or insert_top/bottom
    # for an existing file that's now missing.
    return(list(start = 1, end = 0, found = FALSE, is_fuzzy = FALSE,
                error_msg = paste0("File '", basename(target_file), "' does not exist.")))
  }
  original_lines <- readLines(target_file, warn = FALSE)
  all_funs <- f2p_all_fun_locs(target_file)
  meta = mod$meta

  if ("insert_top" %in% names(meta)) {
    return(list(start = 1, end = 0, found = TRUE, is_fuzzy = FALSE))
  } else if ("insert_bottom" %in% names(meta)) {
    return(list(start = NROW(original_lines)+1, end = NROW(original_lines), found = TRUE, is_fuzzy = FALSE))
  }

  fun_name = meta$function_name %||% meta$insert_after_fun %||% meta$insert_before_fun
  if (is.null(fun_name)) {
      return(list(start = 1, end = 0, found = FALSE, is_fuzzy = FALSE,
                  error_msg = "No function name specified for replacement or relative insertion."))
  }

  loc <- all_funs[all_funs$fun_name==fun_name,]
  if (NROW(loc)==0) {
    return(list(start = 1, end = 0, found = FALSE, is_fuzzy = FALSE,
                error_msg = paste0("Function '", fun_name, "' not found in '", basename(target_file), "'.")))
  }
  loc = loc[1,]

  if ("insert_after_fun" %in% names(meta)) {
    return(list(start = loc$end_line_fun + 1, end = loc$end_line_fun, found = TRUE, is_fuzzy = FALSE))
  } else if ("insert_before_fun" %in% names(meta)) {
    return(list(start = loc$start_line_comment, end = loc$start_line_comment - 1, found = TRUE, is_fuzzy = FALSE))
  } else {
    return(list(start = loc$start_line_comment, end = loc$end_line_fun, found = TRUE, is_fuzzy = FALSE))
  }
}

locate_scope_lines <- function(mod) {
  target_file <- mod$meta$file_path
  if (!file.exists(target_file)) {
    return(list(start = 1, end = 0, found = FALSE, is_fuzzy = FALSE,
                error_msg = paste0("File '", basename(target_file), "' does not exist.")))
  }
  original_lines <- readLines(target_file, warn = FALSE)

  # Case 1: Replace lines
  if (!is.null(mod$meta$replace_lines)) {
    lines_to_replace <- strsplit(mod$meta$replace_lines, "\n")[[1]]
    loc <- find_line_sequence(original_lines, lines_to_replace, approximate = TRUE)
    if (is.null(loc)) {
      return(list(start = 1, end = 0, found = FALSE, is_fuzzy = FALSE,
                  error_msg = paste0("Could not find the 'replace_lines' sequence in '", basename(target_file), "'.")))
    }
    return(list(start = loc$start, end = loc$end, found = TRUE, is_fuzzy = loc$is_fuzzy, error_msg = NULL))
  }

  # Case 2: Insert lines
  insert_after <- strsplit(mod$meta$insert_after_lines %||% "", "\n")[[1]]
  if (length(insert_after) > 0 && nchar(insert_after[1]) > 0) {
    loc <- find_line_sequence(original_lines, insert_after, approximate = TRUE)
    if (is.null(loc)) {
        return(list(start = 1, end = 0, found = FALSE, is_fuzzy = FALSE,
                    error_msg = paste0("Could not find 'insert_after_lines' sequence in '", basename(target_file), "'.")))
    }
    insert_line <- loc$end + 1
    is_fuzzy <- loc$is_fuzzy
  } else {
    is_fuzzy <- FALSE
    # Use tryCatch for get_insertion_line as it can stop()
    res <- tryCatch({
      all_funs <- if (grepl("\\.R$", target_file, ignore.case = TRUE)) f2p_all_fun_locs(target_file) else NULL
      get_insertion_line(mod$meta, original_lines, all_funs)
    }, error = function(e) e)

    if (inherits(res, "error")) {
        return(list(start = 1, end = 0, found = FALSE, is_fuzzy = FALSE, error_msg = res$message))
    }
    insert_line <- res
  }
  return(list(start = insert_line, end = insert_line - 1, found = TRUE, is_fuzzy = is_fuzzy, error_msg = NULL))
}


# --- Location Helpers ---

get_insertion_line <- function(meta, lines, fun_locs) {
  if (isTRUE(meta$insert_top)) return(1)
  if (isTRUE(meta$insert_bottom)) return(length(lines) + 1)

  fun_name <- meta$insert_after_fun %||% meta$insert_before_fun
  if (is.null(fun_name)) {
    # No function-based insertion, default to bottom of file
    return(length(lines) + 1)
  }

  # If fun_locs is NULL (e.g. not an R file) or empty, we can't find the function.
  if (is.null(fun_locs) || nrow(fun_locs) == 0) {
    stop("Cannot locate functions for insertion because no functions were found in '", meta$file, "'.")
  }

  loc <- fun_locs[fun_locs$fun_name == fun_name, ]
  if (nrow(loc) == 0) {
    stop("Function '", fun_name, "' not found in '", meta$file, "' for insertion.")
  }

  if (!is.null(meta$insert_after_fun)) {
    return(loc$end_line_fun[1] + 1)
  }
  if (!is.null(meta$insert_before_fun)) {
    # Insert before the function's preceding comment block
    return(loc$start_line_comment[1])
  }

  # Fallback: default to bottom of the file if no other criteria match
  return(length(lines) + 1)
}

find_line_sequence <- function(source_lines, sequence_to_find, approximate = FALSE, max_dist = 0.2) {
  if (length(sequence_to_find) == 0) return(NULL)
  len_seq <- length(sequence_to_find)
  len_src <- length(source_lines)
  if (len_seq > len_src) return(NULL)

  # 1. Try exact match first
  for (i in 1:(len_src - len_seq + 1)) {
    chunk <- source_lines[i:(i + len_seq - 1)]
    if (all(chunk == sequence_to_find)) {
      return(list(start = i, end = i + len_seq - 1, is_fuzzy = FALSE))
    }
  }

  # 2. If exact match fails and approximate is false, return NULL
  if (!approximate) return(NULL)

  # 3. Try approximate matching
  seq_str <- paste(sequence_to_find, collapse = "\n")
  best_match <- list(start = -1, dist = Inf)
  for (i in 1:(len_src - len_seq + 1)) {
    chunk_lines <- source_lines[i:(i + len_seq - 1)]
    chunk_str <- paste(chunk_lines, collapse = "\n")
    dist <- utils::adist(seq_str, chunk_str, ignore.case = TRUE, costs = 1)[1, 1]
    if (dist < best_match$dist) {
      best_match <- list(start = i, end = i + len_seq - 1, dist = dist)
    }
  }

  normalized_dist <- best_match$dist / nchar(seq_str)
  if (is.finite(normalized_dist) && normalized_dist <= max_dist) {
    warning(paste("Used approximate matching to find line sequence at line", best_match$start))
    return(list(start = best_match$start, end = best_match$end, is_fuzzy = TRUE))
  }

  return(NULL)
}


f2p_all_fun_locs <- function(file_path) {
  restore.point("fp_get_all_function_locations")
  code = readLines(file_path)
  funs_loc = extract_function_source(code)
  funs_loc
}
