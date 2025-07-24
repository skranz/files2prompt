#' Locate the target for a modification
#'
#' This is the main dispatcher function. It takes a `mod` object, finds its
#' full file path, and determines the exact start and end lines for the edit.
#' It populates `mod$meta` with `file_path`, `start_line`, `end_line`, and
#' several status fields: `location_found`, `location_is_fuzzy`, `location_error`.
#' It also populates `potential_locations` for the "Find" functionality.
#'
#' @param mod A single parsed modification object.
#' @param project_dir The root directory of the project.
#' @return The modified `mod` object with location information.
mod_locate_target <- function(mod, project_dir) {
  restore.point("mod_locate_target")

  # Handle case where TOML parsing failed upstream
  if (isTRUE(mod$meta$parse_error)) {
    mod$meta$location_found <- FALSE
    mod$meta$location_is_fuzzy <- FALSE
    mod$meta$location_error <- mod$meta$parse_error_message %||% "TOML metadata could not be parsed."
    mod$meta$start_line <- 1
    mod$meta$end_line <- 0
    return(mod)
  }

  scope <- mod$meta$scope

  # Initialize meta fields for location status
  mod$meta$location_found <- FALSE
  mod$meta$location_is_fuzzy <- FALSE
  mod$meta$location_error <- NULL
  mod$meta$potential_locations <- list()
  mod$meta$num_potential_locations <- 0
  mod$meta$current_match_index <- 0


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
  loc_result <- switch(scope,
    "file" = locate_scope_file(mod),
    "function" = locate_scope_function(mod),
    "lines" = locate_scope_lines(mod),
    list(locations = list(),
         error_msg = paste("Unknown modification scope:", scope))
  )

  # 3. Process the location results
  mod$meta$potential_locations <- loc_result$locations
  mod$meta$num_potential_locations <- length(loc_result$locations)

  if (mod$meta$num_potential_locations > 0) {
    mod$meta$location_found <- TRUE
    mod$meta$current_match_index <- 1
    first_loc <- loc_result$locations[[1]]
    mod$meta$start_line <- first_loc$start
    mod$meta$end_line <- first_loc$end
    mod$meta$location_is_fuzzy <- first_loc$is_fuzzy
    mod$meta$location_error <- NULL
  } else {
    mod$meta$location_found <- FALSE
    mod$meta$start_line <- 1
    mod$meta$end_line <- 0
    mod$meta$location_error <- loc_result$error_msg
  }

  mod
}


# --- Scope-specific Location Finders ---

locate_scope_file <- function(mod) {
  restore.point("locate_scope_file")
  # For `scope="file"`, we replace the whole file.
  if (!file.exists(mod$meta$file_path)) { # New file
    loc <- list(start = 1, end = 0, found = TRUE, is_fuzzy = FALSE)
  } else {
    line_count <- length(readLines(mod$meta$file_path, warn = FALSE))
    loc <- list(start = 1, end = line_count, found = TRUE, is_fuzzy = FALSE)
  }
  list(locations = list(loc), error_msg = NULL)
}

locate_scope_function <- function(mod) {
  restore.point("locate_scope_function")
  target_file <- mod$meta$file_path
  if (!file.exists(target_file)) {
    return(list(locations = list(),
                error_msg = paste0("File '", basename(target_file), "' does not exist.")))
  }
  original_lines <- readLines(target_file, warn = FALSE)
  all_funs <- f2p_all_fun_locs(target_file)
  meta = mod$meta

  if ("insert_top" %in% names(meta)) {
    return(list(locations = list(list(start = 1, end = 0, is_fuzzy = FALSE)), error_msg = NULL))
  } else if ("insert_bottom" %in% names(meta)) {
    n_lines <- NROW(original_lines)
    return(list(locations = list(list(start = n_lines + 1, end = n_lines, is_fuzzy = FALSE)), error_msg = NULL))
  }

  fun_name = meta$function_name %||% meta$insert_after_fun %||% meta$insert_before_fun
  if (is.null(fun_name)) {
      return(list(locations = list(),
                  error_msg = "No function name specified for replacement or relative insertion."))
  }

  # Find exact matches
  locs <- all_funs[all_funs$fun_name == fun_name, ]

  # If no exact match, try fuzzy matching
  is_fuzzy_match <- FALSE
  if (nrow(locs) == 0) {
      fuzzy_matches <- agrep(fun_name, all_funs$fun_name, max.distance = 0.2, value = TRUE)
      if (length(fuzzy_matches) > 0) {
          locs <- all_funs[all_funs$fun_name %in% fuzzy_matches, ]
          is_fuzzy_match <- TRUE
      }
  }

  if (NROW(locs) == 0) {
    return(list(locations = list(),
                error_msg = paste0("Function '", fun_name, "' not found in '", basename(target_file), "'.")))
  }

  # Convert data.frame rows to a list of location objects
  potential_locations <- lapply(seq_len(nrow(locs)), function(i) {
    loc <- locs[i, ]
    if ("insert_after_fun" %in% names(meta)) {
      list(start = loc$end_line_fun + 1, end = loc$end_line_fun, is_fuzzy = is_fuzzy_match)
    } else if ("insert_before_fun" %in% names(meta)) {
      list(start = loc$start_line_comment, end = loc$start_line_comment - 1, is_fuzzy = is_fuzzy_match)
    } else {
      # It's a function replacement.
      has_roxygen_comments <- any(grepl("^\\s*#'", strsplit(mod$payload, "\n")[[1]]))
      start_replace_line <- if (has_roxygen_comments) loc$start_line_comment else loc$start_line_fun
      list(start = start_replace_line, end = loc$end_line_fun, is_fuzzy = is_fuzzy_match)
    }
  })

  list(locations = potential_locations, error_msg = NULL)
}

locate_scope_lines <- function(mod) {
  target_file <- mod$meta$file_path
  if (!file.exists(target_file)) {
    return(list(locations = list(),
                error_msg = paste0("File '", basename(target_file), "' does not exist.")))
  }
  original_lines <- readLines(target_file, warn = FALSE)
  meta <- mod$meta

  # --- Insertion based on line content ---
  if (!is.null(meta$insert_after_lines) || !is.null(meta$replace_lines)) {
    sequence_to_find <- strsplit(meta$replace_lines %||% meta$insert_after_lines, "\n")[[1]]

    locations <- find_line_sequence(original_lines, sequence_to_find, approximate = TRUE)

    if (length(locations) == 0) {
      err_key <- if (!is.null(meta$replace_lines)) "replace_lines" else "insert_after_lines"
      return(list(locations = list(),
                  error_msg = paste0("Could not find the '", err_key, "' sequence in '", basename(target_file), "'.")))
    }

    # If it's an insertion, adjust the location
    if (!is.null(meta$insert_after_lines)) {
        locations <- lapply(locations, function(loc) {
            loc$start <- loc$end + 1
            loc$end <- loc$end
            loc
        })
    }
    return(list(locations = locations, error_msg = NULL))
  }

  # --- Insertion based on position (top, bottom, after_fun) ---
  is_fuzzy <- FALSE
  res <- tryCatch({
    all_funs <- if (grepl("\\.R$", target_file, ignore.case = TRUE)) f2p_all_fun_locs(target_file) else NULL
    get_insertion_line(meta, original_lines, all_funs)
  }, error = function(e) e)

  if (inherits(res, "error")) {
    return(list(locations = list(), error_msg = res$message))
  }
  insert_line <- res

  final_loc <- list(start = insert_line, end = insert_line - 1, is_fuzzy = is_fuzzy)
  return(list(locations = list(final_loc), error_msg = NULL))
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
  if (length(sequence_to_find) == 0) return(list())
  len_seq <- length(sequence_to_find)
  len_src <- length(source_lines)
  if (len_seq > len_src) return(list())

  # 1. Find all exact matches
  exact_matches <- list()
  for (i in 1:(len_src - len_seq + 1)) {
    chunk <- source_lines[i:(i + len_seq - 1)]
    if (all(chunk == sequence_to_find)) {
      exact_matches[[length(exact_matches) + 1]] <- list(start = i, end = i + len_seq - 1, is_fuzzy = FALSE, dist = 0)
    }
  }
  if (length(exact_matches) > 0) {
    return(exact_matches)
  }

  # 2. If no exact match and approximate is false, return empty list
  if (!approximate) return(list())

  # 3. Find all approximate matches
  seq_str <- paste(sequence_to_find, collapse = "\n")
  all_matches <- list()
  for (i in 1:(len_src - len_seq + 1)) {
    chunk_lines <- source_lines[i:(i + len_seq - 1)]
    chunk_str <- paste(chunk_lines, collapse = "\n")
    dist <- utils::adist(seq_str, chunk_str, ignore.case = TRUE, costs = 1)[1, 1]

    normalized_dist <- dist / nchar(seq_str)
    if (is.finite(normalized_dist) && normalized_dist <= max_dist) {
        all_matches[[length(all_matches) + 1]] <- list(start = i, end = i + len_seq - 1, is_fuzzy = TRUE, dist = normalized_dist)
    }
  }

  # Sort matches by distance (best first)
  if (length(all_matches) > 0) {
    all_matches <- all_matches[order(sapply(all_matches, `[[`, "dist"))]
    cat(paste("Used approximate matching to find", length(all_matches), "potential line sequence(s).\n"))
  }

  return(all_matches)
}


f2p_all_fun_locs <- function(file_path) {
  restore.point("fp_get_all_function_locations")
  code = readLines(file_path)
  funs_loc = extract_function_source(code)
  funs_loc
}
