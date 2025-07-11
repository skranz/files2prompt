# FILE: R/mod_r_parser.R
#' @importFrom utils getParseData
#' @keywords internal
fp_parse_r_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  # Using try-catch to handle non-parsable R files gracefully
  tryCatch({
    # parse() with keep.source is necessary for getParseData to work
    parsed_content <- parse(file = file_path, keep.source = TRUE)
    utils::getParseData(parsed_content, includeText = TRUE)
  }, error = function(e) {
    stop("Failed to parse R file '", basename(file_path), "': ", e$message)
  })
}

#' Find the source range of a function, including its roxygen comments.
#' @param parsed_df A data frame from `getParseData()`.
#' @param function_name The name of the function to find.
#' @return A list with start and end line/column, or NULL if not found.
#' @keywords internal
fp_find_function_source <- function(parsed_df, function_name) {
  if (is.null(parsed_df) || nrow(parsed_df) == 0) return(NULL)

  # Find all function definitions
  fun_defs <- parsed_df[parsed_df$token == "FUNCTION", ]
  if (nrow(fun_defs) == 0) return(NULL)

  # Find the specific function name declaration
  target_fun_node <- parsed_df[
    parsed_df$token == "SYMBOL_FUNCTION_CALL" & parsed_df$text == function_name,
  ]
  if (nrow(target_fun_node) != 1) return(NULL) # Must be unique

  # Find the 'expr' parent that defines the function
  parent_expr_id <- target_fun_node$parent
  fun_expr <- parsed_df[parsed_df$id == parent_expr_id, ]
  if (nrow(fun_expr) == 0) return(NULL)

  start_line <- fun_expr$line1
  end_line <- fun_expr$line2
  end_col <- fun_expr$col2

  # Look for preceding comments (walk backwards from the function expr)
  # Comments are siblings to the function's parent expression
  parent_of_parent_id <- fun_expr$parent
  siblings <- parsed_df[parsed_df$parent == parent_of_parent_id, ]
  fun_and_sibs <- siblings[siblings$line1 <= start_line, ]

  # Find comments immediately preceding the function
  prev_tokens <- fun_and_sibs[order(fun_and_sibs$line1, decreasing = TRUE), ]
  comment_start_line <- start_line
  for (i in seq_len(nrow(prev_tokens))) {
    token_row <- prev_tokens[i, ]
    if (token_row$id == fun_expr$id) next # Skip the function expression itself
    if (token_row$token == "COMMENT") {
      comment_start_line <- token_row$line1
    } else {
      # Stop when we hit a non-comment token
      break
    }
  }

  list(
    start_line = comment_start_line,
    end_line = end_line,
    start_col = 1, # Always take the full line for comments
    end_col = end_col
  )
}

#' Get location of all functions in a file
#' @param file_path Path to the R file.
#' @return A named list where names are function names and values are locations.
#' @keywords internal
fp_get_all_function_locations <- function(file_path) {
  parsed_df <- fp_parse_r_file(file_path)
  if (is.null(parsed_df)) return(list())

  fun_symbols <- parsed_df[parsed_df$token == "SYMBOL_FUNCTION_CALL", "text"]
  unique_funs <- unique(fun_symbols)

  locations <- lapply(unique_funs, function(fun_name) {
    fp_find_function_source(parsed_df, fun_name)
  })
  names(locations) <- unique_funs
  Filter(Negate(is.null), locations)
}
