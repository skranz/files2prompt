# FILE: f2p_data.R
#' @importFrom utils read.csv str head
#' @importFrom tools file_ext

#' Get common data file extensions
#' @return A character vector of file extensions.
#' @keywords internal
fp_data_extensions <- function() {
  c("rds", "csv")
}

#' @title Convert data frame to text using head()
#' @param df The data frame
#' @param data_max_rows Number of rows to show
#' @param ... Unused
#' @keywords internal
fp_to_text_head <- function(df, data_max_rows = 6, ...) {
  if (!is.data.frame(df)) return(fp_to_text_str(df))
  # Capture the print output of head()
  paste(capture.output(print(utils::head(df, n = as.integer(data_max_rows)))), collapse = "\n")
}

#' @title Convert data object to text using str()
#' @param df The data object
#' @param ... Unused
#' @keywords internal
fp_to_text_str <- function(df, ...) {
  # Capture the output of str()
  paste(capture.output(utils::str(df)), collapse = "\n")
}

#' @title Convert data frame to text with a glimpse-style summary
#' @param df The data frame
#' @param data_max_rows Number of rows to show in the glimpse
#' @param data_max_cols Number of columns to show
#' @param data_max_text_width Max width for text output
#' @param ... Unused
#' @keywords internal
fp_to_text_glimpse <- function(df, data_max_rows = 10, data_max_cols = NULL, data_max_text_width = 80, ...) {
  if (!is.data.frame(df) && !is.matrix(df)) return(fp_to_text_str(df))
  if (is.matrix(df)) df <- as.data.frame(df)

  # Ensure params are numeric
  data_max_rows <- as.integer(data_max_rows)
  if(!is.null(data_max_cols)) data_max_cols <- as.integer(data_max_cols)
  data_max_text_width <- as.integer(data_max_text_width)

  orig_ncols <- ncol(df)
  df_preview <- if (!is.null(data_max_cols) && orig_ncols > data_max_cols) {
    df[, 1:data_max_cols, drop = FALSE]
  } else {
    df
  }

  info <- paste0(
    "Data Summary\n", "Rows: ", nrow(df), "\n", "Columns: ", ncol(df), "\n"
  )
  if (!is.null(data_max_cols) && orig_ncols > data_max_cols) {
    info <- paste0(info, "Showing first ", data_max_cols, " of ", orig_ncols, " columns.\n")
  }
  info <- paste0(info, "---\n")

  df_rows_preview <- utils::head(df_preview, n = data_max_rows)
  col_details <- vapply(seq_along(df_rows_preview), function(i) {
    col_name <- names(df_rows_preview)[i]
    col_full_vec <- df[[names(df_preview)[i]]]
    col_type <- paste0("<", class(col_full_vec)[1], ">")
    col_vals <- format(df_rows_preview[[i]], trim = TRUE, justify = "none")
    col_vals <- substr(col_vals, 1, 25)
    col_vals_str <- paste(col_vals, collapse = ", ")
    line <- sprintf("%-20s %-10s %s", paste0("$ ", col_name), col_type, col_vals_str)
    if (nchar(line) > data_max_text_width) {
      line <- paste0(substr(line, 1, data_max_text_width - 4), " ...")
    }
    line
  }, character(1))

  paste0(info, paste(col_details, collapse = "\n"))
}

#' Renders a data file into a text summary.
#'
#' This function acts as a dispatcher. It reads a data file, then uses settings
#' from the TOML group configuration to select and call an appropriate rendering
#' function (e.g., 'glimpse', 'head', 'str') to generate a text summary.
#'
#' @param file_path Path to the data file.
#' @param group A list containing the configuration for the current file group.
#' @return A character string with the summary of the data file.
#' @keywords internal
fp_render_data_file <- function(file_path, group) {
  ext <- tolower(tools::file_ext(file_path))
  df <- tryCatch({
    switch(ext,
      "rds"   = readRDS(file_path),
      "csv"   = utils::read.csv(file_path, stringsAsFactors = FALSE),
      stop("Unsupported data file extension for rendering: ", ext)
    )
  }, error = function(e) {
    return(paste("Error reading data file", basename(file_path), ":\n", e$message))
  })

  if (!is.data.frame(df) && !is.matrix(df)) {
      # For lists (from .rda) or other objects, str is the best general summary
      return(fp_to_text_str(df))
  }

  renderer_name <- group$opt_data_renderer %||% "glimpse"
  opts <- group[c("opt_max_rows", "opt_max_cols", "opt_max_text_width")]
  opts <- opts[!sapply(opts, is.null)]
  args <- c(list(df = df), opts)

  render_fun = paste0("fp_to_text_", renderer_name)
  do.call(render_fun, args)
}

