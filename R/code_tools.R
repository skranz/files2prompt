
#' Extract locations of all named functions in a piece of code
#'
#' @param code A single character string or character vector containing R source code.
#' @return A tibble with one row per directly assigned function and columns:
#'         * fun_name - the symbol on the LHS of the assignment
#'         * start_line_fun - line where the function assignment starts
#'         * end_line_fun - line where the function assignment ends
#'         * start_line_comment - first line of the contiguous comment block immediately above
extract_function_source = function(code) {
  restore.point("extract_function_source")
  library(tibble)
  library(dplyr)

  stopifnot(is.character(code))
  code = paste0(code, collapse = "\n")
  if (is.null(code) || nchar(trimws(code)) == 0) return(tibble())

  pd = tryCatch(
    utils::getParseData(parse(text = code, keep.source = TRUE)),
    error = function(e) {
      warning(
        "Code parsing failed in extract_function_source ",
        "(this is ok if the file is not R code or contains syntax errors). Error: ",
        e$message
      )
      return(NULL)
    }
  )

  if (is.null(pd) || nrow(pd) == 0) {
    return(tibble())
  }

  lines_vec = stringi::stri_split_fixed(code, "\n")[[1]]
  fun_tokens = pd %>% filter(token == "FUNCTION")

  if (nrow(fun_tokens) == 0) {
    return(tibble())
  }

  get_direct_assignment_expr = function(function_expr_id) {
    parent_id = pd$parent[pd$id == function_expr_id]
    if (length(parent_id) != 1 || is.na(parent_id)) return(NA_integer_)

    sibling_rows = which(pd$parent == parent_id)
    sibling_tokens = pd$token[sibling_rows]

    has_assignment = any(sibling_tokens %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))
    if (!has_assignment) return(NA_integer_)

    child_expr_ids = pd$id[sibling_rows[pd$token[sibling_rows] == "expr"]]
    if (!(function_expr_id %in% child_expr_ids)) return(NA_integer_)

    parent_id
  }

  get_lhs_name = function(assign_expr_id, function_expr_id) {
    child_rows = which(pd$parent == assign_expr_id)
    child_rows = child_rows[order(pd$col1[child_rows])]

    assign_rows = child_rows[pd$token[child_rows] %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")]
    if (length(assign_rows) == 0) return(NA_character_)

    assign_col = pd$col1[assign_rows[1]]

    if (pd$token[assign_rows[1]] == "RIGHT_ASSIGN") {
      lhs_expr_rows = child_rows[pd$token[child_rows] == "expr" & pd$col1[child_rows] > assign_col]
    } else {
      lhs_expr_rows = child_rows[pd$token[child_rows] == "expr" & pd$col2[child_rows] < assign_col]
    }

    if (length(lhs_expr_rows) == 0) return(NA_character_)

    lhs_expr_id = pd$id[lhs_expr_rows[1]]
    desc_rows = which(pd$parent == lhs_expr_id | pd$parent %in% pd$id[pd$parent == lhs_expr_id])
    symbol_rows = desc_rows[pd$token[desc_rows] == "SYMBOL"]

    if (length(symbol_rows) == 0) {
      symbol_rows = which(pd$parent == lhs_expr_id & pd$token == "SYMBOL")
    }

    if (length(symbol_rows) == 0) return(NA_character_)
    pd$text[symbol_rows[1]]
  }

  get_comment_start = function(start_line_fun) {
    if (is.na(start_line_fun) || start_line_fun <= 1) return(start_line_fun)

    i = start_line_fun - 1
    while (i >= 1) {
      line = stringi::stri_trim_both(lines_vec[i])
      if (!stringi::stri_startswith_fixed(line, "#")) break
      i = i - 1
    }

    i + 1
  }

  res = bind_rows(lapply(seq_len(nrow(fun_tokens)), function(i) {
    ft = fun_tokens[i, ]
    function_expr_id = ft$parent

    assign_expr_id = get_direct_assignment_expr(function_expr_id)
    if (is.na(assign_expr_id)) return(NULL)

    assign_row = which(pd$id == assign_expr_id)
    if (length(assign_row) != 1) return(NULL)

    fun_name = get_lhs_name(assign_expr_id, function_expr_id)
    if (is.na(fun_name) || !nzchar(fun_name)) return(NULL)

    start_line_fun = pd$line1[assign_row]
    end_line_fun = pd$line2[assign_row]

    tibble(
      fun_name = fun_name,
      start_line_fun = start_line_fun,
      end_line_fun = end_line_fun,
      start_line_comment = get_comment_start(start_line_fun)
    )
  }))

  if (nrow(res) == 0) {
    return(tibble(
      fun_name = character(0),
      start_line_fun = integer(0),
      end_line_fun = integer(0),
      start_line_comment = integer(0)
    ))
  }

  res %>%
    filter(
      !is.na(fun_name),
      !is.na(start_line_fun),
      !is.na(end_line_fun),
      !is.na(start_line_comment)
    ) %>%
    distinct(fun_name, start_line_fun, end_line_fun, .keep_all = TRUE) %>%
    arrange(start_line_fun)
}

