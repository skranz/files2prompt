
#' Extract locations of all **named** functions in a piece of code
#'
#' @param code A single character string containing R source code.
#' @return A tibble with one row per function and columns:
#'         * fun_name                – the symbol on the LHS of the assignment
#'         * start_line_fun          – line where the `function` keyword sits
#'         * end_line_fun            – line where the body closes (`}`)
#'         * start_line_comment_above– first line of the contiguous
#'                                      comment block immediately above
extract_function_source = function(code) {
  restore.point("extract_function_source")
  library(tibble)
  library(dplyr)
  stopifnot(is.character(code))
  code = paste0(code, collapse="\n")
  if (is.null(code) || nchar(trimws(code)) == 0) return(tibble())

  # Parse once, keep the full source map
  pd <- tryCatch(
    utils::getParseData(parse(text = code, keep.source = TRUE)),
    error = function(e) {
      warning("Code parsing failed in extract_function_source (this is ok if the file is not R code or contains syntax errors). Error: ", e$message)
      return(NULL)
    }
  )

  if (is.null(pd) || nrow(pd) == 0) {
    return(tibble())
  }

  # Helper look-ups ------------------------------------------------------------
  lines_vec = strsplit(code, "\n", fixed = TRUE)[[1]]

  expr_end_line = pd %>%
    filter(token == "expr") %>%
    select(id, line2) %>%
    deframe()                      # named vector: expr id  -> last line

  # FUNCTION tokens (one per definition)
  fun_tokens = pd %>% filter(token == "FUNCTION")

  if (nrow(fun_tokens) == 0) {
    return(tibble())
  }

  i = 1
  # Assemble results -----------------------------------------------------------
  bind_rows(lapply(seq_len(nrow(fun_tokens)), function(i) {
    ft = fun_tokens[i, ]

    func_expr_id = ft$parent                               # expr holding body


    assign_expr_id = pd$parent[pd$id == func_expr_id]      # expr holding "<-"
    assign_row = which(pd$id == assign_expr_id)
    start_line_fun = pd$line1[assign_row]
    end_line_fun = pd$line2[assign_row]


    child_expr_rows = which(pd$parent==assign_expr_id)
    pd$id[child_expr_rows]
    tokens = pd$token[child_expr_rows]
    symbol_rows = child_expr_rows[tokens=="SYMBOL"]
    fun_name = NA_character_
    if (length(symbol_rows) >0) {
      fun_name = pd$text[symbol_rows[1]]
    } else {
      rows = which(pd$parent==pd$id[child_expr_rows[1]])
      tokens = pd$token[rows]
      symbol_rows = rows[tokens=="SYMBOL"]
      if (length(symbol_rows) >0) {
        fun_name = pd$text[symbol_rows[1]]
      }
    }
    start_line_comment = start_line_fun
    comment_rows = which(pd$parent==-assign_expr_id)
    if (length(comment_rows)>0) {
      start_line_comment = min(c(start_line_fun, pd$line1[comment_rows]))
    }

    tibble(
      fun_name = fun_name,
      start_line_fun = start_line_fun,
      end_line_fun = end_line_fun,
      start_line_comment = start_line_comment
    )
  })) %>% arrange(start_line_fun)
}

