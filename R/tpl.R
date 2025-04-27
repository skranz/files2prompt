tpl_replace_whisker = function(txt, values, open = "{{", close = "}}", require_all_value=FALSE) {
  restore.point("tpl_replace_whisker")
  # Define regex with a capturing group for content inside {{ }}
  pattern = tpl_var_pattern(open, close)

  # Locate positions of all matches in the string
  matches <- stri_match_all_regex(txt, pattern,omit_no_match = TRUE)[[1]]
  if (NROW(matches)==0) return(txt)

  symbols = unique(matches[,2])

  null_inds = which(sapply(values, is.null))
  values[null_inds] = as.list(rep("",length(null_inds)))

  vars = names(values)

  missing = setdiff(symbols, vars)
  if (length(missing)>0 & require_all_value) {
    stop("The whisker symbols ", paste0(missing, collapse=", "), " are not in values.")
  } else if (length(missing) >0) {
    symbols = intersect(symbols, vars)
  }
  if (length(symbols)==0) return(txt)

  values = values[symbols]
  whiskers =  paste0(open, symbols,close)


  val_len = sapply(values, length)
  is_one = val_len <= 1


  one_vals = unlist(values[is_one])
  # Replace all occurrences simultaneously using fixed replacement
  if (length(one_vals)>0) {
    txt <- stri_replace_all_fixed(txt, whiskers[is_one], one_vals, vectorize_all = FALSE)

  }

  # values that are vectors
  # loop through all values
  inds = which(!is_one)
  i = 1
  for (i in inds) {
    txt <- stri_replace_all_fixed(txt, whiskers[i], values[[i]], vectorize_all = TRUE)
  }

  txt
}


example = function() {
  tpl = "Hi {{name}}!"
  escape_regex(c("{{","{[]}"))
}

escape_regex <- function(patterns) {
  stringi::stri_replace_all_regex(
    patterns,
    "([\\\\\\^\\$\\.\\*\\+\\?\\(\\)\\[\\]\\{\\}\\|])",
    "\\\\$1"
  )
}

tpl_var_pattern = function(open = "{{", close = "}}") {
  open = escape_regex(open)
  close = escape_regex(close)
  pattern <- paste0(open,"\\s*(.*?)\\s*", close)
}

tpl_vars = function(txt, open = "{{", close = "}}") {
  pattern = tpl_var_pattern(open, close)
  #pattern <- "\\{\\{\\s*(.*?)\\s*\\}\\}"
  vars <- unique(stri_match_all_regex(txt, pattern)[[1]][,2])
  vars
}
