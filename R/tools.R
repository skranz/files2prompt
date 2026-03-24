
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

is_absolute_path <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(FALSE)
  }
  path <- path.expand(path)
  grepl("^([A-Za-z]:[/\\\\]|[/\\\\]{2}|/)", path)
}
