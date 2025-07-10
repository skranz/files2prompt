
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


# Add this helper function to f2p.R
is_absolute_path <- function(path) {
  # Handles Windows (e.g., "C:/"), UNC (e.g., "\\server"), and Unix-like ("/") paths
  path = normalizePath(path, mustWork = FALSE)
  grepl("^([A-Za-z]:[/\\\\]|\\\\\\\\|/)", path)
}
