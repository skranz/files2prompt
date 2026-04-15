#' Generate an ASCII directory tree from a vector of file paths
#' @param paths Character vector of relative file paths
#' @param root_name String to display as the root node
#' @return A single character string containing the formatted tree
#' @export
fp_generate_tree <- function(paths, root_name = ".") {
  if (length(paths) == 0) return(root_name)

  # Normalize slashes and remove leading `./` or `/`
  paths <- gsub("\\\\", "/", paths)
  paths <- sub("^\\./", "", paths)
  paths <- sub("^/", "", paths)

  # Ensure unique and sorted paths for alphabetical tree structure
  paths <- sort(unique(paths))

  # Recursive inner function to build branches
  build_tree <- function(paths, prefix = "") {
    if (length(paths) == 0) return(character(0))

    # Split at the first directory boundary
    parts <- strsplit(paths, "/", fixed = TRUE)
    first <- sapply(parts, `[`, 1)
    rest <- sapply(parts, function(x) if (length(x) > 1) paste(x[-1], collapse = "/") else NA_character_)

    unique_first <- unique(first)
    lines <- character(0)

    for (i in seq_along(unique_first)) {
      node <- unique_first[i]
      is_last <- (i == length(unique_first))

      connector <- if (is_last) "└── " else "├── "
      child_prefix <- if (is_last) "    " else "│   "

      lines <- c(lines, paste0(prefix, connector, node))

      # Process children of this node
      child_idx <- which(first == node & !is.na(rest))
      if (length(child_idx) > 0) {
        child_paths <- rest[child_idx]
        lines <- c(lines, build_tree(child_paths, paste0(prefix, child_prefix)))
      }
    }
    return(lines)
  }

  tree_lines <- build_tree(paths, prefix = "")
  paste(c(root_name, tree_lines), collapse = "\n")
}
