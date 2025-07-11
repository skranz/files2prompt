#' @importFrom restorepoint restore.point
#' @keywords internal
apply_modification <- function(mod, project_dir) {
  restore.point("apply_modification")
  op <- mod$meta$operation

  # Find the full path of the file to modify
  target_file <- find_project_file(mod$meta$file, project_dir)
  if (is.null(target_file) && op != "complete_file" && !isTRUE(mod$meta$is_new_file)) {
    stop("Could not find file '", mod$meta$file, "' in project.")
  } else if (is.null(target_file)) {
    # For new files, construct the path
    target_file <- file.path(project_dir, mod$meta$file)
  }

  cat("Applying '", op, "' to '", mod$meta$file, "'...\n", sep="")

  # Ensure directory exists for new or rewritten files
  if (op == "complete_file") {
    dir.create(dirname(target_file), showWarnings = FALSE, recursive = TRUE)
  }

  # --- Dispatch based on operation ---
  if (op == "complete_file") {
    writeLines(mod$payload, target_file)
    cat("  -> File created/rewritten.\n")
    return(invisible(TRUE))
  }

  if (op == "modify_lines") {
    if (!file.exists(target_file)) {
        stop("File '", target_file, "' does not exist for modification.")
    }
    original_lines <- readLines(target_file, warn = FALSE)
    payload_lines <- strsplit(mod$payload, "\n")[[1]]

    # Determine action: insert, delete, or replace
    do_insert <- !is.null(mod$meta$insert_at_line)
    do_delete <- !is.null(mod$meta$delete_from) && !is.null(mod$meta$delete_to)

    if (!do_insert && !do_delete) {
      stop("modify_lines requires at least 'insert_at_line' or 'delete_from'/'delete_to'.")
    }

    # --- Line number validation ---
    if (do_insert && (mod$meta$insert_at_line > length(original_lines) + 1 || mod$meta$insert_at_line < 1)) {
        stop("insert_at_line (", mod$meta$insert_at_line, ") is out of bounds for file with ", length(original_lines), " lines.")
    }
    if (do_delete && (mod$meta$delete_from > mod$meta$delete_to || mod$meta$delete_to > length(original_lines))) {
        stop("delete range (", mod$meta$delete_from, "-", mod$meta$delete_to, ") is invalid for file with ", length(original_lines), " lines.")
    }

    new_lines <- original_lines

    # Perform deletion first
    if (do_delete) {
      new_lines <- new_lines[-c(mod$meta$delete_from:mod$meta$delete_to)]
      cat("  -> Deleted lines ", mod$meta$delete_from, "-", mod$meta$delete_to, ".\n", sep="")
    }

    # Perform insertion
    if (do_insert && length(payload_lines) > 0) {
      # Adjust insert_at_line if deletion happened before it
      insert_at <- mod$meta$insert_at_line
      if (do_delete && mod$meta$delete_from < insert_at) {
        deleted_count <- mod$meta$delete_to - mod$meta$delete_from + 1
        insert_at <- insert_at - deleted_count
      }

      before <- if (insert_at > 1) new_lines[1:(insert_at - 1)] else character(0)
      after <- if (insert_at <= length(new_lines)) new_lines[insert_at:length(new_lines)] else character(0)

      new_lines <- c(before, payload_lines, after)
      cat("  -> Inserted ", length(payload_lines), " lines at line ", mod$meta$insert_at_line, ".\n", sep="")
    }

    writeLines(new_lines, target_file)
    return(invisible(TRUE))
  }

  stop("Unknown operation: ", op)
}
