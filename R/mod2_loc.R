# R/mod2_loc.R
#' @keywords internal

# Find a named START_BLOCK/END_BLOCK in one file
mod2_find_block_in_file = function(file, block_name) {
  restore.point("mod2_find_block_in_file")
  if (!file.exists(file)) {
    return(list(locations = list(), error_msg = paste0("File '", basename(file), "' does not exist.")))
  }
  lines = readLines(file, warn = FALSE)
  pat_start = paste0("^[[:space:]]*(//|#|/\\*|<!--)?[[:space:]]*START_BLOCK[[:space:]]+", stringi::stri_replace_all_regex(block_name, "([\\^\\$\\.\\*\\+\\?\\(\\)\\[\\]\\{\\}\\|])", "\\\\$1"), "[[:space:]]*")
  pat_end   = paste0("^[[:space:]]*(//|#|/\\*|<!--)?[[:space:]]*END_BLOCK[[:space:]]+",   stringi::stri_replace_all_regex(block_name, "([\\^\\$\\.\\*\\+\\?\\(\\)\\[\\]\\{\\}\\|])", "\\\\$1"), "[[:space:]]*")

  starts = which(stringi::stri_detect_regex(lines, pat_start))
  ends   = which(stringi::stri_detect_regex(lines, pat_end))
  locs = list()
  if (length(starts) > 0 && length(ends) > 0) {
    for (s in starts) {
      e = ends[ends > s]
      if (length(e) > 0) {
        locs[[length(locs) + 1]] = list(start = s, end = e[1], is_fuzzy = FALSE, file = normalizePath(file, winslash = "/"))
      }
    }
  }
  list(locations = locs, error_msg = if (length(locs) == 0) paste0("Block '", block_name, "' not found in file.") else NULL)
}

# Search a named block across a set of files; return the first match as location 1,
# but still include all matches in potential locations so the "Find" button can cycle.
mod2_find_block_across_files = function(files, block_name) {
  all = list()
  for (f in files) {
    res = mod2_find_block_in_file(f, block_name)
    if (length(res$locations) > 0) {
      all = c(all, res$locations)
    }
  }
  list(locations = all, error_msg = if (length(all) == 0) paste0("Block '", block_name, "' not found in any indexed file.") else NULL)
}

# Fuzzy match payload inside a file using the existing line-sequence finder
mod2_fuzzy_match_in_file = function(file, payload) {
  restore.point("mod2_fuzzy_match_in_file")
  if (!file.exists(file)) {
    return(list(locations = list(), error_msg = paste0("File '", basename(file), "' does not exist.")))
  }
  src = readLines(file, warn = FALSE)
  seq = stringi::stri_split_lines(payload, omit_empty = FALSE)[[1]]
  # Trim leading/trailing blank lines to improve matching
  while (length(seq) > 0 && stringi::stri_trim_both(seq[1]) == "") seq = seq[-1]
  while (length(seq) > 0 && stringi::stri_trim_both(seq[length(seq)]) == "") seq = seq[-length(seq)]
  if (length(seq) == 0) {
    return(list(locations = list(), error_msg = "Empty payload for fuzzy matching."))
  }
  # Use existing approximate finder
  locs = tryCatch(
    find_line_sequence(src, seq, approximate = TRUE, max_dist = 0.25),
    error = function(e) list()
  )
  # Attach file info
  locs = lapply(locs, function(loc) { loc$file = normalizePath(file, winslash = "/"); loc })
  list(locations = locs, error_msg = if (length(locs) == 0) "No approximate match found." else NULL)
}
