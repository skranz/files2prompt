# R/mod2_parser.R
#' @keywords internal

# mod2_detect ---------------------------------------------------------------

mod2_detect = function(text) {
  # Decide mod2 vs classic. Classic takes precedence if "!MODIFICATION" is present.
  if (!is.character(text) || length(text) != 1) return(FALSE)
  if (grepl("!MODIFICATION", text, fixed = TRUE)) return(FALSE)
  # Look for any fenced code block. Accept ```lang or ```{lang}
  grepl("(?m)^```\\s*\\{?[-a-zA-Z0-9_]+\\}?\\s*$", text, perl = TRUE)
}

# mod2_parse_response -------------------------------------------------------

# Build a list of "mod-like" objects:
# list(meta = list(...), payload = <code>)
# These feed directly into the existing review gadget.
mod2_parse_response = function(text, project_dir) {
  restore.point("mod2_parse_response")
  stopifnot(is.character(text), length(text) == 1)

  text = f2p_normalize_ai_response(text)

  lines = stringi::stri_split_lines(text, omit_empty = FALSE)[[1]]
  fences = mod2_locate_fences(lines)

  last_files_rel = tryCatch(
    f2p_last_prompt_index(project_dir, action = "read"),
    error = function(e) character(0)
  )

  non_data_rel = last_files_rel[!mod2_is_data_file(last_files_rel)]
  non_data_abs = file.path(project_dir, non_data_rel)

  mods = lapply(seq_len(nrow(fences)), function(i) {
    restore.point("uhdufhudhfiuhdfhiudhf")
    file_guess = NULL

    f = fences[i, , drop = FALSE]

    str = lines[f$start + 1]
    if (startsWith(str, "# FILE:")) {
      f$start = f$start + 1
      file_guess = trimws(stringi::stri_sub(str, 9))

      find_files = list.files(project_dir, glob2rx(file_guess), recursive = TRUE, full.names = FALSE)
      if (length(find_files) > 0) file_guess = find_files[1]
    }

    code_lines = lines[(f$start + 1):(f$end - 1)]
    payload = paste(code_lines, collapse = "\n")

    expl = mod2_capture_explainer(lines, f$start, f$prev_fence_line, max_explain_lines = 8)

    if (!is.null(file_guess)) {
      guessed_rel = file_guess
    } else {
      guessed_rel = mod2_guess_file_from_prose(
        lines = lines,
        start_idx = f$start,
        stop_idx = f$prev_fence_line,
        candidates_rel = non_data_rel
      )
    }

    guessed_abs = if (length(guessed_rel) == 1) file.path(project_dir, guessed_rel) else NULL

    blk = mod2_extract_block_markers(payload)

    meta = list(
      scope = "lines",
      file = if (!is.null(guessed_rel)) guessed_rel else "(no file matched)",
      description = mod2_make_description(expl),
      from_mod2 = TRUE,
      explain_above = expl
    )
    meta$file_path = guessed_abs
    loc = list(locations = list(), error_msg = NULL)

    if (!is.null(guessed_abs) && file.exists(guessed_abs)) {
      if (!is.null(blk$name) && blk$has_pair) {
        loc = mod2_find_block_in_file(guessed_abs, blk$name)
      }
    } else if (!is.null(blk$name) && blk$has_pair && length(non_data_abs) > 0) {
      loc = mod2_find_block_across_files(non_data_abs, blk$name)
      if (length(loc$locations) > 0) {
        meta$file_path = loc$locations[[1]]$file
        meta$file = mod2_rel_from_abs(meta$file_path, project_dir)
      }
    }

    if (!is.null(meta$file_path) && nzchar(meta$file_path)) {
      target_dir = dirname(meta$file_path)
      meta$file_exists_before = file.exists(meta$file_path)
      meta$dir_exists_before = dir.exists(target_dir)
      meta$is_new_file = !meta$file_exists_before
      meta$is_new_dir = !meta$dir_exists_before
    } else {
      meta$file_exists_before = FALSE
      meta$dir_exists_before = FALSE
      meta$is_new_file = FALSE
      meta$is_new_dir = FALSE
    }

    if (length(loc$locations) > 0) {
      meta$location_found = TRUE
      meta$location_is_fuzzy = isTRUE(loc$locations[[1]]$is_fuzzy)
      meta$start_line = loc$locations[[1]]$start
      meta$end_line = loc$locations[[1]]$end
      meta$potential_locations = loc$locations
      meta$num_potential_locations = length(loc$locations)
      meta$current_match_index = 1
    } else {
      meta$location_found = FALSE
      meta$location_is_fuzzy = FALSE
      meta$start_line = 1
      meta$end_line = 0
      meta$potential_locations = list()
      meta$num_potential_locations = 0
      meta$current_match_index = 0
      meta$location_error = if (!is.null(meta$file_path)) {
        "Could not locate a matching block or fuzzy region in the target file."
      } else {
        "No target file could be mapped from the response. Use Insert Here or create the file."
      }
    }

    list(meta = meta, payload = payload)
  })

  mods
}

# Helpers -------------------------------------------------------------------

mod2_is_data_file = function(rel_path) {
  ext = tolower(tools::file_ext(rel_path))
  data_exts = tryCatch(fp_data_extensions(), error = function(e) c("rds", "csv"))
  ext %in% data_exts
}

mod2_rel_from_abs = function(abs_path, project_dir) {
  abs_path = normalizePath(abs_path, mustWork = FALSE)
  root = normalizePath(project_dir, mustWork = FALSE)
  if (stringi::stri_startswith_fixed(abs_path, paste0(root, .Platform$file.sep))) {
    sub(paste0("^", stringi::stri_replace_all_regex(root, "([\\^\\$\\.\\*\\+\\?\\(\\)\\[\\]\\{\\}\\|])", "\\\\$1"), .Platform$file.sep), "", abs_path)
  } else {
    basename(abs_path)
  }
}

# Locate all code fences; return data.frame with start/end fence lines and previous fence line
mod2_locate_fences = function(lines) {
  n = length(lines)
  is_fence = stringi::stri_detect_regex(lines, "^```")
  fence_idx = which(is_fence)
  # Pair them up: open at odd positions, close at even positions
  opens = fence_idx[seq(1, length(fence_idx), by = 2)]
  closes = fence_idx[seq(2, length(fence_idx), by = 2)]
  if (length(opens) != length(closes)) {
    # Try to recover by truncating the last unmatched
    m = min(length(opens), length(closes))
    opens = opens[seq_len(m)]
    closes = closes[seq_len(m)]
  }
  if (length(opens) == 0) {
    return(data.frame(start = integer(0), end = integer(0), prev_fence_line = integer(0)))
  }
  prevs = c(0, closes[-length(closes)])
  data.frame(start = opens, end = closes, prev_fence_line = prevs)
}

# Explainer capture per user spec
mod2_capture_explainer = function(lines, fence_start_line, prev_fence_line, max_explain_lines = 8) {
  start_scan = ifelse(prev_fence_line > 0, prev_fence_line + 1, 1)
  expl_lines = character(0)
  # Walk upward from fence_start_line - 1 to start_scan
  i = fence_start_line - 1
  seen = 0
  extra_allowed = FALSE
  while (i >= start_scan) {
    ln = lines[i]
    if (stringi::stri_detect_regex(ln, "^```")) break
    # If we already passed max, allow extending only until a blank gap
    if (seen >= max_explain_lines && stringi::stri_trim_both(ln) == "") break
    expl_lines = c(ln, expl_lines)
    seen = seen + 1
    i = i - 1
  }
  # Trim trailing/leading blank lines
  if (length(expl_lines) == 0) return("")
  # Remove code fences if any slipped in
  expl_lines = expl_lines[!stringi::stri_detect_regex(expl_lines, "^```")]
  # Right-trim only; keep original spacing
  out = paste(expl_lines, collapse = "\n")
  stringi::stri_trim_right(out)
}

# Guess file by nearest mention above the fence
mod2_guess_file_from_prose = function(lines, start_idx, stop_idx, candidates_rel) {
  if (length(candidates_rel) == 0) return(NULL)
  # Build lookup of basenames to rel paths (first wins)
  base_map = stats::setNames(candidates_rel, basename(candidates_rel))
  # Search upward from start_idx - 1 down to stop_idx + 1
  for (i in seq(from = start_idx - 1, to = max(stop_idx + 1, 1), by = -1)) {
    ln = lines[i]
    if (stringi::stri_detect_regex(ln, "^```")) break
    # Exact rel match
    hit_rel = candidates_rel[stringi::stri_detect_fixed(ln, candidates_rel)]
    if (length(hit_rel) > 0) return(hit_rel[[length(hit_rel)]])
    # Basename match (in code ticks or plain)
    bases = names(base_map)
    hit_base = bases[stringi::stri_detect_fixed(ln, bases)]
    if (length(hit_base) > 0) return(base_map[[hit_base[[length(hit_base)]]]])
  }
  NULL
}

# Extract optional START_BLOCK/END_BLOCK markers from payload
mod2_extract_block_markers = function(payload) {
  lines = stringi::stri_split_lines(payload, omit_empty = FALSE)[[1]]
  pat_start = "^[[:space:]]*(//|#|/\\*|<!--)?[[:space:]]*START_BLOCK[[:space:]]+([A-Za-z0-9._-]+)"
  pat_end   = "^[[:space:]]*(//|#|/\\*|<!--)?[[:space:]]*END_BLOCK[[:space:]]+([A-Za-z0-9._-]+)"
  start_hit = which(stringi::stri_detect_regex(lines, pat_start))
  end_hit   = which(stringi::stri_detect_regex(lines, pat_end))
  name = NULL
  has_pair = FALSE
  if (length(start_hit) > 0 && length(end_hit) > 0) {
    start_name = stringi::stri_match_first_regex(lines[start_hit[1]], pat_start)[, 3]
    end_name   = stringi::stri_match_first_regex(lines[end_hit[length(end_hit)]], pat_end)[, 3]
    if (!is.na(start_name) && !is.na(end_name) && start_name == end_name) {
      name = start_name
      has_pair = TRUE
    }
  }
  list(name = name, has_pair = has_pair)
}

# Small, friendly description from explainer
mod2_make_description = function(expl) {
  txt = stringi::stri_trim_both(expl)
  if (!nzchar(txt)) return("mod2 fragment")
  parts = stringi::stri_split_lines(txt, omit_empty = TRUE)[[1]]
  head = parts[1]
  head = if (nchar(head) > 120) paste0(substr(head, 1, 117), "...") else head
  head
}

