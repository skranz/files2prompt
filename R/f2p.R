example = function() {
  config_file = "rproject_spec.toml"
  prompt = files2prompt(config_file)
  options(warn=2)
  writeLines(prompt, "C:/libraries/files2prompt/prompt.txt")
  cat(main_prompt)
  guess_token_num(main_prompt)
}

#' Parse a TOML confifugration file
#' @param config_file Path to the TOML config file.
#' @export
fp_parse_config = function(config_file) {
  if (!file.exists(config_file)) {
    stop(paste0("file2prompt config_file ", config_file, " does not exist."))
  }
  cfg = parseTOML(config_file, escape=FALSE)
  cfg$config_file = config_file
  cfg
}

#' Build a prompt from text files
#'
#' Reads a TOML specification in the
#' collects the matching files and returns the assembled prompt
#' that can be parsed to a LLM
#'
#' @param config_file Path to the TOML config file.
#' @param root_dir    Override the `root_dir` declared in the TOML.
#'                    Use `NULL` (default) to respect the spec.
#' @param open,close  Delimiters used in templates (default `{{ … }}`).
#'
#' @return A character vector of length 1 containing the final prompt.
#' @importFrom RcppTOML parseTOML
#' @importFrom stringi stri_split_fixed stri_trim_both
#' @importFrom stringi stri_detect_regex stri_replace_all_fixed
#' @importFrom stringi stri_match_all_regex
#' @importFrom restorepoint restore.point
#' @export
files2prompt = function(config_file,root_dir = NULL, open = "{", close="}",cfg=NULL, verbose=1) {
  restore.point("files2prompt")

  if (is.null(cfg)) {
    if (!file.exists(config_file))
      stop(paste0("config_file ", config_file, " not found."))
    cfg = parseTOML(config_file, escape=FALSE)
  } else {
    config_file = cfg$config_file
    if (is.null(config_file)) {
      config_file = "custom config"
    }
  }

  if (is.null(root_dir)) {
    root_dir = cfg[["root_dir"]] %||% "."
  }
  if (verbose>0)
    cat(paste0("\nCreate prompt for files in ", normalizePath(root_dir), " based on ", basename(config_file), ".\n"))

  # Load snippets to be available in templates
  snippets <- fp_load_snippets()

  subgroup_names = names(cfg)[sapply(cfg, is.list)]

  #main_files = fp_find_group_files(cfg, root_dir)
  subgroups = lapply(subgroup_names, function(g) cfg[[g]][[1]])
  names(subgroups) = subgroup_names
  .main = cfg[setdiff(names(cfg), subgroup_names)]
  .main$template = .main$template %||% fp_default_template()
  .main$file_template  = .main$file_template  %||% fp_default_file_template()

  groups = c(subgroups, list(.main=.main))
  # Find files for each group
  # Omit duplicated files: every file will be shown only once
  # groups that are specified earlier in the spec have precedence
  all_files = NULL
  g = ".main"
  for (g in names(groups)) {
    # Pass .main and snippets so that fp_find_group_files can use global vars for substitution
    files = fp_find_group_files(groups[[g]], root_dir=root_dir, values=c(.main, snippets))
    groups[[g]]$.files = setdiff(files, all_files)
    all_files = union(all_files, files)
  }

  if (verbose>0)
    cat("\nWill add ", NROW(all_files), " files to prompt.\n")


  main_tpl = .main$template

  i = length(groups)
  prompts = sapply(seq_along(groups), function(i) {
    group = groups[[i]]
    if (length(group$.files)==0) return(NULL)
    name = names(groups)[[i]]
    values = c(group, .main[setdiff(names(.main), names(group))])
    values = c(values, snippets[!names(snippets) %in% names(values)]) # Add snippets
    file_tpl = group$file_template
    if (is.null(file_tpl)) file_tpl = .main$file_template
    values$filetext = sapply(group$.files, fp_filetext, group=group, verbose = (verbose >= 2))
    # short file name
    values$filename = basename(group$.files)
    files_prompt = paste0(tpl_replace_whisker(file_tpl,values), collapse="\n")
    if (is.null(group$template) | name==".main") {
      return(files_prompt)
    } else {
      values$files = files_prompt
      res = tpl_replace_whisker(group$template,values)
      return(res)
    }
  })


  # Make main prompt
  tpl_vars = tpl_vars(main_tpl)

  is_sep_group = names(groups) %in% tpl_vars
  values = .main
  values$files = paste0(unlist(prompts[!is_sep_group]), collapse="\n")
  values[names(groups[is_sep_group])] = prompts[is_sep_group]
  values = c(values, snippets[!names(snippets) %in% names(values)]) # Add snippets

  main_prompt = tpl_replace_whisker(main_tpl, values)
  main_prompt
}

#' Heuristic token counter
#'
#' Very rough estimate: _1 token ≈ `bytes_per_token` bytes_.
#' Useful when the Python *tiktoken* library is not available.
#'
#' @param text            Character string to measure.
#' @param bytes_per_token Average bytes per token (default 4 — conservative).
#'
#' @return Integer token count.
#' @export
guess_token_num = function(txt, bytes_per_token=4) {
  ceiling(nchar(txt, type = "bytes") / bytes_per_token)
}

file_pattern_to_regex = function(str) {
  restore.point("file_pattern_to_regex")
  if (length(str)==0) return(NULL)
  vec = stri_split_fixed(str, "\n")[[1]]
  vec = vec[nchar(stri_trim_both(vec))>0]
  if (NROW(vec)==0) return(NULL)
  paste0('(', glob2rx(vec),')', collapse="|")
}

fp_filetext = function(file_path, group, verbose=TRUE) {
  if (verbose) {
    cat(paste0("Add ", basename(file_path), "\n"))
  }

  # Check if it's a data file by its extension
  ext <- tolower(tools::file_ext(file_path))

  if (ext %in% fp_data_extensions()) {
    # It's a data file, so render a summary of it.
    # The rendering function needs the group config for custom parameters.
    tryCatch({
      fp_render_data_file(file_path, group)
    }, error = function(e) {
      paste("Error processing data file", basename(file_path), ":\n", e$message)
    })
  } else {
    # It's a regular text/code file, so read its content directly.
    paste0(readLines(file_path, warn=FALSE), collapse="\n")
  }
}

group_root_dir = function(group, root_dir = ".") {
  group[["root_dir"]] %||% root_dir
}

# f2p.R (New and Final fp_find_group_files)
#' @importFrom tools glob2rx

fp_find_group_files = function(group, root_dir = ".", values = list()) {
  restore.point("fp_find_files")

  # --- Process patterns (with template substitution) ---
  all_values <- c(group, values)
  process_patterns <- function(patterns_str) {
    if (is.null(patterns_str) || nchar(patterns_str) == 0) return(character(0))
    patterns_str <- tpl_replace_whisker(patterns_str, all_values)
    vec <- stri_split_fixed(patterns_str, "\n")[[1]]
    vec <- stri_trim_both(vec)
    vec[nchar(vec) > 0]
  }

  include_patterns <- process_patterns(group[["include_files"]])
  if (length(include_patterns) == 0) return(NULL)

  exclude_patterns <- process_patterns(group[["exclude_files"]])

  # --- Find files using glob patterns (now with recursive support) ---
  search_dir <- path.expand(group_root_dir(group, root_dir))

  find_files_from_globs <- function(patterns) {
    if (length(patterns) == 0) return(character(0))

    files <- lapply(patterns, function(pattern) {
      p <- path.expand(pattern)

      # Handle recursive globstar `**`
      if (grepl("/**/", p, fixed = TRUE)) {
        parts <- strsplit(p, "/**/", fixed = TRUE)[[1]]
        base_dir <- parts[1]
        file_glob <- if (length(parts) > 1) parts[2] else "*"

        if (!is_absolute_path(base_dir)) {
          base_dir <- file.path(search_dir, base_dir)
        }

        if (!dir.exists(base_dir)) return(character(0))

        # Use list.files for recursive search, converting glob to regex
        list.files(
          path = base_dir,
          pattern = glob2rx(file_glob, trim.head = TRUE, trim.tail = TRUE),
          recursive = TRUE,
          full.names = TRUE
        )
      } else {
        # Standard, non-recursive glob
        if (!is_absolute_path(p)) {
          p <- file.path(search_dir, p)
        }
        Sys.glob(p)
      }
    })

    unique(unlist(files))
  }

  included_files <- find_files_from_globs(include_patterns)
  excluded_files <- find_files_from_globs(exclude_patterns)

  final_files <- setdiff(included_files, excluded_files)

  if (length(final_files) == 0) return(NULL)

  # --- Generate short names for display ({{filename}}) ---
  norm_root <- normalizePath(root_dir, mustWork = FALSE)
  short_names <- vapply(final_files, function(f) {
    norm_f <- normalizePath(f, mustWork = FALSE)
    if (startsWith(norm_f, norm_root)) {
      sub(paste0(norm_root, .Platform$file.sep), "", norm_f, fixed = TRUE)
    } else {
      basename(f)
    }
  }, FUN.VALUE = character(1))

  names(final_files) <- short_names
  return(final_files)
}

#' Load template snippets from the package's `inst/snippets` directory.
#' @return A named list where names are snippet basenames and values are content.
#' @keywords internal
fp_load_snippets <- function(package = "files2prompt") {
  snippet_dir <- system.file("snippets", package = package, mustWork = FALSE)
  if (!nzchar(snippet_dir)) return(list())

  snippet_files <- list.files(snippet_dir, pattern = "\\.(md|txt)$", full.names = TRUE)
  if (length(snippet_files) == 0) return(list())

  content <- lapply(snippet_files, function(f) {
    paste(readLines(f, warn = FALSE), collapse = "\n")
  })

  # Name the list elements by the file basename without extension
  names(content) <- tools::file_path_sans_ext(basename(snippet_files))
  content
}


#' Provides a default main template.
#' @return A simple string "{{files}}" to be used as the default template.
#' @keywords internal
fp_default_template = function() {
  "{{files}}"
}

fp_default_file_template = function() {
"
# FILE: {{filename}}
```
{{filetext}}
```
# END OF FILE: {{filename}}

---

"
}
