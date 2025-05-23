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
    files = fp_find_group_files(groups[[g]],root_dir=root_dir)
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
  paste0(readLines(file_path, warn=FALSE), collapse="\n")
}

group_root_dir = function(group, root_dir = ".") {
  group[["root_dir"]] %||% root_dir
}

fp_find_group_files = function(group, root_dir = ".") {
  restore.point("fp_find_files")
  inc = file_pattern_to_regex(group[["include_files"]])
  # If files_include specified: return no files
  # this makes sense in TOML spec that include files
  # only in subgroups
  if (length(inc)==0) return(NULL)
  exc = file_pattern_to_regex(group[["exclude_files"]])
  root_dir = group_root_dir(group, root_dir)

  files = list.files(root_dir, recursive = TRUE,full.names = FALSE,include.dirs = FALSE)
  full_files = list.files(root_dir, recursive = TRUE,full.names = TRUE,include.dirs = FALSE)

  if (length(inc)>0) {
    keep = stri_detect_regex(files, inc)
    files = files[keep]
    full_files = full_files[keep]
  }
  if (length(exc)>0) {
    ignore = stri_detect_regex(files, exc)
    files = files[!ignore]
    full_files = full_files[!ignore]
  }

  names(full_files) = files
  full_files
}

fp_default_template = function() {
"
{{files}}
"
}

fp_default_file_template = function() {
"
{{files}}
"
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
