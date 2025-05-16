#' @keywords internal
generate_prompt_addin <- function() {
  restore.point("addin_find_config_toml")
  library(files2prompt)
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("The add-in requires the {rstudioapi} package.")
  }

  # 1. Is RStudio running a project?
  if (!rstudioapi::isAvailable("1.1.287")) {
    stop("This add-in only works inside RStudio 1.1 or newer.")
  }

  # 2. Locate TOML (default file name)
  config_file = addin_find_config_toml()
  if (!file.exists(config_file)) {
    stop("No config_file found... that is strange.")
  }

  # 3. Determine root_dir
  proj <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  if (!is.null(proj) && dir.exists(proj)) {
    root_dir = proj
  } else {
    root_dir = getwd()
  }

  # 3b
  # Ask before continuing when too many *.R files ------------------------
  opt_num_ask <- 1                     # fallback if none set
  cfg <- tryCatch(
           RcppTOML::parseTOML(config_file, escape = FALSE),
           error = function(e) NULL
         )
  if (!is.null(cfg$opt_addin_ask_files))
    opt_num_ask <- as.integer(cfg$opt_addin_ask_files)

  n_r_files <- length(list.files(
                       root_dir,
                       pattern = "\\.[rR]$",
                       recursive = TRUE,
                       ignore.case = TRUE
                     ))

  if (n_r_files > opt_num_ask) {
    proceed <- rstudioapi::showQuestion(
      title   = "Many R files detected",
      message = paste0(
        "I found ", n_r_files, " R files in â", root_dir,
        "â. Building a prompt from all of them may freeze RStudio.\n\n",
        "Do you still want to continue?"
      ),
      ok      = "Yes â continue",
      cancel  = "No â abort"
    )
    if (!isTRUE(proceed)) {
      cat("\nAborted.")
      return()
    }
  }


  # 4. Build prompt
  prompt <- files2prompt(config_file)

  # 4. Save & copy
  #outfile <- file.path(root_dir, "files2prompt.txt")
  outfile = file.path(tempdir(),"files2prompt.txt")
  writeLines(prompt, outfile)
  cat("\nPrompt written to ", outfile,"")

  if (requireNamespace("clipr", quietly = TRUE)) {
    res = try(clipr::write_clip(prompt), silent=TRUE)
    if (!is(res, "try-error"))
      cat("and copied to clipboard.")
  }
  cat("\nEstimated token count: ", guess_token_num(prompt),"\n")


   # Open the file
  rstudioapi::navigateToFile(outfile)

  ## --- NEW: select the whole document --------------------------------------
  ctx <- rstudioapi::getSourceEditorContext()
  if (isTRUE(try(normalizePath(ctx$path) ==
      normalizePath(outfile)))) {
    last_line <- length(ctx$contents)
    rng <- rstudioapi::document_range(
             rstudioapi::document_position(1, 0),
             rstudioapi::document_position(
               last_line, nchar(ctx$contents[last_line]))
           )
    rstudioapi::setSelectionRanges(id = ctx$id, ranges = list(rng))
  }
  invisible(prompt)
}

addin_find_config_toml <- function() {
  restore.point("addin_find_config_toml")
  is_toml <- function(path) length(path) == 1L &&
                             is.character(path)  &&
                             file.exists(path)   &&
                             grepl("\\.toml$", path, ignore.case = TRUE)

  ## 1 â active editor file ---------------------------------------------------
  if (rstudioapi::isAvailable("1.1.287")) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(),
                    error = function(e) NULL)
    if (!is.null(ctx$path) && nzchar(ctx$path) && is_toml(ctx$path))
      return(normalizePath(ctx$path, winslash = "/"))
  }

  ## 2 â explicit option ------------------------------------------------------
  opt <- getOption("file2prompt")
  if (is.list(opt) && is_toml(opt$toml_file))
    return(normalizePath(opt$toml_file, winslash = "/"))

  ## 3 â dir from option ------------------------------------------------------
  if (is.list(opt) && !is.null(opt$dir) && dir.exists(opt$dir)) {
    tomls <- sort(list.files(opt$dir, pattern = ".*f2p.*\\.toml$", full.names = TRUE))
    if (length(tomls)) return(normalizePath(tomls[1], winslash = "/"))
  }

  ## 4 â project root ---------------------------------------------------------
  proj <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  if (!is.null(proj) && dir.exists(proj)) {
    tomls <- sort(list.files(proj, pattern = ".*f2p.*\\.toml$", full.names = TRUE))
    if (length(tomls)) return(normalizePath(tomls[1], winslash = "/"))
  }

  ## 5 â working directory ---------------------------------------------------------
  wd <- getwd()
  if (!is.null(wd) && dir.exists(wd)) {
    tomls <- sort(list.files(wd, pattern = ".*f2p.*\\.toml$", full.names = TRUE))
    if (length(tomls)) return(normalizePath(tomls[1], winslash = "/"))
  }


  ## 6 â fallback for R packages ---------------------------------------------
  if (!is.null(proj) && file.exists(file.path(proj, "DESCRIPTION"))) {
    pkg_tpl <- system.file("toml/f2p_r_pkg.toml",
                           package = "files2prompt", mustWork = FALSE)
    if (nzchar(pkg_tpl) && file.exists(pkg_tpl))
      return(pkg_tpl)
  }

  ## 7 â generic default ------------------------------------------------------
  def_tpl <- system.file("toml/f2p_default.toml",
                         package = "files2prompt", mustWork = FALSE)
  if (nzchar(def_tpl) && file.exists(def_tpl))
    return(def_tpl)

  stop("No TOML specification file could be located.")
}
