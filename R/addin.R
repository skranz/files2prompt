#' @keywords internal
#' @importFrom shiny h4 hr p tags textOutput renderText actionButton observeEvent req uiOutput renderUI tagList stopApp runGadget dialogViewer
#' @importFrom miniUI miniPage miniContentPanel gadgetTitleBar miniTitleBarButton
#' @importFrom tools R_user_dir
generate_prompt_addin <- function() {
  library(files2prompt)
  restore.point("generate_prompt_addin")

  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("miniUI", quietly = TRUE)) {
    stop("This add-in requires the {shiny} and {miniUI} packages. Please install them.")
  }
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable("1.1.287")) {
    stop("This add-in only works inside RStudio 1.1 or newer.")
  }

  # --- 1. PRE-PROCESSING (before UI) ---
  proj <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  root_dir <- if (!is.null(proj) && dir.exists(proj)) proj else getwd()

  config_file <- addin_find_config_toml()
  if (!file.exists(config_file)) {
    stop("No config_file found, which should not happen due to built-in fallbacks.")
  }
  cfg <- tryCatch(RcppTOML::parseTOML(config_file, escape = FALSE), error = function(e) {
    stop("Error parsing TOML file '", config_file, "':\n", e$message)
  })

  # Find all files to be included by replicating logic from files2prompt()
  subgroup_names <- names(cfg)[sapply(cfg, is.list)]
  subgroups <- if (length(subgroup_names) > 0) lapply(subgroup_names, function(g) cfg[[g]][[1]]) else list()
  names(subgroups) <- subgroup_names
  .main <- cfg[setdiff(names(cfg), subgroup_names)]
  groups <- c(subgroups, list(.main = .main))

  all_files <- c()
  for (g in names(groups)) {
    # Pass .main for template substitution in paths
    files <- fp_find_group_files(groups[[g]], root_dir = root_dir, values = .main)
    all_files <- union(all_files, files)
  }
  num_files <- length(all_files)

  # Check if config is a package template
  pkg_toml_dir <- system.file("toml", package = "files2prompt")
  is_pkg_template <- startsWith(normalizePath(config_file, mustWork = FALSE), normalizePath(pkg_toml_dir, mustWork = FALSE))

  # --- 2. Define the Shiny Gadget UI ---
  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      shiny::h4(shiny::textOutput("info_text")),
      shiny::hr(),
      shiny::actionButton("make_prompt", "Make Prompt", class = "btn-primary"),
      shiny::actionButton("make_prompt_cancel", "Cancel", class = ""),
      shiny::hr(),
      shiny::uiOutput("config_info_ui")
    )
  )

  # --- 3. Define the Shiny Server Logic ---
  server <- function(input, output, session) {
    output$info_text <- shiny::renderText({
      paste("Found", num_files, "files to include in the prompt.")
    })

    output$config_info_ui <- shiny::renderUI({
      elements <- list(
        shiny::tags$p(shiny::tags$b("Using config:"), shiny::tags$br(), shiny::tags$code(config_file))
      )
      if (is_pkg_template) {
        btn_list <- list()
        if (!is.null(proj)) {
           btn_list <- c(btn_list, list(shiny::actionButton("customize_project", "Customize for Project", class="btn-xs")))
        }
        btn_list <- c(btn_list, list(shiny::actionButton("customize_user", "Customize for User", class="btn-xs")))
        elements <- c(elements, list(shiny::tags$div(class = "btn-group", style = "margin-top: 10px;", btn_list)))
      }
      shiny::tagList(elements)
    })

    observeEvent(input$customize_project, {
      shiny::req(!is.null(proj))
      dest_file <- file.path(proj, basename(config_file))
      if (file.exists(dest_file)) {
        rstudioapi::showDialog("File Exists", "A file with this name already exists in your project. No action taken.")
      } else {
        file.copy(config_file, dest_file)
        rstudioapi::showDialog("Copied", paste("Copied config to", dest_file, "and opening for edit."))
        rstudioapi::navigateToFile(dest_file)
      }
    })

    observeEvent(input$customize_user, {
      if (!exists("R_user_dir", where = "package:tools")) {
         rstudioapi::showDialog("Unsupported", "User-level configuration requires a newer version of R.")
         return()
      }
      user_config_dir <- tools::R_user_dir("files2prompt", which = "config")
      dir.create(user_config_dir, showWarnings = FALSE, recursive = TRUE)
      dest_file <- file.path(user_config_dir, basename(config_file))

      if (file.exists(dest_file)) {
        rstudioapi::showDialog("File Exists", paste("A config file already exists at:", dest_file, "No action taken."))
        rstudioapi::navigateToFile(dest_file)
      } else {
        file.copy(config_file, dest_file)
        rstudioapi::showDialog("Copied", paste("Copied config to", dest_file, "and opening for edit."))
        rstudioapi::navigateToFile(dest_file)
      }
    })

    observeEvent(input$make_prompt, {
      # Ask before continuing when too many files
      opt_num_ask <- cfg$opt_addin_ask_files %||% 50
      if (num_files > opt_num_ask) {
        proceed <- rstudioapi::showQuestion(
          title   = "Many files to process",
          message = paste0("You are about to build a prompt from ", num_files, " files.\nThis might take a while.\n\nDo you want to continue?"),
          ok = "Yes, continue", cancel  = "No, cancel"
        )
        if (!isTRUE(proceed)) return()
      }

      prompt <- files2prompt(config_file, root_dir = root_dir, verbose = 0) # run silently
      outfile <- file.path(tempdir(), "files2prompt.txt")
      writeLines(prompt, outfile)
      cat("\nPrompt written to", outfile)

      clip_res <- NULL
      if (requireNamespace("clipr", quietly = TRUE)) {
        clip_res <- try(clipr::write_clip(prompt), silent = TRUE)
        if (!is(clip_res, "try-error")) cat(" and copied to clipboard.")
      }
      cat("\nEstimated token count:", guess_token_num(prompt), "\n")

      rstudioapi::navigateToFile(outfile)
      Sys.sleep(0.5) # Give RStudio time to open the file
      ctx <- rstudioapi::getSourceEditorContext()
      if (isTRUE(try(normalizePath(ctx$path) == normalizePath(outfile)))) {
        last_line <- length(ctx$contents)
        rng <- rstudioapi::document_range(
                 rstudioapi::document_position(1, 1),
                 rstudioapi::document_position(last_line, nchar(ctx$contents[last_line]) + 1)
               )
        rstudioapi::setSelectionRanges(id = ctx$id, ranges = list(rng))
      }
      shiny::stopApp(invisible(prompt))
    })

    observeEvent(input$cancel, {
      shiny::stopApp(invisible(NULL))
    })
  }
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Generate Prompt", width = 500, height = 350))
}

addin_find_config_toml <- function() {
  library(files2prompt)
  restore.point("addin_find_config_toml")
  is_toml <- function(path) length(path) == 1L &&
                             is.character(path)  &&
                             file.exists(path)   &&
                             grepl("\\.toml$", path, ignore.case = TRUE)

  ## 1 active editor file ----------
  if (rstudioapi::isAvailable("1.1.287")) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(),
                    error = function(e) NULL)
    if (!is.null(ctx$path) && nzchar(ctx$path) && is_toml(ctx$path))
      return(normalizePath(ctx$path, winslash = "/"))
  }

  ## 2 explicit option -------------
  opt <- getOption("file2prompt")
  if (is.list(opt) && is_toml(opt$toml_file))
    return(normalizePath(opt$toml_file, winslash = "/"))

  ## 3 dir from option ---------
  if (is.list(opt) && !is.null(opt$dir) && dir.exists(opt$dir)) {
    tomls <- sort(list.files(opt$dir, pattern = ".*f2p.*\\.toml$", full.names = TRUE))
    if (length(tomls)) return(normalizePath(tomls[1], winslash = "/"))
  }

  ## 4 project root ----------
  proj <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  if (!is.null(proj) && dir.exists(proj)) {
    tomls <- sort(list.files(proj, pattern = ".*f2p.*\\.toml$", full.names = TRUE))
    if (length(tomls)) return(normalizePath(tomls[1], winslash = "/"))
  }

  ## 5 User-wide config directory ----
  if (exists("R_user_dir", where = "package:tools")) {
    user_config_dir <- try(tools::R_user_dir("files2prompt", which = "config"), silent = TRUE)
    if (!inherits(user_config_dir, "try-error") && dir.exists(user_config_dir)) {
        tomls <- sort(list.files(user_config_dir, pattern = ".*f2p.*\\.toml$", full.names = TRUE))
        if (length(tomls)) return(normalizePath(tomls[1], winslash = "/"))
    }
  }

  ## 6 working directory ---------------
  wd <- getwd()
  if (!is.null(wd) && dir.exists(wd)) {
    tomls <- sort(list.files(wd, pattern = ".*f2p.*\\.toml$", full.names = TRUE))
    if (length(tomls)) return(normalizePath(tomls[1], winslash = "/"))
  }

  ## 7 fallback for R packages ---------------------------------------------
  if (!is.null(proj) && file.exists(file.path(proj, "DESCRIPTION"))) {
    pkg_tpl <- system.file("toml/f2p_r_pkg.toml",
                           package = "files2prompt", mustWork = FALSE)
    if (nzchar(pkg_tpl) && file.exists(pkg_tpl))
      return(pkg_tpl)
  }

  ## 8 generic default ------------------------------------------------------
  def_tpl <- system.file("toml/f2p_default.toml",
                         package = "files2prompt", mustWork = FALSE)
  if (nzchar(def_tpl) && file.exists(def_tpl))
    return(def_tpl)

  stop("No TOML specification file could be located.")
}
