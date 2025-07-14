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

  # --- Determine source of config file and button visibility ---
  pkg_toml_dir <- system.file("toml", package = "files2prompt")
  user_config_dir <- if (exists("R_user_dir", where = "package:tools")) {
    try(tools::R_user_dir("files2prompt", which = "config"), silent = TRUE)
  } else { "" }
  if (inherits(user_config_dir, "try-error") || !nzchar(user_config_dir)) user_config_dir <- NULL

  # Normalize paths for robust comparison
  norm_config_path <- normalizePath(config_file, winslash = "/", mustWork = FALSE)
  norm_pkg_path    <- normalizePath(pkg_toml_dir, winslash = "/", mustWork = FALSE)
  norm_user_path   <- if (!is.null(user_config_dir)) normalizePath(user_config_dir, winslash = "/", mustWork = FALSE) else NULL
  norm_proj_path   <- if (!is.null(proj)) normalizePath(proj, winslash = "/", mustWork = FALSE) else NULL

  # Check config file's location. Add trailing slash to avoid matching similar parent directories.
  is_from_pkg <- startsWith(norm_config_path, paste0(norm_pkg_path, "/"))
  is_from_user <- if (!is.null(norm_user_path)) startsWith(norm_config_path, paste0(norm_user_path, "/")) else FALSE
  # `is_from_project` is true if the file is in the project dir but not in a user/pkg dir that might be nested inside
  is_from_project <- if (!is.null(norm_proj_path)) {
      startsWith(norm_config_path, paste0(norm_proj_path, "/")) && !is_from_pkg && !is_from_user
    } else {
      FALSE
  }

  # --- Logic for "Customize for Project" button ---
  # Show if config is from package or user, and a project is active.
  show_customize_project_btn <- !is.null(proj) && (is_from_pkg || is_from_user)

  # --- Logic for "Customize for User" button ---
  # Show if config is from package.
  # Also show if from project, and its name matches a package template name.
  show_customize_user_btn <- FALSE
  if (is_from_pkg) {
    show_customize_user_btn <- TRUE
  } else if (is_from_project) {
    pkg_template_files <- list.files(pkg_toml_dir)
    if (basename(config_file) %in% pkg_template_files) {
      show_customize_user_btn <- TRUE
    }
  }

  # --- 2. Define the Shiny Gadget UI ---
  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      shiny::h4(shiny::textOutput("info_text")),
      shiny::hr(),
      shiny::actionButton("make_prompt", "Make Prompt", class = "btn-primary"),
      shiny::actionButton("cancel", "Cancel", class = ""),
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

      btn_list <- list()
      if (show_customize_project_btn) {
         btn_list <- c(btn_list, list(shiny::actionButton("customize_project", "Customize for Project", class="btn-xs")))
      }
      if (show_customize_user_btn) {
        btn_list <- c(btn_list, list(shiny::actionButton("customize_user", "Customize for User", class="btn-xs")))
      }

      if (length(btn_list) > 0) {
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
      prompt <- files2prompt(config_file, root_dir = root_dir, verbose = 0) # run silently

      # Determine output file path based on config
      opt_file <- cfg$opt_prompt_file
      if (is.null(opt_file)) {
        outfile <- file.path(tempdir(), "files2prompt.md")
      } else {
        if (startsWith(opt_file, "<tempdir>/")) {
          filename <- sub("<tempdir>/", "", opt_file, fixed = TRUE)
          outfile <- file.path(tempdir(), filename)
        } else {
          outfile <- file.path(root_dir, opt_file)
        }
      }

      # Ensure the directory for the output file exists
      dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)

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
