Of course! I will implement the requested changes. Here are the proposed modifications to your R project files, following the specified format.

!MODIFICATION addin.R
scope = "function"
file = "addin.R"
function_name = "generate_prompt_addin"
description = '''Updates the 'generate_prompt_addin' function to:
1. Fix the 'Cancel' button.
2. Remove the 'opt_addin_ask_files' logic.
3. Change the default prompt output file to 'files2prompt.md'.
4. Add support for the 'opt_prompt_file' TOML option to specify a custom output path for the prompt file, including support for '<tempdir>/'.
'''
---
```r
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
```
!END_MODIFICATION addin.R
