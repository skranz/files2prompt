# FILE: R/mod_addin.R
#' @importFrom shiny fluidPage verbatimTextOutput actionButton observeEvent
#' @importFrom shiny reactiveValues renderPrint shinyApp stopApp isolate runGadget
#' @importFrom shiny uiOutput renderUI tagList h4 tags conditionalPanel reactive outputOptions req
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniTitleBarButton
#' @keywords internal

#' Navigate to a modification target in RStudio and select/highlight
#'
#' This function is called by the "Apply Modification" addin to show the user
#' where a change will be applied before they approve it.
#'
#' @param mod A single parsed modification object.
#' @param project_dir The root directory of the project.
#' @keywords internal
navigate_to_modification_target <- function(mod, project_dir) {
  # This function requires the rstudioapi
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return(invisible(NULL))
  }

  # --- Find target file ---
  target_file <- find_project_file(mod$meta$file, project_dir)

  # If the file doesn't exist (e.g., a new file), we can't navigate to it.
  if (is.null(target_file)) {
    return(invisible(NULL))
  }

  # --- Navigate to the file first ---
  rstudioapi::navigateToFile(target_file)
  Sys.sleep(0.2) # Give RStudio a moment to open the file and update context

  # --- Now, handle selection or cursor position ---
  scope <- mod$meta$scope
  original_lines <- readLines(target_file, warn = FALSE)
  is_r_file <- grepl("\\.R$", target_file, ignore.case = TRUE)
  all_funs <- if (is_r_file) tryCatch(fp_get_all_function_locations(target_file), error = function(e) list()) else list()

  # Helper to set selection
  set_selection <- function(start_line, end_line, start_col = 1, end_col = -1) {
    if (end_col == -1 && end_line <= length(original_lines) && length(original_lines) > 0) {
      end_col <- nchar(original_lines[end_line]) + 1
    } else if (end_col == -1) {
      end_col <- 1
    }
    rng <- rstudioapi::document_range(
             rstudioapi::document_position(start_line, start_col),
             rstudioapi::document_position(end_line, end_col)
           )
    rstudioapi::setSelectionRanges(list(rng))
  }

  # Helper to set cursor position
  set_cursor <- function(line, col = 1) {
    rstudioapi::setCursorPosition(rstudioapi::document_position(line, col))
  }

  tryCatch({
    switch(scope,
      "file" = {
        if (length(original_lines) > 0) {
          last_line <- length(original_lines)
          set_selection(1, last_line)
        }
      },
      "function" = {
        if (!is.null(mod$meta$function_name)) { # Replacement
          loc <- all_funs[[mod$meta$function_name]]
          if (!is.null(loc)) {
            set_selection(loc$start_line, loc$end_line)
          }
        } else { # Insertion
          insert_line <- get_insertion_line(mod$meta, original_lines, all_funs)
          set_cursor(insert_line)
        }
      },
      "lines" = {
        if (!is.null(mod$meta$replace_lines)) { # Replacement
          lines_to_replace <- strsplit(mod$meta$replace_lines, "\n")[[1]]
          loc <- find_line_sequence(original_lines, lines_to_replace)
          if (!is.null(loc)) {
            set_selection(loc$start, loc$end)
          }
        } else { # Insertion
          insert_after <- strsplit(mod$meta$insert_after_lines %||% "", "\n")[[1]]
          if (length(insert_after) > 0 && nzchar(insert_after[1])) {
              loc <- find_line_sequence(original_lines, insert_after)
              if (!is.null(loc)) {
                  insert_line <- loc$end + 1
                  set_cursor(insert_line)
              }
          } else {
              insert_line <- get_insertion_line(mod$meta, original_lines, all_funs)
              set_cursor(insert_line)
          }
        }
      },
      {} # Default case: do nothing for unknown scope
    )
  }, error = function(e) {
    # If any helper (e.g., get_insertion_line) throws an error, catch it
    # silently so the addin doesn't crash.
    message("Info: Could not highlight target for '", mod$meta$file, "'. ", e$message)
  })

  invisible(NULL)
}


review_modifications_addin <- function() {
  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("miniUI", quietly = TRUE)) {
    stop("The 'shiny' and 'miniUI' packages are required for this addin.")
  }
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
      stop("This addin requires RStudio version 1.2 or higher.")
  }

  # --- 1. Find and parse the AI response file ---
  project_dir <- tryCatch(rstudioapi::getActiveProject(), error = function(e) getwd())
  response_file <- find_ai_response_file()
  raw_text <- paste(readLines(response_file, warn = FALSE), collapse = "\n")
  mod_list <- parse_ai_response(raw_text)
  restore.point("review_modifications_addin")

  if (length(mod_list) == 0) {
    message("No valid modifications found in '", basename(response_file), "'.")
    return(invisible())
  }

  # --- 2. Define the Shiny Gadget UI ---
  ui <- miniUI::miniPage(
    # Add custom CSS for styling
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      #info_ui p {
        white-space: pre-wrap;
        word-wrap: break-word;
        margin-bottom: 5px;
      }
      .btn-container {
        margin-top: 2px;
      }
    "))),


    miniUI::miniContentPanel(
      # Group buttons in a container
      shiny::div(class = "btn-container",
        shiny::conditionalPanel(
          condition = "output.mods_in_progress == true",
          shiny::actionButton("apply", "Apply Change",  class = "btn-xs btn-primary"),
          shiny::actionButton("skip", "Skip Change",  class = "btn-xs"),
          shiny::actionButton("abort", "Abort All", class = "btn-xs")
        ),
        shiny::conditionalPanel(
          condition = "output.mods_in_progress == false",
          shiny::actionButton("finish", "Finish", class = "btn-primary")
        )
      ),
      # Conditional panel for when modifications are in progress
      shiny::conditionalPanel(
        condition = "output.mods_in_progress == true",
        shiny::uiOutput("info_ui")
      ),
      # Conditional panel for when all modifications are done
      shiny::conditionalPanel(
        condition = "output.mods_in_progress == false",
        shiny::h4("All modifications processed."),
        shiny::h5("Log of actions:"),
        shiny::verbatimTextOutput("final_log")
      )
    )

  )

  # --- 3. Define the Shiny Server Logic ---
  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      mods = mod_list,
      current = 1,
      total = length(mod_list),
      log = character(0)
    )

    # --- Reactive controller for conditionalPanels ---
    output$mods_in_progress <- shiny::reactive({
      rv$current <= rv$total
    })
    shiny::outputOptions(output, "mods_in_progress", suspendWhenHidden = FALSE)

    # --- Navigate to file when current mod changes ---
    observeEvent(rv$current, {
      # Ensure we are in RStudio and have a valid mod to show
      req(rv$current <= rv$total)

      shiny::isolate({
        mod <- rv$mods[[rv$current]]
        project_dir <- tryCatch(rstudioapi::getActiveProject(), error = function(e) getwd())

        # This function will handle navigation and selection in RStudio
        navigate_to_modification_target(mod, project_dir)
      })
    })


    # --- Renderers for the current modification ---
    output$info_ui <- shiny::renderUI({
      if (rv$current > rv$total) return(NULL)
      mod = rv$mods[[rv$current]]
      HTML(paste0(
        "<h4>Modification ", rv$current, " of ", rv$total, "</h4>",
        mod_to_html_descr(mod)
      ))
    })

    output$final_log <- shiny::renderPrint({
      cat(paste(rv$log, collapse="\n"))
    })

    # --- Button Observers ---
    observeEvent(input$apply, {
      mod <- rv$mods[[rv$current]]
      tryCatch({
        apply_modification(mod, project_dir)
        log_msg <- paste("APPLIED:", mod$meta$scope, "on", mod$meta$file)
        rv$log <- c(rv$log, log_msg)
      }, error = function(e) {
        log_msg <- paste("ERROR applying to", mod$meta$file, ":", e$message)
        rv$log <- c(rv$log, log_msg)
        rstudioapi::showDialog("Error Applying Change", e$message, url = "")
      })
      rv$current <- rv$current + 1
    })

    observeEvent(input$skip, {
      mod <- rv$mods[[rv$current]]
      log_msg <- paste("SKIPPED:", mod$meta$scope, "on", mod$meta$file)
      rv$log <- c(rv$log, log_msg)
      rv$current <- rv$current + 1
    })

    observeEvent(input$close, {
      shiny::stopApp(invisible(rv$log))
    })
    observeEvent(input$abort, {
       shiny::stopApp(invisible(rv$log))
    })
    observeEvent(input$finish, {
      shiny::stopApp(invisible(rv$log))
    })
  }

  # --- 4. Run the Gadget ---
  cat("Starting modification review tool...\n")
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = "maximize"))
}

mod_meta_add_info = function(meta) {
  if (any(startsWith(names(meta), "insert"))) {
    meta$is_insert = TRUE
  } else {
    meta$is_insert = FALSE
  }
  meta
}

mod_to_html_descr = function(mod) {
  restore.point("mod_to_html_descr")
  meta = mod_meta_add_info(mod$meta)
  str = ""
  if (meta$scope=="function" & meta$is_insert) {
    str = paste0("Add function to <i>", meta$file,"</i>")
  } else if (meta$scope=="function" & !meta$is_insert) {
    str = paste0("Replace function ", meta[["function"]], " in ", meta$file)
  } else if (meta$scope=="file") {
    str = paste0("Write complete ", meta$file)
  } else if (meta$scope=="lines") {
    str = paste0("Write lines in ", meta$file)
  }
  str = paste0("<p>", str, ": ", meta$descr, "\n<pre>", mod$payload, "\n</pre>")
  str
}
