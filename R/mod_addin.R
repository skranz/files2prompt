# FILE: R/mod_addin.R
#' @importFrom shiny fluidPage verbatimTextOutput actionButton observeEvent
#' @importFrom shiny reactiveValues renderPrint shinyApp stopApp isolate runGadget
#' @importFrom shiny uiOutput renderUI tagList h4 tags conditionalPanel reactive outputOptions req
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniTitleBarButton
#' @keywords internal

#' Navigate to a modification target in RStudio and select/highlight
#'
#' This function takes a *located* modification object and uses `rstudioapi`
#' to navigate to the file and select the relevant code range.
#'
#' @param mod A single parsed and *located* modification object. It must
#'   contain `mod$meta$file_path`, `start_line`, and `end_line`.
#' @keywords internal
navigate_to_modification_target <- function(mod, project_dir = tryCatch(rstudioapi::getActiveProject(), error = function(e) getwd())) {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return(invisible(NULL))
  }
  restore.point("navigate_to_modification_target")
  #mod = mod_locate_target(mod, project_dir)

  tryCatch({
    target_file <- mod$meta$file_path
    start_line <- mod$meta$start_line
    end_line <- mod$meta$end_line

    if (is.null(target_file)) {
      return(invisible(NULL))
    }

    # If the file doesn't exist, it must be a new file modification.
    # We create it here so the user sees it appear and we can navigate to it.
    if (!file.exists(target_file)) {
      dir.create(dirname(target_file), showWarnings = FALSE, recursive = TRUE)
      file.create(target_file)
    }

    rstudioapi::navigateToFile(target_file)

    # For an insertion, end_line < start_line. We just set the cursor.
    if (end_line < start_line) {
      rstudioapi::setCursorPosition(rstudioapi::document_position(start_line, 1))
    } else {
      # For replacement, select the range.
      original_lines <- readLines(target_file, warn = FALSE)
      end_col <- if (end_line <= length(original_lines)) {
        nchar(original_lines[end_line]) + 1
      } else {
        1
      }
      rng <- rstudioapi::document_range(
               rstudioapi::document_position(start_line, 1),
               rstudioapi::document_position(end_line, end_col)
             )
      rstudioapi::setSelectionRanges(list(rng))
    }
  }, error = function(e) {
    message("Info: Could not navigate/highlight target for '", mod$meta$file, "'. ", e$message)
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
  library(files2prompt)

  # --- 1. PRE-PROCESSING (before UI) ---
  project_dir <- tryCatch(rstudioapi::getActiveProject(), error = function(e) getwd())
  restore.point("review_modifications_addin")
  response_file <- find_ai_response_file()
  raw_text <- paste(readLines(response_file, warn = FALSE), collapse = "\n")

  # 1a. Parse all mods, stop on any error
  mod_list <- parse_ai_response(raw_text)
  if (length(mod_list) == 0) {
    message("No valid modifications found in '", basename(response_file), "'.")
    return(invisible())
  }

  # 1b. Locate all mods, now without stopping on failure
  cat("Locating all modification targets...\n")
  located_mod_list <- lapply(mod_list, function(mod) {
    # mod_locate_target now adds error info to the mod object instead of stopping
    mod_locate_target(mod, project_dir)
  })
  # Report any location failures
  location_failures <- Filter(function(m) !isTRUE(m$meta$location_found), located_mod_list)
  if (length(location_failures) > 0) {
      cat("\nWarning: Could not locate targets for", length(location_failures), "modification(s):\n")
      for (mod in location_failures) {
          cat(" - File:", mod$meta$file, "| Error:", mod$meta$location_error %||% "Unknown reason", "\n")
      }
  }
  cat("Target location phase complete.\n")


  # --- 2. Define the Shiny Gadget UI ---
  ui <- miniUI::miniPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      #info_ui p { white-space: pre-wrap; word-wrap: break-word; margin-bottom: 5px; }
      .btn-container { margin-top: 2px; }
      .btn-container .btn { margin-right: 5px; }
    "))),

    miniUI::miniContentPanel(
      shiny::div(class = "btn-container",
        shiny::conditionalPanel(
          condition = "output.mods_in_progress == true",
          shiny::actionButton("apply", "Apply",  class = "btn-xs btn-primary"),
          shiny::actionButton("skip", "Skip",  class = "btn-xs"),
          shiny::actionButton("back", "Back", class = "btn-xs"),
          shiny::actionButton("insert_here", "Insert Here", class = "btn-xs"),
          shiny::actionButton("abort", "Cancel", class = "btn-xs")
        ),
        shiny::conditionalPanel(
          condition = "output.mods_in_progress == false",
          shiny::actionButton("back_from_finish", "Back", class = "btn-xs"),
          shiny::actionButton("finish", "Finish", class = "btn-primary")
        )
      ),
      shiny::conditionalPanel(
        condition = "output.mods_in_progress == true",
        shiny::uiOutput("info_ui")
      ),
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
      mods = located_mod_list,
      current = 1,
      total = length(located_mod_list),
      log = character(0)
    )

    output$mods_in_progress <- shiny::reactive({ rv$current <= rv$total })
    shiny::outputOptions(output, "mods_in_progress", suspendWhenHidden = FALSE)

    observeEvent(rv$current, {
      req(rv$current >= 1, rv$current <= rv$total)
      mod <- rv$mods[[rv$current]]
      # RELOCATE target, as previous edits might have changed line numbers
      tryCatch({
        rv$mods[[rv$current]] <- mod_locate_target(mod, project_dir)
      }, error = function(e) {
         rstudioapi::showDialog("Relocation Error", paste("Could not re-locate target for", mod$meta$file, ":\n", e$message))
         return()
      })
      # Only navigate if the location was actually found
      if (isTRUE(rv$mods[[rv$current]]$meta$location_found)) {
        navigate_to_modification_target(rv$mods[[rv$current]])
      }
    }, ignoreInit = FALSE, ignoreNULL = TRUE) # Run on startup

    output$info_ui <- shiny::renderUI({
      req(rv$current <= rv$total)
      mod <- rv$mods[[rv$current]]
      shiny::HTML(paste0("<h4>Modification ", rv$current, " of ", rv$total, "</h4>", mod_to_html_descr(mod)))
    })


    output$final_log <- shiny::renderPrint({ cat(paste(rv$log, collapse="\n")) })

    observeEvent(input$apply, {
      mod <- rv$mods[[rv$current]]

      # If location was not found, treat as a skip.
      if (!isTRUE(mod$meta$location_found)) {
        rv$log <- c(rv$log, paste0("SKIPPED (location not found): Change ", rv$current, " (", mod$meta$scope, ") on '", mod$meta$file, "'."))
        if (rv$current <= rv$total) rv$current <- rv$current + 1
        return()
      }

      tryCatch({
        apply_modification_via_api(mod)
        meta <- mod$meta
        location_desc <- if(meta$end_line < meta$start_line) {
            paste("insertion at line", meta$start_line)
        } else {
            paste("replacement at lines", meta$start_line, "to", meta$end_line)
        }

        scope_desc <- if (meta$scope == "function") {
          fun_name <- meta$function_name %||% meta$insert_after_fun %||% meta$insert_before_fun %||% "(unnamed)"
          paste0("function '", fun_name, "'")
        } else {
          meta$scope
        }

        log_msg <- paste0("APPLIED: Change ", rv$current, " (", scope_desc, ") in '", meta$file, "' (", location_desc, ").")
        rv$log <- c(rv$log, log_msg)

        Sys.sleep(0.5)
        if (rv$current <= rv$total) rv$current <- rv$current + 1
      }, error = function(e) {
        msg <- paste("ERROR applying Change", rv$current, "to", mod$meta$file, ":", e$message)
        rv$log <- c(rv$log, msg)
        rstudioapi::showDialog("Error Applying Change", e$message)
      })
    })

    observeEvent(input$insert_here, {
      mod <- rv$mods[[rv$current]]
      payload <- mod$payload
      ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)

      if (is.null(ctx) || is.null(ctx$id) || !nzchar(ctx$path)) {
        msg <- "Could not get RStudio editor context. Please make sure a file is open and active."
        rstudioapi::showDialog("Error", msg)
        rv$log <- c(rv$log, paste("INSERT FAILED (no context): Change", rv$current))
        return()
      }
      # insertText uses the current selection(s) to insert or replace
      rstudioapi::insertText(text = payload, id = ctx$id)
      rstudioapi::documentSave(id = ctx$id)

      log_msg <- paste0("INSERTED (manually): Change ", rv$current, " at cursor in '", basename(ctx$path), "'.")
      rv$log <- c(rv$log, log_msg)
      # Do not advance current. User should inspect and then click skip.
    })


    observeEvent(input$skip, {
      mod <- rv$mods[[rv$current]]
      rv$log <- c(rv$log, paste0("SKIPPED: Change ", rv$current, " (", mod$meta$scope, ") on '", mod$meta$file, "'."))
      if (rv$current <= rv$total) rv$current <- rv$current + 1
    })

    observeEvent(input$back, {
      if (rv$current > 1) rv$current <- rv$current - 1
    })

    observeEvent(input$back_from_finish, {
      if (rv$total > 0) rv$current <- rv$total
    })


    observeEvent(input$abort, { shiny::stopApp(invisible(rv$log)) })
    observeEvent(input$finish, { shiny::stopApp(invisible(rv$log)) })
  }

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

  location_status_html <- ""
  if (!isTRUE(meta$location_found)) {
    err_msg <- meta$location_error %||% "Target location for modification could not be determined."
    location_status_html <- paste0(
      "<p style='color:red; font-weight:bold;'>Location not found: ",
      htmltools::htmlEscape(err_msg),
      "</p>",
      "<p><i>This change will be skipped if you click 'Apply'. You can use 'Insert Here' to apply it manually at the cursor.</i></p>"
    )
  } else if (isTRUE(meta$location_is_fuzzy)) {
    location_status_html <- paste0(
      "<p style='color:orange; font-weight:bold;'>Note: The target location is an approximate match.",
      " Please review the highlighted code in the editor carefully before applying.</p>"
    )
  }

  descr_str <- ""
  if (meta$scope=="function" && meta$is_insert) {
    descr_str <- paste0("Add function to <i>", htmltools::htmlEscape(meta$file),"</i>")
  } else if (meta$scope=="function" && !meta$is_insert) {
    descr_str <- paste0("Replace function <code>", htmltools::htmlEscape(meta[["function_name"]]), "</code> in <i>", htmltools::htmlEscape(meta$file), "</i>")
  } else if (meta$scope=="file") {
    descr_str <- paste0("Write complete file <i>", htmltools::htmlEscape(meta$file), "</i>")
  } else if (meta$scope=="lines") {
    descr_str <- paste0("Modify lines in <i>", htmltools::htmlEscape(meta$file), "</i>")
  }

  description_html <- paste0(
    "<h5>", descr_str, "</h5>",
    "<p><b>Description:</b> ", htmltools::htmlEscape(meta$descr), "</p>"
  )

  payload_html <- paste0(
    "<h5>Proposed Change:</h5>",
    "<pre>", htmltools::htmlEscape(mod$payload), "</pre>"
  )

  paste0(location_status_html, description_html, payload_html)
}
