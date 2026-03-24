#' Navigate to a modification target in RStudio and select/highlight
#'
#' This function takes a *located* modification object and uses `rstudioapi`
#' to navigate to the file and select the relevant code range.
#'
#' @param mod A single parsed and *located* modification object. It must
#'   contain `mod$meta$file_path`, `start_line`, and `end_line`.
#' @keywords internal
navigate_to_modification_target = function(mod, project_dir = tryCatch(rstudioapi::getActiveProject(), error = function(e) getwd())) {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return(invisible(NULL))
  }
  restore.point("navigate_to_modification_target")

  tryCatch({
    target_file = mod$meta$file_path
    start_line = mod$meta$start_line
    end_line = mod$meta$end_line

    if (is.null(target_file)) {
      return(invisible(NULL))
    }

    # Ensure file exists (so navigation works) for new-file edits
    if (!file.exists(target_file)) {
      dir.create(dirname(target_file), showWarnings = FALSE, recursive = TRUE)
      file.create(target_file)
    }

    # Clamp selection bounds to the file length to avoid off-by-one issues in mod2
    original_lines = readLines(target_file, warn = FALSE)
    n_lines = length(original_lines)

    # allow insert at end: start may be n_lines + 1, end may be < start for insertion
    start_line = max(1, min(start_line, n_lines + 1))
    if (end_line >= 1) {
      end_line = max(0, min(end_line, n_lines))
    }

    rstudioapi::navigateToFile(target_file, line = max(1, min(start_line, n_lines)))

    Sys.sleep(0.1) # small delay so the editor is ready before selection

    if (isTRUE(mod$meta$location_found)) {
      if (end_line < start_line) {
        # Insertion: place the cursor at the insertion line
        rstudioapi::setCursorPosition(rstudioapi::document_position(start_line, 1))
      } else {
        end_col = if (end_line >= 1 && end_line <= n_lines) nchar(original_lines[end_line]) + 1 else 1
        rng = rstudioapi::document_range(
          rstudioapi::document_position(start_line, 1),
          rstudioapi::document_position(end_line, end_col)
        )
        rstudioapi::setSelectionRanges(list(rng))
      }
    }

    # Select again increase chance that it works...
    Sys.sleep(0.5)

    if (isTRUE(mod$meta$location_found)) {
      if (end_line < start_line) {
        # Insertion: place the cursor at the insertion line
        rstudioapi::setCursorPosition(rstudioapi::document_position(start_line, 1))
      } else {
        end_col = if (end_line >= 1 && end_line <= n_lines) nchar(original_lines[end_line]) + 1 else 1
        rng = rstudioapi::document_range(
          rstudioapi::document_position(start_line, 1),
          rstudioapi::document_position(end_line, end_col)
        )
        rstudioapi::setSelectionRanges(list(rng))
      }
    }


  }, error = function(e) {
    message("Info: Could not navigate/highlight target for '", mod$meta$file, "'. ", e$message)
  })

  invisible(NULL)
}

review_modifications_addin = function() {
  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("miniUI", quietly = TRUE)) {
    stop("The 'shiny' and 'miniUI' packages are required for this addin.")
  }
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
      stop("This addin requires RStudio version 1.2 or higher.")
  }
  library(files2prompt)

  # --- 1. PRE-PROCESSING (before UI) ---
  project_dir = tryCatch(rstudioapi::getActiveProject(), error = function(e) getwd())
  restore.point("review_modifications_addin")
  response_file = find_ai_response_file()
  raw_text = paste(readLines(response_file, warn = FALSE), collapse = "\n")

  # Detect classic vs mod2
  use_mod2 = FALSE
  if (exists("mod2_detect", mode = "function")) {
    use_mod2 = mod2_detect(raw_text)
  }

  if (!use_mod2) {
    # Classic path (unchanged logic; we still compute locations here)
    mod_list = parse_ai_response(raw_text)
    if (length(mod_list) == 0) {
      message("No valid modifications found in '", basename(response_file), "'.")
      return(invisible())
    }
    cat("Locating all modification targets...\n")
    located_mod_list = lapply(mod_list, function(mod) {
      mod_locate_target(mod, project_dir)
    })
    location_failures = Filter(function(m) !isTRUE(m$meta$location_found), located_mod_list)
    if (length(location_failures) > 0) {
      cat("\nWarning: Could not locate targets for", length(location_failures), "modification(s):\n")
      for (mod in location_failures) {
        cat(" - File:", mod$meta$file, "| Error:", mod$meta$location_error %||% "Unknown reason", "\n")
      }
    }
    cat("Target location phase complete.\n")
  } else {
    # mod2 path: already "located" (or not) by the mod2 parser
    if (!exists("mod2_parse_response", mode = "function")) {
      stop("mod2 parser not found.")
    }
    located_mod_list = mod2_parse_response(raw_text, project_dir)
    if (length(located_mod_list) == 0) {
      message("No mod2 code fragments found in '", basename(response_file), "'.")
      return(invisible())
    }
  }

  # --- 2. Define the Shiny Gadget UI ---
  ui = miniUI::miniPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      #info_ui p { white-space: pre-wrap; word-wrap: break-word; margin-bottom: 5px; }
      .btn-container { margin-top: 2px; margin-bottom: 8px; }
      .btn-container .btn { margin-right: 5px; }
      #find_status_ui p { font-size: 0.9em; color: #555; margin: 0; padding: 0; }
    "))),

    miniUI::miniContentPanel(
      shiny::uiOutput("action_buttons"),
      shiny::conditionalPanel(
        condition = "output.mods_in_progress == true",
        shiny::uiOutput("find_status_ui"),
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
  server = function(input, output, session) {
    rv = shiny::reactiveValues(
      mods = located_mod_list,
      current = 1,
      total = length(located_mod_list),
      log = character(0),
      use_mod2 = use_mod2
    )

    output$mods_in_progress = shiny::reactive({ rv$current <= rv$total })
    shiny::outputOptions(output, "mods_in_progress", suspendWhenHidden = FALSE)

    # Dynamic buttons so we can hide Apply in mod2 and rename Skip -> Next
    output$action_buttons = shiny::renderUI({
      if (rv$current <= rv$total) {
        mod = rv$mods[[rv$current]]
        btns = list()
        if (!isTRUE(mod$meta$from_mod2)) {
          btns = c(btns, list(shiny::actionButton("apply", "Apply", class = "btn-xs btn-primary")))
        }
        btns = c(
          btns,
          list(
            shiny::actionButton("insert_here", "Insert", class = "btn-xs"),
            shiny::actionButton("copy", "Copy", class = "btn-xs"),
            shiny::actionButton("find", "Find", class = "btn-xs"),
            shiny::actionButton("next", "Next", class = "btn-xs"),
            shiny::actionButton("back", "Back", class = "btn-xs"),
            shiny::actionButton("abort", "Cancel", class = "btn-xs")
          )
        )
        shiny::div(class = "btn-container", btns)
      } else {
        shiny::div(class = "btn-container",
          shiny::actionButton("back_from_finish", "Back", class = "btn-xs"),
          shiny::actionButton("finish", "Finish", class = "btn-primary")
        )
      }
    })

    observeEvent(rv$current, {
      shiny::req(rv$current >= 1, rv$current <= rv$total)
      mod = rv$mods[[rv$current]]
      # Classic mods: re-locate; mod2 mods are already located or not
      if (!isTRUE(mod$meta$from_mod2)) {
        updated_mod = tryCatch({
          mod_locate_target(mod, project_dir)
        }, error = function(e) {
          rstudioapi::showDialog("Relocation Error", paste("Could not re-locate target for", mod$meta$file, ":\n", e$message))
          mod$meta$location_found = FALSE
          mod$meta$location_error = e$message
          return(mod)
        })
        rv$mods[[rv$current]] = updated_mod
        mod = updated_mod
      }

      if (!is.null(mod$meta$file_path) && nzchar(mod$meta$file_path)) {
        navigate_to_modification_target(mod, project_dir)
      }
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    output$info_ui = shiny::renderUI({
      shiny::req(rv$current <= rv$total)
      mod = rv$mods[[rv$current]]
      shiny::HTML(paste0("<h4>Modification ", rv$current, " of ", rv$total, "</h4>", mod_to_html_descr(mod)))
    })

    output$find_status_ui = shiny::renderUI({
      shiny::req(rv$current <= rv$total)
      mod = rv$mods[[rv$current]]
      num_matches = mod$meta$num_potential_locations %||% 0
      if (num_matches > 1) {
        shiny::tags$div(id = "find_status_ui",
          shiny::tags$p(paste0("Found ", num_matches, " potential matches. Showing match ", mod$meta$current_match_index, "."))
        )
      } else {
        NULL
      }
    })

    output$final_log = shiny::renderPrint({ cat(paste(rv$log, collapse = "\n")) })

    observeEvent(input$copy, {
      mod = rv$mods[[rv$current]]
      ok = FALSE
      if (requireNamespace("clipr", quietly = TRUE)) {
        res = try(clipr::write_clip(mod$payload), silent = TRUE)
        ok = !inherits(res, "try-error")
      }
      if (!ok) rstudioapi::showDialog("Copy", "Could not copy to clipboard. Install the 'clipr' package for clipboard support.")
    })

    observeEvent(input$apply, {
      mod = rv$mods[[rv$current]]

      if (!isTRUE(mod$meta$location_found)) {
        reason = mod$meta$location_error %||% "location not found"
        rv$log = c(rv$log, paste0("SKIPPED (", reason, "): Change ", rv$current, " for '", mod$meta$file, "'."))
        if (rv$current <= rv$total) rv$current = rv$current + 1
        return()
      }

      tryCatch({
        apply_modification_via_api(mod)
        meta = mod$meta
        location_desc = if (meta$end_line < meta$start_line) {
          paste("insertion at line", meta$start_line)
        } else {
          paste("replacement at lines", meta$start_line, "to", meta$end_line)
        }
        scope_desc = meta$scope %||% "lines"
        log_msg = paste0("APPLIED: Change ", rv$current, " (", scope_desc, ") in '", meta$file, "' (", location_desc, ").")
        rv$log = c(rv$log, log_msg)

        Sys.sleep(0.5)
        if (rv$current <= rv$total) rv$current = rv$current + 1
      }, error = function(e) {
        msg = paste("ERROR applying Change", rv$current, "to", mod$meta$file, ":", e$message)
        rv$log = c(rv$log, msg)
        rstudioapi::showDialog("Error Applying Change", e$message)
      })
    })

    observeEvent(input$find, {
      mod = rv$mods[[rv$current]]
      num_matches = mod$meta$num_potential_locations %||% 0

      if (num_matches > 1) {
        current_index = mod$meta$current_match_index %||% 1
        new_index = (current_index %% num_matches) + 1
        mod$meta$current_match_index = new_index

        new_loc = mod$meta$potential_locations[[new_index]]
        mod$meta$start_line = new_loc$start
        mod$meta$end_line = new_loc$end
        mod$meta$location_is_fuzzy = new_loc$is_fuzzy %||% FALSE
        if (!is.null(new_loc$file)) {
          mod$meta$file_path = new_loc$file
          mod$meta$file = mod2_rel_from_abs(new_loc$file, project_dir)
        }

        mod$meta$location_found = TRUE
        rv$mods[[rv$current]] = mod
        navigate_to_modification_target(mod, project_dir)
      } else if (num_matches == 1) {
        rstudioapi::showDialog("Find", "Only one potential match was found.")
      } else {
        rstudioapi::showDialog("Find", "No alternative locations could be found for this modification.")
      }
    })

    observeEvent(input$insert_here, {
      mod = rv$mods[[rv$current]]
      ctx = tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)

      if (is.null(ctx) || is.null(ctx$id) || !nzchar(ctx$path)) {
        msg = "Could not get RStudio editor context. Please make sure a file is open and active."
        rstudioapi::showDialog("Error", msg)
        rv$log = c(rv$log, paste("INSERT FAILED (no context): Change", rv$current))
        return()
      }

      # Capture start position before insertion
      # Fallback to (row=1,col=1) if not available
      start_row = tryCatch(ctx$selection[[1]]$range$start[["row"]], error = function(e) 1)
      start_col = tryCatch(ctx$selection[[1]]$range$start[["column"]], error = function(e) 1)

      text = mod$payload
      if (!endsWith(text, "\n")) text = paste0(text, "\n")

      rstudioapi::insertText(text = text, id = ctx$id)
      rstudioapi::documentSave(id = ctx$id)

      # Try to select the just-inserted text
      lines_ins = strsplit(text, "\n", fixed = TRUE)[[1]]
      # If text ended with \n, last element is "", handle end column accordingly
      trailing_blank = length(lines_ins) > 0 && tail(lines_ins, 1) == ""
      n_rows = length(lines_ins) - if (trailing_blank) 1 else 0
      end_row = start_row + max(0, n_rows - 1)
      end_col = if (n_rows <= 1) {
        start_col + nchar(lines_ins[1])
      } else {
        nchar(lines_ins[n_rows]) + 1
      }

      rng = rstudioapi::document_range(
        rstudioapi::document_position(start_row, start_col),
        rstudioapi::document_position(end_row, end_col)
      )
      rstudioapi::setSelectionRanges(list(rng), id = ctx$id)

      log_msg = paste0("INSERTED (manual): Change ", rv$current, " at cursor in '", basename(ctx$path), "'.")
      rv$log = c(rv$log, log_msg)

      # Do NOT auto-advance; user will click Next explicitly
    })

    observeEvent(input[["next"]], {
      mod = rv$mods[[rv$current]]
      rv$log = c(rv$log, paste0("NEXT: Skipped change ", rv$current, " (", mod$meta$scope, ") on '", mod$meta$file, "'."))
      if (rv$current <= rv$total) rv$current = rv$current + 1
    })

    observeEvent(input$back, {
      if (rv$current > 1) rv$current = rv$current - 1
    })

    observeEvent(input$back_from_finish, {
      if (rv$total > 0) rv$current = rv$total
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

  if (isTRUE(meta$parse_error)) {
    error_html = paste0(
      "<p style='color:red; font-weight:bold;'>Metadata Parsing Error: ",
      htmltools::htmlEscape(meta$parse_error_message %||% "Unknown error"),
      "</p>",
      "<h5>Original Metadata Block:</h5>",
      "<pre>", htmltools::htmlEscape(meta$raw_toml %||% ""), "</pre>"
    )
    payload_html = paste0(
      "<h5>Proposed Change:</h5>",
      "<pre>", htmltools::htmlEscape(mod$payload), "</pre>"
    )
    return(paste0(error_html, payload_html))
  }

  action_label = switch(
    meta$scope %||% "",
    file = if (isTRUE(meta$is_new_file)) "Create new file" else "Rewrite existing file",
    "function" = if (isTRUE(meta$is_insert)) "Create new function" else "Modify existing function",
    lines = if (isTRUE(meta$is_insert)) "Insert or append lines" else "Modify existing lines",
    "Modification"
  )

  path_label = if (is_absolute_path(meta$file %||% "")) {
    "Absolute path"
  } else {
    "Project-relative path"
  }

  status_html = ""
  if (!isTRUE(meta$location_found)) {
    err_msg = meta$location_error %||% "Target location for modification could not be determined."
    status_html = paste0(
      "<p style='color:red; font-weight:bold;'>Location not found: ",
      htmltools::htmlEscape(err_msg),
      "</p>"
    )
  } else if (isTRUE(meta$location_is_fuzzy)) {
    status_html = paste0(
      "<p style='color:orange; font-weight:bold;'>Note: Target location is an approximate match. Review highlight.</p>"
    )
  }

  if (isTRUE(meta$is_new_dir)) {
    status_html = paste0(
      status_html,
      "<p style='color:red; font-weight:bold;'>Warning: target directory does not yet exist. This may indicate an incorrect file path.</p>"
    )
  } else if (isTRUE(meta$is_new_file) && (meta$scope %||% "") == "file") {
    status_html = paste0(
      status_html,
      "<p style='color:orange; font-weight:bold;'>Note: this will create a new file.</p>"
    )
  }

  info_html = ""
  if (!is.null(meta$file) && nzchar(meta$file)) {
    info_html = paste0(
      "<p><b>Action:</b> ", htmltools::htmlEscape(action_label), "</p>",
      "<p><b>File:</b> <code>", htmltools::htmlEscape(meta$file), "</code></p>",
      "<p><b>Path type:</b> ", htmltools::htmlEscape(path_label), "</p>"
    )

    if (!is.null(meta$file_path) && nzchar(meta$file_path)) {
      info_html = paste0(
        info_html,
        "<p><b>Resolved path:</b> <code>", htmltools::htmlEscape(meta$file_path), "</code></p>"
      )
    }

    if ((meta$scope %||% "") == "function") {
      fun_ref = meta$function_name %||% meta$insert_after_fun %||% meta$insert_before_fun
      if (!is.null(fun_ref) && nzchar(fun_ref)) {
        info_html = paste0(
          info_html,
          "<p><b>Function:</b> <code>", htmltools::htmlEscape(fun_ref), "</code></p>"
        )
      }
    }
  }

  header_html = ""
  if (isTRUE(meta$from_mod2) && nzchar(meta$explain_above %||% "")) {
    txt = meta$explain_above
    header_html = tryCatch({
      if (requireNamespace("commonmark", quietly = TRUE)) {
        paste0("<div>", commonmark::markdown_html(txt, extensions = TRUE), "</div>")
      } else {
        paste0("<pre>", htmltools::htmlEscape(txt), "</pre>")
      }
    }, error = function(e) {
      paste0("<pre>", htmltools::htmlEscape(txt), "</pre>")
    })
  } else {
    desc_txt = meta$description %||% ""
    if (nzchar(desc_txt)) {
      header_html = paste0("<p>", htmltools::htmlEscape(desc_txt), "</p>")
    }
  }

  payload_html = paste0(
    "<h5>Proposed Change:</h5>",
    "<pre>", htmltools::htmlEscape(mod$payload), "</pre>"
  )

  paste0(status_html, info_html, header_html, payload_html)
}
