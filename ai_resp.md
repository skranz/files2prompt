Hello! As an expert R programmer, I've reviewed your `files2prompt` package. It's a very clever and useful tool for integrating RStudio with LLMs. I've identified several areas for improvement, particularly in the "Apply Modifications" addin, to make it more robust and user-friendly, especially when the AI's proposed locations are ambiguous.

Below is a description of the proposed improvements, followed by the specific code changes in the required `!MODIFICATION` format.

### Codebase Description and Proposed Improvements

The project is well-structured, with distinct responsibilities for parsing, locating, and applying modifications. The core logic for the two main addins, "Generate Prompt" and "Apply Modifications," is sound.

The main area for improvement is in the `mod_loc.R` and `mod_addin.R` files, which handle the process of finding where a modification should be applied and the user's interaction with this process. When an AI response specifies a location that has changed or is ambiguous, the current addin can fail to find the target, leaving the user without much recourse.

My proposed changes address this by:

1.  **Improving Target Location Robustness:** The logic for finding functions (`locate_scope_function`) and line sequences (`find_line_sequence`) has been enhanced to return *all* potential matches (including fuzzy matches), not just the first or best one. This allows the user to cycle through possibilities.
2.  **Introducing a "Find" Button:** A new "Find" button is added to the "Apply Modifications" addin UI. If multiple potential locations are found for a change, this button lets the user cycle through them. If no location was found initially, clicking "Find" can still be useful if the underlying find logic identifies a fuzzy match.
3.  **Enhancing the User Interface:**
    *   The addin now always navigates to the target file, even if the specific lines or function can't be located, allowing for manual inspection.
    *   When a location isn't found, the UI now clearly displays what was being searched for (e.g., the function name or the specific lines of code).
    *   When multiple matches are found, the UI displays a status like "Match 1 of 3" to provide context.
4.  **Improving "Insert Here" Workflow:** After using the "Insert Here" button, the addin will now automatically proceed to the next modification, streamlining the review process, just as if "Apply" had been clicked.

These changes will make the modification review process smoother and more powerful, gracefully handling cases where the AI's context is slightly out of date.

Here are the code modifications:

!MODIFICATION R/mod_addin.R
scope = "file"
file = "R/mod_addin.R"
is_new_file = false
description = '''Updates the "Apply Modifications" addin UI and server logic.

- Adds a "Find" button and status text to the UI.
- Changes the "Insert Here" button to automatically advance to the next modification.
- Modifies the server logic to always navigate to the target file, even if the exact location is not found.
- Implements the "Find" button logic to cycle through potential matches.
- Enhances the description (`mod_to_html_descr`) to show what was searched for when a location is not found.
'''
---
```r
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

    # Only attempt to select/position cursor if the location was found.
    if (isTRUE(mod$meta$location_found)) {
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

  # 1a. Parse all mods, now without stopping on TOML errors
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
      .btn-container { margin-top: 2px; margin-bottom: 8px; }
      .btn-container .btn { margin-right: 5px; }
      #find_status_ui p { font-size: 0.9em; color: #555; margin: 0; padding: 0; }
    "))),

    miniUI::miniContentPanel(
      shiny::div(class = "btn-container",
        shiny::conditionalPanel(
          condition = "output.mods_in_progress == true",
          shiny::actionButton("apply", "Apply",  class = "btn-xs btn-primary"),
          shiny::actionButton("skip", "Skip",  class = "btn-xs"),
          shiny::actionButton("back", "Back", class = "btn-xs"),
          shiny::actionButton("find_target", "Find", class = "btn-xs"),
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
      updated_mod <- tryCatch({
        mod_locate_target(mod, project_dir)
      }, error = function(e) {
         rstudioapi::showDialog("Relocation Error", paste("Could not re-locate target for", mod$meta$file, ":\n", e$message))
         # Ensure mod object is updated with error state
         mod$meta$location_found <- FALSE
         mod$meta$location_error <- e$message
         return(mod)
      })
      rv$mods[[rv$current]] <- updated_mod

      # Always navigate if a file path is available, even if location isn't found
      if (!is.null(updated_mod$meta$file_path) && nzchar(updated_mod$meta$file_path)) {
        navigate_to_modification_target(updated_mod)
      }
    }, ignoreInit = FALSE, ignoreNULL = TRUE) # Run on startup

    output$info_ui <- shiny::renderUI({
      req(rv$current <= rv$total)
      mod <- rv$mods[[rv$current]]
      shiny::HTML(paste0("<h4>Modification ", rv$current, " of ", rv$total, "</h4>", mod_to_html_descr(mod)))
    })

    output$find_status_ui <- shiny::renderUI({
      req(rv$current <= rv$total)
      mod <- rv$mods[[rv$current]]
      num_matches <- mod$meta$num_potential_locations %||% 0
      if (num_matches > 1) {
        shiny::tags$div(id="find_status_ui",
          shiny::tags$p(paste0("Found ", num_matches, " potential matches. Showing match ", mod$meta$current_match_index, "."))
        )
      } else {
        NULL
      }
    })


    output$final_log <- shiny::renderPrint({ cat(paste(rv$log, collapse="\n")) })

    observeEvent(input$apply, {
      mod <- rv$mods[[rv$current]]

      # If location was not found (which includes parse errors), treat as a skip.
      if (!isTRUE(mod$meta$location_found)) {
        reason <- mod$meta$location_error %||% "location not found"
        rv$log <- c(rv$log, paste0("SKIPPED (", reason, "): Change ", rv$current, " for '", mod$meta$file, "'."))
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

    observeEvent(input$find_target, {
      mod <- rv$mods[[rv$current]]
      num_matches <- mod$meta$num_potential_locations %||% 0

      if (num_matches > 1) {
        current_index <- mod$meta$current_match_index %||% 1
        new_index <- (current_index %% num_matches) + 1
        mod$meta$current_match_index <- new_index

        new_loc <- mod$meta$potential_locations[[new_index]]
        mod$meta$start_line <- new_loc$start
        mod$meta$end_line <- new_loc$end
        mod$meta$location_is_fuzzy <- new_loc$is_fuzzy
        
        # This is a valid location, so ensure location_found is true
        mod$meta$location_found <- TRUE

        rv$mods[[rv$current]] <- mod
        navigate_to_modification_target(mod)
      } else if (num_matches == 1) {
         rstudioapi::showDialog("Find", "Only one potential match was found.")
      } else {
         rstudioapi::showDialog("Find", "No alternative locations could be found for this modification.")
      }
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

      # Advance to the next modification
      Sys.sleep(0.5)
      if (rv$current <= rv$total) rv$current <- rv$current + 1
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

  # Handle TOML parse error first
  if (isTRUE(meta$parse_error)) {
    error_html <- paste0(
      "<p style='color:red; font-weight:bold;'>Metadata Parsing Error: ",
      htmltools::htmlEscape(meta$parse_error_message %||% "Unknown error"),
      "</p>",
      "<p><i>The metadata for this block could not be read. 'Apply' will skip this change. You can use 'Insert Here' to apply the payload manually at the cursor.</i></p>",
      "<h5>Original Metadata Block:</h5>",
      "<pre>", htmltools::htmlEscape(meta$raw_toml %||% ""), "</pre>"
    )
    payload_html <- paste0(
      "<h5>Payload:</h5>",
      "<pre>", htmltools::htmlEscape(mod$payload), "</pre>"
    )
    return(paste0(error_html, payload_html))
  }


  location_status_html <- ""
  if (!isTRUE(meta$location_found)) {
    err_msg <- meta$location_error %||% "Target location for modification could not be determined."
    
    searched_for_str <- ""
    if (meta$scope == "function" && !is.null(meta$function_name)) {
      searched_for_str <- paste0("<b>Function name:</b> <code>", htmltools::htmlEscape(meta$function_name), "</code>")
    } else if (meta$scope == "lines" && !is.null(meta$replace_lines)) {
      searched_for_str <- paste0("<b>Lines to replace:</b><pre>", htmltools::htmlEscape(meta$replace_lines), "</pre>")
    } else if (meta$scope == "lines" && !is.null(meta$insert_after_lines)) {
      searched_for_str <- paste0("<b>Insert after lines:</b><pre>", htmltools::htmlEscape(meta$insert_after_lines), "</pre>")
    }

    location_status_html <- paste0(
      "<p style='color:red; font-weight:bold;'>Location not found: ", htmltools::htmlEscape(err_msg), "</p>",
      "<p><i>'Apply' will skip this change. You can use 'Find' to search for fuzzy matches or 'Insert Here' to apply it manually at the cursor.</i></p>",
       if (nzchar(searched_for_str)) paste0("<h5>Searched for:</h5>", searched_for_str) else ""
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
    "<p><b>Description:</b> ", htmltools::htmlEscape(meta$description), "</p>"
  )

  payload_html <- paste0(
    "<h5>Proposed Change:</h5>",
    "<pre>", htmltools::htmlEscape(mod$payload), "</pre>"
  )

  paste0(location_status_html, description_html, payload_html)
}
```
!END_MODIFICATION R/mod_addin.R
!MODIFICATION R/mod_loc.R
scope = "file"
file = "R/mod_loc.R"
is_new_file = false
description = '''This change refactors the location-finding logic to be more robust and to support finding multiple potential matches.

- `mod_locate_target` is updated to orchestrate the new logic, populating the `mod$meta` object with a list of all potential locations.
- `locate_scope_function` now searches for functions by name, with a fallback to fuzzy matching if no exact match is found. It returns all plausible locations.
- `locate_scope_lines` is updated to use the new `find_line_sequence` and handle a list of returned locations.
- `find_line_sequence` is significantly improved. It now returns a list of all possible matches (both exact and approximate), sorted by quality, instead of just the first one or NULL. This enables the "Find" button's cycling behavior in the addin.
'''
---
```r
#' Locate the target for a modification
#'
#' This is the main dispatcher function. It takes a `mod` object, finds its
#' full file path, and determines the exact start and end lines for the edit.
#' It populates `mod$meta` with `file_path`, `start_line`, `end_line`, and
#' several status fields: `location_found`, `location_is_fuzzy`, `location_error`.
#' It also populates `potential_locations` for the "Find" functionality.
#'
#' @param mod A single parsed modification object.
#' @param project_dir The root directory of the project.
#' @return The modified `mod` object with location information.
mod_locate_target <- function(mod, project_dir) {
  restore.point("mod_locate_target")

  # Handle case where TOML parsing failed upstream
  if (isTRUE(mod$meta$parse_error)) {
    mod$meta$location_found <- FALSE
    mod$meta$location_is_fuzzy <- FALSE
    mod$meta$location_error <- mod$meta$parse_error_message %||% "TOML metadata could not be parsed."
    mod$meta$start_line <- 1
    mod$meta$end_line <- 0
    return(mod)
  }

  scope <- mod$meta$scope

  # Initialize meta fields for location status
  mod$meta$location_found <- FALSE
  mod$meta$location_is_fuzzy <- FALSE
  mod$meta$location_error <- NULL
  mod$meta$potential_locations <- list()
  mod$meta$num_potential_locations <- 0
  mod$meta$current_match_index <- 0


  # 1. Find target file
  target_file <- find_project_file(mod$meta$file, project_dir)
  is_new <- is.null(target_file)

  if (is_new && !isTRUE(mod$meta$is_new_file)) {
    mod$meta$location_error <- paste0("Could not find file '", mod$meta$file, "' and it's not marked as a new file.")
    mod$meta$start_line <- 1
    mod$meta$end_line <- 0
    return(mod)
  }
  if (!is_new && isTRUE(mod$meta$is_new_file)) {
    warning("File '", mod$meta$file, "' already exists but is_new_file is true. It may be overwritten.")
  }
  if (is_new) {
    target_file <- file.path(project_dir, mod$meta$file)
  }
  mod$meta$file_path <- target_file

  # 2. Dispatch to find lines based on scope
  loc_result <- switch(scope,
    "file" = locate_scope_file(mod),
    "function" = locate_scope_function(mod),
    "lines" = locate_scope_lines(mod),
    list(locations = list(),
         error_msg = paste("Unknown modification scope:", scope))
  )

  # 3. Process the location results
  mod$meta$potential_locations <- loc_result$locations
  mod$meta$num_potential_locations <- length(loc_result$locations)

  if (mod$meta$num_potential_locations > 0) {
    mod$meta$location_found <- TRUE
    mod$meta$current_match_index <- 1
    first_loc <- loc_result$locations[[1]]
    mod$meta$start_line <- first_loc$start
    mod$meta$end_line <- first_loc$end
    mod$meta$location_is_fuzzy <- first_loc$is_fuzzy
    mod$meta$location_error <- NULL
  } else {
    mod$meta$location_found <- FALSE
    mod$meta$start_line <- 1
    mod$meta$end_line <- 0
    mod$meta$location_error <- loc_result$error_msg
  }

  mod
}


# --- Scope-specific Location Finders ---

locate_scope_file <- function(mod) {
  restore.point("locate_scope_file")
  # For `scope="file"`, we replace the whole file.
  if (!file.exists(mod$meta$file_path)) { # New file
    loc <- list(start = 1, end = 0, found = TRUE, is_fuzzy = FALSE)
  } else {
    line_count <- length(readLines(mod$meta$file_path, warn = FALSE))
    loc <- list(start = 1, end = line_count, found = TRUE, is_fuzzy = FALSE)
  }
  list(locations = list(loc), error_msg = NULL)
}

locate_scope_function <- function(mod) {
  restore.point("locate_scope_function")
  target_file <- mod$meta$file_path
  if (!file.exists(target_file)) {
    return(list(locations = list(),
                error_msg = paste0("File '", basename(target_file), "' does not exist.")))
  }
  original_lines <- readLines(target_file, warn = FALSE)
  all_funs <- f2p_all_fun_locs(target_file)
  meta = mod$meta

  if ("insert_top" %in% names(meta)) {
    return(list(locations = list(list(start = 1, end = 0, is_fuzzy = FALSE)), error_msg = NULL))
  } else if ("insert_bottom" %in% names(meta)) {
    n_lines <- NROW(original_lines)
    return(list(locations = list(list(start = n_lines + 1, end = n_lines, is_fuzzy = FALSE)), error_msg = NULL))
  }

  fun_name = meta$function_name %||% meta$insert_after_fun %||% meta$insert_before_fun
  if (is.null(fun_name)) {
      return(list(locations = list(),
                  error_msg = "No function name specified for replacement or relative insertion."))
  }

  # Find exact matches
  locs <- all_funs[all_funs$fun_name == fun_name, ]
  
  # If no exact match, try fuzzy matching
  is_fuzzy_match <- FALSE
  if (nrow(locs) == 0) {
      fuzzy_matches <- agrep(fun_name, all_funs$fun_name, max.distance = 0.2, value = TRUE)
      if (length(fuzzy_matches) > 0) {
          locs <- all_funs[all_funs$fun_name %in% fuzzy_matches, ]
          is_fuzzy_match <- TRUE
      }
  }

  if (NROW(locs) == 0) {
    return(list(locations = list(),
                error_msg = paste0("Function '", fun_name, "' not found in '", basename(target_file), "'.")))
  }

  # Convert data.frame rows to a list of location objects
  potential_locations <- lapply(seq_len(nrow(locs)), function(i) {
    loc <- locs[i, ]
    if ("insert_after_fun" %in% names(meta)) {
      list(start = loc$end_line_fun + 1, end = loc$end_line_fun, is_fuzzy = is_fuzzy_match)
    } else if ("insert_before_fun" %in% names(meta)) {
      list(start = loc$start_line_comment, end = loc$start_line_comment - 1, is_fuzzy = is_fuzzy_match)
    } else {
      # It's a function replacement.
      has_roxygen_comments <- any(grepl("^\\s*#'", strsplit(mod$payload, "\n")[[1]]))
      start_replace_line <- if (has_roxygen_comments) loc$start_line_comment else loc$start_line_fun
      list(start = start_replace_line, end = loc$end_line_fun, is_fuzzy = is_fuzzy_match)
    }
  })

  list(locations = potential_locations, error_msg = NULL)
}

locate_scope_lines <- function(mod) {
  target_file <- mod$meta$file_path
  if (!file.exists(target_file)) {
    return(list(locations = list(),
                error_msg = paste0("File '", basename(target_file), "' does not exist.")))
  }
  original_lines <- readLines(target_file, warn = FALSE)
  meta <- mod$meta
  
  # --- Insertion based on line content ---
  if (!is.null(meta$insert_after_lines) || !is.null(meta$replace_lines)) {
    sequence_to_find <- strsplit(meta$replace_lines %||% meta$insert_after_lines, "\n")[[1]]
    
    locations <- find_line_sequence(original_lines, sequence_to_find, approximate = TRUE)

    if (length(locations) == 0) {
      err_key <- if (!is.null(meta$replace_lines)) "replace_lines" else "insert_after_lines"
      return(list(locations = list(),
                  error_msg = paste0("Could not find the '", err_key, "' sequence in '", basename(target_file), "'.")))
    }
    
    # If it's an insertion, adjust the location
    if (!is.null(meta$insert_after_lines)) {
        locations <- lapply(locations, function(loc) {
            loc$start <- loc$end + 1
            loc$end <- loc$end
            loc
        })
    }
    return(list(locations = locations, error_msg = NULL))
  }
  
  # --- Insertion based on position (top, bottom, after_fun) ---
  is_fuzzy <- FALSE
  res <- tryCatch({
    all_funs <- if (grepl("\\.R$", target_file, ignore.case = TRUE)) f2p_all_fun_locs(target_file) else NULL
    get_insertion_line(meta, original_lines, all_funs)
  }, error = function(e) e)

  if (inherits(res, "error")) {
    return(list(locations = list(), error_msg = res$message))
  }
  insert_line <- res
  
  final_loc <- list(start = insert_line, end = insert_line - 1, is_fuzzy = is_fuzzy)
  return(list(locations = list(final_loc), error_msg = NULL))
}


# --- Location Helpers ---

get_insertion_line <- function(meta, lines, fun_locs) {
  if (isTRUE(meta$insert_top)) return(1)
  if (isTRUE(meta$insert_bottom)) return(length(lines) + 1)

  fun_name <- meta$insert_after_fun %||% meta$insert_before_fun
  if (is.null(fun_name)) {
    # No function-based insertion, default to bottom of file
    return(length(lines) + 1)
  }

  # If fun_locs is NULL (e.g. not an R file) or empty, we can't find the function.
  if (is.null(fun_locs) || nrow(fun_locs) == 0) {
    stop("Cannot locate functions for insertion because no functions were found in '", meta$file, "'.")
  }

  loc <- fun_locs[fun_locs$fun_name == fun_name, ]
  if (nrow(loc) == 0) {
    stop("Function '", fun_name, "' not found in '", meta$file, "' for insertion.")
  }

  if (!is.null(meta$insert_after_fun)) {
    return(loc$end_line_fun[1] + 1)
  }
  if (!is.null(meta$insert_before_fun)) {
    # Insert before the function's preceding comment block
    return(loc$start_line_comment[1])
  }

  # Fallback: default to bottom of the file if no other criteria match
  return(length(lines) + 1)
}

find_line_sequence <- function(source_lines, sequence_to_find, approximate = FALSE, max_dist = 0.2) {
  if (length(sequence_to_find) == 0) return(list())
  len_seq <- length(sequence_to_find)
  len_src <- length(source_lines)
  if (len_seq > len_src) return(list())

  # 1. Find all exact matches
  exact_matches <- list()
  for (i in 1:(len_src - len_seq + 1)) {
    chunk <- source_lines[i:(i + len_seq - 1)]
    if (all(chunk == sequence_to_find)) {
      exact_matches[[length(exact_matches) + 1]] <- list(start = i, end = i + len_seq - 1, is_fuzzy = FALSE, dist = 0)
    }
  }
  if (length(exact_matches) > 0) {
    return(exact_matches)
  }

  # 2. If no exact match and approximate is false, return empty list
  if (!approximate) return(list())

  # 3. Find all approximate matches
  seq_str <- paste(sequence_to_find, collapse = "\n")
  all_matches <- list()
  for (i in 1:(len_src - len_seq + 1)) {
    chunk_lines <- source_lines[i:(i + len_seq - 1)]
    chunk_str <- paste(chunk_lines, collapse = "\n")
    dist <- utils::adist(seq_str, chunk_str, ignore.case = TRUE, costs = 1)[1, 1]
    
    normalized_dist <- dist / nchar(seq_str)
    if (is.finite(normalized_dist) && normalized_dist <= max_dist) {
        all_matches[[length(all_matches) + 1]] <- list(start = i, end = i + len_seq - 1, is_fuzzy = TRUE, dist = normalized_dist)
    }
  }
  
  # Sort matches by distance (best first)
  if (length(all_matches) > 0) {
    all_matches <- all_matches[order(sapply(all_matches, `[[`, "dist"))]
    warning(paste("Used approximate matching to find", length(all_matches), "potential line sequence(s)."))
  }

  return(all_matches)
}


f2p_all_fun_locs <- function(file_path) {
  restore.point("fp_get_all_function_locations")
  code = readLines(file_path)
  funs_loc = extract_function_source(code)
  funs_loc
}
```
!END_MODIFICATION R/mod_loc.R
