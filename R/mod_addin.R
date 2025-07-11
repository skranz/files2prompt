#' @importFrom shiny fluidPage verbatimTextOutput actionButton observeEvent
#' @importFrom shiny reactiveValues renderPrint shinyApp stopApp isolate runGadget
#' @importFrom miniUI miniPage gadgetTitleBar
#' @keywords internal
review_modifications_addin <- function() {
  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("miniUI", quietly = TRUE)) {
    stop("The 'shiny' and 'miniUI' packages are required for this addin.")
  }

  # --- 1. Find and parse the AI response file ---
  project_dir <- tryCatch(rstudioapi::getActiveProject(), error = function(e) getwd())
  response_file <- find_ai_response_file()
  raw_text <- paste(readLines(response_file, warn = FALSE), collapse = "\n")
  mod_list <- parse_ai_response(raw_text)

  if (length(mod_list) == 0) {
    message("No valid modifications found in '", response_file, "'.")
    return(invisible())
  }

  # --- 2. Define the Shiny Gadget UI ---
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "Apply AI Modifications",
      right = miniUI::miniTitleBarButton("close", "Close", primary = FALSE)
    ),
    miniUI::miniContentPanel(
      verbatimTextOutput("info"),
      verbatimTextOutput("payload")
    ),
    shiny::actionButton("apply", "Apply Change", class = "btn-primary"),
    shiny::actionButton("skip", "Skip Change"),
    shiny::actionButton("abort", "Abort All")
  )

  # --- 3. Define the Shiny Server Logic ---
  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      mods = mod_list,
      current = 1,
      total = length(mod_list),
      log = character(0)
    )

    # --- Renderers for the current modification ---
    output$info <- shiny::renderPrint({
      if (rv$current > rv$total) return(cat("All modifications processed."))
      mod <- rv$mods[[rv$current]]$meta
      cat(
        "Suggestion ", rv$current, " of ", rv$total, "\n",
        "----------------------------------\n",
        "File:      ", mod$file, "\n",
        "Operation: ", mod$operation, "\n",
        "Description: ", mod$description %||% "N/A", "\n",
        sep = ""
      )
    })

    output$payload <- shiny::renderPrint({
      if (rv$current > rv$total) {
        cat("Log of actions:\n", paste(rv$log, collapse="\n"))
        return()
      }
      mod <- rv$mods[[rv$current]]
      cat("--- Code to Apply ---\n")
      cat(mod$payload)
    })

    # --- Button Observers ---
    observeEvent(input$apply, {
      mod <- rv$mods[[rv$current]]
      tryCatch({
        apply_modification(mod, project_dir)
        log_msg <- paste("APPLIED:", mod$meta$operation, "on", mod$meta$file)
        rv$log <- c(rv$log, log_msg)
      }, error = function(e) {
        log_msg <- paste("ERROR applying to", mod$meta$file, ":", e$message)
        rv$log <- c(rv$log, log_msg)
        warning(log_msg)
      })
      rv$current <- rv$current + 1
    })

    observeEvent(input$skip, {
      mod <- rv$mods[[rv$current]]
      log_msg <- paste("SKIPPED:", mod$meta$operation, "on", mod$meta$file)
      rv$log <- c(rv$log, log_msg)
      rv$current <- rv$current + 1
    })

    observeEvent(input$close, {
      shiny::stopApp(invisible(rv$log))
    })
    observeEvent(input$abort, {
       shiny::stopApp(invisible(rv$log))
    })
  }

  # --- 4. Run the Gadget ---
  cat("Starting modification review tool...\n")
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("AI Modifications", width = 800, height = 700))
}
