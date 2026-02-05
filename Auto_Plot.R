--
  title: "Auto_PlotR"
author: "aidan borkan"
date: "2026-02-05"
output: html_document
---
  
 
# ------------------------------------------------------------
# Auto_PlotR
# Shiny app: Upload an .rds containing a LIST of data frames
# Each data frame must include:
#   - Temperature
#   - Abundance
#   - collapsed_uniprot
#
# Features:
#   - Upload up to ~1 GB
#   - Choose which dataframe(s) to plot (All or one)
#   - Choose which collapsed_uniprot to plot (First or user-selected)
#   - Pagination so you can render N plots per page
#   - Progress indicator while generating plots
# ------------------------------------------------------------

# expand up to ~1 GB upload limit for RDS files
options(shiny.maxRequestSize = 1000 * 1024^2)

# load required libraries
library(shiny)
library(ggplot2)
library(gridExtra)

# ----------------------------
# UI section
# ----------------------------
ui <- fluidPage(
  titlePanel("Abundance vs Temperature Plotter (Auto_PlotR)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Choose RDS File", accept = c(".rds")),
      
      tags$hr(),
      
      # These appear only after a valid file is loaded
      uiOutput("df_selector_ui"),
      uiOutput("uniprot_mode_ui"),
      uiOutput("uniprot_selector_ui"),
      
      tags$hr(),
      
      # Pagination controls (useful when plotting many dataframes)
      numericInput("plots_per_page", "Plots per page", value = 10, min = 1, step = 1),
      uiOutput("page_selector_ui"),
      
      tags$hr(),
      helpText("Expected .rds: a list of data frames, each with Temperature, Abundance, collapsed_uniprot.")
    ),
    
    mainPanel(
      plotOutput("plots", height = "900px"),
      verbatimTextOutput("status")
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  # ---- Reactive storage for loaded data ----
  df_list_rv <- reactiveVal(NULL)
  
  # ---- Load uploaded RDS safely ----
  observeEvent(input$dataFile, {
    req(input$dataFile)
    
    x <- tryCatch(
      readRDS(input$dataFile$datapath),
      error = function(e) {
        df_list_rv(NULL)
        output$status <- renderText(paste("Error reading RDS:", e$message))
        return(NULL)
      }
    )
    
    # Validate basic structure: must be a list
    if (is.null(x) || !is.list(x)) {
      df_list_rv(NULL)
      output$status <- renderText("Uploaded file is not a valid LIST of data frames.")
      return()
    }
    
    # Ensure it has names (for nicer UI); if missing, generate
    if (is.null(names(x)) || any(!nzchar(names(x)))) {
      names(x) <- paste0("df_", seq_along(x))
    }
    
    df_list_rv(x)
    output$status <- renderText(sprintf("Loaded %d data frame(s).", length(x)))
  })
  
  # ---- UI: dataframe selector (All or a single df) ----
  output$df_selector_ui <- renderUI({
    req(df_list_rv())
    
    df_names <- names(df_list_rv())
    selectInput(
      "df_choice",
      "Data frame(s) to plot",
      choices = c("All (paged)" = "__ALL__", df_names),
      selected = "__ALL__"
    )
  })
  
  # ---- UI: choose how to select collapsed_uniprot ----
  output$uniprot_mode_ui <- renderUI({
    req(df_list_rv())
    radioButtons(
      "uniprot_mode",
      "collapsed_uniprot selection",
      choices = c("Use first entry in each data frame" = "first",
                  "Choose from dropdown" = "choose"),
      selected = "first"
    )
  })
  
  # ---- Helper: get the active df list based on selection ----
  active_df_list <- reactive({
    req(df_list_rv())
    x <- df_list_rv()
    
    if (identical(input$df_choice, "__ALL__")) {
      return(x)
    } else {
      return(x[input$df_choice])
    }
  })
  
  # ---- Build the set of uniprots available (for dropdown mode) ----
  available_uniprots <- reactive({
    req(active_df_list())
    x <- active_df_list()
    
    # Union of all collapsed_uniprot values across selected dataframes
    vals <- unique(unlist(lapply(x, function(df) {
      if (!is.data.frame(df)) return(character(0))
      if (!"collapsed_uniprot" %in% names(df)) return(character(0))
      unique(as.character(df$collapsed_uniprot))
    }), use.names = FALSE))
    
    vals <- vals[is.finite(match(vals, vals))] # no-op guard; keeps vector clean
    vals <- vals[!is.na(vals) & nzchar(vals)]
    sort(unique(vals))
  })
  
  # ---- UI: collapsed_uniprot selector (only when mode == "choose") ----
  output$uniprot_selector_ui <- renderUI({
    req(df_list_rv())
    req(input$uniprot_mode)
    
    if (input$uniprot_mode != "choose") return(NULL)
    
    u <- available_uniprots()
    if (length(u) == 0) {
      return(helpText("No collapsed_uniprot values found in the selected data frame(s)."))
    }
    
    selectInput(
      "uniprot_choice",
      "Select collapsed_uniprot",
      choices = u,
      selected = u[1]
    )
  })
  
  # ---- Pagination: compute pages based on selected dfs ----
  num_pages <- reactive({
    req(active_df_list())
    n <- length(active_df_list())
    per <- input$plots_per_page
    if (is.null(per) || per < 1) per <- 10
    ceiling(n / per)
  })
  
  output$page_selector_ui <- renderUI({
    req(active_df_list())
    pages <- num_pages()
    if (pages <= 1) return(NULL)
    
    sliderInput(
      "page",
      "Page",
      min = 1,
      max = pages,
      value = 1,
      step = 1
    )
  })
  
  # ---- Helper: validate dataframe columns ----
  has_required_cols <- function(df) {
    required <- c("Temperature", "Abundance", "collapsed_uniprot")
    is.data.frame(df) && all(required %in% names(df))
  }
  
  # ---- Plot builder for one dataframe ----
  process_and_plot <- function(df, df_name, uniprot_mode, uniprot_choice = NULL) {
    
    if (!has_required_cols(df)) {
      return(NULL)
    }
    
    # Decide which uniprot to plot
    if (identical(uniprot_mode, "first")) {
      chosen <- unique(as.character(df$collapsed_uniprot))[1]
    } else {
      chosen <- uniprot_choice
    }
    
    if (is.null(chosen) || !nzchar(chosen)) return(NULL)
    
    df_sub <- df[df$collapsed_uniprot == chosen, , drop = FALSE]
    if (nrow(df_sub) == 0) return(NULL)
    
    ggplot(df_sub, aes(x = Temperature, y = Abundance, color = collapsed_uniprot)) +
      geom_line() +
      theme_minimal() +
      labs(
        title = paste("Abundance vs Temperature:", chosen, "(", df_name, ")"),
        x = "Temperature",
        y = "Abundance"
      )
  }
  
  # ---- Main plotting ----
  output$plots <- renderPlot({
    req(active_df_list())
    req(input$uniprot_mode)
    
    x <- active_df_list()
    df_names <- names(x)
    
    # Determine which page slice to render
    per <- input$plots_per_page
    if (is.null(per) || per < 1) per <- 10
    
    page <- input$page
    if (is.null(page)) page <- 1
    
    start_idx <- (page - 1) * per + 1
    end_idx   <- min(length(x), page * per)
    
    idx <- seq.int(start_idx, end_idx)
    
    # Build plots with progress feedback
    plot_list <- list()
    total <- length(idx)
    
    withProgress(message = "Plotting...", value = 0, {
      for (k in seq_along(idx)) {
        i <- idx[k]
        
        setProgress(
          value = k / total,
          message = sprintf("Plotting %d of %d (page %d)", k, total, page)
        )
        
        p <- process_and_plot(
          df = x[[i]],
          df_name = df_names[i],
          uniprot_mode = input$uniprot_mode,
          uniprot_choice = input$uniprot_choice
        )
        
        if (!is.null(p)) plot_list[[length(plot_list) + 1]] <- p
      }
    })
    
    if (length(plot_list) == 0) {
      output$status <- renderText("No valid plots to display (missing columns or empty selection).")
      return(invisible(NULL))
    }
    
    # Render all plots on the page in a vertical stack
    do.call(grid.arrange, c(plot_list, ncol = 1))
  })
  
}

shinyApp(ui = ui, server = server)
```
