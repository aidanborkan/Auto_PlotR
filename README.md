# Auto_PlotR
Automated temperature curve plotting from RDS file

#we need to increase the maximum upload size to 1GB
options(shiny.maxRequestSize=1000*1024^2)
#load necessary libraries
library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Abundance vs Temperature Plotter for Multiple Dataframes"),

  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Choose RDS File", accept = c(".rds"))
    ),
    mainPanel(
      plotOutput("plots")
    )
  )
)
       
#define server
server <- function(input, output) {
  
  output$plots <- renderPlot({
    req(input$dataFile)
    
    df_list <- tryCatch({
      readRDS(input$dataFile$datapath)
    }, error = function(e) {
      cat("Error in reading RDS file:", e$message, "\n")
      return(NULL)
    })
    
    if(is.null(df_list) || !is.list(df_list)) {
      cat("Uploaded file is not a valid list of dataframes\n")
      return(NULL)
    }
    
    process_and_plot <- function(df, name) {
      if(!all(c("Temperature", "Abundance", "collapsed_uniprot") %in% names(df))) {
        cat(paste("Dataframe", name, "does not have the required columns.\n"))
        return(NULL)
      }
      
      # Filter for the first collapsed_uniprot entry
      first_uniprot <- unique(df$collapsed_uniprot)[1]
      df <- df[df$collapsed_uniprot == first_uniprot, ]
      
      ggplot(df, aes(x = Temperature, y = Abundance, color = collapsed_uniprot)) +
        geom_line() +
        theme_minimal() +
        labs(title = paste("Abundance vs Temperature for", first_uniprot, "in", name), 
             x = "Temperature", y = "Abundance")
    }
    
    num_dataframes <- length(df_list)
    plot_list <- list()
    
    withProgress(message = 'Plotting in progress...', {
      for (i in seq_along(df_list)) {
        setProgress(message = sprintf("Plotting dataframe %d of %d", i, num_dataframes),
                    value = i / num_dataframes)
        plot_list[[i]] <- process_and_plot(df_list[[i]], names(df_list)[i])
      }
    })
    
    plot_list <- Filter(Negate(is.null), plot_list)
    
    if(length(plot_list) > 0) {
      do.call(gridExtra::grid.arrange, c(plot_list, ncol = 1))
    } else {
      cat("No valid plots to display.\n")
    }
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
