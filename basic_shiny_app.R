#
# This is a Shiny web application. You can run the application in RStudio by clicking the 'Run App' button.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# ---
# visual_qc_editor
# title: "Visual Quality Control Tool for Inspecting CTD Profile Data"
# author: "Jeff Jackson"
# date: "17/09/2019"
# last updated: "17/09/2019"
# output: html_document
# ---

library(shiny)
library(oce)

ui <- fluidPage(
  titlePanel("Visual QC Editor"),
  uiOutput("parameters")
)

server <- function(input, output) {
  
  # Load the CTD data
  path <- 'E:/Data/AZMP/2019/COR2019001/DATASHOP_PROCESSING/Step_3_Add_QF_Fields/'
  filesWithPath <- list.files(path = path, pattern = 'CTD_COR2019001_01\\w+_\\w+_DN\\.ODF', full.names = TRUE)
  files <- list.files(path = path, pattern = 'CTD_COR2019001_01\\w+_\\w+_DN\\.ODF', full.names = FALSE)
  ctds <- lapply(filesWithPath, read.ctd)

  # Initialize the flag scheme.
  n <- length(ctds)
  for (i in 1:n)
  {
    print(paste("Initializing flag scheme for file: ", files[i]))
    # Initialize the flag scheme.
    ctds[[i]] <- initializeFlagScheme(ctds[[i]], "DFO")
  }
  
  # Use just the first file for now
  ctd <- ctds[[1]]
  
  outVar <- reactive({
    vars <- names(ctd[['data']])
    return(vars)
  })

    output$parameters = renderUI({
    # Select parameter to plot
    selectInput(inputId = "type", label = strong("Parameter"), choices = outVar())
  })

}

shinyApp(ui = ui, server = server)
