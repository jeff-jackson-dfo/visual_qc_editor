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
library(plotly)
library(oce)

loadData <- function(filename) {
  if (is.null(filename)) return
  ctd <- read.ctd(filename)
}

ui <- fluidPage(
  
  titlePanel("Visual QC Editor"),
  
  # Select file to load.
  selectInput("odf", "ODF File:", choices = NULL),
  
  # Select parameter to plot
  selectInput("parameter", "Parameter", choices = NULL)
  
)

server <- function(input, output, session) {
  
  path <- 'E:/Data/AZMP/2019/COR2019001/DATASHOP_PROCESSING/Step_3_Add_QF_Fields/'
  filesWithPath <- list.files(path = path, pattern = 'CTD_COR2019001_01\\w+_\\w+_DN\\.ODF', full.names = TRUE)
  files <- list.files(path = path, pattern = 'CTD_COR2019001_01\\w+_\\w+_DN\\.ODF', full.names = FALSE)

  updateSelectInput(session, "odf", choices = files)
  
  # Load the CTD data
  ctd <- reactive(
    loadData(input$odf)
  )

  # outVar <- reactive({
  #   names(ctd[['data']])
  # })
  # 
  # observe(
  #   updateSelectInput(session, "parameter", choices = outVar())
  # )

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
