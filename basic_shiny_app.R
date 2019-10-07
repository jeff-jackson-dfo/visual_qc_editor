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
# last updated: "02/10/2019"
# output: html_document
# ---

library(shiny)
library(shinyjs) # shinyjs is handy
library(plotly)
library(oce)

ui <- fluidPage(

  tags$head(
    tags$style(HTML('* {font-family: BentonSans Book; font-size: 16px}'))
  ),
  
  # Style horizontal rules (hr).
  tags$head(
    tags$style(HTML('hr {border-top: 1px solid #000000}'))
  ),
  
  # Create the title and style it.
  h1(id="big-heading", "Visual QC Editor"),
  tags$head(tags$style(
    HTML('#big-heading {
      color: red; 
      font-size: 40px;
      font-style: bold;
    }')
  )),
  
  sidebarLayout
  (
    sidebarPanel
    (
      
      # Text box containing file path and name.
      div
      ( style = "font-size:15px;", 
        textInput(inputId = "filepath", 
                  label = div(style = "font-size:20px", "Selected ODF File:"), 
                  value = ""
                 )
      ),
      
      # Insert Horizontal Rule (line) ----
      hr(),
      
      selectInput(inputId = "parameter", label = strong("Select X Parameter to plot against Pressure"), choices = NULL),

      actionButton("loadButton","Load ODF File")
      
    ),

    mainPanel
    (
      textOutput("filep"),
      textOutput("vars")
    )
  )
)

server <- (function(input, output, session) {
  
  re1 <- eventReactive(
    input$loadButton, 
    {
      fileWithPath <- file.choose()
    }
  )

  output$filep <- renderText({
    re1()
  })
  
  re2 <- eventReactive(
    input$loadButton, 
    {
      df <- dfObject()
    }
  )
  
  # output$plot <- renderUI({
  #   plotOutput("p")
  # })
  
  # Update the parameter list based on the current ODF file.
  # updateSelectInput(session, "parameter", choices = "")
  
  # Get file with path.
  ctdObject <- function()
  {
    # thePath <- 'E:/Data/AZMP/2019/COR2019001/DATASHOP_PROCESSING/Step_3_Add_QF_Fields/'
    # mypath <- choose.dir(default = thePath, caption = "Select folder")
    # as.character(file.choose())
    # fileWithPath <- paste(mypath,'/', myFile, sep = "")
    odfFile <- basename(fileWithPath)
    read.ctd(fileWithPath)
  }
  
  # Get the data object
  dfObject <- function()
  {
    ctd <- ctdObject()

    # Display the current ODF file with path.
    updateTextInput(session, "filepath", value = odfFile)

    # Initialize the flag scheme.
    # ctd <- oce::initializeFlagScheme(ctd, "DFO")

    df_data <- data.frame(ctd[['data']])

    # df_flags <- data.frame(ctd[['metadata']]$flags)
    # nms <- row.names(df_data)
    # if(check(df_data)) return()
    df_data
  }

  # output$vars <- function()
  # {
  #   df <- dfObject()
  #   names(df)
  # }

  # vars <- function()
  # {
  #   myObj <- getData()
  #   vars <- names(myObj)
  #   print("Nice Melons")
  # }
  
  # output$varlist <- renderPrint({
  #   p <- vars()
  #   print(p[1])
  # })
  
  # Output filename as text to MainPanel.
  # output$filename <- renderText({
  #   as.character(input$file1[1])
  # })
  
  #plotting function using ggplot2
  # output$p <- renderPlot({
  # 
  #   plot.ctd <- get_data()
  # 
  #   # Conditions for plotting
  #   if(is.null(plot.ctd)) return()
  # 
  #   # Make sure variable has been loaded
  #   if(plot.ctd$variable == "") return()
  # 
  #   output$plot <- renderPlotly({
  #     p <- ggplot(data = plot.ctd$df_data, aes(x = plot.ctd$variable, y = pressure, key = nms)) + geom_point()
  #     p <- p + scale_y_reverse()
  #     ggplotly(p) %>% layout(dragmode = "lasso")
  #   })
  # 
  # })
  
})

# Create Shiny app ----
shinyApp(ui = ui, server = server)
