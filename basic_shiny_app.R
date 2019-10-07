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
# last updated: "18/09/2019"
# output: html_document
# ---

library(shiny)
library(plotly)
library(oce)

ui <- fluidPage(
  titlePanel("Visual QC Editor"),
  mainPanel(
    uiOutput("plot")
  ),
  sidebarPanel(
    
    # Input: Select a file ----
    fileInput(inputId = "file1", 
              label = "Choose ODF File",
              multiple = FALSE,
              accept = c("text/odf", ".odf")
             ),

    # Text box containing file path and name.
    textInput(inputId = "filepath", label = "Path and Filename:", value = ""),
    
    selectInput(inputId = "parameter", label = strong("Select X Parameter to plot against Pressure"), choices = NULL)
  )
)

server <- (function(input, output, session) {
  
  observe({
    # if(!exists(input$parameter)) return()
    
    vars <- names(plot.ctd)
    
    # Display the current ODF file with path.
    updateTextInput(session, "filepath", value = input$file1)

    # Update the parameter list based on the current ODF file.
    updateSelectInput(session, "parameter", choices = vars)
  })
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  
  # Get the data object
  get_data <- reactive({
    
    # if(!exists(input$dataset)) return() # if no upload
    
    # if(check(input$dataset)) return()
    
    # Initialize the flag scheme.
    # print(paste("Initializing flag scheme for file: ", input$file1))

    ctd <- upload_data()
    
    # Initialize the flag scheme.
    ctd <- initializeFlagScheme(ctd, "DFO")

    df_data <- data.frame(ctd[['data']])
    df_flags <- data.frame(ctd[['metadata']]$flags)
    nms <- row.names(df_data)
    
    if(check(df_data)) return()
    
    df_data
    
  })

  #plotting function using ggplot2
  output$p <- renderPlot({

    plot.ctd <- get_data()

    # Conditions for plotting
    if(is.null(plot.ctd)) return()

    # Make sure variable has been loaded
    if(plot.ctd$variable == "") return()

    output$plot <- renderPlotly({
      p <- ggplot(data = plot.ctd$df_data, aes(x = plot.ctd$variable, y = pressure, key = nms)) + geom_point()
      p <- p + scale_y_reverse()
      ggplotly(p) %>% layout(dragmode = "lasso")
    })

  })
  
  
  # set uploaded file
  upload_data <- reactive({
    
    # Load the CTD data
    # path <- 'E:/Data/AZMP/2019/COR2019001/DATASHOP_PROCESSING/Step_3_Add_QF_Fields/'
    # filesWithPath <- list.files(path = path, pattern = 'CTD_COR2019001_01\\w+_\\w+_DN\\.ODF', full.names = TRUE)
    # files <- list.files(path = path, pattern = 'CTD_COR2019001_01\\w+_\\w+_DN\\.ODF', full.names = FALSE)
    # ctds <- lapply(filesWithPath, read.ctd)

    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    read.ctd(inFile$datapath)
  })
  
})

# Create Shiny app ----
shinyApp(ui = ui, server = server)
