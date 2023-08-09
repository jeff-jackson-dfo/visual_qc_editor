#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(oce)
library(leaflet)
library(ODF)

visualQcApp <- function(...) {
  
  # Define UI for application that draws a histogram
  ui <-  fillPage(title = "Visual Quality Control Editor", bootstrap = TRUE, padding = 10,
                  
    tags$head(
      tags$style(
        HTML('
          #sidebarPanel {background-color: #CCFFFF; width: 100%;}
          #big-heading {color: red; font-size: 40px; font-style: bold; margin-left: 10px; text-align: center;}
          #qflag {color: green; font-size: 12px}
          #show {color: #000000; font-style: bold; font-size: 18px;}
        ')
      )
    ),
    
    # Create the title and style it.
    titlePanel(tags$h1(id = "big-heading", "Visual Quality Flag Editor")),
  
    # Sidebar with a slider input for number of bins
    fillRow(flex = c(1,4),
            fillCol(flex = c(NA,NA,NA),
                    align = "center",
                    id = "sidebarPanel",
                    # Select file component.
                    div(
                      fileInput("file1",
                                "Select ODF File",
                                multiple = TRUE,
                                accept = c("text/ODF", "text/Ocean Data Format", ".odf")
                      ),
                    ),
                    # Select parameter component.
                    div(
                      selectInput(
                        inputId = "parameter",
                        label = strong("Select X Parameter:"),
                        choices = NULL
                      ),
                    ),
                    # Select quality flag component.
                    div(
                      radioButtons("qflag", "Change Quality Flag of Selected Point(s):",
                        choiceNames = list(
                          "Appears-Correct",
                          "Doubtful",
                          "Erroneous"
                        ),
                        choiceValues = list(
                          "1", "3", "4"
                        ),
                      ),
                      actionButton("changeQF", "Change QF", class = "btn-primary")
                    )
            ),
            # Show a plot of the generated distribution
            tabsetPanel(
              id = "tabsetpanel",
              tabPanel("CTD Summary",
                       style="overflow-y:scroll; max-height: 70vh;",
                       # Output the CTD file summary information.
                       htmlOutput("summary")
              ),
              tabPanel("Standard OCE CTD Plot",
                       # Show the standard OCE plot for the current CTD file.
                       div(style = "height:1000px;", plotOutput("oceplot", height = "80%", width = "80%")),
              ),
              tabPanel("Profile Plot",
                       # Show the profile plot for the user selected parameter vs pressure.
                       div(style = "height:1000px;", plotlyOutput("plot", height = "80%", width = "80%"))
              ),
              tabPanel("Leaflet Map",
                       # Show the profile's position on a Leaflet generated map.
                       div(style = "height:1000px;", leafletOutput("mymap", height = "100%", width = "100%"))
              )
              # tabPanel("CTD Position",
              #          style="overflow-y:scroll; max-height: 70vh;",
              #          # Output the CTD file metadata.
              #          htmlOutput("ctd_position")
              # )
            )
    )
  
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
  
    odfData <- reactiveValues(dfCTD = NULL)
    
    observe({
      
      req(input$file1)
      
      # Read in the CTD data.
      ctd <- read.ctd(as.character(input$file1$datapath))
      
      # Initialize the flag scheme.
      ctd <- oce::initializeFlagScheme(ctd, "DFO")
      
      odfData$ctd <- ctd
      
      # Coerce the CTD object's data list into an external data frame.
      odfData$dfCTD <- as.data.frame(ctd[['data']])
      
      # Coerce the CTD object's metadata list into an external list.
      odfData$lMeta <- as.list(ctd[['metadata']])
      
    })
    
    vars <- reactive({
      vars <- names(odfData$dfCTD)
    })
    
    observe({
      updateSelectInput(session,
                        "parameter",
                        choices = vars(),
                        selected = "temperature")
    })
    
    observeEvent(input$parameter, {
      
      flags <- c(
        "1" = "Appears-Correct",
        "3" = "Doubtful",
        "4" = "Erroneous"
      )
      
      updateRadioButtons(session,
                         "qflag",
                         choices = flags,
                         selected = "Appears-Correct",
                         inline = TRUE)
      
    })
    
    output$summary <- renderText({
      req(input$parameter)
      HTML(paste(capture.output(summary(odfData$ctd)), collapse = "<br>"))
    })
    
    output$oceplot <- renderPlot({
      # Make sure that the selectInput drop down list box contains something before continuing.
      req(input$parameter)
      
      # Output the standard OCE profile plot.
      oce::plot(odfData$ctd)
    })
    
    qf <- reactive({
      odfData$ctd
    })
    
    df <- reactive({
      odfData$dfCTD
    })
    
    meta <- reactive({
      odfData$lMeta
    })
    
    # output$ctd_position <- renderText({ paste0("The initial position at the start of the CTD cast: (", odfData$lMeta$initialLatitude, ", ", odfData$lMeta$initialLongitude, ")") })
    
    updateCTD <- function(uctd) {
      odfData$ctd <- uctd
    }
    
    output$plot <- renderPlotly(
      {
        # Make sure that the selectInput drop down list box contains something before continuing.
        req(input$parameter)
        
        # If there is no CTD loaded yet then do nothing.
        if (is.null(df())) {
          return()
        }
        
        # Get data as a data frame.
        cdata <- df()
        
        # Produce the profile plot.
        p <- ggplot(cdata)
        p <- p + geom_point(aes(.data[[input$parameter]], .data[["pressure"]]), color = "black")
        p <- p + geom_line(aes(.data[[input$parameter]], .data[["pressure"]]), color = "red")
        p <- p + theme_bw()
        p <- p + scale_y_reverse()
        # dragmode can be "lasso" for lasso select or "select" for box select.
        gg <- ggplotly(p) %>% layout(dragmode = "select")
        # highlight(gg, dynamic = TRUE)
        
        # supply custom colors to the brush 
        cols <- toRGB(RColorBrewer::brewer.pal(3, "Dark2"), 0.5)
        highlight(gg, on = "plotly_selected", color = cols, dynamic = TRUE)
        
        # Use attrs_selected() for complete control over the selection appearance
        # note any relevant colors you specify here should override the color argument
        s <- attrs_selected(
          showlegend = TRUE,
          mode = "lines+markers",
          marker = list(symbol = "x")
        )
        
        highlight(layout(gg, showlegend = TRUE), selected = s)
        
        # %>% onRender(javascript)
      }
    )
    
    observeEvent(input$changeQF, {
      # Make sure that the selectInput drop down list box contains something before continuing.
      # req(input$parameter)
      
      select_data <- event_data("plotly_selected")
      
      if (!is.null(select_data)) {
        # Debug information for selected point(s).
        # cat(str(select_data))
        # cat(paste("(", sprintf("%.04f", select_data$x), ",", sprintf("%.04f", select_data$y), ")\n",  sep = ""))
        # p <- p + geom_point(aes_string(x = input$parameter[idx], y = "pressure"[idx]), color = "red")
        # p <- p + scale_color_manual(breaks = cdata$qf, values = )
        #   idx <- which(cdata$pressure %in% c(select_data[3]$x) & paste("cdata$", input$parameter, sep = ""))
        # cdata[idx, "col"] <- "red"
      }
      
      # Get the indices for the selected point(s).
      idx <- select_data$pointNumber
      cat(paste0("\nFlagged point(s): ", idx))
      cat(as.character(paste0("\nParameter flagged: ", input$parameter)))
      cat(as.character(paste0("\nQuality flag used: ", input$qflag)))
      
      # Update the CTD object with the updated quality flags for the selected parameter.
      if (!is.null(idx)) {
        cat("\nSetting flags")
        if (input$qflag == "Appears-Correct") {
          qc = setFlags(qf(), as.character(input$parameter), idx, value = 1)
          # updateCTD(qc)
        }
        else if (input$qflag == "Doubtful") {
          qc = setFlags(qf(), as.character(input$parameter), idx, value = 3)
          # updateCTD(qc)
        }
        else if (input$qflag == "Erroneous") {
          qc = setFlags(qf(), as.character(input$parameter), idx, value = 4)
          # updateCTD(qc)
        }
        
        # Save the updated CTD object for debugging purposes (at least at this point).
        save(qc, file = "oceCTD.RData")
        cat("\nQuality controlled oce object saved to file oceCTD.RData")
      }
      
      # Exchange the old CTD object with the updated one with flag changes.
      odfData$ctd <- qc
    })
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng = -63, lat = 45, zoom = 3) %>%
        addTiles() %>% # Add default OpenStreetMap map tiles
        addMarkers(lng = as.numeric(odfData$lMeta$initialLongitude), lat = as.numeric(odfData$lMeta$initialLatitude))
    })
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)

}