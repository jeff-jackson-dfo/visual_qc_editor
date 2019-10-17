#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(oce)
library(plotly)

# Define UI for application that draws a histogram
ui <- fillPage(
  
    padding = 10,
    
    # Use a CSS theme. The file is in the "www" subfolder.
    theme = "bootstrap.pulse.css",
    
    tags$head(tags$style(
        HTML('* {font-family: BentonSans Book; font-size: 20px}')
    )),
    
    # Style horizontal rules (hr).
    tags$head(tags$style(HTML(
        'hr {border-top: 1px solid #000000}'
    ))),
    
    # Create the title and style it.
    tags$h1(id = "big-heading", "Visual QC Editor"),
    tags$head(
      tags$style(
        HTML('
              #big-heading{color: red; font-size: 40px; font-style: bold; }
              #filepath{color: blue; font-size: 16px; font-style: bold; height = "60%"}
             ')
    )),
    
    # Sidebar with a slider input for number of bins
    fluidRow(
      column(3,
            # Insert a break above the button.
            tags$br(),
            # Create the button that when pressed will load the selected ODF file.
            tags$p(style = "text-align: center; border-color: #f00; vertical-align: bottom",
              actionButton("loadButton", tags$b("Load ODF File"))),
            tags$style(
              "#loadButton{color: #A52A2A; font-size: 20px; background-color: #FFDEAD; border-color: #A9A9A9; border-width: 2px}"
        )
      ),
      column(9,
          # Text box containing file path and name.
          tags$div(
            style = "font-size:25px;",
              textInput(
                  inputId = "filepath",
                  label = div(style = "font-size:20px", "Selected ODF File:"),
                  value = ""
              )
          ),
        selectInput(
            inputId = "parameter",
            label = strong("Select X Parameter:"),
            choices = NULL
        )
      )
    ),
    plotlyOutput(
      "profilePlot", width = "100%", height = "80%"
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    re1 <- eventReactive(input$loadButton,
                         {
                             fileWithPath <- file.choose()
                         })
    
    output$filep <- renderText({
        odfFile <- basename(re1())
    })
    
    observe({
        # Display the current ODF file with path.
        odfFile <- basename(re1())
        
        # Display the current ODF file without path in the textInput box.
        updateTextInput(session, "filepath", value = as.character(odfFile))
        
    })
    
    odfData <- reactiveValues()
    
    observe({
        # Read in the CTD data.
        ctd <- read.ctd(re1())
        
        # Initialize the flag scheme.
        ctd <- oce::initializeFlagScheme(ctd, "DFO")
        
        # Coerce the CTD object's data list into an external data frame.
        odfData$dfCTD <- as.data.frame(ctd[['data']])
        
        # Coerce the CTD object's metadata list into an external list.
        odfData$lMeta <- as.list(ctd[['metadata']])
        
        # nms <- as.numeric(row.names(odfData$dfCTD))
    })
    
    output$cn <- renderText({
        print(odfData$lMeta$cruiseNumber)
    })
    
    vars <- reactive({
        
        vars <- names(odfData$dfCTD)
    })
    
    output$nms <- reactive({
        print(odfData$nms)
    })
    
    observe({
        updateSelectInput(session,
                          "parameter",
                          choices = vars(),
                          selected = "temperature")
    })
    
    var <- reactive({
        input$parameter
    })
    
    output$param <- renderText({
        var()
    })
    
    df <- reactive({
        odfData$dfCTD
    })
    
    output$profilePlot <- renderPlotly({
        # Make sure that the selectInput drop down list box contains something before continuing.
        req(input$parameter)
        
        # If there is no CTD loaded yet then do nothing.
        if (is.null(df())) {
            return()
        }
        
        # Produce the profile plot.
        p = ggplot()
        p <-
            ggplot(data = df(), aes_string(x = input$parameter, y = "pressure")) + geom_point()
        p <- p + theme_bw()
        p <- p + scale_y_reverse()
        ggplotly(p) %>% layout(dragmode = "lasso")
      }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
