#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(oce)
library(plotly)

# Define UI for application that draws a histogram
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
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            br(),
            
            # Press button to load an ODF file.
            p(style = "text-align: center; border-color: #f00", 
              actionButton("loadButton",tags$b("Load ODF File"))),
            tags$style("#loadButton{color: blue; font-size: 20px; background-color: yellow; border-color: black; border-width: 5px}"),

            br(),

            # Text box containing file path and name.
            div
            ( style = "font-size:20px;", 
                textInput(inputId = "filepath", 
                          label = div(style = "font-size:20px", "Selected ODF File:"), 
                          value = ""
                )
            ),
            
            # Insert Horizontal Rule (line) ----
            # hr(),

            selectInput(inputId = "parameter", label = strong("Select X Parameter:"), choices = NULL)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            # textOutput("filep"),
            textOutput("params")
            # textOutput("cn")
            
            # plotlyOutput("profilePlot")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    re1 <- eventReactive(
        input$loadButton, 
        {
            fileWithPath <- file.choose()
        }
    )
    
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
        odfData$dfCTD <- data.frame(ctd[['data']])
    })

    # metadata <- reactive({
    #     data.frame(odfData[['metadata']])
    # })
    # 
    # output$cn <- renderText({
    #     print(metadata$cruiseNumber)
    # })
    
    vars <- reactive({
        vars <- names(odfData$dfCTD)
    })
    
    observe({
        updateSelectInput(session, "parameter", choices = vars(), selected = "temperature")
    })
    
    # output$profilePlot <- renderPlotly({
    #     p <- ggplot(data = odfData$dfCTD, aes(x = input$parameter, y = pressure, key = nms)) + geom_point()
    #     p <- p + scale_y_reverse()
    #     ggplotly(p) %>% layout(dragmode = "lasso")
    # })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
