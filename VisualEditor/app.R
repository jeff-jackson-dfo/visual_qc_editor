library(shiny)
library(plotly)
library(oce)
# library("htmltools")
# library(bsplus)
# library(shinyjs)
# library(shinyWidgets)
# library(DT)


# Define UI for application that draws a histogram
ui <- fillPage(
  
  # use_bs_tooltip(),
  # use_bs_popover(),
  
  padding = 10,
    
  # Use a CSS theme. The file is in the "www" subfolder.
  # theme = "bootstrap.pulse.css",
  
  tags$head(
    tags$style(
      HTML('* {font-family: BentonSans Book; font-size: 20px}')
    )
  ),
  
  # Style horizontal rules (hr).
  tags$head(
    tags$style(
      HTML('
        hr {border-top: 1px solid #000000}
      ')
    )
  ),
  
  # Create the title and style it.
  tags$h1(id = "big-heading", "Visual QC Editor"),
  
  tags$head(
    tags$style(
      HTML('
            #big-heading {color: red; font-size: 40px; font-style: bold; }
            #filepath {color: black; font-size: 16px; font-style: bold; height = "60%"}
            #qflag {color: green; font-size: 12px}
           ')
    )
  ),
  
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
    column(3,
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
    ),
    column(4,
           
       # HTML("<div class='container'><br>
       #        <h1>Quality Flag</h1>
       #        <div>
       #          <label id='no-quality-control'>
       #            <input type='radio' value='0' role='button'> 0
       #          </label>
       #          &nbsp;
       #          <label id='appears-correct'>
       #            <input type='radio' value='1' role='button'> 1
       #          </label>
       #          &nbsp;
       #          <label id='appears-inconsistent'>
       #            <input type='radio' value='2' role='button'> 2
       #          </label>
       #        </div>
       #      </div>")
          
       radioButtons(inputId = "qflag",
                    label = "Quality Flag:",
                    choices =
                      c(
                        "0" = "No-Quality-Control",
                        "1" = "Appears-Correct",
                        # "2" = "Appears-Inconsistent",
                        "3" = "Doubtful",
                        "4" = "Erroneous"
                        # "5" = "Changed",
                        # "8" = "QC-By-Originator",
                        # "9" = "Missing"
                        ),
                      selected = "No-Quality-Control",
                      inline = TRUE
                    )
    )
  ),

  plotlyOutput("plot", width = "100%", height = "60%"),
  
  fluidRow(
    # verbatimTextOutput("click"),
    verbatimTextOutput("brush")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Reactive expression (conductor).
    re1 <- eventReactive(input$loadButton, { file.choose() } )

    # Observer (reactive endpoint).
    output$filep <- renderText({
      odfFile <- basename(re1())
    })
    
    # Observer (reactive endpoint).
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
      
      odfData$ctd <- ctd
      
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
    
    qf <- reactive({
      odfData$ctd
    })
    
    df <- reactive({
      odfData$dfCTD
    })

    meta <- reactive({
      odfData$lMeta
    })
    
    # dt <- DT::datatable(as.data.frame(ctd[['data']]))
    
    # output$table <- renderDataTable(dt)
    
    output$plot <- renderPlotly(
      {
        # Make sure that the selectInput drop down list box contains something before continuing.
        req(input$parameter)
        
        select_data <- event_data("plotly_selected")
        
        # If there is no CTD loaded yet then do nothing.
        if (is.null(df())) {
            return()
        }
        
        cdata <- df()
        mdata <- meta()
        idx <- select_data$pointNumber
        
        # i <- which.min(abs(qc[["SA"]] - xy$x)/Sspan + abs(qc[["CT"]] - xy$y)/Tspan)
         # qc <- setFlags(qf(), input$parameter, i = i, value = input$qflag)
        # cat(str(qc[['metadata']]$flags))
        
        # cdata$qf <- as.factor(mdata$flags[var()])

        # Output the QFs for the current X parameter.
        cat(str(mdata$flags[var()]))
        
        # Produce the profile plot.
        p <- ggplot(cdata) 
        p <- p + geom_point(aes_string(x = input$parameter, y = "pressure"), color = "lightgreen")

        if (!is.null(select_data)) {
          # Debug information for selected point(s).
          # cat(str(select_data))
          # cat(paste("(", sprintf("%.04f", select_data$x), ",", sprintf("%.04f", select_data$y), ")\n",  sep = ""))
          # p <- p + geom_point(aes_string(x = input$parameter[idx], y = "pressure"[idx]), color = "red")
          # p <- p + scale_color_manual(breaks = cdata$qf, values = )
          #   idx <- which(cdata$pressure %in% c(select_data[3]$x) & paste("cdata$", input$parameter, sep = ""))
          # cdata[idx, "col"] <- "red"
        }
        
        p <- p + theme_bw()
        p <- p + scale_y_reverse()
        # dragmode can be "lasso" for lasso select or "select" for box select.
        ggplotly(p) %>% layout(dragmode = "select")
      }
    )

    # Selected
    selected <- reactive({
      event_data("plotly_selected", source = "master")
    })
    
    output$brush <- renderPrint({
      req(input$parameter)
      d <- event_data("plotly_selected")
       if (!is.null(d)) d
     })
    
    # eventReactive( input$qflag, {
    #   d <- event_data("plotly_selected")
    #   if (!is.null(d)) d
    # })
    
}

# Run the application
shinyApp(ui = ui, server = server)
