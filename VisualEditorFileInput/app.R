# Load Required Libraries
library(shiny)
library(plotly)
library(oce)
library(shinyWidgets)

# Define User Interface (UI) for the application.
ui <- fillPage(
  
  # includeCSS("E:/R_HOME/Visual_QC_Editor/visual_qc_editor/VisualEditorFileInput/vqce.css"),
  
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
            #sidebarPanel {background-color: #CCFFFF; margin: 10px; width: 100%; text-align: center; vertical-align: center;}
            #big-heading {color: red; font-size: 40px; font-style: bold; margin-left: 10px; text-align: center;}
            #qflag {color: green; font-size: 12px}
            #show {color: #000000; font-style: bold; font-size: 18px;}
           ')
    )
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # id = "sidebarPanel",
      div(
        fileInput("file1", "Select ODF File",
                  multiple = TRUE,
                  accept = c("text/ODF",
                             "text/Ocean Data Format",
                             ".odf")
        )
      ),
      div(
        selectInput(
          inputId = "parameter",
          label = strong("Select X Parameter:"),
          choices = NULL
        )
      ),
      # Quality Flag choices.
      div(
        radioButtons(
          inputId = "qflag",
          label = "Quality Flag:",
          choices =
            c(
              # "0" = "No-Quality-Control",
              "1" = "Appears-Correct",
              # "2" = "Appears-Inconsistent",
              "3" = "Doubtful",
              "4" = "Erroneous"
              # "5" = "Changed",
              # "8" = "QC-By-Originator",
              # "9" = "Missing"
            ),
          selected = "Appears-Correct",
          inline = TRUE
        )
      )
      # div(
      #   # Action Button when pressed opens a modal dialog with summary information for the current CTD file.
      #   actionButton("show1", "Show CTD Summary")
      # ),
      # div(  
      #   # Action Button when pressed opens a modal dialog with summary information for the current CTD file.
      #   actionButton("show2", "Show OCE CTD Plot")
      # )   
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("CTD Summary",
          # Output the CTD file summary information.
          textOutput("summary")
        ),
        tabPanel("Standard OCE CTD Plot",
          # Show the standard OCE plot for the current CTD file.
          plotOutput("oceplot")
        ),
        tabPanel("Profile Plot",
          # Show the profile plot for the user selected parameter vs pressure.
          plotlyOutput("plot")
        )
      )
    )
    # textOutput("cn")
    # textOutput("odfFile")
    # fluidRow(
    # verbatimTextOutput("brush"),
    # textOutput("odfFile"),
    # textOutput("filePath")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Observer (reactive endpoint).
    output$odfFile <- renderText({
      req(input$file1)
      input$file1$name
    })

    # inFilePath <- reactive({
    #   req(input$file1)
    #   cat(str(input$file1))
    #   input$file1$datapath
    # })
    
    # output$filePath <- renderText({
    #   as.character(inFilePath())
    # })
    
    # Observer (reactive endpoint).
    observe({
      req(input$file1)

      # Display the current ODF file with path.
      odfFile <- input$file1$name

      # Display the current ODF file without path in the textInput box.
      updateTextInput(session, "filepath", value = as.character(odfFile))
    })

    odfData <- reactiveValues()

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

    output$cn <- renderText({
        print(odfData$lMeta$cruiseNumber)
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

    output$summary <- renderText({
      req(input$parameter)
      capture.output(summary(odfData$ctd))
    })
    
    # When the "show" action button is pressed; a modal dialog displays with a summary of the current CTD object.
    observeEvent(input$show1, {
      # Make sure a file was successfully loaded before proceeding further.
      req(input$parameter)
      # Display a modal dialog.
      sendSweetAlert(
        session = session,
        title = "",
        text = p(HTML(paste(capture.output(summary(odfData$ctd)), collapse = "<br>")), style = "font-size:16px; text-align:left;"),
        type = "",
        width = "1000px",
        html = TRUE
      )
    })

    output$oceplot <- renderPlot(
      {
        # Make sure that the selectInput drop down list box contains something before continuing.
        req(input$parameter)
        
        # Output the standard OCE profile plot.
        oce::plot(odfData$ctd)
      }
    )
    
    observeEvent(input$show2, {
      req(input$parameter)
      showModal(
        modalDialog(size = "l",
          plotOutput("oceplot")
        )
      )
    })

    output$plot <- renderPlotly(
      {
        # Make sure that the selectInput drop down list box contains something before continuing.
        req(input$parameter)

        select_data <- event_data("plotly_selected")

        # If there is no CTD loaded yet then do nothing.
        if (is.null(df())) {
            return()
        }

        # Get data as a data frame.
        cdata <- df()
        # cat(str(cdata))
        
        # Get the metadata as a list.
        mdata <- meta()
        # cat(str(mdata$flags))
        
        # Get the indices for the selected point(s).
        idx <- select_data$pointNumber
        cat(idx, "\n")
        cat(as.character(input$parameter), "\n")
        cat(as.character(input$qflag), "\n")
        
        if (!is.null(idx)) {
          if (input$qflag == "Appears-Correct") {
            qc = setFlags(qf(), as.character(input$parameter), idx, value = 1)
          }
          else if (input$qflag == "Doubtful") {
            qc = setFlags(qf(), as.character(input$parameter), idx, value = 3)
          }
          else if (input$qflag == "Erroneous") {
            qc = setFlags(qf(), as.character(input$parameter), idx, value = 4)
          }
          
          saveRDS(qc, file = "oceCTD.RData")
          cat("qc oce object saved to file oceCTD.RData")
          
          # ps <- paste("qc[['metadata']]$flags$", as.character(input$parameter), "[idx]", sep = "")
          # pander::evals("ps")
        }

        # cdata$qf <- as.factor(mdata$flags[var()])

        # Output the QFs for the current X parameter.
        # cat(str(mdata$flags[var()]))

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
