library(shiny)
library(plotly)
library(oce)
library(shinyWidgets)

# Define User Interface (UI) for the application.

ui <- fillPage(title = "Visual Quality Flag Editor", bootstrap = TRUE, padding = 10,

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
    # sidebarLayout(
    #     sidebarPanel(
    fillRow(flex = c(1,4),
        fillCol(flex = c(NA,NA,NA),
            align="center",
            id = "sidebarPanel",
            # Select file component.
            div(
                fileInput("file1", "Select ODF File",
                          multiple = TRUE,
                          accept = c("text/ODF",
                                     "text/Ocean Data Format",
                                     ".odf")
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
        ),

        # Show a plot of the generated distribution
        mainPanel(
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
