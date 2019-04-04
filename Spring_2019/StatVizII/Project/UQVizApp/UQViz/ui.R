library(shiny)
library(shinydashboard)
library(plotly)
library(shinyalert)
library(shinyWidgets)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
  ),
  dashboardBody(
    useShinyalert(),
    tabsetPanel(
      tabPanel("Data Source",
               fluidRow(
                 column(
                   width = 3,
                   radioButtons(inputId = 'radioDataSource',
                                label = 'Data Source',
                                choices = c("Sample Data",
                                            "File Upload"),
                                selected = "Sample Data"),
                   uiOutput(outputId = "fileInput")
                 )
               )
               
      ),
      tabPanel("Visualization Options",
               fluidRow(
                 column(
                   width = 2,
                   offset = 0,
                   verticalLayout(
                     # checkboxInput(inputId = "checkPbox",
                     #               label = "Show/Hide P-box",
                     #               value = TRUE),
                     # checkboxInput(inputId = "checkCDFs",
                     #               label = "Show/Hide CDFs",
                     #               value = FALSE)
                     br(),
                     uiOutput(outputId = "pbxBttn"),
                     br(),
                     uiOutput(outputId = "cdfBttn")
                     
                   )  
                   
                 ),
                 column(
                   width = 2,
                   offset = 0,
                   numericInput(inputId = "pboxLower", label = "Lower P-box Percentile",
                                value = 0.05, width = 250),
                   numericInput(inputId = "pboxUpper", label = "Upper P-box Perentile.",
                                value = 0.95, width = 250)
                 ),
                 column(
                   width = 3,
                   offset = 0,
                   sliderInput(inputId = "sliderAlpha", label = "CDF Transparency",
                               value = 0.3,
                               min = 0,
                               max = 1,
                               step = 0.1,
                               ticks = FALSE)
                 ),
                 column(
                   width = 1,
                   offset = 0,
                   checkboxInput(inputId = "extractInt", label = "Extract Interval", value = FALSE)
                   
                 ),
                 column(
                   width = 2,
                   # radioButtons(inputId = "extractType", label = "Interval Type: ", choices = c("SRQ Interval", "Probability Interval")),
                   # numericInput(inputId = "valIn", label = "", value = NULL)
                   uiOutput(outputId = "extractTypeInput"),
                   uiOutput(outputId = "valInput")
                 )
               )
               
      ),
      tabPanel("Labeling",
               fluidRow(
                 column(
                   width = 2,
                   textInput(inputId = 'titleInput', label = "Title", placeholder = "Plot Title", value = "")
                 ),
                 column(
                   width = 2,
                   textInput(inputId = 'xaxisInput', label = "X axis Label", placeholder = "X-axis", value = "SRQ") #,
                   # textInput(inputId = 'yaxisInput', label = "Y axis Label", placeholder = "Y-axis", value = "Probability")
                 )
               )),
      tabPanel("Help")
    ),
    fluidRow(
      box(
        width = 12,
        height = 625,
        # main plot
        plotOutput("uqPlot", height = 475),
        fluidRow(
          column(
            width = 3,
            textInput(inputId = "pngName", value = "", placeholder = "file name", label = "File Name (omit file extension)", width = 200),
            downloadButton(outputId = 'savePng')
            
          )
        )
      )
    )
  )
)


# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#        sliderInput("bins",
#                    "Number of bins:",
#                    min = 1,
#                    max = 50,
#                    value = 30)
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#        plotOutput("distPlot")
#     )
#   )
# ))
