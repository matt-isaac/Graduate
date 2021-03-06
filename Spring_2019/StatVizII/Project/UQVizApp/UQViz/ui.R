library(shiny)
library(shinydashboard)
library(plotly)
library(shinyalert)
library(shinyWidgets)

cranURL <- "https://cran.r-project.org/web/packages/"

dashboardPage(
  dashboardHeader(
    title = "UQViz"
  ),
  dashboardSidebar(
  ),
  dashboardBody(
    useShinyalert(),
    tabsetPanel(
      tabPanel("Data Source",
               fluidRow(
                 column(
                   width = 3,
                   radioButtons(inputId = "radioDataSource",
                                label = "Data Source",
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
                     br(),
                     uiOutput(outputId = "pbxBttn"),
                     br(),
                     uiOutput(outputId = "cdfBttn")
                     
                   )  
                   
                 ),
                 column(
                   width = 2,
                   offset = 0,
                   numericInput(inputId = "pboxLower", 
                                label = "Lower P-box Percentile",
                                value = 0.05, 
                                width = 250),
                   numericInput(inputId = "pboxUpper", 
                                label = "Upper P-box Perentile.",
                                value = 0.95, 
                                width = 250)
                 ),
                 column(
                   width = 3,
                   offset = 0,
                   sliderInput(inputId = "sliderAlpha", 
                               label = "CDF Transparency",
                               value = 0.2,
                               min = 0,
                               max = 1,
                               step = 0.1,
                               ticks = FALSE)
                 ),
                 column(
                   width = 1,
                   offset = 0,
                   checkboxInput(inputId = "extractInt", 
                                 label = "Extract Interval", 
                                 value = FALSE)
                   
                 ),
                 column(
                   width = 2,
                   uiOutput(outputId = "extractTypeInput"),
                   uiOutput(outputId = "valInput")
                 )
               )
               
      ),
      tabPanel("Labeling",
               fluidRow(
                 column(
                   width = 2,
                   textInput(inputId = "titleInput", 
                             label = "Title", 
                             placeholder = "Plot Title", 
                             value = "")
                 ),
                 column(
                   width = 2,
                   textInput(inputId = "xaxisInput", 
                             label = "X axis Label", 
                             placeholder = "X-axis", 
                             value = "SRQ") #,
                   # textInput(inputId = "yaxisInput", 
                   #           label = "Y axis Label", 
                   #           placeholder = "Y-axis", 
                   #           value = "Probability")
                 )
               )),
      tabPanel("User Guide",
               h3("Overview"),
               h5("UQViz is an interactive tool that can be used 
                  to create meaningful visualizations of
                  '2D Uncertainty Quantification' 
                  (for more information on 2D UQ, see ", 
                  tags$a(
                    href = paste0("http://verification.asmedigitalcollection",
                                  ".asme.org",
                                  "/article.aspx?articleid=",
                                  "2703289&resultClick=1"),
                         "this article",
                         target = "_blank"), 
                  "). Users can select data source and adjust various 
                  visual components and labels. The title and axis
                  label can also be customized, and the visualization 
                  can be downloaded and saved."),
               h3("Data Source"),
               tags$p("The 'Data Source' tab contains controls for 
                      the user to select the source from 
                      which data for the visualization will be obtained.
                      If ", 
                      tags$b('Sample Data'),
                      "is selected, a small built-in data set is used as 
                      the data source for the 
                      visualization. This feature is provided for convenience, 
                      and can be used for experimentation and 
                      exploration. If ",
                      tags$b('File Upload'), "
                      is selected, users can upload a ",
                      tags$code(".csv"),
                      "file containing
                      data to be used in the visualization. This ",
                      tags$code(".csv"),
                      "file should be formatted such that the 'x' values over 
                      which the CDFs are evaluated 
                      are in the first column, and the CDFs are 
                      in the following columns."
               ),
               h3("Visualization Options"),
               tags$p("Several visualization options are provided to
                      allow customizable graphics to be
                      created. The ",
                      tags$b("Show/Hide P-box"), " and the ", 
                      tags$b("Show/Hide CDFs"), " buttons toggle
                      back and forth between showing and 
                      hiding the red P-box and the black ensemble of CDFs. 
                      Note that showing the CDF 
                      ensemble may (depending on the size of the uploaded
                      file) significantly increase
                      plot rendering times. If the user wishes to
                      display the CDFs on the final version
                      of the plot, it is suggested to adjust all
                      visualization options and controls 
                      while the CDFs are hidden, keeping the plot
                      rendering times to a reasonable
                      length. Then, as the last step, show the CDF
                      ensemble. This way, the user will only
                      have to wait once for the lengthened rendering time."),
               tags$p("The ", 
                      tags$b("Lower P-box Percentile"), 
                      " and ", 
                      tags$b("Upper P-box Percentile"),
                      "values can also be adjusted. This will 
                      shift the boundaries of the P-box on 
                      the visualization."),
               tags$p("Adjusting the ", 
                      tags$b("CDF Transparency"), 
                      " slider will adjust the 
                      transparency of each CDF in the CDF ensemble.
                      A value of 0 corresponds to 
                      completely transparent, while a value 
                      of 1 corresponds to completely opaque. 
                      This control will only affect the visualization 
                      when the CDFs are being shown.
                      This feature can be useful when there is a
                      high degree of overplotting due to 
                      a large number of CDFs. Allowing transparency
                      on the CDFs can assist in 
                      visualizing high-density regions on the plot."),
               tags$p("When the ", 
                      tags$b("Extract Interval"), 
                      " box is checked,
                      several more controls appear. 
                      Extracting an interval is one way that 
                      P-boxes can be interpreted in practice.
                      Users can decide whether to extract an ", 
                      tags$b("SRQ Interval"), 
                      " or a ", 
                      tags$b("Probability Interval"), 
                      ". If extracting an SRQ interval, users 
                      will need to select a probablity value; if 
                      extracting a probability interval, users 
                      will need to select an SRQ value. 
                      Labels and dotted lines will be displayed 
                      on the visualization, showing the 
                      probability or SRQ value selected and the 
                      corresponding interval."),
               h3("Labeling"),
               tags$p("Users also have the capability to choose
                      a custom ",
                      tags$b("Title"), 
                      " and a custom ", 
                      tags$b("X axis Label"), 
                      "to futher customize the plot to a 
                      specific context or application. Since this
                      visualization was designed to always show 
                      probability on the y axis, the y axis label 
                      cannot be changed."),
               h3("Saving a Plot"),
               tags$p("Users can enter a ",
                      tags$b("File Name"), 
                      " and can click the " ,
                      tags$b("Download"), 
                      " button to download and save a ",
                      tags$code(".png"),
                      " file of the current version of the visualization.
                      These controls are located below the plot.
                      Note that the chosen
                      file name should omit the ",
                      tags$code(".png"), 
                      " file extension.")
               
      ),
      tabPanel("References",
               tags$h4("The following software packages
                       were used in development: "),
               tags$div(tags$ul(
                 tags$li(tags$span(tags$a(href = "https://www.R-project.org/",
                                          target = "_blank",
                                          "R"))),
                 tags$li(tags$span(tags$a(href = "http://www.rstudio.com/",
                                          target = "_blank",
                                          "RStudio"))),
                 tags$li(tags$span(tags$a(
                   href = paste0(cranURL, "shiny/index.html"),
                   target = "_blank",
                   "shiny"))),
                 tags$li(tags$span(tags$a(
                   href = paste0(cranURL, "ggplot2/index.html"),
                   target=  "_blank",
                   "ggplot2"))),
                 tags$li(tags$span(tags$a(
                   href = paste0(cranURL,"shinyWidgets/index.html"),
                   target = "_blank",
                   "shinyWidgets"))),
                 tags$li(tags$span(tags$a(
                   href = paste0(cranURL,"shinydashboard/index.html"),
                   target = "_blank",
                   "shinydashboard"))))
               )
      )
    ),
    fluidRow(
      box(
        width = 12,
        height = 625,
        plotOutput("uqPlot", 
                   height = 475),
        fluidRow(
          column(
            width = 3,
            textInput(inputId = "pngName", 
                      value = "", 
                      placeholder = "file name", 
                      label = "File Name (omit file extension)", 
                      width = 200),
            downloadButton(outputId = 'savePng')
          )
        )
      )
    )
  )
)