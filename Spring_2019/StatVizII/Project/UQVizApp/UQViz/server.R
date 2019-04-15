# Load needed packages
library(shiny)
library(ggplot2)
library(plotly)
library(shinyalert)
library(shinyWidgets)

## Increase max file upload size
options(shiny.maxRequestSize = 15*1024^2)

###############################################
# Functions ###################################
###############################################

# Function to calculate p-box lower and upper bounds
calc_pbox <- function(data, p_upper, p_lower){
  pbox <- apply(values$data, 
                1, 
                FUN = quantile, 
                probs = c(p_lower, p_upper))
  
  pbox <- t(pbox)
  
  values$pbox <- data.frame(x = values$data[,1], 
                            lower = pbox[,1], 
                            upper = pbox[,2])
  return(pbox)
}

# function to calculate the SRQ value for a given probability level
calc_srq <- function(prob_in){
  srq_l <- values$pbox[which.min(abs(values$pbox$lower - prob_in)),]$x
  srq_u <- values$pbox[which.min(abs(values$pbox$upper - prob_in)),]$x
  
  return(c(srq_l, srq_u))
}

# function to calculate the probability value for a probability level
calc_probs <- function(srq_in){
  prob_l <- values$pbox[which.min(abs(values$pbox$x - srq_in)),]$lower
  prob_u <- values$pbox[which.min(abs(values$pbox$x - srq_in)),]$upper
  
  return(c(prob_l, prob_u))
}

###############################################
# Reactive Values##############################
###############################################

values <- reactiveValues()

###############################################
# Read in Sample Data #########################
###############################################
cdf_arr <-  read.csv("data/cdfs.csv", header = FALSE)
cdf_arr <-  t(cdf_arr)
cdf_df <-  data.frame(cdf_arr)

values$valmax <- max(cdf_df[,1])
values$valmin <- min(cdf_df[,1])
values$userin <- NULL
values$showcdf <- FALSE
values$showpbx <- TRUE

###############################################
# Server Logic ################################
###############################################
shinyServer(function(input, output) {

  # toggle button to show/hide cdf ensemble
  observeEvent(input$cdfToggle, {
    if (values$showcdf == FALSE){
      shinyalert(
        title = "",
        callbackR = function(x){
          values$showcdf <- x
        },
        text = paste("Showing the CDF ensemble on the plot may significantly",
                     "increase plot render times. Do you want to continue?"),
        showCancelButton = TRUE,
        confirmButtonText = "Show CDFs",
        cancelButtonText = "Don't Show CDFs",
        confirmButtonCol = "#52D755",
        type = "info"
      )
    } else {
    values$showcdf <- FALSE
    }
  })
  
  observeEvent(input$pbxToggle, {
    if (values$showpbx == FALSE){
      values$showpbx <- TRUE
    } else {
      values$showpbx <- FALSE
    }
  })
  
  # toggle button to show/hide p-box
  output$pbxBttn <- renderUI({
    if (values$showpbx == TRUE){
      lbl = "Hide P-box"
      sty = "bordered"
    } else {
      lbl = "Show P-box"
      sty = "simple"
    }
    
    actionBttn(inputId = "pbxToggle", 
               label = lbl, 
               style = sty,
               size = "sm", 
               color = "primary")
  })
  
  output$cdfBttn <- renderUI({
    if (values$showcdf == TRUE){
      lbl = "Hide CDFs"
      sty = "bordered"
    } else {
      lbl = "Show CDFs"
      sty = "simple"
    }
    
    actionBttn(inputId = "cdfToggle", 
               label = lbl, 
               style = sty,
               size = "sm", 
               color = "primary")
  })
  
  output$uqPlot <- renderPlot({
    # Use selected data source
    if (input$radioDataSource == "Sample Data"){
      values$data <- cdf_df
    } else {
      req(input$fileIn)
      df <- read.csv(input$fileIn$datapath,
                     header = FALSE)
      df <- data.frame(df)
      values$data <- df
    }

    # set up initial plot
    req(values$data)
    plt <- ggplot(data = values$data, 
                  aes(values$data[,1]))
    
    # plot cdf ensemble if user selects
    if (values$showcdf == TRUE){
      for(i in names(values$data)[-1]){ 
        plt <- plt + 
          geom_line(aes_string(y = i, 
                               color = shQuote("CDFs")), 
                    alpha = input$sliderAlpha)
      }
      plt <- plt
    }
    
    # get nice breaks for x-axis
    px <- pretty(values$data[,1])
    
    # Labels and additional plot formatting
    plt <- plt +
      xlab(input$xaxisInput) +
      # ylab(input$yaxisInput) +
      ylab("Probability") +
      scale_x_continuous(breaks = px, 
                         limits = range(px)) +
      ggtitle(input$titleInput) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, 
                                      size = 18, 
                                      face = "bold"),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)) 
    
    # show p-box if user selects
    if (values$showpbx == TRUE){
      pbox <- calc_pbox(values$data, 
                        (1 - input$pboxLower), 
                        (1 - input$pboxUpper))
      plt <- plt + 
        geom_line(aes(y = pbox[,1], col = "Pbox"), lwd = 1) +
        geom_line(aes(y = pbox[,2], col = "Pbox"),  lwd = 1) +
        scale_colour_manual(name="", values=c("Pbox"="red"))
    }
    
    # Calculate and show extracted interval on plot
    # if user selects
    if (input$extractInt == TRUE){
      req(input$valIn)
      
      # if interval is an SRQ interval
      if (input$extractType == "SRQ Interval"){
        if (input$valIn < 0){
          values$userin <- 0
        } else if (input$valIn > 1) {
          values$userin <- 1
        } else {
          values$userin <- input$valIn
        }
        srq_vec <- calc_srq(values$userin)
        plt <- plt + 
          geom_hline(yintercept = values$userin, lty = 2) +
          geom_segment(mapping = aes(x = srq_vec[1], 
                                     y = 0, 
                                     xend = srq_vec[1], 
                                     yend = 1), 
                       lty = "longdash") +
          geom_segment(mapping = aes(x = srq_vec[2], 
                                     y = 0, 
                                     xend = srq_vec[2], 
                                     yend = 1), 
                       lty = "longdash") +
          geom_text(aes(srq_vec[1],0.05, 
                        label = round(srq_vec[1],2), 
                        vjust = 3, 
                        hjust = 0.5), 
                    size = 5) +
          geom_text(aes(srq_vec[2],0.05, 
                        label = round(srq_vec[2],2), 
                        vjust = 3, 
                        hjust = 0.5), 
                    size = 5) +
          geom_text(aes(min(values$data[,1]), 
                        values$userin, 
                        label = round(values$userin,2),
                        vjust = -0.5, 
                        hjust = 2), 
                    size = 5)
      } else {
        # if interval is a probability interval
        req(input$valIn)
        if (input$valIn < values$valmin){
          values$userin <- values$valmin
        } else if (input$valIn > values$valmax) {
          values$userin <- values$valmax
        } else {
          values$userin <- input$valIn
        }
        prob_vec <- calc_probs(values$userin)
        plt <- plt + 
          geom_segment(mapping = aes(x = values$userin, 
                                     y = 0, 
                                     xend = values$userin, 
                                     yend = 1), 
                       lty = 2) +
          geom_hline(yintercept = prob_vec[1], 
                     lty = "longdash") +
          geom_hline(yintercept = prob_vec[2], 
                     lty = "longdash") +
          geom_text(aes(min(values$data[,1]), 
                        prob_vec[1], 
                        label = round(prob_vec[1],2), 
                        vjust = -0.5, 
                        hjust = 2), 
                    size = 5) +
          geom_text(aes(min(values$data[,1]), 
                        prob_vec[2], 
                        label = round(prob_vec[2],2), 
                        vjust = -0.5, 
                        hjust = 2), 
                    size = 5) +
          geom_text(aes(values$userin, 
                        0.05, 
                        label = round(values$userin,2), 
                        vjust = 3, 
                        hjust = 0.5), 
                    size = 5)
      }
    }
    
    if (values$showpbx && values$showcdf){
      plt <- plt +
        scale_color_manual(name = "", 
                           values = c("CDFs" = "black", 
                                      "Pbox" = "red")) +
        guides(colour = 
                 guide_legend(override.aes = 
                                list(linetype = c(1, 1),
                                     lwd = c(0.5, 1)))) +
        theme(legend.text = element_text(size = 12))
    } else if (values$showpbx){
      plt <- plt +
        scale_color_manual(name = "", 
                           values = c("Pbox" = "red")) +
        theme(legend.text = element_text(size = 12))
    } else if (values$showcdf){
      plt <- plt +
        scale_color_manual(name = "", 
                           values = c("CDFs" = "black")) +
        theme(legend.text = element_text(size = 12))
    }
    
    values$plot <- plt
    
    plt
   })
  
  output$extractTypeInput <- renderUI({
    if (input$extractInt == TRUE){
      radioButtons(inputId = "extractType", 
                   label = "Interval Type: ", 
                   choices = c("SRQ Interval", 
                               "Probability Interval"))
    }
  })
  
  
  output$valInput <- renderUI({
    if (input$extractInt == TRUE){
      if (input$extractType == "Probability Interval"){
        lbl = "SRQ Input Value"
        val = round(quantile(values$data[,1], 0.1), 0)
      } else {
        lbl = "Probability Input Value"
        val = 0.95
      }
      numericInput(inputId = "valIn", 
                   label = lbl, 
                   value = val)
    }
  })
  
  output$fileInput <- renderUI({
    if (input$radioDataSource == "File Upload"){
      fileInput(inputId = "fileIn", 
                label = "Upload .csv File")
    }
  })
  
  # help from https://stackoverflow.com/questions
  # /14810409/save-plots-made-in-a-shiny-app
  output$savePng <- downloadHandler(
    filename = function(){
      paste(input$pngName, 
            ".png", 
            sep="")
    },
    content = function(file){
      ggsave(file, 
             plot = values$plot, 
             device = "png", 
             width = 12)
    }
  )
  
})
