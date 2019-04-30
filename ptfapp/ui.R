# KSAT Shiny app ------------------
# This app takes in user soil variables and runs selected KSAT 
# pedotransfer functions to predict saturated hydraulic conductivity

# Load required packages ------
# Shiny and interface packages
library(shiny)
library(shinyjs)
library(htmltools)    # User to format the column header of the table
# Machine learning related packages
library(caret)
library(gbm)
# Table manipulation packages
library(DT)
library(dplyr)
#library(data.table)
library(readr)
# Plotting packages
# library(RColorBrewer) # Used for the colorpalettes in the app
# library(ggplot2)      # Used for the plots in the app
# UI ------
ui <- fluidPage(
  titlePanel(title = "Soil Saturated Hydraulic Conductivity Pedotransfer",
             windowTitle = "Ksat Pedotransfer"),
  # Separate header row
  tags$hr(style="border-color: #bfbfbf; border-top: 3px solid;"),
  
  # Start Sidebar ----------
  
    # Start sidebar panel ---------------
  sidebarPanel(#width = 5,
    selectInput("select_model", h3("Select PTF Model"), 
                choices=model.names.display),
    actionButton("load_model", "Load Model"),
    br(),
    radioButtons(inputId = "single_or_file",
                 label = "Run Mode:",
                 choices = c("Single" = "single_mode",
                             "File Upload" = "file_mode"),
                 inline = TRUE),
    h3("Soil Properties"),
    
    # Start Tabset Panel-------------------
    tabsetPanel(
      # Start tab set pannel Single input ----------
      tabPanel("Single Input",
               shinyjs::useShinyjs(),
               h4("Soil Separates (%)"),
               fluidRow(
                 column(6,
                        numericInput("clayInput", "Clay ", value = "", min=0, max = 100),
                        # numericInput("siltInput", "Silt ", value = "",  min=0, max = 100)
                        #fluidRow(textOutput("SandInput"), align = 'right'),
                        strong("Sand "),
                        textOutput('sandInput', inline = TRUE)
                 ),
                 column(6,
                        numericInput("siltInput", "Silt ", value = "",  min=0, max = 100)
                 )
                 
               ),
               hr(),
               #
               h4("Sand  Subcategories (%)"),
               fluidRow(
                 column(6,
                        numericInput("vcsInput", "Very coarse sand ", value = "", min=0, max = 100),
                        numericInput("csInput", "Coarse sand", value = "", min=0, max = 100),
                        numericInput("msInput", "Medium sand", value = "",  min=0, max = 100)
                 ),
                 column(6,
                        numericInput("fsInput", "Fine sand", value = "",  min=0, max = 100),
                        numericInput("vfsInput", "Very fine sand ", value = "",  min=0, max = 100)
                 )
               ),
               
               #
               hr(),
               numericInput("bdInput", HTML("Bulk Density (g cm<sup>-3</sup>)"), value = "",  min=0.1, max = 2.65),
               numericInput("ocInput", "Organic Carbon (%)", value = "",  min=0, max = 75),
               # 
               hr(),
               actionButton("single_run", "Run Prediction")
      ),
      # Start Tab pannel upload -----------------
      tabPanel("Upload File",
               br(),
               fileInput(inputId = 'upload_file',
                         label = "Choose CSV File", 
                         multiple=FALSE,
                         accep=c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
               ),

               hr(),
               actionButton("file_run", "Run Batch Prediction")
               
      )
      # End Tab pannel uplad ---------------
      
    )
    # End tabset pannel --------------
    
  ),
  
  # Satrt main panel -----------------
  mainPanel(
    h4("Selected Model Details"),
    strong('Model Name:'),
    textOutput('modelID', inline = TRUE),
    br(),
    strong('Required Input:'),
    textOutput('model_inputs', inline = TRUE),
    br(),
    strong("Run Mode:"),
    textOutput("run_mode_text", inline = TRUE),
    br(),
    strong("Model Loaded?"),
    textOutput("test_model_load", inline = TRUE),
    #tableOutput("in_display"),

    verbatimTextOutput(outputId = "testing"),
    
    tags$hr(style="border-color: #092f44; border-top: 1px solid;"),
 
    fluidRow(
      column(width = 12,
             h3("Model Prediction"),
             DT::dataTableOutput("in_out_table"),
             #tableOutput("in_out_table")
             DT::dataTableOutput("single_in_out_table")
      )
    ),
    # download data
    downloadButton("downloadData", "Download Data"),
    br(),
    hr(),
    strong('Remarks'),
    textOutput(outputId = "message"),
    tags$head(tags$style("#message{color: red;
                                 font-size: 15px;
                                 }"
                         )
    )
  )
    
# End UI.
)
