# ---
#   title: "Predict_Ksat.R Run"
# author: "Sammuel Araya"
# output:
#   html_document:
#   keep_md: yes
# ---
#' The code is written assuming the following directory structure:
#' 
# --Project Directory\
#  |--Predict_Ksat.R (THIS FILE)
#  |
#  |--Soil_Variable_Template.csv
#  |
#  |--ptfapp\
#  | |-- Models\
#  |   |-- (Model and pre-processing .rds files)
#  |
#  |--Functions\
#  | |--Function_Predict.R
#  |
#  |--Predicted_Ks_and_Soil_Variables.csv (OUTPUT FROM RUNNING THIS CODE)
#
#' ## 1. Specify ML algorithm and Hierarchy
#' - Possible ML algorithm values are "RF" or "BRT"
#' - Possible Hierarchy values are: 30, 31, 32, 70, 71, or 72). 
#' See Araya & Ghezzehei 2019 for model names.#' 
#'
## *************************************************
Model_Alg <- "RF"
Model_Hier <- "72"
# Specify soil variable locatoin and file name to save output.
wDir <- getwd() # assuming the files are in the working directory, change if needed
soil.dt <- read.csv(file.path(wDir,"Soil_Variable_Template.csv")) # Location of your soil variables
predicted_file_name <- "Predicted_Ks_and_Soil_Variables.csv" # File name to save predicted file
## *************************************************

#' ## 2. Set directory and import files
#' This assumes the file directory structure shown above 
#' where the input soils and model are located. 
#' You can change values here for different folder structure
#' 
#+ Directories_and_Files
## Set directry path
#wDir <- getwd()
modelDir <- file.path(wDir,"ptfapp","Models")
funDir <- file.path(wDir, "Functions")
#
## Load the PredictFunction, an elaborate wrapper for caret's predict fucntion.
# PTF Predict function (`PredictFunction`)
source(file.path(funDir, "Function_Predict.R"))
#
## Load data to center and scale inputs
usksat.pre <- readRDS(file.path(modelDir, "USKSAT_preProc.rds"))
#
## Load the ML Model
#+ Load_model_libraries, message=FALSE, warning=FALSE 
library(caret) 
# Get file name of model
if(Model_Alg == "BRT"){
  Model_Name <-  paste0("fGBM_", Model_Hier, ".rds")
  
  library(gbm) # BRT model
  
}else if(Model_Alg == "RF"){
  Model_Name <-  paste0("RF_", Model_Hier, ".rds") 
  
  library(randomForest) # Random Forest model
}
# Read in model
ptf_model <- readRDS(file.path(modelDir, Model_Name))

#' ## 3. Run PTF prediction
#' Raw Ks prediction in ln(cm/hr)
Prediction_output = PredictFunction(i.model = ptf_model,
                                    pre.proc = usksat.pre, 
                                    in_data = soil.dt, 
                                    Alg = Model_Alg)

#' List of predictors used
Prediction_output[[2]]
Predicted.dt <- Prediction_output[[1]]

#' Appended predictions to input table and save.
soil.dt <- cbind(soil.dt, Predicted.dt)
print(soil.dt)
write.csv(soil.dt, file.path(predicted_file_name) )
