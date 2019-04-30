#' # Run PTF Model
# **************************************************
#' 1. Specify ML algorithm (i.e. BRT or RF) and 
#'  Hierarchy (i.e. 30, 31, 32, 70, 71, or 72). See Araya & Ghezzehei 2019 for model names.#' 
## *************************************************
Model_Alg <- "BRT"
Model_Hier <- "72"
# Specify soil variable locatoin and file name to save output.
soil.dt <- read.csv(file.path(wDir,"Soil_Variable_Template.csv")) # Location of your soil variables
predicted_file_name <- "Predicted_Ks_and_Soil_Variables.csv" # File name to save predicted file
## *************************************************
#' 2. Set directory where the input soils and model are located. Everything should be in the same folder.
#+ Load_Files, echo=FALSE
# Directories
wDir <- getwd()
modelDir <- file.path(wDir,"PTF_Models")
funDir <- file.path(wDir, "Functions")
# PTF Predict function (`PredictFunction`)
source(file.path(funDir, "Function_Predict.R"))
#
# Data to center and scale inputs
usksat.pre <- readRDS(file.path(modelDir, "USKsat_preProc.rds"))
#
# Choosen model
Model_Name <- paste0(Model_Alg,"_", Model_Hier, ".rds")
ptf_model <- readRDS(file.path(modelDir, Model_Name))
#
#+ Libraries, include=FALSE
library(caret) # Model training ++ wrapper
library(gbm) # BRT model
#library(randomForest) # Random Forest model


#' Raw Ks prediction in ln(cm/hr)
# ptf_model <- ptf_model$bestTune
Prediction_output = PredictFunction(i.model = ptf_model,
                                    pre.proc = usksat.pre, 
                                    in_data = soil.dt, 
                                    Alg = Model_Alg)

#' List of predictors used
Prediction_output[[2]]
Predicted.dt <- Prediction_output[[1]]

#' Appended predictions to input table and save.
soil.dt <- cbind(soil.dt, Predicted.dt)
write.csv(soil.dt, file.path(predicted_file_name) )
