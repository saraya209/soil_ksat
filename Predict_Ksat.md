# Predict_Ksat.R Run
The code is written assuming the following directory structure:



```r
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
```

## 1. Specify ML algorithm and Hierarchy
- Possible ML algorithm values are "RF" or "BRT"
- Possible Hierarchy values are: 30, 31, 32, 70, 71, or 72). 
See Araya & Ghezzehei 2019 for model names.#' 



```r
## *************************************************
Model_Alg <- "RF"
Model_Hier <- "72"
# Specify soil variable locatoin and file name to save output.
wDir <- getwd() # assuming the files are in the working directory, change if needed
soil.dt <- read.csv(file.path(wDir,"Soil_Variable_Template.csv")) # Location of your soil variables
predicted_file_name <- "Predicted_Ks_and_Soil_Variables.csv" # File name to save predicted file
## *************************************************
```

## 2. Set directory and import files
This assumes the file directory structure shown above 
where the input soils and model are located. 
You can change values here for different folder structure



```r
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
```

```r
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
```

## 3. Run PTF prediction
Raw Ks prediction in ln(cm/hr)


```r
Prediction_output = PredictFunction(i.model = ptf_model,
                                    pre.proc = usksat.pre, 
                                    in_data = soil.dt, 
                                    Alg = Model_Alg)
```

List of predictors used


```r
Prediction_output[[2]]
```

```
## [1] "rVCOS, rCOS, rMS, rFS, rVFS, rSilt, rClay, Db, logOC"
```

```r
Predicted.dt <- Prediction_output[[1]]
```

Appended predictions to input table and save.


```r
soil.dt <- cbind(soil.dt, Predicted.dt)
print(soil.dt)
```

```
##    rClay rSilt rSand rVCOS rCOS rMS rFS rVFS  Db logOC      Min.  X1st.Qu.
## X1    20    20    60    12   12  12  12   12 1.2   1.5 -5.298317  1.190888
## X2    60     5    35     7    7   7   7    7 1.4   0.5 -6.887953 -2.060898
## X3    10    80    10     2    2   2   2    2 1.8   0.2 -7.523941 -3.171763
##        Median       Mean  X3rd.Qu.     Max.
## X1  2.1089873  1.9376043 3.1450213 6.042633
## X2  0.4264191  0.1302798 2.2192035 5.906983
## X3 -1.1130991 -1.0712241 0.8650144 6.222576
```

```r
write.csv(soil.dt, file.path(predicted_file_name) )
```

