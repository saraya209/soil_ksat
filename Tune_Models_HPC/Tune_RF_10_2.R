#' # Random Forest Model (Samuel N Araya)
#' Model Particulars
# ***********************
model.type = "RF"
#' 7.6 Model number
model.number = 76
#' Training database name
data.fname = "KsatxTrain.rds"
#' List of relevant predictors (columns)
rel.cols <- c("log.Ksat",
              # 7-class texture
              "rVCOS","rCOS","rMS","rFS","rVFS","rSilt","rClay",
              # Bulk density
              "Db",
              # log Organic Carbon
              "logOC",
              # Particle sizes
              "d10", "d50", "logCU"
)

# ***********************
#' Load the required libraries, libraries should be installed before hand.
#+ setup, include=FALSE
library(plyr)
library(caret) # platform for training
library(randomForest) # RF model
library(doMC) # Parallel computation
#'
#' Set up file directory
wDir <- getwd()
print(wDir)
#'
#' Import Transformed Training data table
train.dt = readRDS(file = file.path(wDir,data.fname))
#'
#' Subset by relevant predictors (columns)
train.dt <- subset(train.dt, select = rel.cols)
#' Print summary of data table
sapply(train.dt,summary)
#'
#' Define tuning parameters
# feature size
n.p = ncol(train.dt)
rf_grid <- expand.grid(mtry = seq(1,n.p))
#'
#' Define training control methods
ctrl_tr <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5,
                        returnResamp = "all")
#'
#' Save start time
ptm <- proc.time() # save start time
#'
#' Set up parallel proccessing
ncores <- detectCores() # get available cores
print(ncores)
registerDoMC(ncores) # register to work on all cores
#'
#' Tune GBM model
#set.seed(10) # Uncertain how set.seed works with repeated cv and with parallel proccessing.
rf_fit <- train(log.Ksat~., data = train.dt,
                method = "rf",
                tuneGrid = rf_grid,
                trControl = ctrl_tr,
                importance = TRUE,
                ntree = 5000,
                verbose = TRUE)
#'
#' Compute elapsed time
elapsed <- proc.time() - ptm
print(elapsed)
#'
#' Save model
model.fname = paste0(model.type, "_" , model.number,".rds")
saveRDS(rf_fit, file = file.path(wDir,model.fname))
#'
#' Exit R
q()
