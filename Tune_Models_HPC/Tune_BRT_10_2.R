#' # Generalized Boosted Tree Model (Samuel N Araya)
#' Model Particulars
# ***********************
model.type = "GBM"
#' 7.6 Model number
model.number = "76i2"
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
library(gbm) # GBM model
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
gbm_grid <- expand.grid(interaction.depth = 1:100,
                        n.trees = floor((500:1000)*50),
                        shrinkage = .01,
                        n.minobsinnode = 10)
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
gbm_fit <- train(log.Ksat~., data = train.dt,
                 method = "gbm",
                 tuneGrid = gbm_grid,
                 #tuneLength = 100, # for course tunning
                 trControl = ctrl_tr,
                 verbose = FALSE)
#'
#' Compute elapsed time
elapsed <- proc.time() - ptm
print(elapsed)
#'
#' Save model
model.fname = paste0(model.type, "_" , model.number,".rds")
saveRDS(gbm_fit, file = file.path(wDir,model.fname))
#'
#' Exit R
q()
