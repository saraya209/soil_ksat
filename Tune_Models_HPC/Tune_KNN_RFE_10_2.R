#' # KNN Model (Samuel N Araya)
#' Model Particulars
# ***********************
model.type = "KNNRFE"
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
library(caret) # platform
library(kknn) # KNN model
library(doMC) # Parallel computation.
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
#' Define tuning parameter ranges
knn_grid <- expand.grid(k = 2:60 )
#'
#' Define training control methods
ctrl_tr <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5,
                        returnResamp = "all")
#'
#' Define Recursive Feature Elimination control methods
ctrl_rfe <- rfeControl(functions = caretFuncs,
                       method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       returnResamp = "all")
#'
#' Subset of predictors (columns)
n = ncol(train.dt) - 2
Pred_subsets = c(3:n)
#'
#' Save start time
ptm <- proc.time() # save start time
#'
#' Set up parallel proccessing
ncores <- detectCores() # get available cores
print(ncores)
registerDoMC(ncores) # register to work on all cores
#'
#' Tune KNN model
#set.seed(10) # Uncertain how set.seed works with repeated cv and with parallel proccessing.
knnrfe_profile = rfe(log.Ksat~., data = train.dt,
                  sizes = Pred_subsets,
                  method = "knn",
                  tuneGrid = knn_grid,
                  #tuneLength = 50,
                  rfeControl = ctrl_rfe,
                  #Inner resampling method
                  trControl = ctrl_tr,
                  verbose = FALSE)
#'
#' Compute elapsed time
elapsed <- proc.time() - ptm
print(elapsed)
#'
#' Save model
model.fname = paste0(model.type, "_" , model.number,".rds")
saveRDS(knnrfe_profile, file = file.path(wDir,model.fname) )
#'
#' Exit R
q()
