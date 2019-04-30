#' # SVM Model Ksat (Samuel N Araya)
#' Model Particulars
# ***********************
model.type = "SVM"
#' 3.1 Model number
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
require(caret) # platform
require(kernlab) # SVM, RVM...
require(doMC) # Parallel computation.
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
svm_grid <- expand.grid(
						 C = c(2^(-2:5), 2^(seq(5.5,8, by= 0.5)))
						 )
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
#' Tune SVM-RBF model
#set.seed(10) # Uncertain how set.seed works with repeated cv and with parallel proccessing.
svm_profile = train(log.Ksat~., data = train.dt,
                     method = "svmRadialCost",
                     tuneGrid = svm_grid, # use for fine tunning
                     #tuneLength = 10,
                     trControl = ctrl_tr,
                     verbose = FALSE)
#'
#' Compute elapsed time
elapsed <- proc.time() - ptm
print(elapsed)
#'
#' Save model
model.fname = paste0(model.type, "_" , model.number,".rds")
saveRDS(svm_profile, file = file.path(wDir,model.fname))
#'
#' Exit R
q()
