#' ---
#' title: USKSAT Tidying Up, Pre-Processing and Data Spliting
#' author: Samuel N. Araya
#' date: February 15, 2019
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#'      keep_md: yes
#' ---
#' Load the required libraries and files
#+ Load_Required, message=FALSE
# libraries
library(knitr)
library(tidyverse)
library(magrittr)
library(data.table)
library(ggpubr)
library(caret) 
library(mlr)
# Set Directories
wDir <- getwd()
dataDir <- file.path(wDir, "Data")
funDir <- file.path(wDir, "Functions")
# texture related functions
source(file = file.path(funDir,"Functions_TextureRelated.R"))
# Import table
dt = read.csv(file = file.path(dataDir,"USKsat_OpenRefined.csv"), na.strings = c("","NA"), stringsAsFactors = F )

#' ## Tidy Up Textural Size Classes
#' Check if textural classes add up to 100.
dt = dt%>%
  mutate(sum.Sand = round(VCOS+ COS+ MS+ FS+ VFS,1),
         diff.Sand = round(100 - Clay - Silt,1),
         sum.vs.Sand = round(sum.Sand - Sand,1),
         diff.vs.Sand = round(diff.Sand - Sand, 1),
         diff.vs.sum.Sand = round(sum.Sand - diff.Sand,1)
  )

#' Count of rows with no texture size fraction data vs no sub-sand size data vs
#' (False = data is present)
plyr::count(is.na(dt[, c("diff.Sand", "sum.Sand")]))

#' ### Fix Textural size classes
#' Use protocol from `FixTexFractions` function to handle irregularity
dt.tex = data.table(dt)
dt.tex[, c("rVCOS","rCOS","rMS","rFS","rVFS","rSand","rSilt","rClay")
       := FixTexFractions(VCOS, COS, MS, FS, VFS, Sand, Silt, Clay, b=5),
       by = 1:nrow(dt.tex)]
#' check if there were rows with condition outside of FixTexFractions function.
which(dt.tex$rClay ==9999)

#' Check if new textural sizes add up to 100 (with b margin of error).
dt.tex = dt.tex%>%
  mutate(rsum.Sand = round(rVCOS+ rCOS+ rMS+ rFS+ rVFS,1),
         rdiff.Sand = round(100 - rClay - rSilt,1),
         rsum.vs.Sand = round(rsum.Sand - rSand,1),
         rdiff.vs.Sand = round(rdiff.Sand - rSand, 1),
         rdiff.vs.sum.Sand = round(rsum.Sand - rdiff.Sand,1)
  )

range(dt.tex$rsum.vs.Sand, na.rm = T)
range(dt.tex$rdiff.vs.sum.Sand, na.rm = T)

#' Summarize data: 3-texture, 7-texture, Db and OC
mlr::summarizeColumns(dt.tex[, c("Ksat_cmhr","rdiff.Sand", "rsum.Sand", "Db", "OC")])
#
#' Count of rows in with no texture size data or no sub-sand size data
#'  in the corrected vs original data(False = data is present)
plyr::count(is.na(dt.tex[, c("rdiff.Sand", "diff.Sand")]))
plyr::count(is.na(dt.tex[, c("rsum.Sand", "sum.Sand")]))

#' Subset rows without new textural size data
dt.tex2 = subset(dt.tex, !is.na(rdiff.Sand))

#' Count of rows with no bulk density data, with no OC data or no 7-texture size classes 
#' (False = data is present)
plyr::count(is.na(dt.tex2[, c("rsum.Sand","Db", "OC")]))

#' Size of data subset with different missing variables
dt.tex2 %>% 
  summarise(total.count=n(), 
            no.SubTex=sum(is.na(rsum.Sand)), 
            no.Db=sum(is.na(Db)),
            no.OC=sum(is.na(OC)),
            count.SubtexOCDB = sum(!is.na(rsum.Sand & Db & OC)),
            count.SubtexDB = sum(!is.na(rsum.Sand & Db)),
            count.OCDB = sum(!is.na(Db & OC)),
            countDB = sum(!is.na(Db )),
            count.Subtex = sum(!is.na(rsum.Sand)),
            count.tex = sum(!is.na(rdiff.Sand))
  )


#' rename tabel to do More manipulations
dt = dt.tex2

#' ### Assign textural class 
dt = data.table(dt)
dt[, "rTexclass"
   := AssignTexClass(rSand, rClay),
   by = 1:nrow(dt)]
#' Make textrue ordered factor
dt$rTexclass = factor(dt$rTexclass, levels = c("CLAY", "SILTY CLAY","SANDY CLAY",
                                               "SILTY CLAY LOAM", "CLAY LOAM","SANDY CLAY LOAM",
                                               "SILT", "SILT LOAM","LOAM",
                                               "SANDY LOAM", "LOAMY SAND","SAND") )


#' ## Calculate percentile particle sizes
#' Calculate the 10th, 50th and 60th percentile particle sizes 
#' and coefficient of uniformity (as d60/d10).
dt[,c("d10" ,"d50", "d60", "CU")
   := CalculatePercentileSize(rClay,rSilt,rVCOS,rCOS,rMS,rFS,rVFS),
   by = 1:nrow(dt)]

#' Calculate percentilee sizesfrom only sand and silt clay
dt[,c("d10_2" ,"d50_2", "d60_2", "CU_2")
   := CalculatePercentileSize_Coarse(rClay,rSilt),
   by = 1:nrow(dt)]

#' ## Calculate Complexed Organic Carbon 
#' COC variable (Dexter et al. 2008)
#' $$ COC = IF [OC < \frac{Cl}{n}] THEN[OC] ELSE [\frac{Cl}{n}] $$
#' 
#' Where 'n' is the fraction of clay that complexes 1g of organic carbon
#' We use a value of n = 10 similar to Dexter et al. (2008)
dt[,"COC"
   := CalculateCOC(rClay, OC),
   by = 1:nrow(dt)] 

#' ## Log transform variables to make distribution more normal
#
dt = dt %>%
  mutate(logOC = ifelse(OC == 0, log(0.001), log(OC)),
         log10OC = ifelse(OC == 0, log10(0.001), log10(OC)),
         log.Ksat = log(Ksat_cmhr),
         log10.Ksat = log10(Ksat_cmhr),
         logCU = log(CU),
         logCU_2 = log(CU_2)
  )
#' ### Check normality and imporovemnt with log transformation
#' 
#' Ksat Visual inspection: density and Q-Q plot
ggdensity(dt, x = "Ksat_cmhr",
          add = "mean", rug = TRUE, 
          fill = "#00AFBB",
          xlim = c(0, 500)
)
ggqqplot(dt$Ksat_cmhr,
         ylim = c(0, 500))

ggdensity(dt, x = "log.Ksat",
          add = "mean", rug = TRUE, 
          fill = "#00AFBB"
)
ggqqplot(dt$log.Ksat)
#' OD Visual inspection: density and Q-Q plot
ggdensity(dt, x = "OC",
          add = "mean", rug = TRUE, 
          fill = "#00AFBB")
ggqqplot(dt$OC)

ggdensity(dt, x = "logOC",
          add = "mean", rug = TRUE, 
          fill = "#00AFBB")
ggqqplot(dt$logOC)
#' KShapiro-Wilk’s test (p-value indicate if normality should be rejected).
#' The data size is too large to make this test really useful but both, the log transformed
#' and non-transformed measurements failed this normality test..
shapiro.test(dt$Ksat_cmhr[1:5000])
shapiro.test(dt$log.Ksat[1:5000])

shapiro.test(dt$OC[1:5000])
shapiro.test(dt$logOC[1:5000])

#/*
#' ## Save database for Modeling
saveRDS(dt, file = file.path(dataDir,"Ksat_tidydata5.rds"))
#*/

#' ## Summarize  usksat
mlr::summarizeColumns(dt)

#' # Prepare Data For Model Building 
#' 
#' ## Remove columns that are not relevant
# Relevant columns
col.name = c("log.Ksat", "Height.cm", "Dia.cm", "Db", "logOC", "log10OC", "COC",
             "rVCOS", "rCOS", "rMS", "rFS", "rVFS", "rSand", "rSilt","rClay",
             "d10", "d50", "d60", "logCU", "d10_2", "d50_2", "d60_2", "logCU_2")

dt = subset(dt,select = col.name)
dim(dt)

#' ## Remove rows with missing values.
#' Count of missing values by column:
dt %>% summarise_all(funs(sum(is.na(.))))
colSums(is.na(dt))

#' Remove all rows with missing values.
dt.full = dt
n = nrow(dt)
dt = na.omit(dt)
#' Data size is reduced by
{{n - nrow(dt)}}
#' , from 
{{n}} 
#'  to
{{nrow(dt)}}

#' ## Check for  zero or near-zero variance predictors
nzv.dt<- nearZeroVar(dt,saveMetrics = T)
nzv.dt
#' Height and Diameter have near zero variance and should be removed
new.colnames = rownames( nzv.dt[nzv.dt$nzv==FALSE,])
dt_with_dim = dt # save a version with dimentions
dt = subset(dt,select = new.colnames)

#' ## Check for correlated predictors
#' The predictors such as d10 and d10_2 represent the same property
#' with one derived using the 7 textural classes and the other using 
#' only 3 textural classes. These two sets will not be used together
#' for modeling so it does not make sense to check the correlation between those,
#' hence we use a sub set that only uses one set.
#Sub set to those predictors derived from 7 textural classes
dt.sub = subset(dt, select = -c(d10_2, d50_2, d60_2, logCU_2))

descrCor = cor(dt.sub)
corp = findCorrelation(descrCor, cutoff = 0.85, names = T)
corp
#' d50, d60 and rSand  have high multicollinearity (r2>.85). 
#' Avoid using these three predictors together, especially for SVM and RVM models.
#' 
#' ## Check linear dependencies
comboInfo <- findLinearCombos(dt.sub)
comboInfo
#' no linear dependencies
#' 

#' # Data splitting into training and testing dataset
#' Training set = 75% of data
set.seed(5)
trainIndex = createDataPartition(dt$log.Ksat, p = 0.75,
                                 times = 1,
                                 list = FALSE,
                                 groups = 3)

train.dt = dt[trainIndex,]
test.dt = dt[-trainIndex,]


#' Visualize distribution of log Ksat in training set and testing set
#+ plotKsat, echo=FALSE
dt.both = melt(list(KsatTrain = train.dt$log.Ksat, KsatTest = test.dt$log.Ksat))
ggplot(dt.both)+ geom_density(aes(x = value, fill = L1) , alpha = 0.45) +
  theme_bw()

#' ## Center and Scale Predictors
preProcValues = preProcess(train.dt[,-1], method = c("center","scale"))

trainTrans.dt = predict(preProcValues,train.dt)
testTrans.dt = predict(preProcValues,test.dt)

#' Summary of transformed and non-transformed training subset.
#+ summTrain, echo=FALSE, message=FALSE
knitr::kable(mlr::summarizeColumns(train.dt), "html", 
             caption = "USKSAT Training Subset")
#+ summXTrain, echo=FALSE, message=FALSE
knitr::kable(mlr::summarizeColumns(trainTrans.dt), "html", 
             caption = "Centered and Scaled USKSAT Training Subset")


#/*
#' Save pre-proccessing values and tables
#Pre-proccessing values
saveRDS(preProcValues,file.path(dataDir,"Ksat_preProc.rds"))

#Pre-proccessed tables 
saveRDS(trainTrans.dt, file.path(dataDir,"KsatxTrain.rds"))
saveRDS(testTrans.dt, file.path(dataDir, "KsatxTest.rds"))

#non transformed tables
saveRDS(train.dt, file.path(dataDir, "KsatTrain.rds"))
saveRDS(test.dt, file.path(dataDir, "KsatTest.rds"))
#*/