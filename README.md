# Saturated Hydraulic Conductivity Pedotransfer

[![Lab Website](https://github.com/saraya209/soil_ksat/blob/master/imgs/ucmerced_25.png)](http://soilphysics.ucmerced.edu "Soil Physics @ UC Merced")
[![Twitter](https://github.com/saraya209/soil_ksat/blob/master/imgs/Twitter_Social_Icon_Square_Color_33.png)](https://twitter.com/SamuelA209 "@SamuelA209")

This repository contains machine learning based Pedotransfer models that predict saturated hydraulic conductivity (K<sub>s</sub>) including the [training data](./Data/USKSAT_OpenRefined.csv) and all R scripts used to build the models. Detailed description of this work is available in our paper: <a href="https://doi.org/10.1029/2018WR024357" target="_blank">Araya and Ghezzehei (2019)</a>.

Summaries of data preparation procedures for the machine learning from the USKSAT data is found [**here**](./USKsat_Data_Prep.md). Summary report of analysis we did on the partial effect of bulk density and organic carbon concentration on K<sub>s</sub> is found [**here**](./Structure_Partial_Effect.md).

## Running Pedotransfer App
![](https://github.com/saraya209/soil_ksat/blob/master/imgs/GUI.png)

I have developed an app with GUI based on shiny. To run the app locally is easy with RStudio editor:
1. Download the `ptfapp` folder and all it's contents. **GitHub has restricted access to some of the models in the `ptf/Models` folder due to their size. Please download the models from my [GoogleDrive folder](https://drive.google.com/drive/folders/1GidQH1UFWRuPCYK8wccFjPcx-rQqENbR?usp=sharing).** (If you prefer not to download all of the models, download only the ones you want from.)
2. Open the RStudio project file `ptfapp.Rproj` in your machine. Open the `ui.R` script in your RStudio editor; RStudio will recognize the *Shiny* script and provide a **Run App** button (at the top of the editor).
3. Before running the app for the first time, you may need to install the required R packages by running the following codes in the R console.
```r
# Shiny packages
install.packages("shiny")
install.packages("shinyjs")
install.packages("htmltools")
# Machine learning related packages
install.packages("caret")
install.packages("gbm")
# Table manipulation packages
install.packages("DT")
install.packages("dplyr")
install.packages("readr")
```

## Repository Contents
- R Scripts
  - [USKsat_Data_Prep.R](./USKsat_Data_Prep.R) ([**Formatted file here**](./USKsat_Data_Prep.md)): Tidy up and pre-process USKSAT database to prepare for model building.
  - [Structure_Partial_Effect.R](./Structure_Partial_Effect.R) ([**Formatted file here**](./Structure_Partial_Effect.md)): Scripts to do analysis of partial effect of soil structural variables.
  - [Predict_Ksat.R](./Predict_Ksat.R): Script to predict Ksat using selected model. More description given in 'How To Use Models' section below.
  - [Other_PTF_Test.R](./Other_PTF_Test.R): Script to run predictions using  nine alternative PTF models we compared.
  - [Tune_Models_HPC](./Tune_Models_HPC): Sample scripts used to tune models on multi-core cluster.

- Data
  - [USKSAT database](https://www.doi.org/10.2136/sssaj2015.02.0067) that has been aggregated and gone through some tidying up ([USKSAT_OpenRefined.csv](./Data/USKSAT_OpenRefined.csv)).
  - Cleaned USKSAT database ([USKsat_tidydata5.rds](./Data/USKsat_tidydata5.rds), [USKsat_tidydata5.csv](./Data/USKsat_tidydata5.csv)).
  - Metadata for USKsat_Tidy ([USKsat_Tidydata_METADATA.xlsx](./Data/USKsat_Tidydata_METADATA.xlsx)).
  - Training and Testing subsets of Cleaned USKSAT (*USKsatTrain* and *USKsatTest* files, same files with *_dim* include sample dimension variable).
  - Pre-processing data used to center and scale data created by `preProcess` function from [caret](https://cran.r-project.org/web/packages/caret/index.html) package ([USKsat_preProc.rds](./Data/USKsat_preProc.rds)).

- [Pedotransfer Models](./ptfapp/Models)
  - All hierarchy models and the pre-processing data. **GitHub has restricted access to some of the models in the `ptf/Models` folder due to their size. Please download the models from my [GoogleDrive folder](https://drive.google.com/drive/folders/1GidQH1UFWRuPCYK8wccFjPcx-rQqENbR?usp=sharing).** 

- Functions
  - [Functions_TextureRelated.R](./Functions/Functions_TextureRelated.R): Set of functions to assign textural class, calculate percentile sizes and complexed organic carbon.
  - [Function_OtherPTF.R](./Functions/Function_OtherTF.R): Set of functions to calculate Ksat using nine other PTF models.
  - [ModelPerformanceFunction.R](./Functions/ModelPerformanceFunction.R): Function to calculate model performance.
  - [Function_Predict.R](./Functions/Function_Predict.R): Function to predict Ksat using our PTF models.

## Running Models Using Script
You can run the models to predict the saturated hydraulic conductivity  of soils using the `Predict_Ksat.R` script. To run the models in your machine:
1. Download at least these five items (save them in the same directory, check scripts to fix file locatoins in your machine.):
  - a model of your choice and the [`USKsat_preProc.rds`](./Data/USKsat_preProc.rds) file from [ptfapp/Models](./ptfapp/Models) folder,
  - the [`Soil_Variable_Template.csv`](./Soil_Variable_Template.csv) file,
  - the [`Predict_Ksat.R`](./Predict_Ksat.R) script, and
  - the [`Function_Predict.R`](./Functions/Function_Predict.R) file from the [Functions](./Functions) folder.
2. Fill and save the `Soil_Variables_Template.csv` table with your soil variables.
3. Modify lines 6 to 10 in the  `Predict_Ksat.R` as needed.

For the prediction to run on your machine, you must have the `caret` package and either `gbm` or `randomForest` packages installed depending on whether you are using the BRT or the RF models. You should be able to install the packages prior to running `Predict_Ksat.R` as follows.
```r
install.packages('caret', repos = 'https://cran.r-project.org')
install.packages('gbm', repos = 'https://cran.r-project.org')
install.packages('randomForest', repos = 'https://cran.r-project.org')
```

## License
This work is licensed under a Creative Commons Attribution 4.0 International License. - see the [LICENSE.md](LICENSE.md) file for details
