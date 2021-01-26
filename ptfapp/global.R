# Initialize variables
predicted_dt <- NULL
# load model names
#model.names <- list.files(pattern="^f.*.rds$")
# Old Model file naming
# model.names <- list("BRT 3-0" = "fGBM_30.rds",
#                     "BRT 3-1" = "fGBM_31.rds",
#                     "BRT 3-2" = "fGBM_32.rds", 
#                     "BRT 7-0" = "fGBM_70.rds", 
#                     "BRT 7-1" = "fGBM_71.rds", 
#                     "BRT 7-2" = "fGBM_72.rds")

####**** Update January 2021 file names
model.names <- list("BRT 3-0" = "BRT_30.rds",
                    "BRT 3-1" = "BRT_31.rds",
                    "BRT 3-2" = "BRT_32.rds", 
                    "BRT 7-0" = "BRT_70.rds", 
                    "BRT 7-1" = "BRT_71.rds", 
                    "BRT 7-2" = "BRT_72.rds")

model.names.display <- c("BRT 3-0", "BRT 3-1", "BRT 3-2",
                         "BRT 7-0", "BRT 7-1", "BRT 7-2")
## List of models with corresponding required inputs
model.inputs <- list("BRT 3-0" = c("Clay","Silt","Sand"),
                     "BRT 3-1" = c("Clay","Silt","Sand", "BD"),
                     "BRT 3-2" = c("Clay","Silt","Sand", "BD", "OC"), 
                     "BRT 7-0" = c("Clay","Silt","VCOS", "COS","MS","FS","VFS"), 
                     "BRT 7-1" = c("Clay","Silt","VCOS", "COS","MS","FS","VFS", "BD"), 
                     "BRT 7-2" = c("Clay","Silt","VCOS", "COS","MS","FS","VFS", "BD", "OC")
                     )
## Corresponding variable names inside the models.
predictor.rename <- data.frame(input.name = c("Clay","Silt", "Sand","VCOS", "COS","MS","FS","VFS", "BD", "OC"),
                               pred.name = c("rClay","rSilt", "rSand","rVCOS", "rCOS","rMS","rFS","rVFS", "Db", "logOC"))

# load scaling means and standard deviations
scale_mean <- readRDS(file.path("Models","scaling_means"))
scale_sd <- readRDS(file.path("Models","scaling_sds"))

#Scale and center predictors function
scale_fun <- function(val, mean, std){
  (val-mean)/std
}

# Function to run predictions from single UI input or uploaded file
run_ptf_fun <- function(ptf_model, dt, batch.predict = FALSE){
  
  
  
  
  
  #Initialize return values
  num.msg  <- NULL
  sel.model.inputs <- model.inputs[[ptf_model]]
  
  dt <- subset(dt, select= sel.model.inputs)
  
  # Check all inputs exist in table
  
  
  if (!identical(sel.model.inputs, colnames(dt))){
    showModal(modalDialog(
      title = "File Error",
      HTML(paste0("The uploaded file header does not match expected model inputs.<br>",
                  "Expected inputs: <b>", 
                  paste(sel.model.inputs, collapse = ", "),
                  "</b><br> Uploaded columns: <b>", 
                  paste(colnames(dt), collapse = ", "),
                  "</b><br> Missing: <b>",
                  paste((sel.model.inputs[!sel.model.inputs %in% colnames(dt)]), 
                        collapse = ", "),
                  "</b>") ),
      easyClose = TRUE,
      footer = NULL
    ))
  }else{
    # Log-transform OC
    if (ptf_model %in% c("BRT 3-2", "BRT 7-2")){
      dt <- dt%>%
        mutate(OC = ifelse(OC >= 0,
                           log(OC + 0.01), NA))
      
    }
  }
  if(batch.predict){
    # Remove any row with NA.
    ini.len <- length(dt)
    dt <- na.omit(dt) 
    fin.len <- length(dt)
    # Warning if rows removed
    if (ini.len > fin.len){
      rem.len <- ini.len - fin.len
      num.msg <<- paste("Warning:", rem.len, 
                        "row(s) with missing and/or non-numeric values removed. ")
    }
    
  }
  
  # Make a copy for output.
  in.dt <- dt 
  if (ptf_model %in% c("BRT 3-2", "BRT 7-2")){
    in.dt$OC  <- round(exp(in.dt$OC),3)
  }
  #rename header to match model input
  names(dt) <- predictor.rename$pred.name[match(names(dt), predictor.rename$input.name)]
  
  # subset and match order of predictors to model
  sel.m <- loaded_m()
  pred.var <- sel.m$finalModel$var.names
  dt <- dt[pred.var]
  # scale_mean <- scale_mean[pred.var]
  # scale_sd <- scale_sd[pred.var]
  
  # Scale Inputs
  # (i.e center and scale predictors)
  scaled.dt <- dt %>% 
    dplyr::mutate(across(everything(), 
                         ~ (.x - scale_mean[[cur_column()]]) / scale_sd[[cur_column()]])) 
  # Predict Ks
  Predicted_Ks <- predict.train(sel.m, newdata = scaled.dt#,
                                #n.trees = sel.m$n.trees
  )
  # Convert units from log_e(cm/hr) to cm/day
  Predicted_Ks <- exp(Predicted_Ks) * 24
  pred.dt <- cbind(in.dt, Predicted_Ks)
  # Generate Warnings 
  # (Possible BUG: first condition may get ignored)
  pred.dt <- pred.dt %>%
    dplyr::mutate(id=1:n()) %>% 
    dplyr::group_by(id) %>% 
    dplyr::mutate(Model_Name = ptf_model,
                  Remarks = NA)
  
  ## Particle size adding up to 100%?
  if (ptf_model %in% model.names.display[1:3]){
    pred.dt <- pred.dt %>% 
      dplyr::mutate(Remarks = ifelse(abs(100 - sum(Clay,Silt,Sand)) > 5,
                                     "Separates Not 100%!",
                                     "") )
  } else if(ptf_model %in% model.names.display[4:6]){
    pred.dt <- pred.dt %>% 
      dplyr::mutate(Remarks = ifelse(abs(100 - sum(Clay,Silt,VCOS, COS, MS, FS, VFS)) > 5,
                                     "Separates Not 100%!",
                                     "") )
  }
  
  ## Bulk density in range?
  if(ptf_model %in% model.names.display[c(2,3,5,6)]){
    pred.dt <- pred.dt %>% 
      dplyr::mutate(Remarks = ifelse(BD < 0.05 | BD > 2.6,
                                     paste(Remarks,
                                           "BD outside training data range!"),
                                     Remarks) )
  } 
  ## OC in range?
  if(ptf_model %in% model.names.display[c(3,6)]){
    pred.dt <- pred.dt %>% 
      dplyr::mutate(Remarks = ifelse(OC > 18.5,
                                     paste(Remarks,
                                           "OC outside training data range!"),
                                     Remarks) )
  }
  
  pred.dt <- pred.dt %>% 
    dplyr::ungroup() %>% 
    dplyr::select( -id)
  
  
  return(pred.dt)
  
  
}

# dt <- readr::read_csv(file = "Soil_Variables.csv",
#                              col_names  = TRUE,
#                              col_types = cols(.default = "d"))
