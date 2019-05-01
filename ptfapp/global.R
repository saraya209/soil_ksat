# Initialize variables
predicted_dt <- NULL
# load model names
#model.names <- list.files(pattern="^f.*.rds$")
model.names <- list("BRT 3-0" = "fGBM_3_1.rds",
                    "BRT 3-1" = "fGBM_3_2.rds",
                    "BRT 3-2" = "fGBM_3_3.rds", 
                    "BRT 7-0" = "fGBM_7_1.rds", 
                    "BRT 7-1" = "fGBM_7_2.rds", 
                    "BRT 7-2" = "fGBM_7_3.rds")
model.names.display <- c("BRT 3-0", "BRT 3-1", "BRT 3-2",
                         "BRT 7-0", "BRT 7-1", "BRT 7-2")
model.inputs <- list("BRT 3-0" = c("Clay","Silt","Sand"),
                     "BRT 3-1" = c("Clay","Silt","Sand", "BD"),
                     "BRT 3-2" = c("Clay","Silt","Sand", "BD", "OC"), 
                     "BRT 7-0" = c("Clay","Silt","VCOS", "COS","MS","FS","VFS"), 
                     "BRT 7-1" = c("Clay","Silt","VCOS", "COS","MS","FS","VFS", "BD"), 
                     "BRT 7-2" = c("Clay","Silt","VCOS", "COS","MS","FS","VFS", "BD", "OC")
                     )
predictor.rename <- data.frame(input.name = c("Clay","Silt", "Sand","VCOS", "COS","MS","FS","VFS", "BD", "OC"),
                               pred.name = c("rClay","rSilt", "rSand","rVCOS", "rCOS","rMS","rFS","rVFS", "Db", "logOC"))

# load scaling means and standard deviations
scale_mean <- readRDS(file.path("Models","scaling_means"))
scale_sd <- readRDS(file.path("Models","scaling_sds"))

#Scale and center predictors function
scale_fun <- function(val, mean, std){
  (val-mean)/std
}

# Function to run prediction from uploaded file
#display_table = data.frame()
run_ptf_fun <- function(ptf_model, dt){
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
    
    # Make a copy for output.
    in.dt <- dt 
    if (ptf_model %in% c("BRT 3-2", "BRT 7-2")){
      in.dt$OC  <- round(exp(in.dt$OC),3)
    }
    #rename header to match model input
    names(dt) <- predictor.rename$pred.name[match(names(dt), predictor.rename$input.name)]
    
    # subset and match order of predictors to model
    sel.m <- loaded_m()
    pred.var <- sel.m$var.names
    dt <- dt[pred.var]
    scale_mean <- scale_mean[pred.var]
    scale_sd <- scale_sd[pred.var]
    
    # Scale Inputs
    scaled.dt <- mapply(scale_fun,
                          val = dt,
                          mean = scale_mean, 
                          std = scale_sd)
    # Predict Ks
    Predicted_Ks <- predict(sel.m, 
                       newdata = dt,
                       n.trees = sel.m$n.trees)
    
    pred.dt <- cbind(in.dt, Predicted_Ks)
    # Generate Warnings 
    # ************** BUG: first condition gets ignored!
    pred.dt <- pred.dt %>%
      dplyr::mutate(id=1:n()) %>% 
      dplyr::group_by(id) %>% 
      dplyr::mutate(Model_Name = ptf_model,
                    Remarks = ifelse(Model_Name %in% model.names.display[1:3],
                                     ifelse(abs(100 - sum(Clay,Silt,Sand)) > 5,
                                            "Separates Not 100%!",
                                            ""),
                                     ""),
                    Remarks = ifelse(Model_Name %in% model.names.display[4:6],
                                     ifelse(abs(100 - sum(Clay,Silt,VCOS, COS, MS, FS, VFS)) > 5,
                                            "Separates Not 100%!",
                                            ""),
                                     ""),
                    Remarks = ifelse(Model_Name %in% model.names.display[c(2,3,5,6)],
                                     ifelse(BD < 0.05 | BD > 2.6,
                                            paste(Remarks,
                                                  "BD outside training data range!"),
                                            Remarks),
                                     Remarks),
                    Remarks = ifelse(Model_Name %in% model.names.display[c(3,6)],
                                     ifelse(OC > 18.5,
                                            paste(Remarks,
                                                  "OC outside training data range!"),
                                            Remarks),
                                     Remarks)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select( -id)
    
    
    return(pred.dt)
    }

}

# Function to run single prediction
s_run_ptf_fun <- function(ptf_model, dt){
  #Initialize return values
  sel.model.inputs <- model.inputs[[ptf_model]]
  
  dt <- subset(dt, select= sel.model.inputs)
  
  # Check all inputs exist in table
    # Log-transform OC
    if (ptf_model %in% c("BRT 3-2", "BRT 7-2")){
      dt <- dt%>%
        mutate(OC = ifelse(OC >= 0,
                           log(OC + 0.01), NA))
      
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
    pred.var <- sel.m$var.names
    dt <- dt[pred.var]
    scale_mean <- scale_mean[pred.var]
    scale_sd <- scale_sd[pred.var]
    
    # Scale Inputs
    scaled.dt <- mapply(scale_fun,
                        val = dt,
                        mean = scale_mean, 
                        std = scale_sd)
    # Predict Ks
    Predicted_Ks <- predict(sel.m, 
                            newdata = dt,
                            n.trees = sel.m$n.trees)
    
    pred.dt <- cbind(in.dt, Predicted_Ks)
    # Generate Warnings 
    # ************** BUG: first condition gets ignored!
    pred.dt <- pred.dt %>%
      dplyr::mutate(id=1:n()) %>% 
      dplyr::group_by(id) %>% 
      dplyr::mutate(Model_Name = ptf_model,
                    Remarks = ifelse(Model_Name %in% model.names.display[1:3],
                                     ifelse(abs(100 - sum(Clay,Silt,Sand)) > 5,
                                            "Separates Not 100%!",
                                            ""),
                                     ""),
                    Remarks = ifelse(Model_Name %in% model.names.display[4:6],
                                     ifelse(abs(100 - sum(Clay,Silt,VCOS, COS, MS, FS, VFS)) > 5,
                                            "Separates Not 100%!",
                                            ""),
                                     ""),
                    Remarks = ifelse(Model_Name %in% model.names.display[c(2,3,5,6)],
                                     ifelse(BD < 0.05 | BD > 2.6,
                                            paste(Remarks,
                                                  "BD outside training data range!"),
                                            Remarks),
                                     Remarks),
                    Remarks = ifelse(Model_Name %in% model.names.display[c(3,6)],
                                     ifelse(OC > 18.5,
                                            paste(Remarks,
                                                  "OC outside training data range!"),
                                            Remarks),
                                     Remarks)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select( -id)
    
    
    return(pred.dt)
  
  
}
# dt <- readr::read_csv(file = "Soil_Variables.csv",
#                              col_names  = TRUE,
#                              col_types = cols(.default = "d"))
