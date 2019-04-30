# Predict Function
PredictFunction <- function(i.model = ptf_model,
                            pre.proc = usksat.pre, 
                            in_data = soil.dt, 
                            Alg = Model_Alg){
  # Returns predicted Ks data frame
  # 
  # Args:
  #   i.model: One of BRT or RF model files
  #   pre.proc: The centering and scaling file for the model.
  #   in_data: Data frame of soil variables with column names that exist in models
  #   Alg: Machine learning algorithm, RF or BRT
  #
  # Returns:
  #   A list containing
  #   1. Data frame of predictions. If Alg= "RF" it returns the statistical
  #         summary of predictoin including mean
  #   2. List of predictors used in model(useful to verify model hierarchy used)  
  #
  #
  # Pre-proccess data by centering and scaling
   # extra_cols because the centering and scaling file has those...
  extra_cols <- c("d10", "d50", "d60", "logCU", "d10_2", "d50_2", 
                  "d60_2", "logCU_2")
  in_data[,extra_cols] <- NA
  in_scaled.dt <- predict(pre.proc, in_data)
  
  # subset data to predictors required by selected model
  model_p <- predictors(i.model)
  in_sub.dt <- subset(in_scaled.dt, select = model_p)
  
  
  if(Alg == "RF"){
    Ks_predicted <- predict(i.model$finalModel, newdata = in_sub.dt, 
                              predict.all=TRUE)
    
    Ks_predicted <- data.frame(t(Ks_predicted$individual))
    Ks_predicted <- t(sapply(Ks_predicted, summary))
    Ks_predicted <- data.frame(Ks_predicted)
  }else if(Alg=="BRT"){
    #predict
    Ks_predicted  <- predict(i.model, newdata = in_sub.dt)
    Ks_predicted <- data.frame(Ks_predicted)
    
  }
  
  #Prepare output
  model_p_print <- paste(model_p, collapse = ", ")
  return(list(Ks_predicted, model_p_print))
}
#