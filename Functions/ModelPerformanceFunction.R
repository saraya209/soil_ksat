#' Model performance analysis
#Function for summarizing model performance statistics and plots
computeModelPerformance = function(y.measure, y.predict, modelname,
                                   common.log = FALSE,
                                   p.interval = FALSE, model_y.all
                                   ){
  ## Computes model perfromance statistics and plots
  # 
  # Args:
  #   y.measure: vector of measured values
  #   y.predict: vector of model predicted values equivalent to measured
  #   modelname: text to be used for sub-title in plot
  #   common.log: if TRUE, transform measured and predicted values to common log (base 10) and cm/day
  #   p.interval: if TRUE, prediction interval will be calculated from model_y.all matrix
  #   model_y.all: matrix of individual predictions, like from random forest model
  #
  # Returns:
  #   List with:
  #   1. Measured vs predicted ont-to-one plot
  #   2. Residual vs predicted plot
  #   3. Table with performance matrix (RMSE, R-squared, and ME)
  #   Only When p.interval is TRUE
  #   4. Measured vs predicted ont-to-one plot with prediction interval bars 
  #   5. Residual vs predicted plot with prediction interval bars
  #   6. Prediction interval vs predicted values plot
  #
  if(common.log){
    # Transform log_e cm/hr to log_10 cm/day
    y.predict <- log10(exp(y.predict)*24)
    y.measure <- log10(exp(y.measure)*24)
    # Plot labels log 10
    pred.label = expression("Predicted "~log[10](cm/day) )
    measure.label = expression("Measured "~log[10](cm/day) )
    res.label = expression("Residuals "~log[10](cm/day) )
    int.label = expression("Prediction Interval "~log[10](cm/day) )
  }else{
    # Plot labels log e
    pred.label = expression("Predicted "~log[e](cm/hr) )
    measure.label = expression("Measured "~log[e](cm/hr) )
    res.label = expression("Residuals "~log[e](cm/hr) )
    int.label = expression("Prediction Interval "~log[e](cm/hr) )
  }
  # calculate residual errrors
  res.pm = y.predict - y.measure
  res.mp = y.measure- y.predict
  
  # values for plot
  p.max = ceiling(max(y.predict, y.measure))
  p.min = floor(min(y.predict, y.measure))
 
  N = length(na.omit(res.pm))
  
  # Root mean square eroror (RMSE) and R-squared
  Perf = postResample(y.predict,y.measure)
  # save RMSE and R^2 variables for plot
  pRMSE = signif(Perf[1],4)
  pR2 = signif(Perf[2],4)
  # Mean error (ME)
  ME = mean(res.pm, na.rm = T)
  Perf[3] = ME
  names(Perf)[3] = "ME"
  # save ME variable for plot
  pME = signif(ME, 4)
  
  ## Performance Plots
  # Measured vs. predicted plot 
  mp = ggplot() +  
    geom_point(aes(y = y.measure, x = y.predict),alpha = 0.3, size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype = 2 ) + 
    annotate("text",x = p.min, y = p.max, 
             label = paste("RMSE = ", pRMSE, "\nR-squared = ", pR2, "\nME = ", pME),
             vjust = "inward", hjust = "inward", parse = F) +
    labs(title = "Measured versus predicted values",
         subtitle = modelname,
         caption = paste0("N = ", prettyNum(N,big.mark = ",")),
         x = pred.label,
         y = measure.label )+
    coord_fixed(ratio = 1, xlim = c(p.min,p.max), ylim = c(p.min,p.max))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+ 
    theme_bw()
  
  # Residual vs. predicted plot
  rp = ggplot() + 
    geom_point(aes(y = res.mp, x = y.predict),alpha = 0.3, size = 1.5) + 
    geom_hline(yintercept = 0, linetype = 2 ) + 
    labs(title = "Residual versus predicted values",
         subtitle = modelname,
         caption = paste0("N = ", prettyNum(N,big.mark = ",")),
         x = pred.label,
         y = res.label ) +
    #coord_fixed(xlim = c(p.min,p.max))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
    theme_bw()
  
  # Executed only if p.interval = TRUE
  if(p.interval){
    if(common.log){
      model_y.all <- log10(exp(model_y.all)*24)
    }
    model_y.int <- t(apply(model_y.all, 1, 
                            function(x){ 
                              c(quantile(x, c(0.05,0.25,0.75,0.95) ) )
                            }
                          )
                    )
    y.predict.high75 = model_y.int[,3] # 75%
    y.predict.low75 = model_y.int[,2] # 25%
    y.predict.high95 = model_y.int[,4] # 95%
    y.predict.low95 = model_y.int[,1] # 5%
    
    # Prediction 1:1 plot with error bar
    mp2 = ggplot() +  
      # geom_ribbon(aes(x = y.measure, ymin = y.predict.low, ymax = y.predict.high),
      #             alpha=0.5, fill = "grey70") +
      geom_errorbarh(aes(y = y.measure, x = y.predict,
                         xmax = y.predict.high95, xmin = y.predict.low95, height = .2),
                     color = "grey80")+
      geom_point(aes(y = y.measure, x = y.predict), alpha = 0.3, size = 1.5) +
      geom_abline(intercept = 0, slope = 1, linetype = 2 ) + 
      annotate("text",x = p.min, y = p.max, 
               label = paste("RMSE = ", pRMSE, "\nR-squared = ", pR2, "\nME = ", pME),
               vjust = "inward", hjust = "inward", parse = F) +
      labs(title = "Measured versus predicted values",
           subtitle = paste("With 95% prediction interval.", modelname, sep = " "),
           caption = paste0("N = ", prettyNum(N,big.mark = ",")),
           x = pred.label,
           y = measure.label )+
      coord_fixed(ratio = 1, xlim = c(p.min,p.max), ylim = c(p.min,p.max))+
      #coord_flip()+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+ 
      theme_bw()
    
    # Residual vs. predicted plot
    res.mp.high = y.measure - y.predict.high95
    res.mp.low = y.measure - y.predict.low95
    rp2 = ggplot() + 
      # geom_ribbon(aes(x = y.predict, ymin=res.mp.low, ymax=res.mp.high),
      #             alpha=0.75) +
      geom_errorbar(aes(x = y.predict, ymax = res.mp.high, ymin = res.mp.low, width = .2),
                    color = "grey80")+
      geom_point(aes(y = res.mp, x = y.predict), alpha = 0.3, size = 1.5) + 
      geom_hline(yintercept = 0, linetype = 2 ) + 
      labs(title = "Residual versus predicted values",
           subtitle = paste("With 95% prediction interval.", modelname, sep = " "),
           caption = paste0("N = ", prettyNum(N,big.mark = ",")),
           x = pred.label,
           y = res.label ) +
      #coord_fixed(xlim = c(p.min,p.max))+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
      theme_bw()
    
    # Prediction intervals
    int.high75 = y.predict.high75 - y.predict
    int.low75 = y.predict.low75 - y.predict
    int.high95 = y.predict.high95 - y.predict
    int.low95 = y.predict.low95 - y.predict
    #
    p.int = ggplot() + 
      
      geom_ribbon(aes(x = y.predict, ymin=int.low95, ymax=int.high95, fill = "Q95"),
                  alpha=0.5) +
      geom_ribbon(aes(x = y.predict, ymin = int.low75, ymax=int.high75, fill = "Q75"),
                  alpha=0.5) +
      #geom_errorbar(aes(x = y.predict, ymax = res.mp.high, ymin = res.mp.low, width = .2),
      #             color = "grey80")+
      #geom_point(aes(y = res.mp, x = y.predict), alpha = 0.25, size = 1) + 
      geom_hline(yintercept = 0, linetype = 2 ) + 
      labs(title = "Prediction interval versus predicted values",
           subtitle = modelname,
           caption = paste0("N = ", prettyNum(N,big.mark = ",")),
           x = pred.label,
           y = int.label ) +
      #coord_fixed(xlim = c(p.min,p.max))+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
      theme_bw()+
      scale_fill_manual(name="Prediction Interval", 
                          values = c(Q75 = "#FFA54F", 
                                     Q95 = "#60AFFE"),
                          labels = c("75 %", "95 %")
                          )
    
    return(list(mp,rp,"Perf"= Perf, mp2,rp2, p.int))
  }else{
    return(list(mp,rp,"Perf"= Perf))
  }
}
