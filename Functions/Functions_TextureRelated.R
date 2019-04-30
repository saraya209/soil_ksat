#' # Collection of Functions related to soil texture and particle sizes
#'
#' Size limits of USDA soil separates (diameter in micro meter):
#' - Sand = 2000 - 50
#' - Silt = 50 - 2
#' - Clay = < 2
#' 
#' Subdivisions of sand separate (diameter in micro meter):
#' - Very coarse sand = 2000 - 1000
#' - Coarse sand = 1000 - 500
#' - Medium sand = 500 - 250
#' - Fine sand = 250 - 100
#' - Very fine sand = 100 - 50
#
AssignTexClass = function(sand, clay, vcs, cs, ms, fs, vfs, 
                          subclass = FALSE, percent = TRUE){
  # Assigns USDA textural classes from particle size fractions
  # 
  # Args:
  #   sand,clay,...: Ratio or percent of particles of size sand, clay, etc.
  #   subclass: Should USDA textural sub classes be assigned?
  #             the default FALSE means only major classes will be assigned
  #   percent: Whether the size fractions are in ratio (0-1) or in percentages.
  #
  # Returns:
  #   USDA Textural class or sub class string
  #
  # Option only major textural classes
  if(subclass == FALSE){
    # Convert ratio to percentage
    if(percent == FALSE){
      sand = 100*sand
      clay = 100*clay
    }
    
    silt = 100-sand-clay  # Calculate silt percentage by difference
    # Error handling
    #   Report without breaking if function is used in iteration
    if(sand+clay >105) return("Error. Sum of fractions > 105")
    #   Standard error reporting
    # if(sand+clay >105){
    #   stop("Error. Sum of fractions > 105")
    # } 
    
    if((silt + 1.5*clay) < 15)   return('SAND')
    if((silt + 1.5*clay >= 15) & (silt + 2*clay < 30))   return('LOAMY SAND')
    if((clay >= 7 & clay < 20) & (sand > 52) & ((silt + 2*clay) >= 30) | (clay < 7 & silt < 50 & (silt+2*clay)>=30))   return('SANDY LOAM')
    if((clay >= 7 & clay < 27) & (silt >= 28 & silt < 50) & (sand <= 52))   return('LOAM')
    if((silt >= 50 & (clay >= 12 & clay < 27)) | ((silt >= 50 & silt < 80) & clay < 12))   return('SILT LOAM')
    if(silt >= 80 & clay < 12)   return('SILT')
    if((clay >= 20 & clay < 35) & (silt < 28) & (sand > 45))    return('SANDY CLAY LOAM')
    if((clay >= 27 & clay < 40) & (sand > 20 & sand <= 45))   return('CLAY LOAM')
    if((clay >= 27 & clay < 40) & (sand  <= 20))   return('SILTY CLAY LOAM')
    if(clay >= 35 & sand > 45)   return('SANDY CLAY')
    if(clay >= 40 & silt >= 40)   return('SILTY CLAY')
    if(clay >= 40 & sand <= 45 & silt < 40)   return('CLAY')
  }else{
  # Option textural sub classes
    # Convert ratio to percentage
    if(percent == FALSE){
      sand = 100*sand
      clay = 100*clay
      vcs = 100*vcs
      cs = 100*cs
      ms = 100*ms
      fs = 100*fs
      vfs = 100*vfs
    }
    
    silt = 100-sand-clay # Calculate silt percentage by difference
    # Error handling
    #   Report without breaking if function is used in iteration
    if(sand+clay >105) return("Error. Sum of fractions > 105")
    if(abs(sum(vcs,cs,ms,fs,vfs, na.rm = T) - sand) >5){
      return(" Error. Sum of sand sub-fractions not equal to sand")
    } 
    
    if((silt + 1.5*clay) < 15){
      if(vcs + cs >= 25 & ms < 50 & fs < 50 & vfs < 50)  return('COARSE SAND')
      if(vcs+cs+ms >= 25 & vcs+cs < 25 & fs < 50 & vfs < 50)  return('SAND')
      if(ms >= 50 & vcs+cs >= 25)  return('SAND')
      if(fs >= 50 & fs > vfs)  return('FINE SAND')
      if(vcs+cs+ms < 25 & vfs < 50)  return('FINE SAND')
      if(vfs >= 50)  return('VERY FINE SAND')
    }
    if((silt + 1.5*clay >= 15) & (silt + 2*clay < 30)){
      if(vcs+cs >= 25 & ms < 50 & fs < 50 & vfs < 50)  return('LOAMY COARSE SAND')
      if(vcs+cs+ms >= 25 & vcs+cs < 25 & fs < 50 & vfs < 50)  return('LOAMY SAND')
      if(ms >= 50 & vcs+cs >= 25)  return('LOAMY SAND')
      if(fs >= 50)  return('LOAMY FINE SAND')
      if(vcs+cs+ms < 25 & vfs < 50)  return('LOAMY FINE SAND')
      if(vfs >= 50)  return('LOAMY VERY FINE SAND')
    }
    if((clay >= 7 & clay < 20) & (sand > 52) & ((silt + 2*clay) >= 30) | (clay < 7 & silt < 50 & (silt+2*clay)>=30)){
      if(vcs+cs >= 25 & ms < 50 & fs < 50 & vfs < 50)  return('COARSE SANDY LOAM')
      if(vcs+cs+ms >= 30 & vfs >= 30 & vfs < 50)  return('COARSE SANDY LOAM')
      if(vcs+cs+ms >= 30 & vcs+cs < 25 & fs < 30 & vfs < 30)  return('SANDY LOAM')
      if(vcs+cs+ms <= 15 & fs < 30 & vfs < 30 & fs+vfs < 40)  return('SANDY LOAM')
      if(vcs+cs >= 25 & ms >= 50)  return('SANDY LOAM')
      if(fs >= 30 & vfs < 30 & vcs+cs < 25)  return('FINE SANDY LOAM')
      if(vcs+cs+ms >= 15 & vcs+cs+ms < 30 & vcs+cs < 25)  return('FINE SANDY LOAM')
      if(fs+vfs >= 40 & fs >= vfs & vcs+cs+ms <= 15)  return('FINE SANDY LOAM')
      if(vcs+cs >= 25 & fs >= 50)  return('FINE SANDY LOAM')
      if(vfs >= 30 & vcs+cs+ms < 15 & vfs > fs)  return('VERY FINE SANDY LOAM')
      if(fs+vfs >= 40 & vfs > fs & vcs+cs+ms < 15)  return('VERY FINE SANDY LOAM')
      if(vcs+cs >= 25 & vfs >= 50)  return('VERY FINE SANDY LOAM')
      if(vcs+cs+ms >= 30 & vfs >= 50)  return('VERY FINE SANDY LOAM')
    }
    if((clay >= 7 & clay < 27) & (silt >= 28 & silt < 50) & (sand <= 52))   return('LOAM')
    if((silt >= 50 & (clay >= 12 & clay < 27)) | ((silt >= 50 & silt < 80) & clay < 12))   return('SILT LOAM')
    if(silt >= 80 & clay < 12)   return('SILT')
    if((clay >= 20 & clay < 35) & (silt < 28) & (sand > 45))    return('SANDY CLAY LOAM')
    if((clay >= 27 & clay < 40) & (sand > 20 & sand <= 45))   return('CLAY LOAM')
    if((clay >= 27 & clay < 40) & (sand  <= 20))   return('SILTY CLAY LOAM')
    if(clay >= 35 & sand > 45)   return('SANDY CLAY')
    if(clay >= 40 & silt >= 40)   return('SILTY CLAY')
    if(clay >= 40 & sand <= 45 & silt < 40)   return('CLAY')
  }
}


FixTexFractions = function(VCOS,COS,MS,FS,VFS,Sand, Silt, Clay, b = 5){
  # Protocol to check and correct inconsistencies in textural size fractions.
  #
  # Args:
  #   VCOS, COS,...: Percent of particles of size VCOS, COS, etc.
  #   b: acceptable margin of error when summing up percentages. defaults to 5
  # 
  # Returns:
  #   Verified or corrected percentage of the size fractions.
  #
  sum.Sand = round(sum(VCOS,COS,MS,FS,VFS, na.rm = T),2)
  diff.Sand = round(100 - Clay - Silt,2)
  sum.vs.Sand = round(sum.Sand - Sand,2)
  diff.vs.Sand = round(diff.Sand - Sand, 2)
  diff.vs.sum.Sand = round(sum.Sand - diff.Sand,2)
  
  if(is.na(diff.Sand)){ #if % Clay or silt is missing
    rVCOS = rCOS = rMS = rFS = rVFS = rSand = rSilt = rClay = NA
  }else if(abs(sum.vs.Sand) < b & abs(diff.vs.Sand) < b){ # SumSand = diff.Sand = Sand
    rVCOS = VCOS
    rCOS = COS
    rMS = MS
    rFS = FS
    rVFS = VFS
    rSand = Sand
    rSilt = Silt
    rClay = Clay
  }else if(abs(sum.vs.Sand)< b & abs(diff.vs.Sand) >= b){
    rVCOS = rCOS = rMS = rFS = rVFS = rSand = rSilt = rClay = NA
  }else if(abs(sum.vs.Sand) >= b & abs(diff.vs.Sand) < b){
    rVCOS = rCOS = rMS = rFS = rVFS = NA
    rSand = Sand
    rSilt = Silt
    rClay = Clay
  }else if(abs(diff.vs.Sand) >= b & abs(diff.vs.sum.Sand) < b){
    rVCOS = VCOS
    rCOS = COS
    rMS = MS
    rFS = FS
    rVFS = VFS
    rSand = diff.Sand
    rSilt = Silt
    rClay = Clay
  # NA if inconsistency is over the margin of error to be fixed 
  }else if(abs(diff.vs.Sand) >= b & abs(sum.vs.Sand) >=b){
    rVCOS = rCOS = rMS = rFS = rVFS = rSand = rSilt = rClay = NA
  # 9999 for any other error.
  }else { 
    rVCOS = rCOS = rMS = rFS = rVFS = rSand = rSilt = rClay = 9999
  }
  return(list( as.numeric(rVCOS), as.numeric(rCOS), as.numeric(rMS), 
               as.numeric(rFS), as.numeric(rVFS), as.numeric(rSand), 
               as.numeric(rSilt), as.numeric(rClay)))
}

CalculatePercentileSize = function(clay,silt,VCOS,COS,MS,FS,VFS){
  # Calculate the 10th, 50th and 60th percentile particle sizes, and
  # the coefficient of uniformity (d60/d10)
  #
  # Args: 
  #   clay, silt,...: Percentage of particles of size sand, clay, etc.
  #
  # Returns:
  #   A list of d10, d50, d60, and CU.
  #
  # Case 1: All fractin sizes available
  if (!is.na(sum(clay,silt,VCOS,COS,MS,FS,VFS, na.rm=FALSE))){
    # Particle size limits: clay to VCOS in micro-meter. 
    # A very small diameter of 0.01 is assigned as the minimum size 
    # limit for clay.
    Dia = c(.01, 2, 50, 100, 250, 500, 1000, 2000)
    logDia = log10(Dia)
    
    # Calculate the cumulative size distribution by adding up fractions
    # A very small negative percentage is added so that the commulative
    # distribution does not start from 0.
    sizes = c(-0.001,clay,silt,VFS,FS, MS, COS, VCOS)
    cum.sizes = cumsum(sizes)
    cum.total = cum.sizes[8]
    # Rescale the cumulative, so that it adds up to 100%
    cum.sizes = 100*cum.sizes/cum.total
    
    # Calculate the particle size percentiles 
    ft = approxfun(cum.sizes, logDia, rule = 1, method = "linear")
    d10 = signif(10^ft(10),4)
    d50 = signif(10^ft(50),4)
    d60 = signif(10^ft(60),4)
    CU = signif(d60/d10,4)
    
    # Case 2: Only clay and silt data available
  }else if(!is.na(sum(clay,silt, na.rm = FALSE))){
    sand = 100-clay-silt
    # Particle size limits: Slay to Sand in micro-meter.  
    # A very small diameter (0.01) is given as the minimum size limit for clay.
    Dia = c(.01, 2, 50, 2000)
    logDia = log10(Dia)
    
    # Calculate the cumulative size distribution by adding up fractions
    # A very small negative percentage is added so that the commulative
    # distribution does not start from 0.
    sizes = c(-0.001,clay,silt,sand)
    cum.sizes = cumsum(sizes)
    cum.total = cum.sizes[4]
    # Rescale the cumulative, so that it adds up to 100%
    cum.sizes = 100*cum.sizes/cum.total
    
    # Calculate the particle size percentiles 
    ft = approxfun(cum.sizes, logDia, rule = 1, method = "linear")
    d10 = signif(10^ft(10),4)
    d50 = signif(10^ft(50),4)
    d60 = signif(10^ft(60),4)
    CU = signif(d60/d10,4)
    
    #Case 3: Silt or Clay data missing
  }else{
    d10 = d50 = d60 = CU = NA
  }
  return(list(d10,d50,d60,CU))
}

CalculatePercentileSize_Coarse = function(clay,silt){
  # This function is special case of CalculatePercentileSize. 
  # It only uses sand, silt and clay sizes.
  
  # Calculate the 10th, 50th and 60th percentile particle sizes, and
  # the coefficient of uniformity (d60/d10)
  #
  # Args: 
  #   clay, silt: Percent of particles of size clay and silt.
  #
  # Returns:
  #   A list of d10, d50, d60, and CU.
  #
    # Case 2: Only clay and silt data available
  if(!is.na(sum(clay,silt, na.rm = FALSE))){
    sand = 100-clay-silt
    # Clay to Sand fractions' particle size limits in micro-meter. 
    # A very small diameter (0.01) is given as the minimum size limit for clay.
    Dia = c(.01, 2, 50, 2000)
    logDia = log10(Dia)
    
    # Calculate the cumulative size distribution by adding up fractions
    # A very small negative percentage is added so that the commulative
    # distribution does not start from 0.
    sizes = c(-0.001,clay,silt,sand)
    cum.sizes = cumsum(sizes)
    cum.total = cum.sizes[4]
    # Rescale the cumulative, so that it adds up to 100%
    cum.sizes = 100*cum.sizes/cum.total
    
    # Calculate the particle size percentiles 
    ft = approxfun(cum.sizes, logDia, rule = 1, method = "linear")
    d10 = signif(10^ft(10),4)
    d50 = signif(10^ft(50),4)
    d60 = signif(10^ft(60),4)
    CU = signif(d60/d10,4)
    
    #Case 3: Silt or Clay data missing
  }else{
    d10 = d50 = d60 = CU = NA
  }
  return(list(d10,d50,d60,CU))
}

CalculateCOC = function(clay, oc, n = 10){
  # Calculate the Complexed Organic Carbon fraction described in Dexter et al. 2008.
  #
  # Args: 
  #   clay: Fraction of clay size particles (either in fraction, 1g/1g of soil or in percent)
  #   OC:  Fraction of organic carbon (either in fraction 1g/1g of soil or in percent)
  #   n: grams of clay that complex 1g of organic carbon (defaults to 10)
  #
  # Returns:
  #   Complexed organic carbon fraction in units of clay and OC.
  #
  if(!is.na(sum(clay,oc, na.rm = FALSE))){ #check clay and OC are real numbers
    # Calculate COC
    coc <- min(oc,(clay/n))
  }else{
    coc <- NA
  }
  
  return(as.numeric(coc))
}