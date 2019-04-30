## Code for estimating saturated hydraulic conductivity (Ksat) 
## using nine PTFs:
##	- the Ghanbarian et al. (2015) Contrast Pattern Assisted Regression (CPXR) model and 
## seven other models given in Ghanbarian et al. (2017) Table 2.
##	- Nemes (2005)
## Some of the codes are based on: 
## http://dx.doi.org/10.1016/j.catena.2016.10.015.
## http://www.knoesis.org/resources/researchers/vahid/behzad.html

## Samuel N. Araya (samuel.negusse@gmail.com)


PTF_ks <- function(sand, clay, bd, oc, L, dia, percent = TRUE){
  # Calculates saturated hydraulic conductivity from Ghanbarian et al. (2015) SHC2 equation
  # and seven other models cited in Ghanbarian et al. (2017).
  # 
  # Args:
  #   sand,clay,...: Ratio or percent of particles of size sand and clay
  #   bd: bulk density of soil (g/cm^3)
  #   oc: organic carbon content (%)
  #   l: sample length (cm)
  #   d: sample diameter (cm)
  #   percent: Whether the size fractions are in ratio (0-1) or in percentages (0-100).
  #
  # Returns:
  #   Named list with Saturated hydraulic conducivity [(cm/day)] from all models and 
  #     patterns matched from Ghanbarian et al (2015)
  #
  # Convert ratio to percent
  if(percent == FALSE){
    sand = 100*sand
    clay = 100*clay
  }
  silt = 100-sand-clay  # Calculate silt percent
  
  ## Calculate geometric mean diameter, d_g (mm), and 
  ## geometric standard deviation of mean particle diameter s_g (mm) (Shirazi and Boersma, 1984)
  a <- 0.01 * ( (clay * log(0.001)) + (silt * log(0.026)) +(sand * log(1.025)) )

  b <- sqrt( 0.01 * ( (clay * log(0.001)^2) + (silt * log(0.026)^2) + (sand * log(1.025)^2) )  - a^2)
  
  d_g <- exp(a)
  s_g <- exp(b)
  
  # Calculate porosity from bulk density and particle density = 2.65
  phi <- 1 - (bd / 2.65)
  #
  ### Method 1: Ghanbarian et al (2015) SHC2 model
  # Steps:
  #   1. Match all relevant patterns (pattern == 0 if non match )
  #   2. Ks estimated using all local models that match pattern (or base model if non match)
  #   3. Average Ks predictions for the final prediction (geometric mean)
  
  # 1. Match pattern
  # Initialize `pattern` as empty vector
  pattern = c()
  if (d_g >= 0.495 & d_g < 0.74 & clay < 15.8 & s_g >= 1.55 & s_g < 6.96 & silt >= 0.2 
      & silt < 20.3 & sand >= 74.4 & sand < 99.1 & bd >= 1.23 & bd < 1.6 ){ 
    pattern = append(pattern,1) 
    }
  if (sand >= 49.6 & sand < 74.4 & L >= 2.5 & L < 36.9 & dia >= 3.2 & dia < 7.5){ 
      pattern = append(pattern,2)  
    }
  if (silt >= 40.5 & silt < 60.6 & d_g >= 0.004 & d_g < 0.25 
              & s_g >= 6.96 & s_g < 12.38 & bd >= 1.23 & bd < 1.6 ){ 
      pattern = append(pattern,3)  
    }
  if (silt >= 0.2 & silt < 20.3 & clay < 15.8 & s_g >= 1.55 & s_g < 6.96 
              & sand >= 74.4 & sand < 99.1 ){ 
      pattern = append(pattern,4)  
    }
  if (silt >= 0.2 & silt < 20.3 & d_g >= 0.004 & d_g < 0.25 & L >= 2.5 & L < 36.9){ 
      pattern = append(pattern,5)  
    }
  if (s_g >= 6.96 & s_g < 12.38 & clay < 15.8 & L >= 2.5 & L < 36.9 
              & dia >= 3.2 & dia < 7.5 & silt >= 20.3 & silt < 40.5 
              & sand >= 49.6 & sand < 74.4 & d_g >= 0.004 & d_g < 0.25 ){
      pattern = append(pattern,6)  
    }
  if (silt >= 60.6 & silt < 80.7 & L >= 2.5 & L < 36.9 & bd >= 1.23 & bd < 1.6 
                & d_g >= 0.004 & d_g < 0.25 & sand >= 0.1 & sand < 24.9 ){ 
      pattern = append(pattern,7) 
    }
  if (sand >= 49.6 & sand < 74.4 & L >= 2.5 & L < 36.9 & silt >= 20.3 
              & silt < 40.5 & dia >= 3.2 & dia < 7.5){ 
      pattern = append(pattern,8)  
    }
  if (clay >= 15.8 & clay < 31.5 & L >= 2.5 & L < 36.9 
              & d_g >= 0.004 & d_g < 0.25 & bd >= 1.23 & bd < 1.6 ){ 
      pattern = append(pattern,9)  
    }
  if (sand >= 49.6 & sand < 74.4 & L >= 2.5 & L < 36.9 & bd >= 1.23 
              & bd < 1.6 & dia >= 3.2 & dia < 7.5 ){ 
      pattern = append(pattern,10)  
    }
  if (silt >= 20.3 & silt < 40.5 & L >= 2.5 & L < 36.9 & clay < 15.8 
              & sand >= 49.6 & sand < 74.4){ 
      pattern = append(pattern,11)  
    }
  if (s_g >= 1.55 & s_g < 6.96 & clay < 15.8){ 
      pattern = append(pattern,12)  
    }
  if (clay < 15.8 & d_g >= 0.004 & d_g < 0.25 & L >= 2.5 & L < 36.9 ){ 
      pattern = append(pattern,13)  
    }
  if (clay < 15.8 & L >= 2.5 & L < 36.9){ 
      pattern = append(pattern,14)  
    }
  if (is.null(pattern)){
      pattern = 0 
      }
  
  #2. Predict using local model corresponding to each pattern
  # initialize all pattern predictions to NA
  K10 = K11 = K12 = K13 = K14 = K15 = K16 = K17 = K18 = K19 = K110 = K111 = K112 = K113 = K114 = NA 
  if ( 0 %in% pattern ){ ## basline regression model, $f_0$
    K10 = -1179.9976 + 11.8905 * sand + 11.9014 * silt + 11.8540 * clay + 5.2531 * d_g + 0.0279 * s_g -3.8603 * bd + -0.0387 * dia}
  if ( 1 %in% pattern){ 
    K11 = -1355.0792 + 14.0726 * sand + 9.9092 * silt + 0.0000 * clay - 50.1960 * d_g + 18.3884 * s_g - 12.9864 * bd - 0.2364 * dia - 0.0135 * L}
  if ( 2 %in% pattern ){ 
    K12 = 70.7377 - 0.5178 * sand - 0.6428 * silt + 0.0000 * clay - 30.3909 * d_g - 1.7316 * s_g - 4.6557 * bd + 2.6720 * dia - 0.0245 * L}
  if ( 3 %in% pattern ){ 
    K13 = 52.0616 + 0.5796 * sand - 0.5553 * silt + 0.0000 * clay - 230.6342 * d_g - 2.6810 * s_g - 1.1748 * bd + 0.3488 * dia - 0.0641 * L}
  if ( 4 %in% pattern ){ 
    K14 = -1033.6761 + 10.4057 * sand + 9.6609 * silt + 7.6045 * clay + 4.5560 * d_g + 4.5402 * s_g - 5.1402 * bd - 0.2013 * dia - 0.0006 * L}
  if ( 5 %in% pattern ){ 
    K15 = 53.5240 + 0.0949 * sand - 0.5390 * silt + 0.0000 * clay - 73.4111 * d_g - 1.5771 * s_g - 7.7699 * bd - 0.3555 * dia + 0.4150 * L}
  if ( 6 %in% pattern ){ 
    K16 = 762.4359 - 2.2777 * sand - 7.0926 * silt + 0.0000 * clay - 770.7954 * d_g - 22.6883 * s_g - 2.0712 * bd - 3.8762 * dia - 1.7193 * L}
  if ( 7 %in% pattern ){ 
    K17 = -50.4691 - 1.5926 * sand + 0.5580 * silt + 0.0000 * clay + 587.4053 * d_g + 7.4134 * s_g - 20.5499 * bd + 0.4336 * dia - 0.1798 * L}
  if ( 8 %in% pattern ){ 
    K18 = -243.3795 + 1.0718 * sand + 2.1596 * silt + 0.0000 * clay + 156.7400 * d_g + 5.0354 * s_g - 1.4163 * bd + 6.1225 * dia + 1.1349 * L}
  if ( 9 %in% pattern ){ 
    K19 = 13.7040 + 0.0419 * sand - 0.1043 * silt + 0.0000 * clay - 60.0944 * d_g - 0.0354 * s_g - 2.8371 * bd + 0.1564 * dia - 0.0114 * L}
  if ( 10 %in% pattern ){ 
    K110 = 281.9100 - 1.3948 * sand - 2.3683 * silt + 0.0000 * clay - 142.4181 * d_g - 5.2810 * s_g - 22.6087 * bd + 0.4585 * dia - 2.8566 * L}
  if ( 11 %in% pattern ){ 
    K111 = 148.3718 - 0.4490 * sand - 1.2957 * silt + 0.0000 * clay - 145.3112 * d_g - 4.2241 * s_g - 4.2887 * bd - 0.0533 * dia - 0.0857 * L}
  if ( 12 %in% pattern ){ 
    K112 = -896.5401 + 9.2031 * sand + 9.0852 * silt + 9.7194 * clay - 5.3507 * d_g - 1.3652 * s_g - 4.6936 * bd - 0.1804 * dia - 0.0005 * L}
  if ( 13 %in% pattern ){ 
    K113 = 76.0376 + 0.1459 * sand - 0.6629 * silt + 0.0000 * clay - 144.6754 * d_g - 3.1245 * s_g - 1.3179 * bd + 0.0301 * dia - 0.0606 * L}
  if ( 14 %in% pattern ){ 
    K114 = -204.8986 + 2.1700 * sand + 2.1471 * silt + 2.3956 * clay + 2.5379 * d_g - 0.3229 * s_g - 3.7845 * bd - 0.1985 * dia + 0.0861 * L}
  # 3. Average predictions where they exist.
  vK1 = c(K10, K11, K12, K13, K14, K15, K16, K17, K18, K19, K110, K111, K112, K113, K114)
  K1 = mean(vK1, na.rm = T)
  # convert log_e(cm/day) to cm/day
  K1 = exp(K1)
  
  
  ### Method 2: Brakensiek et al. (1984) model (verified)
  K2 = exp(19.52348 * phi
            - 8.96847
            - 0.028212 * clay
            + 0.00018107 * (sand^2)
            - 0.0094125 * (clay^2)
            - 8.395215 * (phi^2)
            + 0.077718 * sand * phi
            - 0.00298 * (sand^2) * (phi^2)
            - 0.019492 * (clay^2) * (phi^2)
            + 0.0000173 * (sand^2) * clay #sand^2 * clay
            + 0.02733 * (clay^2) * phi 
            + 0.001434 * (sand^2) * phi
            - 0.0000035 * (clay^2) * sand)
  # convert cm/hr to cm/day
  K2 = K2 * 24
  
  
  ### Method 3: Campbell & Shiozawa (1994) model (verified)
  K3 = 5.4 * exp((-0.07 * silt) - (0.167 * clay))
  # convert cm/hr to cm/day
  K3 = K3 * 24
  
  
  ### Method 4: Cosby et al. (1984) model (verified)
  K4 = 2.54 * (10^(-0.6 + 0.012 * sand - 0.0064 * clay))
  # convert cm/hr to cm/day
  K4 = K4 * 24
  
  
  ### Method 5: Jabro (1992) model (verified)
  if ( silt < 0.1 | clay < 0.1){
    K5 = -99
  }else{
    tempK5 = 9.56 - 0.81 * log10(silt) - 1.09 * log10(clay) - 4.64 * bd
    K5 = (10^tempK5)
    # convert cm/hr to cm/day
    K5 = K5 * 24
  }
  
  
  ### Method 6: Pucket et al. (1985) model (verified)
  K6 = 15.696 * exp(-0.1975 * clay)
  #convert cm/hr to cm/day
  K6 = K6 * 24
  
  
  ### Method 7: Dane & Puckett (1994) model (verified)
  K7 = 30.384 * exp(-0.144 * clay)
  # convert cm/hr to cm/day
  K7 = K7 * 24
  
  
  ### Method 8: Saxton et al. (1986) model (verified)
  # Ks (cm/hr)
  K8 = exp(12.012- 
             0.0755 * sand + 
             (-3.895 + 
                0.03671 * sand - 
                0.1103 * clay + 
                0.00087546 * (clay^2)) / 
             phi)
  # Convert (cm/hr) to (cm/day)
  K8 = K8 * 24
  
  
  ### Method 9: Nemes et al. (2005) 
  # OC to OM conversion:
  om = oc * 1.724 ## most commonly used conversion.
  #om = oc * 2 ## More accurate according to Pribyl (2010)
  # Auxiliary variables
  x1 = -3.663 + 0.046 * sand 
  x2 = -0.887 + 0.083 * clay 
  x3 = -9.699 + 6.451 * bd 
  x4 = -0.807 + 1.263 * om
  z1 = (-0.428 + 0.998*x1 + 0.651*(x1^2)  + 0.130*(x1^3))
  
  z2 = (0.506*x1 - 0.188*x2 - 0.327*x3 - 0.094*x4)
  
  z3 = (-0.268 + 0.885*z1 + 0.544*(z1^2) - 0.682*(z1^3) +
          0.320*z2 - (0.134*z1*z2) + (1.119*(z1^2)*z2) + 0.050*(z2^2) -
          (0.645*z1*(z2^2)) + 0.160*(z2^3)+ 0.126*x4 - (0.144*z1*x4)- 
          (0.372*(z1^2)*x4) + (0.247*z2*x4) + (0.795*z1*z2*x4) - (0.344*(z2^2)*x4) +
          0.038*(x4^2) - (0.071*z1*(x4^2)) + (0.020*z2*(x4^2)) - 0.015*(x4^3) )
  
  z4 = (0.102 + 1.383*z3 + 0.302*(z3^2) + 0.103*(z3^3) +
          0.331*x2 + 0.693*z3*x2 + 0.541*(z3^2)*x2 + 0.198*(x2^2) +
          0.429*z3*(x2^2) + 0.092*(x2^3) + 0.060*x3 + (0.277*z3*x3) + 
          0.417*(z3^2)*x3 + 0.242*x2*x3 + 0.929*z3*x2*x3 + 
          0.319*(x2^2)*x3 + 0.026*(x3^2) + 0.094*z3*(x3^2) + 0.116*x2*(x3^2) )
  # Ks (cm/day)
  K9 = (10^(0.571+(0.956*z4))) * 24
          
  
  #
  return(list(Ghanbarian2015Pattern = paste(pattern, collapse = ", "),
              Ghanbarian2015SHC2 = K1,
              Brakensiek1984 = K2,
              CampbellShiozawa1994 = K3,
              Cosby1984 = K4,
              Jabro1992  = K5,
              Pucket1985 = K6,
              DanePucket1994 = K7,
              Saxton1986 = K8,
              Nemes2005 = K9) )
}
