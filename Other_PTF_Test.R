#' # Calculate Ks from models other popular PTFs 
#' Samuel Araya 
rm(list=ls())
#' Load the required libraries
#+ Load-Libraries, message=FALSE
library(tidyverse)
library(data.table)
library(mlr)
library(ggthemes)
library(caret) # Model training and tunning wrapper
library(MASS) # for producing density data
library(viridis) # color for ggplot
library(scales) # to access break formatting functions
library(ggExtra)

#' Import table
# File Directories
wDir <- getwd()
dataDir <- file.path(wDir,"Data")
modelDir <- file.path(wDir,"PTF_Models")

#Import files
train.dt = readRDS(file.path(dataDir, "KsatTrain_dim.rds"))
test.dt = readRDS(file.path(dataDir, "KsatTest_dim.rds"))
test.dt = data.table(test.dt)
train.dt = data.table(train.dt)

# Run predictions
source("Function_OtherPTF.R")

test_p.dt <- test.dt[ , PTF_ks(sand = rSand, clay = rClay, bd = Db, oc = exp(logOC), L = Height.cm, dia = Dia.cm),
                      by = seq_len(nrow(test.dt))]
# Clean prediction and convert to log10(cm/day)
test_p.dt[test_p.dt == -99] <- NA
test_p.dt[ , 3:11] = log10(test_p.dt[ , 3:11]+0.00001)

test.dt = cbind(test.dt, test_p.dt)

# Import Ksat predictions
us_test.dt = readRDS(file = file.path(dataDir,"KsatTest_predictions_2019.rds") )

# check both tables match exactly using BD and LogOC rows...
table(us_test.dt$Db - test.dt$Db, us_test.dt$logOC - test.dt$logOC)

us_pred.dt = cbind(us_test.dt, test.dt[ , 23:33])

# Save all predicted table
#saveRDS(us_pred.dt, file = file.path(dataDir,"KsatTest_predictions2019_4.rds") )

# Analyze
mlr::summarizeColumns(us_pred.dt)
# Change column name for Rosetta prediction
colnames(us_pred.dt)[24] = "RosettaH3"

model_list = c("BRT3.0", "BRT3.1", "BRT3.2", "BRT7.2", "RosettaH3", "Ghanbarian2015SHC2", "Nemes2005",
               "CampbellShiozawa1994", "DanePucket1994", "Jabro1992", "Saxton1986",
               "Pucket1985", "Brakensiek1984", "Cosby1984")
model_label = c("BRT-3-0","BRT-3-1", "BRT-3-2", "BRT-7-2", "Rosetta3-H3", "Ghanbarian et al. (2015)", "Nemes et al. (2005)",
               "Campbell & Shiozawa (1994)", "Dane & Puckett (1994)", "Jabro (1992)", "Saxton et al. (1986)",
               "Puckett et al. (1985)", "Brakensiek et al (1984)", "Cosby et al. (1984)")
# Melt table
sub_us.dt <- us_pred.dt %>% 
  #dplyr::select( -c(Height.cm, Dia.cm, COC, seq_len, Ghanbarian2015Pattern))%>%
  #dplyr::select(Ks_log10cmday, everything())%>%
  gather(Model_ID2, Ks, c(BRT3.0,BRT3.1, BRT3.2, BRT7.2, RosettaH3, Ghanbarian2015SHC2, Nemes2005,
                          CampbellShiozawa1994, DanePucket1994, Jabro1992, Saxton1986,
                          Pucket1985, Brakensiek1984, Cosby1984) )

p_us.dt <- sub_us.dt%>%
  filter(Model_ID2 !="BRT7.1" & !is.na(Ks))%>%
  #filter(!is.na(Ks) & Ks != -Inf )%>%
  mutate(Model_ID2 = factor(Model_ID2, 
                            levels = model_list,
                            labels = model_label ) )

# Accuracy

acc.dt <- p_us.dt%>%
  group_by(Model_ID2)%>%
  filter(Ks>-5)%>% ## Removes 0 cm/day prediction from Nemes et al. (2005)
  summarise(n.count=n(), 
            RMSLE = postResample(Ks,Ks_log10cmday)[1],
            Rsq = postResample(Ks,Ks_log10cmday)[2],
            MLE = mean((Ks-Ks_log10cmday), na.rm = T)
  )

#Plot 1:1
#Filter only needed models
# p_us.dt <- sub_us.dt%>%
#   filter(Model_ID2 !="BRT7.1" & !is.na(Ks))  #%>%
  # mutate(Model_ID2 = factor(Model_ID2, 
  #                           levels = model_list,
  #                           labels = c("Rosetta3 (H3)", "BRT (3-1)", "BRT (3-2)","BRT (7-2)")
  # )
  # )
# label names
pred.label = bquote("Predicted "~K[s]~"[cm/day]" )
measure.label = bquote("Measured "~K[s]~"[cm/day]" )
res.label = bquote("Residuals ["~log[10](cm/day)~"]" )
# linear scale
lpred.label = "Predicted Ks [cm/day]"
lmeasure.label = "Measured Ks [cm/day]"
lres.label = "Residuals [cm/day]"
# Measured vs. predicted plot 

#Create density column
get_density <- function(x, y, n = 100) {
  ## Function to get density of points in 2 dimensions.
  ##
  ##Parameters:
  #   x: A numeric vector.
  #   y: A numeric vector.
  #   n: Create a square n by n grid to compute density.
  ##
  ## @return The density within each square.
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

p_us.dt = p_us.dt%>%
  dplyr::group_by(Model_ID2)%>%
  dplyr::mutate(density = get_density(Ks, Ks_log10cmday),
         density2 = (density-min(density))/(max(density)-min(density)) 
  )

#****************** Plot All
mp = ggplot() + 
  geom_point(data = p_us.dt, aes(y = 10^Ks_log10cmday, x = 10^Ks, color = density2),
             #fill=NA, 
             size = 0.8 #, shape = 1
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = 2 ) + 
  
  geom_text(data = acc.dt ,aes(x = 10^-2, y = 10^5,
                              label = paste("RMSLE = ", format(RMSLE, digits = 3),
                                            "\nMLE = ", format(MLE, digits = 1)
                              )),
            vjust = "inward", hjust = "inward", parse = F) +
  geom_text(data = acc.dt ,fontface = "bold", aes(x = 10^-2, y = 10^3.7,
                              label = paste("R^2~'='~", format(Rsq, digits = 3)
                              )),
            vjust = "inward", hjust = "inward", parse = T) +
  
  labs(x = pred.label,
       y = measure.label )+
  # Log Scale:
  coord_fixed(ratio = 1, xlim = c(10^-2,10^5), ylim = c(10^-2,10^5))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(colour = "gray") +
  
  facet_wrap(~Model_ID2, labeller = label_value, ncol = 4)+
  theme_bw(base_size = 14) + 
  theme(panel.grid = element_blank(),
        strip.background = element_blank(), #element_rect( fill="white"),
        panel.border = element_rect(colour = "black"),
        axis.title=element_text(size=rel(1.25),face="bold"),
        strip.text = element_text(size=rel(0.87), face = "bold")) + #
  
  scale_color_viridis(name="Point\nDensity",
                      option="viridis"
                      #breaks = c(0.05,0.25,0.5,0.75,0.95,1)#,
  )
## mp

ggsave(filename = file.path(reportOutDir,"oneToOneFacet_Alls2019_5.pdf"), plot = mp, 
       scale = 1.45, 
       width = 8.25, height = 8, units = "in", dpi = 300)

