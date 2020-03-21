# ---------------------------------------------------------------------------------------
# Program           : RFunction_format-values.R
# Modified from     : 
# Author            : Chang
# Date created      : 20190319
# Purpose           : 
# Note              : Format numeric or character values
# Reference         : 
# Function internal : 
# How to run this   : source(paste0(locRFunction,"RFunction_format-values.R"))
#----------------------------------------------------------------------------------------
# Run dependency    : 
# Type File
#---------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Sys.Date() History
#----------------------------------------------------------------------------------------
# 20190319
#----------------------------------------------------------------------------------------

#--------------------------------------------------------
# Format numeric values to three decimal places
#--------------------------------------------------------
# Create a function to round numeric values to third decimal places
Round.numeric.3decimal <-function(x) round(x,digits = 3)

# Test your function
Round.numeric.3decimal(1.23456)

#-----------------------------------------------------------------
# Conditionally format sample size per journal requirement. 
## https://academic-oup-com.ezproxy.library.uq.edu.au/ije/pages/Instructions_To_Authors
## Figures should not be used to start a sentence and those between 999 and 9999 should not be separated by spaces or commas while those over 10 000 should have a space after the thousand. 
#-----------------------------------------------------------------
Separate.thousand.space <- function(x){
  if (x >= 10000){
    prettyNum(x,big.mark=" ",scientific=FALSE)
  } 
}

# Format p values
format.p.values.2.digits <- function(x) format.pval(x,digits = 2,eps = 0.001,nsmall = 3) 

#--------------------------------------------------------
# Add labels to variables
#--------------------------------------------------------
# Predictors, exposure, outcome used in manuscript 4
alcohol.var.abbre <- c("DPW","ESDPW")
alcohol.var.label <- c("Drinks per week","Estimated standard drinks per week")

caffein.var.abbre <- c("ECCPD")
caffein.var.label <- c("Caffeine consumed per day")

cannabis.var.abbre <- c("CI")
cannabis.var.label <- c("Cannabis initiation")

nicotine.var.abbre <- c("SI","AI","PYOS","CPD","SC")
nicotine.var.label <- c("Smoking initiation","Age at initiating regular smoking","Pack years of smoking","Cigarettes per day","Smoking cessation")

manu4.var.name.label <- data.frame( var.abbre=c(alcohol.var.abbre,caffein.var.abbre,cannabis.var.abbre,nicotine.var.abbre)
                                   ,var.label=c(alcohol.var.label,caffein.var.label,cannabis.var.label,nicotine.var.label)
                                   ,stringsAsFactors = F) # dim(manu4.var.name.label) 9 2
