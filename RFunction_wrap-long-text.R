# ---------------------------------------------------------------------------------------
# Program           : RFunction_wrap-long-text.R
# Modified from     : https://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels
# Author            : 
# Date created      : 20190203
# Purpose           : wrap long text to multiple lines by fixed length
# Note              : 
# Reference         : https://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels
#                     https://tolstoy.newcastle.edu.au/R/help/04/12/8610.html
# Function internal : wrap.labels()
# How to run this   : source(paste0(locRFunction,"RFunction_correlation-plot_corrplot-modified.R"))
#----------------------------------------------------------------------------------------------
# Run dependency    : 
# Type File
#---------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Sys.Date() History
#----------------------------------------------------------------------------------------
# 20190203   Used this function in PRS_UKB_201711_step18-04-07_barPlot_variance-explained-by-GSCAN-PRSs-of-target-phenotypes.R
#----------------------------------------------------------------------------------------

# Core wrapping function
wrap.it <- function(x, len){ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
  }

# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

a <- c("I don't feel competent enough to solve problems in R", "I don't feel competent enough to solve problems in R")

wrap.labels(a, 10)
