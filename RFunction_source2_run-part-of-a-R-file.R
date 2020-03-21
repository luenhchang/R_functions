#-------------------------------------------------------------------------------------------------
# Program       : RFunction_source2_run-part-of-a-R-file.R
# Modified from : 
# Date created  : 20180803
# Purpose       : run part of a R file by start line and end line number
# Source        : https://stackoverflow.com/questions/12214963/source-only-part-of-a-file
# Note: 
#----------------------------------------------------------------------------------------
# function internal: source2()
# Type File
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Sys.Date()  History
#----------------------------------------------------------------------------------------
# 2018-08-03  
#----------------------------------------------------------------------------------------
# Run just part of a R file

source2 <- function(filePath=NULL, start_line=NULL, end_line=NULL, ...) {
  file.lines <- scan(filePath
                     , what=character()
                     , skip=start_line-1
                     , nlines=end_line-start_line+1
                     , sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

# Example of running part of a file
#filePathScript= paste0(locScripts,"PRS_UKB_201711_step18-01-01_adjust-significance-threshold-for-multiple-testing-in-a-single-data-set_pheno-group2-5.R")

#source2(filePath=filePathScript,start_line=207,end_line=273)
