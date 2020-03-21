####################################################################################################
# program name  : Rfunction_output_PNG.R
# purpose       : output a plot in PNG format
# programmer  	: Chang
# date created	: 2015-08-02
# note			    : 	
#---------------------------------------------------------------------------------------------------------------
# input files	  : 	
# output files  :	
#---------------------------------------------------------------------------------------------------------------
# Sys.Date()      history
#-----------------------------------------------------------------------------------------------------------------------
# 2015-08-02      program header copied from LungCancerArray_02_plots.R
#----------------------------------------------------------------------------------------------------------------------#/

output_PNG <- function(outputDir
                       ,outputFile
                       ,plotWidth
                       ,plotHeight
                       ,plotRes
                       ,inputFile) {
  png(paste(outputDir,outputFile,sep="")
      ,width  = plotWidth
      ,height = plotHeight
      ,res    = plotRes
      )
  
  source(inputFile)
  
  dev.off()
                        }

