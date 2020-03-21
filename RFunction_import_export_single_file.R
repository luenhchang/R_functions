# ---------------------------------------------------------------------------------------
# Program           : RFunction_import_export_single_file.R
# Modified from     : 
# Author            : Chang
# Date created      : 20181204
# Purpose           : 
# Note              : read.table(), read.csv() and related functions read everything in as character strings, then depending on arguments to the function (specifically colClasses, but also others) and options the function will then try to "simplify" the columns. If enough of the column looks numeric and you have not told the function otherwise, then it will convert it to a numeric column, this will drop any leading 0's (and trailing 0's after the decimal).
# Reference         : 
# Function internal : ExportFileTabSeparated(), ExportFileSpaceSeparated(), ImportATabSeparatedFile() 
# How to run this   : source(paste0(locRFunction,"RFunction_import_export_single_file.R"))
#----------------------------------------------------------------------------------------------
# Run dependency    : 
# Type File
#---------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Sys.Date() History
#----------------------------------------------------------------------------------------
# 20180928
#----------------------------------------------------------------------------------------

homeDir="/mnt/backedup/home/lunC/"
locRFunction=paste0(homeDir,"scripts/RFunctions/")

ExportFileCommaSeparated <-function(data
                                  ,missing.values.as="NA"
                                  ,output.file.path){
  # Export a data.frame as a tab-separated file
  #
  # Args:
  #   data: Name of input data
  #   output.file.path: Absolute path of output file
  write.table(data
              ,file=output.file.path
              ,sep=","
              ,col.names = T
              ,row.names = F
              ,quote = F
              ,na = missing.values.as)
}

ExportFileTabSeparated <-function(data
                                  ,missing.values.as="NA"
                                  ,output.file.path){
  # Export a data.frame as a tab-separated file
  #
  # Args:
  #   data: Name of input data
  #   output.file.path: Absolute path of output file
  write.table(data
              ,file=output.file.path
              ,sep="\t"
              ,col.names = T
              ,row.names = F
              ,quote = F
              ,na = missing.values.as)
}

ExportFileSpaceSeparated <-function(data
                                    ,missing.values.as="NA"
                                    ,output.file.path){
  # Export a data.frame as a tab-separated file
  #
  # Args:
  #   data: Name of input data
  #   output.file.path: Absolute path of output file
  write.table(data
              ,file=output.file.path
              ,sep=" "
              ,dec="."
              ,col.names = T
              ,row.names = F
              ,quote = F
              ,na = missing.values.as)
}

ExportFileSpaceSeparatedHeadless <-function(data
                                            ,output.file.path){
  # Export a data.frame as a tab-separated file
  #
  # Args:
  #   data: Name of input data
  #   output.file.path: Absolute path of output file
  write.table(data
              ,file=output.file.path
              ,sep=" "
              ,dec="."
              ,col.names = F # GCTA software expects no headers
              ,row.names = F
              ,quote = F
              ,na = "NA" # GCTA software expects missing values as NA or -9
              )
}

AppendDataToATabSeparatedFile <- function(data,output.file.path){
  write.table(data
              ,file=output.file.path
              ,sep="\t"
              ,dec="."
              ,col.names = F 
              ,row.names = F
              ,quote = F
              ,na = "NA" # GCTA software expects missing values as NA or -9
              ,append = TRUE
  )
  
  
}
  
ImportASpaceSeparatedFile <-function(input.file.path
                                     ,data.name){
  # Import a tab-separated text file 
  #
  # Args:
  #   input.file.path:  Absolute path of an input text file that is tab-separated
  #   data: Name of data file 
  data <- read.table(input.file.path
                     ,header = T
                     ,sep = " "
                     ,stringsAsFactors = F
                     ,na.strings = c(".","NA")
  )
  assign(data.name, data, envir = .GlobalEnv)
}

ImportATabSeparatedFile <-function(input.file.path
                                  ,data.name){
  # Import a tab-separated text file 
  #
  # Args:
  #   input.file.path:  Absolute path of an input text file that is tab-separated
  #   data: Name of data file 
  data <- read.table(input.file.path
                     ,header = T
                     ,sep = "\t"
                     ,stringsAsFactors = F
                     ,na.strings = c(".","NA")
                     )
  assign(data.name, data, envir = .GlobalEnv)
}