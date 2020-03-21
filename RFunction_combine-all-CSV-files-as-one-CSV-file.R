# ------------------------------------------------------------------------------
# Program:        RFunction_combine-all-CSV-files-as-one-CSV-file.R
# Modified from:  NU_100_func_all_CSV_to_1CSV.R
# date created:   20180811
# Author:         Chiu, MC
# Note            : (1) CSV files in the input folders should contain same number and names of variables 
# Possible errors : 
# (1) unmatched inputFileFolderPath returns this error: 
#  Error in strsplit(Files, "[.]")[[i]] : subscript out of bounds

# (2) CSV files differ in number of variables or names
# Error in rbind(deparse.level, ...) : 
#   numbers of columns of arguments do not match
#------------------------------------------------------------------------------------------------------
# run dependency    : 
# input files	      : 
# output files      : 
#------------------------------------------------------------------------------------------------------
# Sys.Date()  History
#--------------------------------------------------------------------------------------------------
# 2018-04-02  Allowed folders to contain non-CSV files, such as another folder
# 2016-12-18  changed output file prefix "ROut_" to "r_"  
#--------------------------------------------------------------------------------------------------

all_CSV_to_1CSV <- function(inputFileFolderPath    # directory of input data folder
                            ,output_dir # directory where single combined CSV will be exported to
                            ,outputFile # name of the output file without file extension .csv
                            ){ 

# Save CSV file names in an object
csv_Files <- list.files(path = inputFileFolderPath, pattern = ".csv")
  
C_data <- NULL

# Vertically combine all CSV files per input folder
for (i in 1:length(csv_Files)){
  a <- read.csv(paste0(inputFileFolderPath,csv_Files[i]),head=T)
  C_data <- rbind(C_data,a)
  a <- NULL
}

write.csv(C_data
          ,paste0(output_dir,outputFile,".csv")
          ,row.names = F
          ,na="" )

# Print the CSV combining result
# print(paste0("all CSV files in the directory "
#              ,inputFileFolderPath
#              , "have been combined and exported to "
#              ,output_dir
#              ,outputFile,".csv"))
cat("all CSV files in the following input folder:\n"
    ,inputFileFolderPath
    , "\nhave been combined and exported as the following file\n "
    ,output_dir
    ,outputFile
    ,".csv"
    ,sep="")

}

#----------------------------------------------------------------------------------------------#
#---------------------------------------This is the end of this file---------------------------#
#----------------------------------------------------------------------------------------------#
