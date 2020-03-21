# ---------------------------------------------------------------------------------------
# Program           : RFunction_correlation-matrix.R
# Modified from     : PRS_UKB_201711_step18-06-01_function_correlation_plot.R
# Author            : Chang
# Date created      : 20180928
# Purpose           : Compute phenotypic correlation between any two variables of an input data. The output result is a correlation matrix
# Note              : 
# Reference         : https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
#                     http://www.statisticssolutions.com/correlation-pearson-kendall-spearman/
# Function internal : CalculateCorrBetween2Variables(), CalculateCorrBetween2GroupsOfVariables()
# How to run this   : source(paste0(locRFunction,"RFunction_correlation-matrix.R"))
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

CalculateCorrBetween2Variables <-function(input.data
                                          ,correlation.method
                                          ,output.file.path){
  # Calculate phenotypic correlation between any two variables of an input data file
  #
  # Args:
  #   input.data: Name of input data
  #   correlation.method: a character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
  #     (1) Spearman's and Kendall's correlation coefficient ARE suited for continuous data. Pearson's correlation coefficient is suited for continuous data, but tests on its value can be done only for bidimensionnal normally distributed data only. https://www.researchgate.net/post/Which_test_do_I_use_to_estimate_the_correlation_between_an_independent_categorical_variable_and_a_dependent_continuous_variable
  #     (2) If the categorical variable has two categories (dichotomous), you can use the Pearson correlation or Spearman correlation.
  
  #   output.file.path: Absolute path of output file
  # Returns: 
  #   The correlation coefficient between variable 1 and variable 2
  #
  # Output: 
  #   A correlation matrix exported as a space-separated txt file
  
  # Get column names from input.data
  colnames <- colnames(input.data)
  # Create an empty matrix for holding correlation  
  phenotypic.correlation.matrix <- matrix(NA
                             , nrow=length(input.data)
                             , ncol=length(input.data)
                             , dimnames = list(colnames,colnames))
  
  # Calculate phenotypic correlation or correlation between any 2 columns
  for (i in 1:length(input.data)){
    for (j in 1:length(input.data)){
      
      # Select observations where both variable 1 and variable 2 are non-missing
      input.data.complete.cases <- input.data[complete.cases(input.data[,c(i,j)]),]
      
      # Get variable 1
      variable1 <- input.data.complete.cases[,i]
      
      # Get variable 2
      variable2 <- input.data.complete.cases[,j]
      
      # Compute correlation
      correlation <- cor(variable1,variable2,method=correlation.method,use = "complete.obs")
      
      # Print the result on R console
      cat(correlation.method, "correlation between ", colnames[i], " and ", colnames[j], " is", correlation, "\n")
      
      phenotypic.correlation.matrix[i,j] <- correlation
    }
  }  
  
  # Replace correlation=NA with 0
  phenotypic.correlation.matrix[is.na(phenotypic.correlation.matrix) ] <- 0
  
  # Export correlation matrix as a txt with format required by Dale Nyholt's R script http://neurogenetics.qimrberghofer.edu.au/matSpDlite/
  write.table(phenotypic.correlation.matrix
              ,file=output.file.path
              ,sep = " "
              ,dec = "."
              ,row.names = T
              ,col.names = T)
}

# Compute phenotypic correlation between every variable of input data 1 and every variable of input data 2
CalculateCorrBetween2GroupsOfVariables <-function(input.data
                                                  ,start.end.group.1
                                                  ,start.end.group.2
                                                  ,correlation.method
                                                  ,output.data.name
                                                  ,output.file.path){
  # Calculate phenotypic correlation between every variable of group 1 and every variable of group 2 of the same input data file
  #
  # Args:
  #   input.data: Name of input data
  #   start.end.group.1: c(x1,x2) where x1 and x2 are starting and end column number for group 1 variables 
  #   start.end.group.2: c(y1,y2) where y1 and y2 are starting and end column number for group 2 variables 
  #   correlation.method: a character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
  #     (1) Spearman's and Kendall's correlation coefficient ARE suited for continuous data. Pearson's correlation coefficient is suited for continuous data, but tests on its value can be done only for bidimensionnal normally distributed data only. https://www.researchgate.net/post/Which_test_do_I_use_to_estimate_the_correlation_between_an_independent_categorical_variable_and_a_dependent_continuous_variable
  #     (2) If the categorical variable has two categories (dichotomous), you can use the Pearson correlation or Spearman correlation.
  #   output.data.name: If not NULL, user-defined name for the output correlation matrix. group 1 and group 2 variable names will form the row names and col names of the output correlation matrix
  #   output.file.path: If not NULL, absolute file path of the output correlation matrix 
  # Returns: 
  #   The correlation coefficient between a group 1 variable and group 2 variable
  #
  # Output: 
  #   A correlation matrix exported as a R object and/or space-separated txt file
  
  # Divide the input data into two data sets
  data1 <- input.data[,start.end.group.1]
  data2 <- input.data[,start.end.group.2]
  
  # Create an empty matrix for holding correlation  
  phenotypic.corr.coeff.matrix <- matrix(NA
                                          , nrow=length(data1)
                                          , ncol=length(data2)
                                          , dimnames = list(colnames(data1),colnames(data2)))
  
  # Calculate phenotypic correlation or correlation between every variable of data 1 and every variable of data2
  for (i in 1:length(data1)){
    for (j in 1:length(data2)){
      
      # Select observations where both a group 1 variable and group 2 variable are non-missing
      name.var.group.1 <- colnames(data1[i])
      name.var.group.2 <- colnames(data2[j])
      
      input.data.complete.cases <- input.data[complete.cases(input.data[,c(name.var.group.1,name.var.group.2)]),]
      
      # Get group 1 variable
      group1.variable <- input.data.complete.cases[,name.var.group.1]
      
      # Get group 2 variable
      group2.variable <- input.data.complete.cases[,name.var.group.2]
      
      # Compute correlation
      correlation <- cor(group1.variable,group2.variable,method=correlation.method,use = "complete.obs")
      
      # Print the result on R console
      cat(correlation.method, "correlation between ", name.var.group.1, " and ", name.var.group.2, " is", correlation, "\n")
      
      phenotypic.corr.coeff.matrix[i,j] <- correlation
    }
  }
  
  # Replace correlation=NA with 0
  phenotypic.corr.coeff.matrix[is.na(phenotypic.corr.coeff.matrix) ] <- 0
  
  # Name the correlation matrix with user-defined name
  if (length(output.data.name)==1){
    assign(output.data.name
           , phenotypic.corr.coeff.matrix
           ,envir=globalenv() # Make sure to assign it to the global environment
           )
  }
  
  # Export correlation matrix as a txt with format required by Dale Nyholt's R script http://neurogenetics.qimrberghofer.edu.au/matSpDlite/
  if (length(output.file.path) == 1){
  write.table(phenotypic.corr.coeff.matrix
              ,file=output.file.path
              ,sep = " "
              ,dec = "."
              ,row.names = T
              ,col.names = T)
  }
  
  
  
}
  