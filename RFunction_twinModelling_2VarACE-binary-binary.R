#!/usr/bin/env Rscript

# -----------------------------------------------------------------------------------------------
# File name     : RFunction_twinModelling_2VarACE-binary-binary.R
# Modified from : /mnt/backedup/home/lunC/scripts/twin-modelling-openMx_4varaite_4var-common-pathway_NathanGillespie/multi_chol_4_variables[1]all-binary.R
# Purpose       : The original script runs multivariate twin modelling with 3 ordinal and 1 binary dependent variables. This script reduces number of dependent variables to two, and number of thresholds to 1 for two binary dependent variables. A binary variable is simply an ordinal variable with just 1 threshold (i.e. 2 categories- 1 or 0).   
# Date created  : 20181026
# Func internal : RunBivariateOrdiOrdiCholeksyDecomACE()
# Func external : umx::umxThresholdMatrix()
# Original file : /mnt/backedup/home/lunC/scripts/twin-modelling-openMx_4varaite_4var-common-pathway_NathanGillespie/multi_chol_4_variables.R (Nathan Gillespie)
# Author        : Chang
# Input data    : Input data must contain (1) a column ZYGOSITY for subsetting data from MZ (ZYGOSITY=1,2) or DZ (ZYGOSITY=3:6) twins,(2) phenotype variable names NOT having '.',(3) mxModels here haven't included covariates. The 2 phenotypes must be residualised against covariates
# Question: (1) labels differ from c("a11","a21","a22") pp37 Bivariate.pdf
# Note          : OpenMx only runs if the two dependent variables contain NO zero in their 16 cell frequencies, as checked at PRS_UKB_201711_step21-05-01_count-cases-controls-binary_univariate_bivariate.R. The pair of dependent variables with zero will be discarded from the analysis, resulting in no output. Try recoding these variables to include more ordinal thresholds to improve the cross tabs
#----------------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------------
# Import packages
#------------------------------------------------------------------------------------------------------
library(OpenMx)
library(umx)

#------------------------------------------------------------------------------------------------------
# Set up optimiser for the modelling
#------------------------------------------------------------------------------------------------------
#mxOption(NULL, "Default optimizer", "NPSOL")
mxOption(NULL, "Default optimizer", "SLSQP")
mxOption(NULL,"mvnRelEps",0.0055)

#------------------------------------------------------------------------------------------------------
# Set up the main function
#------------------------------------------------------------------------------------------------------

RunBivariateOrdiOrdiCholeksyDecomACE <-function( input.data
                                                 ,dep.var.names
                                                 ,covar.names
                                                 ,var.suffix.twin1.twin2
                                                 ,dep.var.labels
                                                 ,output.file.prefix
                                                 ,output.folder.path.model.status
                                                 ,output.folder.path.model.fit
                                                 ,output.folder.path.pheno.gene.env.corr) {
  # Run bivariate twin modelling for 2 binary dependent variables.
  #
  # Args: 
  #   input.data:                             Name of data frame containing binary phenotypes reshaped as in twin format
  #   dep.var.names:                          Names of two binary dependent variables without ID suffixes
  #   var.suffix.twin1.twin2:                 Suffixes of dependent variables of twin 1 and twin 2 (e.g. "_01" for twin1, "_02" for twin2)
  #   dep.var.labels:                         Labels for the 2 dependent variables, which will form part of the output file names
  #   output.file.prefix:                     Prefix for every output file
  #   output.folder.path.model.status:        Output folder path for CSV files containing mxStatus codes 
  #   output.folder.path.model.fit:           Output folder path for CSV files containing model fitting results
  #   output.folder.path.pheno.gene.env.corr: Output folder path for CSV files containing phenotypic, genetic and env correlations
  # Returns:
  # Outputs:
  
# Start timer
time_start <- proc.time()    
  
#---------------------------------------------------------------------------------------------
# Specify names of dependent variables, covariates recorded from twin1 and twin2
#---------------------------------------------------------------------------------------------
dep.var <- dep.var.names  #dep.var <- c("nicdep4","aspddx4")
covariates <- covar.names #c("sex","age")

var.suffix.twin1 <- var.suffix.twin1.twin2[1]
var.suffix.twin2 <- var.suffix.twin1.twin2[2]

dep.var.twin1 <- paste0(dep.var,var.suffix.twin1) # ,"depdx_01","dsmiv_conductdx_01","ftnd_dep_01","mania_scrn_01","alcdep4_01","panic4_01","sp_dsm4_01"

covariates.twin1 <- paste0(covariates,var.suffix.twin1)

dep.var.twin2 <- paste0(dep.var,var.suffix.twin2) # ,"depdx_02","dsmiv_conductdx_02","ftnd_dep_02","mania_scrn_02","alcdep4_02","panic4_02","sp_dsm4_02"

covariates.twin2 <- paste0(covariates,var.suffix.twin2)

Vars 	<- c(dep.var.twin1,covariates.twin1, dep.var.twin2,covariates.twin2)

# Specify names of dependent variables recorded from twin1 and twin2
selVars <- c(dep.var.twin1,dep.var.twin2)

#---------------------------------------------------------------------------------------------
# Set the Binary dependent variables up for OpenMx (converting them to mxFactor)
#---------------------------------------------------------------------------------------------
## Make a function to convert binary variables to mxFactor
binary.to.mxFactor <- function(x) mxFactor(x, levels = c(0:1))

## Apply this function to all the binary columns
data <- input.data
data[,selVars] <- lapply(data[selVars],binary.to.mxFactor)

#-----------------------------------------------------------------------------------------------
# Subset data from MZ twins
#-----------------------------------------------------------------------------------------------
mzdata	<- subset(data, ZYGOSITY<=2, Vars)

#-----------------------------------------------------------------------------------------------
# Subset data from DZ twins
#-----------------------------------------------------------------------------------------------
dzdata	<- subset(data, ZYGOSITY>=3, Vars)

#---------------------------------------------------------------------------------------------------------------
# Fill up missing values of covariates with -9 
#---------------------------------------------------------------------------------------------------------------
library(car)

# Make a function to recode data
Recode.NA.as.minus9 <- function(x) recode(x,"NA=-9")

# Apply this function to multiple columns, multiple data sets
mzdata[,c("sex_01","sex_02","age_01","age_02")] <- lapply(mzdata[c("sex_01","sex_02","age_01","age_02")],Recode.NA.as.minus9)

dzdata[,c("sex_01","sex_02","age_01","age_02")] <- lapply(dzdata[c("sex_01","sex_02","age_01","age_02")],Recode.NA.as.minus9)

str(mzdata)
# 'data.frame':	539 obs. of  8 variables:
# $ nicdep4_01: Ord.factor w/ 2 levels "0"<"1": 1 1 2 2 1 1 NA NA 1 1 ...
# $ aspddx4_01: Ord.factor w/ 2 levels "0"<"1": 1 1 1 1 1 1 NA 1 1 1 ...
# $ sex_01    : num  2 1 1 -9 2 1 -9 1 1 1 ...
# $ age_01    : num  27 31 30 -9 28 30 -9 25 56 31 ...
# $ nicdep4_02: Ord.factor w/ 2 levels "0"<"1": NA NA 2 2 NA NA 1 NA 1 2 ...
# $ aspddx4_02: Ord.factor w/ 2 levels "0"<"1": NA NA 1 1 NA NA 1 1 1 1 ...
# $ sex_02    : num  -9 -9 1 -9 -9 -9 2 1 1 1 ...
# $ age_02    : num  -9 -9 30 -9 -9 -9 35 25 56 35 ...

str(dzdata)
# 'data.frame':	1202 obs. of  8 variables:
# $ nicdep4_01: Ord.factor w/ 2 levels "0"<"1": 1 1 2 2 1 2 NA 1 1 2 ...
# $ aspddx4_01: Ord.factor w/ 2 levels "0"<"1": 1 1 1 1 1 1 NA 1 1 1 ...
# $ sex_01    : num  1 2 2 2 2 2 -9 2 2 1 ...
# $ age_01    : num  36 41 34 36 35 38 -9 32 30 48 ...
# $ nicdep4_02: Ord.factor w/ 2 levels "0"<"1": 2 NA NA 2 2 NA 2 1 2 1 ...
# $ aspddx4_02: Ord.factor w/ 2 levels "0"<"1": 1 NA NA 1 1 NA 1 1 1 1 ...
# $ sex_02    : num  2 -9 -9 2 1 -9 2 2 1 2 ...
# $ age_02    : num  36 -9 -9 36 35 -9 30 32 30 48 ...

#--------------------------------------------------------------------------------------------------------------
# Get starting values for thresholds
#--------------------------------------------------------------------------------------------------------------
threshold.values <- umx::umxThresholdMatrix(mzdata[selVars])
threshold.values.round <- round(threshold.values[[2]]$values,2)

starting.values.thresholds.MZTwins <- c(mean(threshold.values.round[1,c(1,3)]), mean(threshold.values.round[1,c(2,4)]))
starting.values.thresholds.MZTwins.lbound <- starting.values.thresholds.MZTwins-1
starting.values.thresholds.MZTwins.ubound <- starting.values.thresholds.MZTwins+1

# Declare starting values
 nv     	<- length(selVars)/2 # number of dependent variables in the study: 2 here 
 ntv    	<- nv*2    # number of all variables from twin1, twin2. 4 here
 thVals 	<- .5     # What is this? It is not in use by any other variables/object
 nth    	<- 1      # number of thresholds is 1 for binary variables
 svLTh  	<- 1.1    # start value for first threshold
 svITh  	<- 1.0    # start value for increments
 #lbTh   	<- matrix(rep(c(-3,(rep(0.001,nth-1))),nv),nrow=nth,ncol=nv)     # lower bounds for thresholds
 lbTh   	<- matrix(starting.values.thresholds.MZTwins.lbound,nrow=nth,ncol=nv)     # lower bounds for thresholds
 
 #ubTh   	<- matrix(rep(c(5,(rep(3,nth-1))),nv),nrow=nth,ncol=nv)     # upper bounds for thresholds
 ubTh   	<- matrix(starting.values.thresholds.MZTwins.ubound,nrow=nth,ncol=nv)     # upper bounds for thresholds
 
 #labTh  	<- c(paste("t",1:nth,"v1",sep=""),paste("t",1:nth,"v2",sep=""),paste("t",1:nth,"v3",sep=""),paste("t",1:nth,"v4",sep=""))
 labTh  	<- c(paste("t",1:nth,"v1",sep=""),paste("t",1:nth,"v2",sep=""))

 aLabs   	<- paste("a",rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="")
 cLabs   	<- paste("c",rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="")
 eLabs   	<- paste("e",rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="")
 
# 1 by 4 laten mean matrix. All variabes have laten mean of 0. Add sex and age to the means here.
 means  	<- mxMatrix( type="Zero", nrow=1, ncol=nv, name="means" )
 ageB     <- mxMatrix( type="Full", nrow=1, ncol=nv, free=TRUE, labels=c("ageB1"), values= 0.02, lbound=-2.0, name="ageB" )
 sexB     <- mxMatrix( type="Full", nrow=1, ncol=nv, free=TRUE, labels=c("sexB1"), values=-0.16, lbound=-2.0, name="sexB" )    
 age1 	<- mxMatrix( type="Full", nrow=1, ncol=1, free=F, labels=c("data.age_01"), name="age1")
 age2 	<- mxMatrix( type="Full", nrow=1, ncol=1, free=F, labels=c("data.age_02"), name="age2")
 sex1 	<- mxMatrix( type="Full", nrow=1, ncol=1, free=F, labels=c("data.sex_01"), name="sex1")
 sex2 	<- mxMatrix( type="Full", nrow=1, ncol=1, free=F, labels=c("data.sex_02"), name="sex2")
 modth1	<- mxAlgebra( expression= cbind(age1%x%ageB, age2%x%ageB), name="modth1")
 modth2	<- mxAlgebra( expression= cbind(sex1%x%sexB, sex2%x%sexB), name="modth2") 
 expM  	<- mxAlgebra(expression=cbind(means,means)+modth1+modth2, name="expM" )

# Set up threshold matrixes
# (original: 2 by 4 threshold matrix: 1st 3 variables have 2 thresholds (i.e. these are ordinal variables with 3 categories), 4th variable is binary & has 1 threshold)
 # thresh	<- mxMatrix( type="Full"
 #                     , nrow=nth
 #                     , ncol=nv
 #                     , free=c(T,T, T,T, T,T, T,F)
 #                     , values=c(-0.95,0.94, 0.65,0.64, 0.84,0.22, 0.93,0)
 #                     , lbound=lbTh
 #                     , ubound=ubTh
 #                     , labels=labTh
 #                     , name="thresh" )

#---------------------------------------------------------------------------------------------------------- 
# Set up threshold matrixes
#----------------------------------------------------------------------------------------------------------
## Use values estimated from MZ twins by umxThresholdMatrix() as values for values= option
 thresh	<- mxMatrix( type="Full"
                     , nrow=nth
                     , ncol=nv
                     , free=c(T,T)
                     , values= starting.values.thresholds.MZTwins #c(0.27, 1.98,0.47, 1.7)
                     , lbound=lbTh
                     , ubound=ubTh
                     , labels=labTh
                     , name="thresh" )
 
 unit	<- mxMatrix( type="Lower", nrow=nth,ncol=nth, free=FALSE, values=1, name="unit")
 
 expTh	<- mxAlgebra( expression= cbind(unit%*%thresh,unit%*%thresh), name="expTh")
 
#  a		<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=T, values=
#  	  		c(0.43,
# 			  0.34,0.18,
# 			  0.46,0.18,0.57,
# 			  0.16,0.25,0.13,0.10), lbound=-0.99, ubound=0.99, labels=aLabs, name="a" ) 
 a		<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=T, values=
                   c(0.43,
                     0.34,0.18), lbound=-0.99, ubound=0.99, labels=aLabs, name="a" ) 
 
 
  c		<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=T, values=0.01, lbound=-0.99, ubound=0.99, labels=cLabs, name="c" )
  
#   e		<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=T, values=
#  		   	c(0.52,
# 		   	  0.04,0.30,		   
# 		   	  0.26,0.03,0.33,			   
# 		   	  0.11,0.23,0.10,0.44), lbound=-0.99, ubound=0.99, labels=eLabs, name="e" )  
  e		<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=T, values=
                    c(0.52,
                      0.04,0.30), lbound=-0.99, ubound=0.99, labels=eLabs, name="e" ) 

# Matrices generated to hold variance A, C, and E
 A		<- mxAlgebra( expression= a%*%t(a), name="A" )
 C		<- mxAlgebra( expression= c%*%t(c), name="C" )
 E		<- mxAlgebra( expression= e%*%t(e), name="E" )   			  

# Expected MZ and DZ covariance matrices
covMZ  	<- mxAlgebra( expression= rbind( cbind(A+C+E, A+C), cbind(A+C, A+C+E)), name="expCovMZ" )
covDZ  	<- mxAlgebra( expression= rbind( cbind(A+C+E, 0.5%x%A+C), cbind( 0.5%x%A+C, A+C+E)), name="expCovDZ" )

 unitM	<- mxMatrix( type="Unit", nrow=nv, ncol=1, name="unitM" )
 ConVar 	<- mxConstraint( expression=diag2vec( A+C+E )==unitM, name="ConVar" )

 corrP	<- mxAlgebra( expression=cov2cor(A+C+E), name="corrP" ) 
 corrA	<- mxAlgebra( expression=cov2cor(A), name="corrA" )
 corrE	<- mxAlgebra( expression=cov2cor(E), name="corrE" )
 
 rowVC 	<- rep('VC',nv)
 colVC 	<- rep(c('A','C','E','SA','SC','SE'),each=nv)
 VC		<- mxAlgebra( expression=cbind(A,C,E,A/(A+C+E),C/(A+C+E),E/(A+C+E)), dimnames=list(rowVC,colVC), name="VC" )  

# Data objects for Multiple Groups
 dataMZ 	<- mxData( observed=mzdata, type="raw" )
 dataDZ 	<- mxData( observed=dzdata, type="raw" )

 # Objective objects for Multiple Groups
 objMZ  	<- mxExpectationNormal( covariance="expCovMZ", means="expM", thresholds="expTh", dimnames=selVars )
 objDZ  	<- mxExpectationNormal( covariance="expCovDZ", means="expM", thresholds="expTh", dimnames=selVars )
 FitFn   	<- mxFitFunctionML()
 #ci<- mxCI(c("corrA[2,1]"))
 
ConfInt <- mxCI(c("corrP","corrA","corrE"))

# Combine Groups
pars   	<- list( thresh,unit,expTh, a,c,e,A,C,E, corrP,corrA,corrE, unitM,ConVar, VC)
defs		<- list( means,ageB,sexB,sex1,sex2,age1,age2, modth1,modth2,expM)
modelMZ	<- mxModel( pars, defs, dataMZ, covMZ, objMZ, FitFn, name="MZ" )
modelDZ	<- mxModel( pars, defs, dataDZ, covDZ, objDZ, FitFn, name="DZ" )
modelFit		<- mxFitFunctionMultigroup(c("MZ","DZ"))

#ACE_chol_DIAG 	<- mxModel( "ACE_CUD", modelMZ, modelDZ, modelFit)  
CholAceModel  <- mxModel(name="CholeskyACE", pars, modelMZ, modelDZ, ConfInt,modelFit ) # 

#omxGetParameters(ACE_chol_DIAG) 
omxGetParameters(CholAceModel)

# ------------------------------------------------------------------------------
# Run Cholesky Decomposition ACE model
# ------------------------------------------------------------------------------
print("Running Cholesky Decomposition ACE model")

CholAceFit <- mxTryHardOrdinal(CholAceModel, extraTries=50, greenOK=T,checkHess=F,intervals=T)

# mx Status code: 6
statusCode_ACE <- CholAceFit$output$status$code

CholAceSumm <- summary(CholAceFit)

param2VarACE <- CholAceSumm$parameters

CholAceSumm$CI 
#                             lbound   estimate    ubound note
# CholeskyACE.corrP[1,1]          NA  1.0000000        NA  !!!
# CholeskyACE.corrP[2,1]  0.03902798  0.1516824 0.2609547     
# CholeskyACE.corrP[1,2]  0.03901985  0.1516824 0.2609564     
# CholeskyACE.corrP[2,2]          NA  1.0000000        NA  !!!
# CholeskyACE.corrA[1,1]          NA  1.0000000        NA  !!!
# CholeskyACE.corrA[2,1] -1.00000000  0.6182918 1.0000000     
# CholeskyACE.corrA[1,2] -1.00000000  0.6182918 1.0000000     
# CholeskyACE.corrA[2,2]          NA  1.0000000        NA  !!!
# CholeskyACE.corrE[1,1]          NA  1.0000000        NA  !!!
# CholeskyACE.corrE[2,1] -0.66447375 -0.2696201 0.1761226     
# CholeskyACE.corrE[1,2] -0.66447053 -0.2696201 0.1761226     
# CholeskyACE.corrE[2,2]          NA  1.0000000        NA  !!!

# ------------------------------------------------------------------------------
# Run Cholesky Decomposition AE model
# ------------------------------------------------------------------------------
print("Running Cholesky Decomposition AE model")

CholAeModel   <- mxModel( CholAceFit, name="CholeskyAE" )

CholAeModel <- omxSetParameters(CholAeModel,label=cLabs,free=F,values=0)

CholAeFit <- mxTryHardOrdinal(CholAeModel, extraTries=50, greenOK=T,checkHess=F,intervals=T)

# Mx status code: 6
statusCode_AE <- CholAeFit$output$status$code

CholAeSumm <- summary(CholAeFit, verbose=F)

param2VarAE <- CholAeSumm$parameters
CholAeSumm$CI

# Test if AE is better than ACE in bivariate
CholACEFit_vs_CholAEFit <- mxCompare(CholAceFit, CholAeFit)

# Get phenotypic correlation from MZ twins (rMZ)
expCovMZ	<- CholAeFit$MZ$algebras$expCovMZ$result

# Get phenotypic correlation from DZ twins (rDZ)
expCovDZ	<- CholAeFit$DZ$algebras$expCovDZ$result

# ------------------------------------------------------------------------------
# Run Cholesky Decomposition CE model
# ------------------------------------------------------------------------------
# print("Running Cholesky Decomposition CE model")
# 
# CholCeModel   <- mxModel( CholAceFit, name="CholeskyCE" )
# CholCeModel   <- omxSetParameters(CholCeModel, label=aLabs,free=F,values=0)
# 
# CholCeFit <- mxTryHardOrdinal(CholCeModel,extraTries=5, greenOK=T,checkHess=F,intervals=F)
#  							
# statusCode_CE <- CholCeFit$output$status$code # 6

# ------------------------------------------------------------------------------
# Cholesky Decomposition A Model
# ------------------------------------------------------------------------------
print("Running Cholesky Decomposition A model")

CholAModel   <- mxModel( CholAeFit, name="CholeskyA" )

# Drop e21 (set its value to 0) to see if the fit changes, see if this correlation is significant
CholAModel   <- omxSetParameters( CholAModel, labels="e21", free=FALSE, values=0 )

CholAFit <- mxTryHardOrdinal(CholAModel, extraTries=50, greenOK=T,checkHess=F,intervals=T)

# status code= 0
statusCode_A <- CholAFit$output$status$code

CholASumm <- summary(CholAFit, verbose=F)

param2VarA <- CholASumm$parameters

CholASumm$CI
#                         lbound  estimate    ubound note
# CholeskyA.corrP[1,1]         NA 1.0000000        NA  !!!
# CholeskyA.corrP[2,1] 0.06663262 0.1740983 0.2778370     
# CholeskyA.corrP[1,2] 0.06663262 0.1740983 0.2778370     
# CholeskyA.corrP[2,2]         NA 1.0000000        NA  !!!
# CholeskyA.corrA[1,1]         NA 1.0000000        NA  !!!
# CholeskyA.corrA[2,1] 0.13953496 0.3808769 0.6212671     
# CholeskyA.corrA[1,2] 0.13953496 0.3808769 0.6963354     
# CholeskyA.corrA[2,2]         NA 1.0000000        NA  !!!
# CholeskyA.corrE[1,1]         NA 1.0000000        NA  !!!
# CholeskyA.corrE[2,1]         NA 0.0000000        NA  !!!
# CholeskyA.corrE[1,2]         NA 0.0000000        NA  !!!
# CholeskyA.corrE[2,2]         NA 1.0000000        NA  !!!

CholAEFit_vs_CholAFit <- mxCompare(CholAeFit, CholAFit)
#         base comparison ep minus2LL   df       AIC   diffLL diffdf         p
# 1 CholeskyAE       <NA> 10 4119.919 5284 -6448.081       NA     NA        NA
# 2 CholeskyAE  CholeskyA  9 4121.696 5285 -6448.304 1.776866      1 0.1825346

# ------------------------------------------------------------------------------
# Cholesky Decomposition E Model
# ------------------------------------------------------------------------------
print("Running Cholesky Decomposition E model")

CholEModel   <- mxModel( CholAeFit, name="CholeskyE" )

CholEModel   <- omxSetParameters( CholEModel, labels="a21", free=FALSE, values=0 )

CholEFit     <- mxTryHardOrdinal(CholEModel, extraTries=50, greenOK=T,checkHess=F, intervals=T)

statusCode_E <- CholEFit$output$status$code # 0

CholESumm <- summary(CholEFit, verbose=F)
param2VarE <- CholESumm$parameters

# Does it make sense to compare AE with E?
CholAEFit_vs_CholEFit <- mxCompare(CholAeFit, CholEFit)
#         base comparison ep minus2LL   df       AIC   diffLL diffdf           p
# 1 CholeskyAE       <NA> 10 4119.919 5284 -6448.081       NA     NA          NA
# 2 CholeskyAE  CholeskyE  9 4128.141 5285 -6441.859 8.222109      1 0.004138308

#-----------------------------------------------------------------------------------------------------#
#-------------------- output mxStatus, parameters estimate from all models----------------------------#
#-----------------------------------------------------------------------------------------------------#
# add additional info: model names, mx status code, optimizer
param2VarACE$model <-"CholeskyACE"  #column 11
param2VarACE$mxStatus <- statusCode_ACE #column 12
param2VarACE$optimizer <- CholAceSumm$optimizerEngine #column 13

param2VarAE$model <-"CholeskyAE"
param2VarAE$mxStatus <- statusCode_AE
param2VarAE$optimizer <-CholAeSumm$optimizerEngine

# param2VarCE$model <-"CholeskyCE"
# param2VarCE$mxStatus <- statusCode_CE
# param2VarCE$optimizer <-CholCeSumm$optimizerEngine

param2VarA$model <-"CholeskyA"
param2VarA$mxStatus <- statusCode_A
param2VarA$optimizer <-CholASumm$optimizerEngine   

param2VarE$model <-"CholeskyE"
param2VarE$mxStatus <- statusCode_E
param2VarE$optimizer <-CholESumm$optimizerEngine

param2VarAllModels <- rbind(param2VarACE,param2VarAE,param2VarA,param2VarE)
param2VarAllModels$depVar1 <- dep.var[1] #column 14
param2VarAllModels$depVar2 <- dep.var[2] #column 15

# add dependent variables
param2VarAllModelsOrdered <- param2VarAllModels[,c(14,15,13,12,11,1:10)]

print("Exporting model statuses")

write.csv(param2VarAllModelsOrdered
          ,row.names = F # remove row number
          ,file=paste0(output.folder.path.model.status
                       ,output.file.prefix
                       ,dep.var.labels[1],"_",dep.var.labels[2]
                       ,"_01_modelStatus_paramEsti"
                       ,".csv")
          ,na="")

#---------------------------------------------------------------------------------------------------#
#---------------------------------- output model comparisons----------------------------------------#
#---------------------------------------------------------------------------------------------------#
cholesModelComparisons <- rbind( CholACEFit_vs_CholAEFit
                                 ,CholAEFit_vs_CholAFit
                                 ,CholAEFit_vs_CholEFit)

cholesModelComparisons$depVar1 <- dep.var[1] # column 10
cholesModelComparisons$depVar2 <- dep.var[2] # column 11

# reorder columns
cholesModelComparisonsOrdered <- cholesModelComparisons[,c(10,11,1:9)]

print("Exporting model fits")

write.csv(cholesModelComparisonsOrdered
          ,row.names = F # remove row number
          ,file=paste0(output.folder.path.model.fit
                       ,output.file.prefix
                       ,dep.var.labels[1],"_",dep.var.labels[2]
                       ,"_02_modelFits"
                       ,".csv")
          ,na="" )

#--------------------------------------------------------------------------------------------------------#
# Remove all the cross path coefficients, for testing the sig of phenotypic correlation, by equating both genetic (a21) and env correlation (e21) to zero
#--------------------------------------------------------------------------------------------------------#
print("Running Cholesky AE model equating both genetic (a21) and env correlation (e21) to zero")

CholModel   <- mxModel( CholAeFit, name="Chol" )

# Drop genetic and phenotypic correlation to see if there is association between two variables
# HERE 2 univariate models
CholModel   <- omxSetParameters( CholModel, labels=c("a21", "e21"), free=FALSE, values=0 )

CholFit     <- mxTryHardOrdinal(CholModel, extraTries=50, , greenOK=T,checkHess=F, intervals=T)

CholSumm <- summary(CholFit, verbose=F)

testTable <- mxCompare(CholAeFit, c( CholFit,  CholEFit ,CholAFit))

#----------------------------------------------------------------------------------#
#---------------------- Test correlations =1 vs correlations !=1-------------------#
#----------------------------------------------------------------------------------#
# test if the genetic and env correlation can be quated to 1

print("Testing if environmental correlation can be quated to 1")

CholAModel1   <- mxModel( CholAeFit, name="CholA" )

# Equate env correlation to 1, see if the correlation is qual to 1
CholAModel1   <- omxSetParameters( CholAModel1, labels="e21", free=FALSE, values=1 )

CholAFit1     <- mxTryHardOrdinal(CholAModel1, extraTries=50, greenOK=T, checkHess=F, intervals=T)

CholASumm1 <- summary(CholAFit1, verbose=F)
#CholAeSumm

print("Testing if genetic correlation can be quated to 1")
CholEModel1   <- mxModel( CholAeFit, name="CholE" )
CholEModel1   <- omxSetParameters( CholEModel1, labels="a21", free=FALSE, values=1 )
CholEFit1     <- mxTryHardOrdinal(CholEModel1, extraTries=50, greenOK=T,checkHess=F, intervals=T)
CholESumm1    <- summary(CholEFit1, verbose=F)

print("Testing if both genetic and environmental correlation can be quated to 1")
CholModel1   <- mxModel( CholAeFit, name="Chol" )
CholModel1   <- omxSetParameters( CholModel1, labels=c("a21", "e21"), free=FALSE, values=1 )
CholFit1     <- mxTryHardOrdinal(CholModel1, extraTries=50, greenOK=T,checkHess=F, intervals=T)
CholSumm1    <- summary(CholFit1, verbose=F)

testTable1 <- mxCompare(CholAeFit, c( CholFit1,  CholEFit1 ,CholAFit1))

#------------------------------------------------------------------------------------------#
#---------Extract rP, rE and rG from Cholesky AE models------------------------------------#
#------------------------------------------------------------------------------------------#
corrTypes <-c("PhenoCor", "GeneticCor", "EnviroCor")
colnames <-c("estimate", "lbound", "ubound", "pvalue1","pvalue2")

outputMatrix <-as.data.frame(matrix(0
                                    ,nrow=length(corrTypes)
                                    ,ncol=length(colnames))
                             ,row.names=corrTypes)
colnames(outputMatrix)[1:5] <-colnames

#--------------------insert result into the ouput data.frame row by row-------------------------#
outputMatrix[1,]<-c(CholAeSumm$CI["CholeskyAE.corrP[2,1]",c("estimate","lbound", "ubound") ]
                    ,testTable$p[2]
                    ,testTable1$p[2] )

outputMatrix[2,] <-c(CholAeSumm$CI["CholeskyAE.corrA[2,1]",c("estimate","lbound", "ubound") ]
                     ,testTable$p[3]
                     ,testTable1$p[3] )

outputMatrix[3,]<-c(CholAeSumm$CI["CholeskyAE.corrE[2,1]",c("estimate","lbound", "ubound") ]
                    ,testTable$p[4]
                    ,testTable1$p[4] )

# add additional info
outputMatrix$depVar1 <- dep.var[1] #column 6
outputMatrix$depVar2 <- dep.var[2] #column 7 
outputMatrix$corrTypes <- corrTypes  #column 8

# calculate run time
timeDiff <- proc.time() - time_start
outputMatrix$time <- timeDiff[1] #column 9

outputMatrixOrdered <-outputMatrix[,c(9,6,7,8,1:5)]

print("Exporting rP, rG, rE")

write.csv(outputMatrixOrdered
          ,file=paste0(output.folder.path.pheno.gene.env.corr
                       ,output.file.prefix
                       ,dep.var.labels[1],"_",dep.var.labels[2]
                       ,"_03_rPrGrE"
                       ,".csv")
          ,row.names=F
          ,na="" )

#---------------------------------------------------------------------------------------------------------
# End the main function
#---------------------------------------------------------------------------------------------------------

} # End the RunBivariateOrdiOrdiCholeksyDecomACE() function

#------------------------------------------------------------------------------------------------------#
#------------------------------------------This is the end of this file--------------------------------#
#------------------------------------------------------------------------------------------------------#