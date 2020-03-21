# -----------------------------------------------------------------------------------------------
# File name     : RFunction_twinModelling_2VarACE_residualised_phenotypes.R
# Modified from : D:\Now\library_genetics_epidemiology\slave_NU\NU_analytical_programs_R\NU_012c_func_2VarResidACE.R
# Purpose       : To examine the source of factors that make traits correlate or co-vary. Rationale: Traits may be correlated due to shared genetic factors (A) or shared environmental factors (C or E)
# Date created  : 20180810
# func internal : bivarCholeksyDecomACE()
# Original file : RR_14.0_Genetic_correlations_function_openMx3_withPvalues
# Author        : Chang
# Input data    : Input data must contain (1) a column ZYGOSITY for subsetting data from MZ (ZYGOSITY=1,2) or DZ (ZYGOSITY=3:6) twins,(2) phenotype variable names NOT having '.',(3) mxModels here haven't included covariates. The 2 phenotypes must be residualised against covariates
# Question: (1) labels differ from c("a11","a21","a22") pp37 Bivariate.pdf
# Ori file  : twinMulAceCon.R
# Author: Hermine Maes
#  Date: 03 04 2014
#
# Multivariate Twin Analysis model to estimate causes of variation
# Matrix style model - Raw data - Continuous data
# ---------|---------|---------|---------|---------|---------|-------|---------|---------|---------|

# Load Library
library(OpenMx)

bivarCholeksyDecomACE<-function( data
                                ,variables   
                                ,var_suffixes
                                ,varNameShort
                                ,outputFilePrefix
                                ,outputFullPath01 
                                ,outputFullPath02
                                ,outputFullPath03) { 
  # Run bivariate twin modelling for 2 binary dependent variables.
  #
  # Args: 
  #   data:              Name of data frame containing residualised phenotypes in twin format
  #   variables:         Names of two residualised phenotypes without ID suffixes
  #   var_suffixes:      ID suffixes (e.g. "_01" for twin1, "_02" for twin2)
  #   varNameShort:      
  #   outputFilePrefix:  
  #   outputFullPath01:  output folder path for CSV files containing mxStatus codes 
  #   outputFullPath02:  output folder path for CSV files containing model fitting results
  #   outputFullPath03:  output folder path for CSV files containing phenotypic, genetic and env correlations
  # Returns:
  # Outputs:
  

# use default optimiser
mxOption(NULL,"Default optimizer","NPSOL")  

# timer starts
time_start <- proc.time()    

nv <- length(variables)

#selvariables <- c(paste(variables,"_T01",sep=""),paste(variables,"_T02",sep=""))
# Combine strings of dependent variable names part 1 and their suffixes to refer to the 2 phenotypes of twin 1 and twin 2
selvariables <- c(paste(variables,var_suffixes[1],sep=""),paste(variables,var_suffixes[2],sep=""))

ntv <- nv*2

# Select Data for Analysis
mzData    <- subset(data, ZYGOSITY %in% c(1:2), selvariables)
dzData    <- subset(data, ZYGOSITY %in% c(3:6), selvariables)

# ------------------------------------------------------------------------------
# prepare Cholesky Decomposition ACE Model
# ------------------------------------------------------------------------------
# Set Starting Values for means and path coefficients
svMe      <- rep(0.1, ntv)                
svPa      <- rep(0.1, nv*(nv+1)/2)

#labA<-paste("a", 1:(nv*(nv+1)/2), sep="") # [1] "a1" "a2" "a3"
#labC<-paste("c", 1:(nv*(nv+1)/2), sep="") # [1] "c1" "c2" "c3"
#labE<-paste("e", 1:(nv*(nv+1)/2), sep="") # [1] "e1" "e2" "e3"

# create Labels for (?path coefficients in ) Lower Triangular Matrices
labA     <- paste("a",rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="") #[1] "a11" "a21" "a22"
labC     <- paste("c",rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="") #[1] "c11" "c21" "c22"
labE     <- paste("e",rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="") #[1] "e11" "e21" "e22"

# Matrices declared to store a, c, and e Path Coefficients
pathA     <- mxMatrix( type="Lower"
                       ,nrow=nv
                       ,ncol=nv
                       ,free=TRUE
                       ,values=svPa
                       ,labels=labA
                       ,name="a" )

pathC     <- mxMatrix( type="Lower"
                       ,nrow=nv
                       ,ncol=nv
                       ,free=TRUE
                       ,values=svPa
                       ,labels=c("c11","c21","c22")
                       ,name="c" )

pathE     <- mxMatrix( type="Lower"
                       ,nrow=nv
                       ,ncol=nv
                       ,free=TRUE
                       ,values=svPa
                       ,labels=c("e11","e21","e22")
                       ,name="e" )

# Matrices generated to hold variance A, C, and E 
covA      <- mxAlgebra( expression=a %*% t(a), name="A" )
covC      <- mxAlgebra( expression=c %*% t(c), name="C" )
covE      <- mxAlgebra( expression=e %*% t(e), name="E" )

# Algebra to compute total variances and standard deviations (diagonal only)
## covP: total phenotypic variances is partitioned into A, C, E
## matI: an identity matrix (elements on diagonal are ones and else are zeros)
## Inverse of the SD used to standardise
covP      <- mxAlgebra( expression=A+C+E, name="V" )
matI      <- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I")
invSD     <- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD")

# Algebra to compute standardised path coefficent a, c, and e
## standardised path coefficent a= sqrt( squared a/(squared a+ squared c+ squared e))
## standardised path coefficent c= sqrt( squared c/(squared a+ squared c+ squared e))
## standardised path coefficent e= sqrt( squared e/(squared a+ squared c+ squared e))
## sum of these 3 standardised coefficients is 100% (i.e. total phenotypic variance)
StPathA     <- mxAlgebra(expression= iSD %*% a, name="sta")
StPathC     <- mxAlgebra(expression= iSD %*% c, name="stc")
StPathE     <- mxAlgebra(expression= iSD %*% e, name="ste")

# Algebra for expected Mean and Variance/Covariance Matrices in MZ & DZ twins
meanG     <- mxMatrix( type="Full"
                       ,nrow=1
                       ,ncol=ntv
                       ,free=TRUE
                       ,values=svMe
                       #,labels=c("PSYCH6","SOMA6")
                       ,labels=variables
                       ,name="expMean" )

covMZ     <- mxAlgebra( expression= rbind(  cbind(V, A+C)
                                           ,cbind(A+C, V))
                        ,name="expCovMZ" )

covDZ     <- mxAlgebra( expression= rbind(  cbind(V, 0.5%x%A+C)
                                           ,cbind(0.5%x%A+C, V))
                        ,name="expCovDZ" )

# Data objects for Multiple Groups
dataMZ    <- mxData( observed=mzData, type="raw" )

dataDZ    <- mxData( observed=dzData, type="raw" )

# Algebra to compute standardised variance A, C, and E
## StVA: A/V= (standardised path coefficient a)**2 . **2 means squared
## StVC: C/V= (standardised path coefficient c)**2
## StVE: E/V= (standardised path coefficient e)**2
StVA     <- mxAlgebra(expression= sta**2, name="StVA")
#StVA     <- mxAlgebra(expression= A/V, name="StVA")

StVC     <- mxAlgebra(expression= stc**2, name="StVC")
#StVC     <- mxAlgebra(expression= C/V, name="StVC")

StVE     <- mxAlgebra(expression= ste**2, name="StVE")
#StVE     <- mxAlgebra(expression= E/V, name="StVE")

# Algebra to compute correlations
## CorA     : genetic correlation
## CorC     : common env correlation
## CorE     : unique env correlation
## CorPheno : phenotypic correlation
### correlation xy= COVariance xy/(SDx*SDy)
CorA     <- mxAlgebra(expression= solve(sqrt(I*A)) %&% A, name="CorA") #same as name ="rA"
CorC     <- mxAlgebra(expression= solve(sqrt(I*C)) %&% C, name="CorC") #same as name ="rC"
CorE     <- mxAlgebra(expression= solve(sqrt(I*E)) %&% E, name="CorE") #same as name ="rE"

CorPheno<-mxAlgebra(expression= sqrt(StVA[1,1])*sqrt(StVA[2,2]+StVA[2,1])*CorA[2,1] + sqrt(StVE[1,1])*sqrt(StVE[2,2]+StVE[2,1])*CorE[2,1] ,name="CorPheno") 

#CorPheno <- mxAlgebra(expression = iSD%&%V, name ="CorPheno") #same as name ="rP"

StandObj<-c(StVA, StVC, StVE , CorA ,CorC, CorE , CorPheno)

# Objective objects for Multiple Groups
objMZ     <- mxExpectationNormal( covariance="expCovMZ", means="expMean", dimnames=selvariables )
objDZ     <- mxExpectationNormal( covariance="expCovDZ", means="expMean", dimnames=selvariables )

# Combine Groups
fitFunction <- mxFitFunctionML()

pars      <- list( pathA, pathC, pathE, covA, covC, covE, covP, matI,
                   invSD, meanG,StPathA,StPathC,StPathE, StandObj)

modelMZ   <- mxModel( pars, covMZ, dataMZ, objMZ, name="MZ", fitFunction )

modelDZ   <- mxModel( pars, covDZ, dataDZ, objDZ, name="DZ" , fitFunction)

modelFit      <- mxFitFunctionMultigroup( c("MZ", "DZ") )

ConfInt<-mxCI(c( "CorA", "CorE", "CorPheno", "StVA", "StVC","StVE"))

CholAceModel  <- mxModel(name="CholeskyACE", pars, modelMZ, modelDZ, ConfInt,modelFit )

# ------------------------------------------------------------------------------
# Run Cholesky Decomposition ACE model
# ------------------------------------------------------------------------------
CholAceFit    <- mxRun(CholAceModel, intervals=T)
CholAceFit    <- mxRun(CholAceFit, intervals=T)

# mx Status code: 1
statusCode_ACE <- CholAceFit$output$status$code

CholAceSumm    <- summary(CholAceFit)

param2VarACE <- CholAceSumm$parameters
#       name  matrix row col      Estimate  Std.Error lbound ubound lboundMet uboundMet
# 1     a11       a   1   1  1.022375e+00 0.35340298     NA     NA     FALSE     FALSE
# 2     a21       a   2   1  1.545149e-02 0.48638252     NA     NA     FALSE     FALSE
# 3     a22       a   2   2  3.533664e-05 0.74269855     NA     NA     FALSE     FALSE
# 4     c11       c   1   1  7.341102e-01 0.37689584     NA     NA     FALSE     FALSE
# 5     c21       c   2   1  9.997452e-01 0.80574844     NA     NA     FALSE     FALSE
# 6     c22       c   2   2  1.025214e-01 7.81446149     NA     NA     FALSE     FALSE
# 7     e11       e   1   1  1.604733e+00 0.07611992     NA     NA     FALSE     FALSE
# 8     e21       e   2   1  3.134660e-01 0.12178322     NA     NA     FALSE     FALSE
# 9     e22       e   2   2  1.715426e+00 0.05204399     NA     NA     FALSE     FALSE
# 10 PSYCH6 expMean   1   1  4.751034e-02 0.06444429     NA     NA     FALSE     FALSE
# 11  SOMA6 expMean   1   2 -1.017515e-03 0.06358751     NA     NA     FALSE     FALSE

CholAceSumm$CI
# > CholAceSumm$CI
# lbound     estimate     ubound note
# CholeskyACE.CorA[1,1]      1.000000e+00 1.000000e+00 1.00000000  !!!
# CholeskyACE.CorA[2,1]     -1.000000e+00 9.999974e-01 1.00000000     
# CholeskyACE.CorA[1,2]     -1.000000e+00 9.999974e-01 1.00000000     
# CholeskyACE.CorA[2,2]      1.000000e+00 1.000000e+00 1.00000000  !!!
# CholeskyACE.CorE[1,1]      1.000000e+00 1.000000e+00 1.00000000     
# CholeskyACE.CorE[2,1]      5.364099e-02 1.797571e-01 0.29071510     
# CholeskyACE.CorE[1,2]      5.364101e-02 1.797571e-01 0.29071508     
# CholeskyACE.CorE[2,2]      1.000000e+00 1.000000e+00 1.00000000  !!!
# CholeskyACE.CorPheno[1,1]  3.261590e-02 1.263918e-01 0.28353534     
# CholeskyACE.StVA[1,1]      9.437033e-17 2.513022e-01 0.43312649     
# CholeskyACE.StVA[2,1]      3.206746e-15 5.893299e-05 0.14396912     
# CholeskyACE.StVA[1,2]      0.000000e+00 0.000000e+00 0.00000000  !!!
# CholeskyACE.StVA[2,2]      3.125664e-16 3.082254e-10 0.19691206     
# CholeskyACE.StVC[1,1]      5.631231e-03 1.295682e-01 0.32692140     
# CholeskyACE.StVC[2,1]      8.974456e-03 2.467156e-01 0.32277659     
# CholeskyACE.StVC[1,2]      0.000000e+00 0.000000e+00 0.00000000  !!!
# CholeskyACE.StVC[2,2]      6.523868e-15 2.594460e-03 0.20804891     
# CholeskyACE.StVE[1,1]      5.263380e-01 6.191296e-01 0.72959628     
# CholeskyACE.StVE[2,1]      2.106253e-03 2.425485e-02 0.06369653     
# CholeskyACE.StVE[1,2]      0.000000e+00 0.000000e+00 0.00000000  !!!
# CholeskyACE.StVE[2,2]      6.330237e-01 7.263762e-01 0.80412201

#-------------------output estimates for path coefficients----------------------------------------#
#names(CholAceSumm$parameters)
#[1] "name" "matrix"  "row" "col" "Estimate"  "Std.Error" "lbound"  "ubound"  "lboundMet" "uboundMet"
# CholAceSumm <- data.frame(model=rep("Cholesky ACE",times=11)
#                          ,pathCoefficient=CholAceSumm$parameters$name
#                          ,estimate=CholAceSumm$parameters$Estimate
#                          ,StdError=CholAceSumm$parameters$Std.Error )

# ------------------------------------------------------------------------------
# Cholesky Decomposition AE Model
# ------------------------------------------------------------------------------
CholAeModel   <- mxModel( CholAceFit, name="CholeskyAE" )

CholAeModel   <- omxSetParameters( CholAeModel
                                   #,labels=paste("c", 1:(nv*(nv+1)/2), sep="")
                                   ,labels=c("c11","c21","c22")
                                   ,free=FALSE
                                   ,values=0 )

CholAeFit     <- mxRun(CholAeModel, intervals=T)

CholAeFit     <- mxRun(CholAeFit, intervals=T)

# mx status code:0
statusCode_AE <- CholAeFit$output$status$code
 
CholAeSumm <- summary(CholAeFit, verbose=F)

param2VarAE <- CholAeSumm$parameters
CholAeSumm$CI

# test if AE is better than ACE in bivariate
CholACEFit_vs_CholAEFit <- mxCompare(CholAceFit, CholAeFit)

#          base comparison ep minus2LL   df      AIC        diffLL diffdf  p
# 1 CholeskyACE       <NA> 11 12206.29 2999 6208.287            NA     NA NA
# 2 CholeskyACE CholeskyAE  8 12206.29 3002 6202.287 -1.489752e-09      3  1

# > CholACEFit_vs_CholAEFit
#          base comparison ep minus2LL   df      AIC   diffLL diffdf          p
# 1 CholeskyACE       <NA> 11 10381.76 2476 5429.761       NA     NA         NA
# 2 CholeskyACE CholeskyAE  8 10391.37 2479 5433.365 9.603868      3 0.02225167
# P values for

##rG : genetic correlation
##rE : env correlation
##rP : phenotypic correlation

# ------------------------------------------------------------------------------
# Cholesky Decomposition A Model
# ------------------------------------------------------------------------------
CholAModel   <- mxModel( CholAeFit, name="CholeskyA" )

#CholAModel   <- omxSetParameters( CholAModel, labels="e2", free=FALSE, values=0 )
# drop e21 (set its value to 0) to see if the fit changes to see if this correlation is significant
CholAModel   <- omxSetParameters( CholAModel, labels="e21", free=FALSE, values=0 )

CholAFit     <- mxRun(CholAModel, intervals=T)

# mx status code: 0
statusCode_A <- CholAFit$output$status$code

CholASumm <- summary(CholAFit, verbose=F)

param2VarA <- CholASumm$parameters

CholASumm$CI

CholAEFit_vs_CholAFit <- mxCompare(CholAeFit, CholAFit)
#         base comparison ep minus2LL   df      AIC  diffLL diffdf            p
# 1 CholeskyAE       <NA>  8 12206.29 3002 6202.287      NA     NA           NA
# 2 CholeskyAE  CholeskyA  7 12299.82 3003 6293.817 93.5306      1 3.999681e-22

# ------------------------------------------------------------------------------
# Cholesky Decomposition E Model
# ------------------------------------------------------------------------------
CholEModel   <- mxModel( CholAeFit, name="CholeskyE" )

#CholEModel   <- omxSetParameters( CholEModel, labels="a2", free=FALSE, values=0 )
CholEModel   <- omxSetParameters( CholEModel, labels="a21", free=FALSE, values=0 )

CholEFit     <- mxRun(CholEModel, intervals=T)

statusCode_E <- CholEFit$output$status$code

CholESumm <- summary(CholEFit, verbose=F)
param2VarE <- CholESumm$parameters

# does it make sense to compare AE with E?
CholAEFit_vs_CholEFit <- mxCompare(CholAeFit, CholEFit)

#--------------- output mxStatus, parameters estimate from all models----------------------------#
# add additional info: model names, mx status code, optimizer
param2VarACE$model <-"CholeskyACE"  #column 11
param2VarACE$mxStatus <- statusCode_ACE #column 12
param2VarACE$optimizer <- CholAceSumm$optimizerEngine #column 13
  
param2VarAE$model <-"CholeskyAE"
param2VarAE$mxStatus <- statusCode_AE
param2VarAE$optimizer <-CholAeSumm$optimizerEngine

param2VarA$model <-"CholeskyA"
param2VarA$mxStatus <- statusCode_A
param2VarA$optimizer <-CholASumm$optimizerEngine   
  
param2VarE$model <-"CholeskyE"
param2VarE$mxStatus <- statusCode_E
param2VarE$optimizer <-CholESumm$optimizerEngine
  
param2VarAllModels <- rbind(param2VarACE,param2VarAE,param2VarA,param2VarE)
param2VarAllModels$depVar1 <- variables[1] #column 14
param2VarAllModels$depVar2 <- variables[2] #column 15

# add dependent variables
param2VarAllModelsOrdered <- param2VarAllModels[,c(14,15,13,12,11,1:10)]

write.csv(param2VarAllModelsOrdered
          ,row.names = F # remove row number
          ,file=paste0(outputFullPath01
                       ,outputFilePrefix
                       ,varNameShort[1],"_",varNameShort[2]
                       ,"_01_modelStatus_paramEsti"
                       ,".csv")
            ,na="")

#---------------------------------- output model comparisons----------------------------------------#

cholesModelComparisons <- rbind( CholACEFit_vs_CholAEFit
                                ,CholAEFit_vs_CholAFit
                                ,CholAEFit_vs_CholEFit)

cholesModelComparisons$depVar1 <- variables[1] # column 10
cholesModelComparisons$depVar2 <- variables[2] # column 11

# reorder columns
cholesModelComparisonsOrdered <- cholesModelComparisons[,c(10,11,1:9)]

write.csv(cholesModelComparisonsOrdered
          ,row.names = F # remove row number
          ,file=paste0(outputFullPath02
                       ,outputFilePrefix
                       ,varNameShort[1],"_",varNameShort[2]
                       ,"_02_modelFits"
                       ,".csv")
          ,na="" )

#------------------------------ what is this model "Chol"?
# remove all the cross path coefficients, for testing the sig of phenotypic correlation, by equating both
## genetic (a21) and env correlation (e21) to zero
CholModel   <- mxModel( CholAeFit, name="Chol" )

# drop genetic and phenotypic correlation to see if there is association between two variables
# HERE 2 univariate models
CholModel   <- omxSetParameters( CholModel, labels=c("a21", "e21"), free=FALSE, values=0 )

CholFit     <- mxRun(CholModel, intervals=T)

CholSumm <- summary(CholFit, verbose=F)

# what are testTable and testTable1?
## what is Chol?
testTable<-mxCompare(CholAeFit, c( CholFit,  CholEFit ,CholAFit))

#---------------------- Test correlations =1 vs correlations !=1-------------------#
# test if the genetic and env correlation can be quated to 1
CholAModel1   <- mxModel( CholAeFit, name="CholA" )

# equate env correlation to 1, see if the correlation is qual to 1
CholAModel1   <- omxSetParameters( CholAModel1, labels="e21", free=FALSE, values=1 )

CholAFit1     <- mxRun(CholAModel1, intervals=T)

CholASumm1 <- summary(CholAFit1, verbose=F)
#CholAeSumm

CholEModel1   <- mxModel( CholAeFit, name="CholE" )
CholEModel1   <- omxSetParameters( CholEModel1, labels="a21", free=FALSE, values=1 )
CholEFit1     <- mxRun(CholEModel1, intervals=T)
CholESumm1    <- summary(CholEFit1, verbose=F)

CholModel1   <- mxModel( CholAeFit, name="Chol" )
CholModel1   <- omxSetParameters( CholModel1, labels=c("a21", "e21"), free=FALSE, values=1 )
CholFit1     <- mxRun(CholModel1, intervals=T)
CholSumm1    <- summary(CholFit1, verbose=F)

testTable1 <- mxCompare(CholAeFit, c( CholFit1,  CholEFit1 ,CholAFit1))

#--------------------------------extract rP, rE and rG-------------------------------------#
corrTypes <-c("PhenoCor", "GeneticCor", "EnviroCor")
colnames <-c("estimate", "lbound", "ubound", "pvalue1","pvalue2")

outputMatrix <-as.data.frame(matrix(0
                                    ,nrow=length(corrTypes)
                                    ,ncol=length(colnames))
                             ,row.names=corrTypes)
colnames(outputMatrix)[1:5] <-colnames

#--------------------insert result into the ouput data.frame row by row-------------------------#
outputMatrix[1,]<-c(CholAeSumm$CI["CholeskyAE.CorPheno[1,1]",c("estimate","lbound", "ubound") ]
                    ,testTable$p[2]
                    ,testTable1$p[2] )

outputMatrix[2,] <-c(CholAeSumm$CI["CholeskyAE.CorA[2,1]",c("estimate","lbound", "ubound") ]
                     ,testTable$p[3]
                     ,testTable1$p[3] )
  
outputMatrix[3,]<-c(CholAeSumm$CI["CholeskyAE.CorE[2,1]",c("estimate","lbound", "ubound") ]
                    ,testTable$p[4]
                    ,testTable1$p[4] )

# add additional info
outputMatrix$depVar1 <- variables[1] #column 6
outputMatrix$depVar2 <- variables[2] #column 7 
outputMatrix$corrTypes <- corrTypes  #column 8

# calculate run time
timeDiff <- proc.time() - time_start
outputMatrix$time <- timeDiff[1] #column 9

outputMatrixOrdered <-outputMatrix[,c(6,7,8,1:5)]

write.csv(outputMatrixOrdered
          ,file=paste0(outputFullPath03
                       ,outputFilePrefix
                       ,varNameShort[1],"_",varNameShort[2]
                       ,"_03_rPrGrE"
                       ,".csv")
          ,row.names=F
          ,na="" )
          
# ends the function
}

#------------------------------------------------------------------------------------------------------#
#------------------------------------------This is the end of this file--------------------------------#
#------------------------------------------------------------------------------------------------------#