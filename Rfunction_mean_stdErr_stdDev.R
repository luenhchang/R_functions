#------------------------------------------------------------------------------------------
# file name: Rfunction_mean_stdErr_stdDev.R
# path: "C:/Now/R/Rfunction_mean_stdErr_stdDev.R"
# REMEMBER to double quote variable name when calling the function
#------------------------------------------------------------------------------------------

mean_SE_SD <- function(Data, GroupVar, Yvar){
  ddply(Data, GroupVar, function(x) return(c( 
		N = length(na.omit(x[[Yvar]])),
		mean_Yvar= mean(x[[Yvar]], na.rm=TRUE),
		sd = sd(x[[Yvar]], na.rm=TRUE),# standard deviation
		se = sd(x[[Yvar]], na.rm=TRUE)/sqrt(length(na.omit(x[[Yvar]]))) #standard error of mean, derived from standard error
	))) }

# an exmaple how to specify function parameters
# mean_SE_SD(Data = qry_weighR1_oriFgPeriods 
#          ,GroupVar = c("trt_group", "trt_label", "typePeriod", "orderPeriod")
#          ,Yvar = "Period")
