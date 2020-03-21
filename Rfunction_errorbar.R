#------------------------------------------------------------------------------------------
# file name: Rfunction_errorbar.R
# ref: http://monkeysuncle.stanford.edu/?p=485
# path: "C:/Now/R/Rfunction_errorbar.R"
# REMEMBER to double quote variable name when calling the function
# graphics: C:\Now\(Project) RFID_r2 weighing_r1r2(active use)\rfidR1R2R3_02_plotFolder\fig00_trial_barplot_errorbar.tiff
#------------------------------------------------------------------------------------------
# author's codes for summarising data using apply()
## apply(x, margin, fun) where x= an array or matrix, 
## margin=1: apply function to rows, margin=2: apply function to columns
## fun: the function to be applied


#    y <- rnorm(500, mean=1)
#    y <- matrix(y,100,5)
#    y.means <- apply(y,2,mean)
#    y.sd <- apply(y,2,sd)
#    barx <- barplot(y.means, names.arg=1:5,ylim=c(0,1.5), col="blue", axis.lty=1, xlab="Replicates", ylab="Value (arbitrary units)")
#    error.bar(barx,y.means, 1.96*y.sd/10)

#    y1 <- rnorm(500, mean=1.1)
#    y1 <- matrix(y1,100,5)
#    y1.means <- apply(y1,2,mean) 
#    y1.sd <- apply(y1,2,sd)
     
#    yy <- matrix(c(y.means,y1.means),2,5,byrow=TRUE) #class(yy) [1] "matrix"
#   ee <- matrix(c(y.sd,y1.sd),2,5,byrow=TRUE)*1.96/10

# add bar plot (not part of the function)
#barx <- barplot(yy, 
#                beside=TRUE,
#                col=c("blue","magenta"), 
#                ylim=c(0,1.5), 
#                names.arg=1:5, 
#                axis.lty=1, 
#                xlab="Replicates", 
#                ylab="Value (arbitrary units)"
#                )
# function to make error bars
    error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
                
      if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
      stop("vectors must be same length")
      arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
    }

# example of how to call the function 
#error.bar(x=barx,   # assign bar plot to this object
#          y=yy,     # matrix containing values of bar height
#          upper=ee) # matrix containing values of error bars, should be in same 2D dimention as y

