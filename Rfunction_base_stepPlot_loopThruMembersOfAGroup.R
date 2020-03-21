#------------------------------------------------------------------------------------------
# file name: Rfunction_stepPlot_loopThruMembersOfAGroup.R
# path: "C:/Now/R/Rfunction_stepPlot_loopThruMembersOfAGroup.R"
# plot type: one step function per member of a group, 3 groups overlapped in one plot
# function name: stepPlotLoop
# author: Eirik Sovik
# Sys.time()                Update  
#-------------------------------------------------------------------------------------------
# "2014-12-23 16:40:17 CST" standardised same scale on x and y across all plots
#"2013-11-06 10:59:58 EST"  removed top, right border using bty='n' in plot()
#"2013-10-17 10:33:54 EST"  function variable need to be [[]] in the for loop (e.g. data[[xVar]])
#                           and they need to be "quoted" when calling the function (e.g. "age")
#-------------------------------------------------------------------------------------------

stepPlotLoop <-function(data, xVar, yVar){

  xValues <- data[[xVar]] # wrong: data$xVar
  yValues <- data[[yVar]]
# set up an empty plot containing x y axises. Can't be inside the lopp otherwise one empty plot is
# produced per iteration and you end up having just one step curve instead of many 
  plot(0,0
       , xlim=c(0,40)     # xlim=c(min(xValues), max(xValues))
       , ylim=c(0,5750)  # ylim=c(min(yValues), max(yValues)) 
       , type='n'
       , bty='n'   #box type "n": Draws nothing around the plot
       , cex.axis=2.5 #Size of axis text (the values that indicate the axis tick labels)
       , xaxt="n" # suppress default x axis
       , yaxt="n" # suppress default y axis
       , xlab=""
       , ylab="")
# tailor x axis
  xTicks <- c(0,10,20,30,40)
  axis(1 
       ,at= xTicks  #a numeric vector indicating where tic marks should be drawn
       ,labels=xTicks
       ,cex.axis=2
       ,las=0 # las=0 if tick labels parallel to axis
       )
# tailor y axis
  yTicks <- c(0,1000,2000,3000,4000,5000) # y ticks shown at these values
  yTickLabels <- c(0,1,2,3,4,5) # set tick labels to thousandths
  axis(2 
       ,at= yTicks
       ,labels=yTickLabels
       ,cex.axis=2
       ,las=2 # las=2 if tick labels perpendicular to axis
       )

  #axis(1, at=y, labels=round(y,digits=2), col.axis="black", las=0, cex.axis=1.5, tck=-.01)
#axis(side = 1, at= xValues, cex.axis=2, tck = -.015, labels = round(xValues, digits=0))
#axis(side = 2, at= yValues, cex.axis=2, tck = -.015, labels = round(yValues, digits=0))
  
  for (i in unique(data$bee)){
    #set up 3 logical vectors, each for subsetting a trt_label group. Note observations are marked with true if meet the condition, else false
    subset1 <- data$trt_label=='ace'    & data$bee==i    
    subset2 <- data$trt_label=='ctrlAM' & data$bee==i
    subset3 <- data$trt_label=='met'    & data$bee==i
    # how are x1, x2 and x3 related to the 3 logical vectors?
    x1 <- data[[xVar]][subset1] # values of age in ace group #class(x1) [1] "numeric"
    x2 <- data[[xVar]][subset2] # values of age in ctrlAM group
    x3 <- data[[xVar]][subset3] # values of age in met group
    #setting x axis breaks and horizontal position of the plot
    xrange <- max(x1,x2,x3)-min(x1,x2,x3) # range of x breaks
    startx <- min(x1,x2,x3) - 0.2*xrange  # shift the plot to slightly right
    
    # ity=1 solid line, ity=2 dashed, ity=3 dotted line
    # add step functions for ace group
      points(x1,data[[yVar]][subset1], col='grey', type='s', lty=1, lwd=6) #grey solid
    # add step functions for ctrlAM group
      points(x2,data[[yVar]][subset2],col='black',type='s', lty=3, lwd=4) # black dashed
    # add step functions for met group
      points(x3,data[[yVar]][subset3],col='black',type='s', lty=1, lwd=3) # black solid
  }
}

#for (j in c(1:9)){
#  mtext(LETTERS[j], side=3, adj=0.05, cex=1.25, line=-1.75) 
#}

###########################################################################################
# section 2 how to call the function
###########################################################################################
#p1 <- stepPlotLoop(data=test, xVar="age", yVar="cumSumDurLeftByBeeAge") # wrong: xVar=age


## Note
#test$age <- Data.frame Test, Column named Data
#test[[age]] <- data.frame Test, column named the value of Data
#test[["age"]] <-
#Test[["age"]]
#Test[[age]]
