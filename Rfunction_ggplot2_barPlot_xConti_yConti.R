#------------------------------------------------------------------------------------------
# file name: Rfunction_barPlot_xConti_yConti.R
# path: "C:/Now/R/Rfunction_barPlot_xConti_yConti.R"
# plot type: overlapped line plots of different groups where x and y are numeric
# function name:LinePlotGroup
# Sys.time()date: "2013-08-05 16:55:17 EST"
# status: working
#-------------------------------------------------------------------------------------------
colour3bars     <- c("white","blue","orange")

barPlotGroup <- function(Data, Xvar="FD_order", Yvar="proportion_FD", Ymin=0, Ymax=1,BreakIncreY=0.2, 
                       LegendTitle="MB expansion",LegendPosition=c(1,0.8),
                       PlotTitle="", PlotTitleSize=20,   
                       AxisTextSize=15,GroupBreaks, GroupLabels, GroupColour,
                       YAxisTitleSize=element_text(size=20), XAxisText=element_text(size=20),
                       LegendTitleSize=10, LegendTextSize=10
                       
                       ){
  Data$FD_order <- Data$FD  ## ben - I had to add to make your code work for me
  Fig01 <-ggplot(Data, aes_string(x=Xvar, y=Yvar, fill='trt_label')) + 
						geom_bar(stat = "identity", position=position_dodge(), colour="black") + # colour="black" to give black outline of bars
						scale_y_continuous("Rate of final stage reached (%)", limits=c(Ymin,Ymax), expand=c(0,0), 
                               breaks=seq(Ymin,Ymax,by=BreakIncreY)) +
						scale_x_discrete("stage", 
														 limits=c(1,2,3,4), #replace with c("t1","t2","t3","t4") if x values are characters
														 labels=c("missing","in-hive", "orientation", "foraging") #values to print on x axis
														)+
						scale_fill_manual(name  =LegendTitle,
															breaks=GroupBreaks,
															labels=GroupLabels,
															values=GroupColour)+           
						theme_bw() +   # make background theme black and white   
						theme(axis.title.x = element_blank(), #font size of x axis title
  								axis.title.y = YAxisTitleSize, #font size of y axis title
									axis.text.x  = XAxisText,                              #font size of x axis text 
									axis.text.y  = element_text(size=AxisTextSize),                              #font size of y axis text  
									legend.position=LegendPosition,
									legend.title=element_text(size=LegendTitleSize),                                #font size of legend title
									legend.text = element_text(colour="black", size =LegendTextSize, face = "bold"), #font size of legend text
                  legend.key.size=unit(10,'points'), ## ben - added to shrink the legend 
    				      legend.background=element_blank(), ## ben - added to get rid of white background
									panel.grid.major = element_line(size = 0.5, colour = '#FFFFFF'),
									panel.grid.minor = element_line(colour = NA), # colour = NA to suppress gridlines, reappear if colour='black'
                  plot.title=element_text( face="bold", size=PlotTitleSize)
									)+
              ggtitle(PlotTitle)
                                  }
