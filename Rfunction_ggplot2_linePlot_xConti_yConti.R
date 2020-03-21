#------------------------------------------------------------------------------------------
# file name: Rfunction_linePlot_xConti_yConti.R
# plot type: overlapped line plots of different groups where x and y are numeric
# function name:LinePlotGroup
# date created: 27-Jul-13
# status: working
#-------------------------------------------------------------------------------------------
 LinePlotGroup<-function(Data=df13a, Xvar='age', Yvar='total_dur_min', LineSize=1.2,
                    Xmin, Xmax, BreakIncreX,      
                    Ymin, Ymax,BreakIncreY,YaxisTitle='Minutes outside hive',                       
                    LegendTitle="MB expansion",
                    AxisTextSize=10,LegendPosition,
                    GroupBreaks, GroupLabels, GroupColour,Shapes=c(16,17,15),
                    xAxisTitleOnOrOff=element_blank(),yAxisTitleOnOrOff=element_blank(),# replace with element_text(size=20) if axis titles want   
                    XAxisText=element_text(size=10),
                    LegendTitleSize=10, LegendTextSize=10,LegendKeySize=10,
                    PlotTitle="", PlotTitleSize=10)
                    {
  ggplot(Data, aes_string(x=Xvar, y=Yvar,colour='trt_label', group='trt_label')) +
  geom_line(position=pd, size=LineSize) +
  scale_x_continuous("Age", limits=c(Xmin,Xmax), expand=c(0,0), breaks=seq(Xmin,Xmax,by=BreakIncreX))+
  scale_y_continuous(YaxisTitle, limits=c(Ymin,Ymax), expand=c(0,0), breaks=seq(Ymin,Ymax,by=BreakIncreY)) +
  scale_colour_manual(name="groups", #Legend title
                   breaks=GroupBreaks,
                   labels=GroupLabels,
                   values=GroupColour
                    )+
  theme_bw() +   # make background theme black and white   
  theme(axis.title.x = xAxisTitleOnOrOff, #font size of x axis title
  			axis.title.y = yAxisTitleOnOrOff, #font size of y axis title
				axis.text.x  = XAxisText,                              #font size of x axis text 
				axis.text.y  = element_text(size=AxisTextSize),                              #font size of y axis text  
				legend.position=LegendPosition,
				legend.title=element_text(size=LegendTitleSize),                                #font size of legend title
				legend.text = element_text(colour="black", size =LegendTextSize, face = "bold"), #font size of legend text
        legend.key.size=unit(LegendKeySize,'points'), ## ben - added to shrink the legend 
    		legend.background=element_blank(), ## ben - added to get rid of white background
				panel.grid.major = element_line(size = 0.5, colour = '#FFFFFF'),
				panel.grid.minor = element_line(colour = NA), # colour = NA to suppress gridlines, reappear if colour='black'
        plot.title=element_text( face="bold", size=PlotTitleSize)
				)+
  ggtitle(PlotTitle)
                    }

#------------------------------------------------------------------------------------------
# plot type: overlapped line plots of different groups where x and y are numeric
# function name:LinePlotGroup_minOutR3
# plot each replicate of rfidR3, coz don't know for loop
# date created: 27-Jul-13
# status: working
#-------------------------------------------------------------------------------------------
 LinePlotGroup_minOutR3 <-function(Data=df13a, Xvar='age', Yvar='total_dur_min',YaxisTitle='total minutes outside hive', 
                                   LineSize=1.2,
                                  Xmin=1, Xmax=40, BreakIncreX=5,      
                                  Ymin=0, Ymax=3300,BreakIncreY=500,                       
                                  LegendTitle="Precocious topical",
                                  AxisTextSize=10,LegendPosition=c(0.85,0.8),
                                  GroupBreaks=c('ace','ctrlAM','met'), GroupLabels=c("acetone", "untreated", "methoprene"), 
                                  GroupColour=colour3Lines,Shapes=c(16,17,15),
                                  XAxisText=element_text(size=10),
                                  LegendTitleSize=10, LegendTextSize=10,LegendKeySize=10,
                                  PlotTitle="homing hive", PlotTitleSize=10)
                                  {
  ggplot(Data, aes_string(x=Xvar, y=Yvar,colour='trt_label', group='trt_label')) +
  geom_line(position=pd, size=LineSize) +
  scale_x_continuous("Age", limits=c(Xmin,Xmax), expand=c(0,0), breaks=seq(Xmin,Xmax,by=BreakIncreX))+
  scale_y_continuous(YaxisTitle, limits=c(Ymin,Ymax), expand=c(0,0), breaks=seq(Ymin,Ymax,by=BreakIncreY)) +
  scale_colour_manual(name="groups", #Legend title
                   breaks=GroupBreaks,
                   labels=GroupLabels,
                   values=GroupColour
                    )+
  theme_bw() +   # make background theme black and white   
  theme(axis.title.x = element_text(size=20), #font size of x axis title
    		axis.title.y = element_text(size=20), #font size of y axis title
				axis.text.x  = XAxisText,                              #font size of x axis text 
				axis.text.y  = element_text(size=AxisTextSize),                              #font size of y axis text  
				legend.position=LegendPosition,
				legend.title=element_text(size=LegendTitleSize),                                #font size of legend title
				legend.text = element_text(colour="black", size =LegendTextSize, face = "bold"), #font size of legend text
        legend.key.size=unit(LegendKeySize,'points'), ## ben - added to shrink the legend 
    		legend.background=element_blank(), ## ben - added to get rid of white background
				panel.grid.major = element_line(size = 0.5, colour = '#FFFFFF'),
				panel.grid.minor = element_line(colour = NA), # colour = NA to suppress gridlines, reappear if colour='black'
        plot.title=element_text( face="bold", size=PlotTitleSize)
				)+
  ggtitle(PlotTitle)
                    }
