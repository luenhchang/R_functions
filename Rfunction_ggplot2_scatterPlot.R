#------------------------------------------------------------------------------------------
# file name: Rfunction_ggplot2_scatterPlot.R
# path: "C:/Now/R/Rfunction_ggplot2_scatterPlot.R"
# Sys.time(): "2013-09-19 19:43:56 EST"
#  working
#---------------------------------------------------

############################################################################################################
# SECTION load packages
#--------------------------------------------------------------------------------------------------------------------------
#package          details
#--------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(grid)    #Function unit is in library grid, needed for legend.key.size= unit()
#--------------------------------------------------------------------------------------------------------------------------

############################################################################################################
# step plot function
#--------------------------------------------------------------------------------------------------------------------------
                        #setting varies with subplots
ScatterPlot <- function(Data , XVar= 'days_exp_cum', YVar= 'MeanDayDirWt',
                     Xmin, Xmax, BreakIncreX,      
                     Ymin, Ymax, BreakIncreY,                       
                     GroupLabels, GroupColour, Shapes,
                     LegendTitle="MB expansion", PlotTitle="",
                        #setting same across subplot
                     PointSize=1, RegLineSize=2.0,   
                     axisTitleY=element_text(size=10), AxisTextSize=10, LegendPosition=c(0.8,0.3),
                     XAxisText=element_text(size=10),
                     LegendTitleSize=10, LegendTextSize=10, LegendKeySize=10, PlotTitleSize=10
                       ){
  
      GroupBreaks <-unique(Data$trt_label)
  
      ggplot(Data, aes_string(x=XVar, y=YVar, colour='trt_label', shape='trt_label')) + 
              geom_point(size=PointSize)+
              geom_smooth(method="lm", se = FALSE, size=RegLineSize)+ # "se = FALSE" to remove default shade
              scale_x_continuous("Age at first foraging in days", limits=c(Xmin,Xmax), expand=c(0,0), 
                                breaks=seq(Xmin,Xmax,by=BreakIncreX)
                                 )+
              scale_y_continuous("Lifespan in days", limits=c(Ymin,Ymax), expand=c(0,0), 
                                breaks=seq(Ymin,Ymax,by=BreakIncreY)
                                 ) +
              scale_colour_manual(name=LegendTitle,
          								breaks=GroupBreaks,
                          labels=GroupLabels,
                          values=GroupColour
                          ) +
							scale_shape_manual(name=LegendTitle,
			  									breaks=GroupBreaks,
                          labels=GroupLabels,
                          values=Shapes # numbers in the values=c() refer to different shapes
                                ) +
            theme_bw() +   # make background theme black and white   
    
						theme(axis.title.x = element_blank(), #font size of x axis title
  								axis.title.y = axisTitleY, #font size of y axis title
									axis.text.x  = XAxisText,                             #font size of x axis text 
									axis.text.y  = element_text(size=AxisTextSize),       #font size of y axis text  
									legend.position = LegendPosition,
									legend.title= element_text(size=LegendTitleSize),      #font size of legend title
									legend.text = element_text(colour="black", size = LegendTextSize, face = "bold"), #font size of legend text
                  legend.key.size= unit(LegendKeySize,'points'), ## ben - added to shrink the legend 
    				      legend.background= element_blank(), ## ben - added to get rid of white background
									panel.grid.major = element_line(size = 0.5, colour = '#FFFFFF'),
									panel.grid.minor = element_line(colour = NA), # colour = NA to suppress gridlines, reappear if colour='black'
                  plot.title= element_text( face="bold", size=PlotTitleSize)
									)+
              ggtitle(PlotTitle)
                    }
