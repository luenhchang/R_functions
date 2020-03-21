#---------------------------------------------------------------------------------------------------------
# file name: Rfunction_stepPlot[1].R
# path: "C:/Now/R/Rfunction_stepPlot[1].R"
#---------------------------------------------------------------------------------------------------------
# date          change
#---------------------------------------------------------------------------------------------------------
# 11 Sep 2013   legend set removable. Now turned off. Set legend.position if to turn on
#---------------------------------------------------------------------------------------------------------

                     # plot specifications vary with data subsets. Modify them when calling the function    
stepPlot <- function(Data,xVar, yVar, LegendTitle="", GroupLabels, Plottitle="",labelPosiY,
                     # plot specifications remain the same over data subsets. Ignore these setting when calling the function  
                     GroupColour=c("black","blue","orange"), LineTypeGroup=c("solid","solid","solid"), 
                     LineSize=1,
  #legend.position="none" removes all legends, supressing other legend setting below
  #to restore legends, set LegendPosition=c(0.5,0.8)    
                     LegendPosition="none", 
                     YaxisTitle="", YAxisTitleSize=element_blank(),
                     XAxisText=element_text(size=10),AxisTextSize=10,
                     LegendTitleSize=10, LegendTextSize=10,LegendKeySize=10,  
                     PlotTitleSize=15
                      ){
# define x limits (Xmin, Xmax), x break increments (BreakIncreX),level of breaks (GroupBreaks),horizontal position of label text (labelPosiX)
  Xmin <- min(Data[xVar])-1 
  Xmax <- max(Data[xVar])+1
  BreakIncreX <- round((Xmax-Xmin)/6)
  GroupBreaks <-unique(Data$trt_label) 
  #labelPosiX <-min(Data[xVar])+2
  
# define y maximal limit (limitYMax),y break increments (BreakIncreY)   
  library(plyr)
  limitYMax <- round_any(max(Data[yVar]), 100, f = ceiling) # round to nearest greater than maximal Y
  BreakIncreY <- round_any(max(Data[yVar])/5, 100, f = ceiling)
  
# step plot  
  ggplot(Data, aes_string(x=xVar, y=yVar, group='trt_label'))+
  geom_step(aes(colour=trt_label, linetype=trt_label), direction='hv',size= LineSize)+ #specify step curve from different group with colours, colour by default
  scale_y_continuous(YaxisTitle, limits=c(0,limitYMax), expand=c(0,0), breaks=seq(0,limitYMax,by=BreakIncreY))+
  scale_x_continuous("Age of adults in days", limits=c(Xmin, Xmax), expand=c(0,0), breaks=seq(Xmin,Xmax,by=BreakIncreX)) +
  scale_colour_manual(name=LegendTitle,
        							breaks=GroupBreaks,
        							labels=GroupLabels,
        							values=GroupColour
                      )+  # change default colours to manually specified grey scale
  scale_linetype_manual(name  =LegendTitle,
                        breaks=GroupBreaks,
                        labels=GroupLabels,
                        values=LineTypeGroup
                        )+    
  guides(colour = guide_legend(LegendTitle), linetype = guide_legend(LegendTitle))+ # merge two legends into a single one
# maek background theme black and white                       
  theme_bw() +   
  theme(axis.title.x = element_blank(),# use element_text(size=) to define font size of x axis title
        axis.title.y = YAxisTitleSize, #font size of y axis title
        axis.text.x  = XAxisText,                              #font size of x axis text 
        axis.text.y  = element_text(size=AxisTextSize),                              #font size of y axis text  
        legend.position=LegendPosition,
        legend.title=element_text(size=LegendTitleSize),                                #font size of legend title
        legend.text = element_text(colour="black", size = LegendTextSize, face = "bold"), #font size of legend text
        legend.key.size=unit(LegendKeySize,'points'), ## ben - added to shrink the legend 
        legend.background=element_blank(), ## ben - added to get rid of white background
        panel.grid.major = element_line(size = 0.5, colour = '#FFFFFF'),
        panel.grid.minor = element_line(colour = NA), # colour = NA to suppress gridlines, reappear if colour='black'
        plot.title=element_text( face="bold", size=PlotTitleSize) # adjust plot title size
        )+
  ggtitle(Plottitle)
  
  # add label text
  #+ geom_text(aes(labelPosiX, labelPosiY, label="test"), colour="black",size=5)                                   }
                                         }
                             
                                        
  #not working when inside the function                       
  #geom_text(aes(labelPosiX, labelPosiY, label=labelText), colour="black",size=5)                         
                      
                    

