#--------------------------------------
# file name: Rfunction_plotOutput.R
# purpose: save a single plot as a tiff 
# date updated: 28-Jul-13
# status: good
#-------------------------------------------------------------
#-- setup basic information for the plots --#
onePlotTiff <- function(plotId="Fig03e",xlabel="Age of adult in days",ylabel="Cumulative rate of orientation",
                    fileName="C:/Now/(Project) RFID_r2 weighing_r1r2(active use)/rfidR1R2R3_02_plotFolder/ch03_fig02_cumRateOri_E.tiff",
                    height=3.5, width=3.5,xLabelSize=15, yLabelSize=15)          
        { xlabel <- xlabel #x axis title
          ylabel <- ylabel #y axis title

          cols <- 1
          rows <- 1
          plotlist <- list(plotId)
          
          tiff(fileName,height=height*ppi, width=width*ppi,res=ppi) #size of pdf: 9 inch square, ggsave() only saves a single plot so use pdf() here
          #-- setup the grid page to hold the graphs --#
            library(gridExtra)   
            grid.newpage()
            plotCols = cols  # Number of columns of plots
            plotRows = rows  # Number of rows needed, calculated from # of cols
            
            vp_heights <- c(0.5, rep(5,rows), 0.5 ) #rep(5,rows): replicate the value (5 here) 3 times (rows=3)
            vp_widths  <- c(0.5, rep(5,cols), 0.5 )
            pushViewport( viewport( layout = grid.layout(rows+2,#grid.layout() sets up a regular grid of viewports
                                                         cols+2, 
                                                         heights= unit(vp_heights, "null"),
                                                         widths = unit(vp_widths , "null"))))
            vplayout <- function(x, y)
              viewport(layout.pos.row = x, layout.pos.col = y)
          #--- add in graphs ---#
            for (i in 1:length(plotlist)) {
              curRow = ceiling(i/plotCols) + 1
              curCol = (i-1) %% plotCols + 2
              print(plotlist[[i]], vp = vplayout(curRow, curCol ))
                                            }
          #--- add y-label and x-label  ---#
            grid.text(xlabel, gp=gpar(fontsize=xLabelSize), vjust=0.2, vp = viewport(layout.pos.row = rows+2, 
                                                                  layout.pos.col = 1:(cols+2)), check.overlap=F )
            grid.text(ylabel, gp=gpar(fontsize=yLabelSize), vjust=0.6, vp = viewport(layout.pos.row = 1:(rows+2), 
                                                                  layout.pos.col = 1 ), rot=90, check.overlap=F )
          dev.off()
              }
