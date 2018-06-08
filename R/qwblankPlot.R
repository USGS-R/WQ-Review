#'Blank sample timeseries
#'
#' Takes output data object from readNWISodbc and prints a boxplot of blank results
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param plotparm A character vector of the parameter to plot.
#' @param site.selection A character vector of site IDs to plot
#' @param printPlot Logical. Prints plot to graphics device if TRUE
#' @examples 
#' data("exampleData",package="WQReview")
#' qwblankPlot(qw.data = qw.data,
#'                        site.selection = "All",
#'                        plotparm = "00915",
#'                        printPlot = TRUE
#'                        )
#' @import ggplot2
#' @importFrom stringr str_wrap
#' @export

qwblankPlot <- function(qw.data,
                        plotparm,
                        site.selection = "All",
                        printPlot = TRUE){
  
  dqi.colors <- c("#000000", "#E69F00")
  names(dqi.colors) <- c("DQI = R","DQI = I, S, or P")
  
  if(length(plotparm) > 1)
  {
    stop("Only one parmater can be supplied")
  }
  
  give.n <- function(x){
    return(c(y = median(x)*1.05, label = length(x))) 
    # experiment with the multiplier to find the perfect position
  }
  
  if(site.selection == "All")
  {
    plotData <- subset(qw.data$PlotTable,
                       PARM_CD==(plotparm) &
                         MEDIUM_CD %in% c("OAQ","OA"))
    
    
  }else {
    plotData <- subset(qw.data$PlotTable,SITE_NO %in% (site.selection) & 
                         PARM_CD==(plotparm) &
                         MEDIUM_CD %in% c("OAQ","OA"))
  }
  
  #plotData$status <- NA
  plotData$status[plotData$DQI_CD == "R"] <- "DQI = R"
  plotData$status[plotData$DQI_CD %in% c("I","S","P")] <- "DQI = I, S, or P"
  plotData <- plotData[plotData$REMARK_CD != "<",]
  
  p1 <- ggplot(data=plotData,aes(x=PARM_NM,y=RESULT_VA,color=status,group=status)) +
    geom_boxplot() + geom_point(size=3,position=position_dodge(width=0.75)) +
    ylab("Concentration") + xlab("Parameter") + ggtitle("Comparison of Blank Detections") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90),
                       panel.grid.minor = element_line())
  p1 <- p1 + stat_summary(fun.data = give.n, 
                          geom = "text", 
                          fun.y = median,
                          color="black",
                          position=position_dodge(width=0.75))
  p1 <- p1 + scale_color_manual("DQI Status",values = dqi.colors)
  
  if(nrow(plotData) == 0)
  {
    p1 <- ggplot() + 
      geom_text(aes(x=1,y=1,label="No detections")) + ggtitle("Comparison of Blank Detections") +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
  }
  
  if(printPlot)
  {
    print(p1)
  }else{}
  return(p1)
}
