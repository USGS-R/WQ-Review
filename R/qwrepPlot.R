#' Boxplot of replicate relative percent difference
#' 
#' Takes output data object from readNWISodbc and prints a boxplot of replicate RPD
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param site.selection A character vector of site IDs to plot
#' @param plotparm A character vector of pcodes to plot
#' @param facet Character string of either "multisite" for plotting all sites on one plot or "Facet" for plotting sites on individual plots
#' @param scales Character string to define y axis on faceted plots. Options are "free","fixed","free_x", or "free_y"
#' @param printPlot Logical. Prints plot to graphics device if TRUE
#' @examples 
#' data("exampleData",package="WQReview")
#' qwrepPlot(qw.data = qw.data,
#'                        site.selection = "06733000",
#'                        plotparm = "00915",
#'                        printPlot = TRUE)
#' @import ggplot2
#' @importFrom stringr str_wrap
#' @export

qwrepPlot <- function(qw.data,
                      plotparm,
                      site.selection = "All",
                      facet = "multisite",
                      scales="fixed",
                      printPlot = TRUE){
  
  dqi.colors <- c("#000000", "#E69F00")
  names(dqi.colors) <- c("DQI = R","DQI = I, S, or P")
  
  if(length(plotparm) > 1)
  {
    stop("Only one parmater can be supplied")
  }
  give.n <- function(x){
    return(c(y = median(x)*1.10, label = length(x))) 
    # experiment with the multiplier to find the perfect position
  }
  
  hline <- data.frame(yint=c(-5,5,-10,10),RPD=c("+/- 5%","+/- 5%","+/- 10%","+/- 10%"))
  
  repTable <- repTabler(qw.data)
  if(site.selection == "All")
  {
    plotData <- subset(repTable,PARM_CD%in%(plotparm))
  } else {
    plotData <- subset(repTable,PARM_CD%in%(plotparm) & SITE_NO %in% site.selection)
  }
  
  ### issue is with this chunk, no status is returned, need to subset data similar to qwcbPlot and run repTabler if needed?
  plotData$status <- NA
  plotData$status[plotData$Env_DQI_CD == "R" & plotData$Rep_DQI_CD == "R" ] <- "DQI = R"
  plotData$status[plotData$Env_DQI_CD %in% c("I","S","P") |
                    plotData$Rep_DQI_CD %in% c("I","S","P")] <- "DQI = I, S, or P"
  
  p1 <- ggplot(data=plotData,aes(x=PARM_NM,y=relPercent_diff,color=status,group=status)) +
    geom_boxplot() + geom_point(size=3,position=position_dodge(width=0.75)) +
    ylab("Relative percent difference") + xlab("Parameter") + ggtitle("Replicate Agreement Boxplot") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90),
                       panel.grid.minor = element_line())
  p1 <- p1 + stat_summary(fun.data = give.n, 
                          geom = "text", 
                          fun.y = median,
                          color="black",
                          position=position_dodge(width=0.75))
  p1 <- p1 + geom_hline(data = hline,aes(yintercept = yint,linetype=RPD),show.legend=TRUE) 
  p1 <- p1 + scale_color_manual("DQI Status",values = dqi.colors)
  if ( facet == "Facet")
  {
          p1 <- p1 + facet_wrap(~ STATION_NM, nrow = 1, scales=scales) 
  } else{}
  
  
  if(printPlot)
  {
    print(p1)
  }else{return(p1)}
}

