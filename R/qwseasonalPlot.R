#' Seasonal plot
#' 
#' Takes output data object from readNWISodbc and prints a boxplot of a parameter by month.
#' @param qw.data A qw.data object generated from readNWISodbc
#' @param new.threshold The threshold value in seconds from current system time for "new" data.
#' @param site.selection A character vector of site IDs to plot
#' @param plotparm A character vector of parameters to plot
#' @param show.points A logical to show points on plot
#' @param log.scale A logical to plot y axis on log scale
#' @param highlightrecords A character vector of record numbers to highlight in plot

#' @export

qwseasonalPlot <- function(qw.data,
                           new.threshold,
                           site.selection,
                           plotparm,
                           facet = "multisite",
                           begin.date = NULL,
                           end.date = NULL,
                           show.q = FALSE,
                           show.smooth = FALSE,
                           highlightrecords = " ",
                           print = TRUE){
  
  
  
  plotdata <- subset(qw.data$PlotTable,SITE_NO %in% (site.selection) & PARM_CD==(plotparm))
  
  if(length(site.selection) == 1)
  {
    maintitle <- str_wrap(unique(plotdata$STATION_NM[which(plotdata$SITE_NO == (site.selection))]), width = 25)
  } else if (length(site.selection) > 1)
  {
    maintitle <- "Multisite boxplot"
  } else (maintitle <- "No site selected")
  
  ylabel <- str_wrap(unique(qw.data$PlotTable$PARM_DS[which(qw.data$PlotTable$PARM_CD==(plotparm))]), width = 25)
  p1 <- ggplot(data=plotdata,aes(x=DOY,y=RESULT_VA, color=MEDIUM_CD))
  p1 <- p1 + geom_point()
  
  if ( facet == "Facet")
  {
          p1 <- p1 + facet_wrap(~ STATION_NM, nrow = 1, scales="free_y")
  }else{}
  
  #p1 <- p1 + scale_x_discrete("Month", breaks=levels(qw.data$PlotTable$SAMPLE_MONTH), drop=FALSE)
  p1 <- p1 + scale_color_manual("Medium code",values = medium.colors)
  p1 <- p1 + scale_shape_manual("Remark code",values = qual.shapes)
  p1 <- p1 + ylab(paste(ylabel,"\n"))
  p1 <- p1 + ggtitle(maintitle)
  
  if(nrow(subset(plotdata, RECORD_NO %in% highlightrecords)) >0 )
  {
          p1 <- p1 + geom_point(data=subset(plotdata, RECORD_NO %in% highlightrecords),aes(x=DOY,y=RESULT_VA),size=7,alpha = 0.5)
  }
  
  if(nrow(subset(plotdata, SAMPLE_MD >= (Sys.time()-new.threshold))) > 0)
  {
          if(all(is.finite(plotdata$perc.diff[which(plotdata$SAMPLE_MD >= (Sys.time()-new.threshold))])) &
                     
                     nrow(subset(plotdata, SAMPLE_MD >= (Sys.time()-new.threshold))) > 0)
                  
                  
                  
          {
                  p1 <- p1 + geom_text(data=subset(plotdata,SAMPLE_MD >= (Sys.time()-new.threshold)),
                                       aes(x=DOY,y=RESULT_VA,color = MEDIUM_CD,label="New",hjust=1.1),show_guide=F)      
          }else{}
  } else{}
  if(print==TRUE)
  {
          print(p1)
  }else {return(p1)}
  
}