#' Parameter timeseries plot
#' 
#' Takes output data object from readNWISodbc and prints a plot of parameter vs time.
#' @param qw.data A qw.data object generated from readNWISodbc
#' @param new.threshold The threshold value in seconds from current system time for "new" data.
#' @param site.selection A character vector of site IDs to plot
#' @param plotparm A character vector of parameters to plot
#' @param begin.date Character string of begining date range to plot (yyyy-mm-dd)
#' @param end.date Character string of ending date range to plot (yyyy-mm-dd)
#' @param show.q Logical to plot instantaneous hydrograph
#' @param show.smooth Logical to add a loess smooth to plot
#' @param highlightrecords A character vector of record numbers to highlight in plot
#' @param wySymbol Make current water-year highlighted.
#' @param printPlot Logical. Prints plot to graphics device if TRUE

#' @export

qwtsPlot <- function(qw.data,
                     site.selection,
                     plotparm,
                     facet = "multisite",
                     new.threshold = 60*60*24*30,
                     show.q = FALSE,
                     show.smooth = FALSE,
                     highlightrecords = " ",
                     wySymbol = FALSE,
                     printPlot = TRUE){
  ## Sets color to medium code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
  medium.colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00","#D55E00")
  names(medium.colors) <- c("WS ","WG ","WSQ","WGQ","OAQ","OA ")
  ## Sets color to medium code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
  qual.shapes <- c(19,0,2,5,4,3,6,7,8,9,11)
  
  names(qual.shapes) <- c("Sample","<",">","E","A","M","N","R","S","U","V")

    plotdata <- subset(qw.data$PlotTable,SITE_NO %in% (site.selection) & 
                         PARM_CD == (plotparm))
    Qplotdata <- subset(qw.data$PlotTable,SITE_NO %in% (site.selection) & 
                          PARM_CD %in% c(plotparm,"00061"))
  
  if (length(site.selection) == 1)
  {
    maintitle <- str_wrap(unique(qw.data$PlotTable$STATION_NM[which(qw.data$PlotTable$SITE_NO == (site.selection))]), width = 25)
  } else (maintitle <- "Multisite plot")
  
  ylabel <- str_wrap(unique(plotdata$PARM_DS[which(plotdata$PARM_CD==(plotparm))]), width = 25)
  p1 <- ggplot(data=plotdata,aes(x=SAMPLE_START_DT,y=RESULT_VA,shape = REMARK_CD, color = MEDIUM_CD))
  p1 <- p1 + geom_point(size=3)
  if ( facet == "Facet")
  {
          p1 <- p1 + facet_wrap(~ STATION_NM, nrow = 1, scales="free_y")
  }
  
  p1 <- p1 + ylab(paste(ylabel,"\n")) + xlab("Date")
  p1 <- p1 + scale_colour_manual("Medium code",values = medium.colors)
  p1 <- p1 + scale_shape_manual("Remark code",values = qual.shapes)
  p1 <- p1 + theme_bw()
  p1 <- p1 + ggtitle(maintitle)
  
  #check for highlighted records and put different symbols
  if(
    nrow((data=subset(plotdata, RECORD_NO %in% highlightrecords))) > 0
  ){
    
    p1 <- p1 + geom_point(data=subset(plotdata, RECORD_NO %in% highlightrecords),aes(x=SAMPLE_START_DT,y=RESULT_VA),size = 7,alpha=0.5, color ="#D55E00" ,shape=19)
  }
  
  
  ##Check for new samples and label them. 
  if(nrow(subset(plotdata, RESULT_MD >= (Sys.time()-new.threshold))) > 0)
  {
    p1 <- p1 + geom_text(data=subset(plotdata, RESULT_MD >= (Sys.time()-new.threshold)),
                         aes(x=SAMPLE_START_DT,y=RESULT_VA,color = MEDIUM_CD,label="New",hjust=1.1),show_guide=F)      
  }else{}
  
  ##highlight this water year's data
  if(wySymbol == TRUE)
  {
          p1 <- p1 + geom_point(data=subset(plotdata, as.character(waterYear(SAMPLE_START_DT)) == as.character(waterYear(Sys.time()))),
                                aes(x=SAMPLE_START_DT,y=RESULT_VA),size=5,alpha = 0.5, color = "#F0E442",shape=19)
  }
  
  p1 <- p1 + theme(axis.text.x = element_text(angle = 90)) #+ ggtitle(maintitle)
  
  ###Add smooth if checked
  if((show.smooth)==TRUE){
    p1 <- p1 + geom_smooth(data = subset(plotdata, REMARK_CD=="Sample" & 
                                           MEDIUM_CD %in%(c("WS ","WG "))))
  } 
  
  
  p3 <- ggplot(data=subset(Qplotdata, MEDIUM_CD == "WS " & 
                                   REMARK_CD == "Sample"),aes(x=SAMPLE_START_DT,y=RESULT_VA,shape = REMARK_CD, color = MEDIUM_CD))
  p3 <- p3 + facet_grid(PARM_NM~.,scales="free_y") + geom_point(data=subset(Qplotdata,PARM_CD == plotparm)) + geom_line(data=subset(Qplotdata,PARM_CD == "00061" & MEDIUM_CD=="WS "))
  p3 <- p3 + theme_bw()
  p3 <- p3 + scale_colour_manual("Medium code",values = medium.colors)
  p3 <- p3 + scale_shape_manual("Remark code",values = qual.shapes)
  p3 <- p3 + ylab("Value, see individual parameters for units") + xlab("Date")
  p3 <- p3 + ggtitle(maintitle)
  ###Add Q if checked
  if((show.q)==TRUE){ 
          p2 <- ggplot(data=subset(Qplotdata, MEDIUM_CD == "WS " & 
                                           REMARK_CD == "Sample"),aes(x=SAMPLE_START_DT,y=RESULT_VA,shape = REMARK_CD, color = MEDIUM_CD))
          p2 <- p2 + facet_grid(PARM_NM~.,scales="free_y") + geom_point(data=subset(Qplotdata,PARM_CD == plotparm)) + geom_line(data=subset(Qplotdata,PARM_CD == "00061" & MEDIUM_CD=="WS "))
          p2 <- p2 + theme_bw()
          p2 <- p2 + scale_colour_manual("Medium code",values = medium.colors)
          p2 <- p2 + scale_shape_manual("Remark code",values = qual.shapes)
          p2 <- p2 + ylab("Value, see individual parameters for units") + xlab("Date")
          p2 <- p2 + ggtitle(maintitle)
    if((show.smooth)==TRUE){
      p2 <- p2 + geom_smooth(data=subset(Qplotdata,PARM_CD == plotparm & MEDIUM_CD %in%  MEDIUM_CD %in% c("WS ","WG ")))
    }
   
    
    if(printPlot == TRUE)
    {
      print(p2) 
    } else(return(p2))
    
  }
  if ((show.q)==FALSE)
        {
          if(printPlot == TRUE)
                  {
                  print(p1) 
                  } else(return(p1))
  }
   
}
