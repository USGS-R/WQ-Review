#' Boxplot of replicate relative percent difference
#' 
#' Takes output data object from readNWISodbc and prints a boxplot of replicate RPD
#' @param reports A reports object generated from readNWISodbc
#' @param new.threshold The threshold value in seconds from current system time for "new" data.
#' @param site.selection A character vector of site IDs to plot
#' @param plotparm A character vector of pcodes to plot
#' @param show.points Show samples points
#' @param log.scale Plot y axis on a log scale
#' @param highlightrecords A character vector of record numbers to highlight in plot

#' @export

qwrepBoxPlot <- function(reports,
                    site.selection = "401723105400000",
                    plotparm = "00915",
                    new.threshold = 60*60*24*30,
                    show.points = FALSE,
                    highlightrecords = NULL,
                    new.reps = Sys.time() - 60*60*24*30){

  hline <- data.frame(yint=c(-5,5,-10,10),RPD=c("+/- 5%","+/- 5%","+/- 10%","+/- 10%"))
    
  plotdata <- subset(reports$repTable,PARM_CD%in%(plotparm) & SITE_NO %in% site.selection)
  plotdata$historic <- "Historic"
  plotdata$historic[which(plotdata$Env_SAMPLE_START_DT >= new.reps)] <- "New"
  
  if(length(site.selection) == 1)
  {
    maintitle <- str_wrap(paste(unique(plotdata$STATION_NM[which(plotdata$SITE_NO == (site.selection))]),
                                " Replicate agreement boxplot",
                                sep=""), width = 25)
  } else if (length(site.selection) > 1)
  {
    maintitle <- "Multisite replicate agreement boxplot"
  } else (maintitle <- "No site selected")
  
  ylabel <- "Relative percent difference"
  
  p1 <- ggplot(data=plotdata,aes(x=PARM_NM,y=relPercent_diff, color=historic))
  p1 <- p1 + geom_boxplot()
  #p1 <- p1 + scale_colour_manual("Medium code",values = medium.colors)
  #p1 <- p1 + scale_shape_manual("Remark code",values = qual.shapes)
  p1 <- p1 + ylab(paste(ylabel,"\n"))
  p1 <- p1 + scale_x_discrete("Analyte")
  p1 <- p1 + geom_hline(data = hline,aes(yintercept = yint,linetype=RPD),show_guide=TRUE) 
  p1 <- p1 + facet_wrap(~ PARM_NM, nrow = 1, scales="free") 
  p1 <- p1 + theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x=element_blank())
  p1 <- p1 + ggtitle(maintitle)
  

  if((show.points)==TRUE){
    p2 <- p1 + geom_point(aes(color=historic),size=3)
    
    ##Highlight records
    if(nrow(subset(reports$repTable,PARM_CD%in%(plotparm) &
                     c(Env_RECORD_NO,Rep_RECORD_NO) %in% highlightrecords)) > 0)
    {
      p2 <- p2 + geom_point(data=subset(reports$repTable,PARM_CD%in%(plotparm) &
                                          c(Env_RECORD_NO,Rep_RECORD_NO) %in% highlightrecords),aes(x=PARM_NM,y=relPercent_diff, color=historic),size=7,alpha=0.5)
    }else{}
    
    ###Label new data
    #if(nrow(subset(plotdata, RESULT_MD >= (Sys.time()-new.threshold))) > 0)
    #{
    #  p2 <- p2 + geom_text(data=subset(plotdata,RESULT_MD >= (Sys.time()-new.threshold)),
    #                       aes(x=SAMPLE_MONTH,y=RESULT_VA,color = MEDIUM_CD,label="New",hjust=1.1),show_guide=F)      
    #}else{}
    
 return(p2)
  }else( return(p1))
}