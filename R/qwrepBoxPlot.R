#' Boxplot of replicate relative percent difference
#' 
#' Takes output data object from readNWISodbc and prints a boxplot of replicate RPD
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param new.threshold The threshold value in seconds from current system time for "new" data.
#' @param site.selection A character vector of site IDs to plot
#' @param plotparm A character vector of pcodes to plot
#' @param show.points Show samples points
#' @param highlightrecords A character vector of record numbers to highlight in plot
#' @param new.reps The threshold value in seconds from current system time for "new" replicate samples.
#' @param printPlot Logical. Prints plot to graphics device if TRUE
#' @examples 
#' data("exampleData",package="WQReview")
#' qwrepBoxPlot(reports = reports,
#'                        site.selection = "06733000",
#'                        plotparm = "00915",
#'                        new.threshold = 60*60*24*30,
#'                        show.points = FALSE,
#'                        highlightrecords = NULL,
#'                        new.reps = Sys.time() - 60*60*24*30,
#'                        printPlot = TRUE)
#' @import ggplot2
#' @importFrom stringr str_wrap
#' @export

qwrepBoxPlot <- function(qw.data,
                    site.selection,
                    plotparm,
                    new.threshold = 60*60*24*30,
                    show.points = FALSE,
                    highlightrecords = NULL,
                    new.reps = Sys.time() - 60*60*24*30,
                    printPlot = TRUE){
        
        give.n <- function(x){
                return(c(y = median(x)*1.05, label = length(x))) 
                # experiment with the multiplier to find the perfect position
        }

  hline <- data.frame(yint=c(-5,5,-10,10),RPD=c("+/- 5%","+/- 5%","+/- 10%","+/- 10%"))
    
  plotdata <- subset(qw.data$reports$repTable,PARM_CD%in%(plotparm) & SITE_NO %in% site.selection)
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
  p1 <- p1 + stat_summary(fun.data = give.n, geom = "text", fun.y = median,color="black")
  #p1 <- p1 + scale_colour_manual("Medium code",values = medium.colors)
  #p1 <- p1 + scale_shape_manual("Remark code",values = qual.shapes)
  p1 <- p1 + ylab(paste(ylabel,"\n"))
  p1 <- p1 + scale_x_discrete("Analyte")
  p1 <- p1 + geom_hline(data = hline,aes(yintercept = yint,linetype=RPD),show_guide=TRUE) 
  p1 <- p1 + facet_wrap(~ PARM_NM, nrow = 1, scales="free") 
  p1 <- p1 + theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x=element_blank())
  p1 <- p1 + ggtitle(maintitle) + theme_bw()
  

  if((show.points)==TRUE){
    p2 <- p1 + geom_point(aes(color=historic),size=3)
    
    ##Highlight records
    if(nrow(subset(plotdata,PARM_CD%in%(plotparm) &
                     c(Env_RECORD_NO,Rep_RECORD_NO) %in% highlightrecords)) > 0)
    {
      p2 <- p2 + geom_point(data=subset(plotdata,PARM_CD%in%(plotparm) &
                                          c(Env_RECORD_NO,Rep_RECORD_NO) %in% highlightrecords),aes(x=PARM_NM,y=relPercent_diff),size=7,alpha=0.5, color = "#F0E442",shape=19)
    }else{}
    
    ###Label new data
    if(nrow(subset(plotdata,PARM_CD%in%(plotparm) &
                   Env_SAMPLE_START_DT >= (Sys.time()-new.threshold))) > 0)
    {
      p2 <- p2 + geom_text(data=subset(plotdata,PARM_CD %in% (plotparm) &
                                               Env_SAMPLE_START_DT >= (Sys.time()-new.threshold)),
                           aes(x=PARM_NM,y=relPercent_diff,color=historic,label="New",hjust=1.1),show_guide=F)      
    }else{}
    
    if(printPlot)
    {print (p2)}
    else{return(p2)}

    
  }else{
          if(printPlot)
                  {
                  print(p1)
          }else{return(p1)}
  }

}