#' Chargebalance timeseries plot
#' 
#' Takes output list from readNWISodbc and prints a plot of charge balance vs time. Requires charge balance = TRUE in NWISPullR
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param new.threshold The threshold value in seconds from current system time for "new" data.
#' @param site.selection A character vector of site IDs to plot
#' @param begin.date Character string of begining date range to plot (yyyy-mm-dd)
#' @param end.date Character string of ending date range to plot (yyyy-mm-dd)
#' @param show.smooth Add a loess smooth to plot
#' @param highlightrecords A character vector of record numbers to highlight in plot
#' @param wySymbol Make current water-year highlighted.
#' @param printPlot Logical. Prints plot to graphics device if TRUE
#' @examples 
#' data("exampleData",package="WQReview")
#' qwcbPlot <- function(qw.data = qw.data,
#'                        site.selection = "06733000",
#'                        facet = "multisite",
#'                        new.threshold = 60*60*24*30,
#"                        show.smooth = FALSE,
#'                        highlightrecords = NULL,
#'                        wySymbol = FALSE,
#'                        printPlot = TRUE)
#' @import stringr 
#' @import ggplot2
#' @import plyr
#' @export

qwcbPlot <- function(qw.data,
                    site.selection,
                    facet = "multisite",
                    new.threshold = 60*60*24*30,
                    show.smooth = FALSE,
                    highlightrecords = NULL,
                    wySymbol = FALSE,
                    printPlot = TRUE){
  
        ###Run ion balance function if not run already and join to qw.data$PlotTable
        if(is.null(qw.data$PlotTable$perc.diff))
        {
        tryCatch({       
        chargebalance.table <- ionBalance(qw.data = qw.data,wide=FALSE)

        ###Check that a balance was calculated
        ###Join charge balance table to plot table
                chargebalance.table <- chargebalance.table[c("RECORD_NO","sum_cat","sum_an","perc.diff","complete.chem")]
                qw.data$PlotTable <- join(qw.data$PlotTable,chargebalance.table[!duplicated(chargebalance.table$RECORD_NO), ],by="RECORD_NO")           
        }, warning = function(w) {
        }, error = function(e) {
                stop("Insufficient data to calculate charge balance. Check your qw.data$PlotTable data")
        })
        
        } else {}
        
  ## Sets color to medium code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
  medium.colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00","#D55E00")
  names(medium.colors) <- c("WS ","WG ","WSQ","WGQ","OAQ","OA ")
  
  ## Sets shape to Remark code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
  qual.shapes <- c(19,0,2,5)
  names(qual.shapes) <- c("Complete","Incomplete")
  
  plotdata <- subset(qw.data$PlotTable,SITE_NO %in% (site.selection) & 
                                    !duplicated(RECORD_NO) == TRUE
                                    )
  
  ###Set the modified date to most recent modification for that record
  
  ###setup the hline from charge balance
  hline <- data.frame(yint=c(-5,5,-10,10),Imbalance=c("+/- 5%","+/- 5%","+/- 10%","+/- 10%"))
  if(length(site.selection) == 1)
  {
    maintitle <- str_wrap(unique(plotdata$STATION_NM[which(plotdata$SITE_NO == (site.selection))]), width = 25)
  } else if (length(site.selection) > 1)
  {
    maintitle <- "Multisite charge balance plot"
  } else (maintitle <- "No site selected")
  
  ylabel <- "Charge balance Percent difference\n( [sum(cat)-sum(an)]/[tot. charge] * 100 )"
  p1 <- ggplot(data=plotdata,aes(x=SAMPLE_START_DT,y=perc.diff,shape = complete.chem, color = MEDIUM_CD))
  p1 <- p1 + geom_point(size=3)
  p1 <- p1 + ylab(paste(ylabel,"\n")) + xlab("Date")
  p1 <- p1 + scale_colour_manual("Medium code",values = medium.colors)
  p1 <- p1 + scale_shape_manual("Chemistry status",values = qual.shapes)
  p1 <- p1 + scale_y_continuous(limits=c(-100,100))
  
  if(facet == "Facet"){
          p1 <- p1 + facet_wrap(~ STATION_NM, nrow = 1, scales="free_y")
  } else {}
  
  #p1 <- p1 + scale_x_datetime(limits=c(as.POSIXct((begin.date)),as.POSIXct((end.date))))
  ##Highlighted records labels
  if(nrow(subset(plotdata, RECORD_NO %in% highlightrecords)) >0 )
  {
    p1 <- p1 + geom_point(data=subset(plotdata, RECORD_NO %in% highlightrecords),aes(x=SAMPLE_START_DT,y=perc.diff),size=7,alpha = 0.5, color = "#D55E00",shape=19)
  }
  
  
  
  ###New sample labels


  if(nrow(subset(plotdata, RESULT_MD >= (Sys.time()-new.threshold))) > 0)
  {
          if(all(is.finite(plotdata$perc.diff[which(plotdata$SAMPLE_MD >= (Sys.time()-new.threshold))])) &
             
             nrow(subset(plotdata, SAMPLE_MD >= (Sys.time()-new.threshold))) > 0)
                  
                  
                  
          {
                  p1 <- p1 + geom_text(data=subset(plotdata,RESULT_MD >= (Sys.time()-new.threshold)),
                                       aes(x=SAMPLE_START_DT,y=perc.diff,color = MEDIUM_CD,label="New",hjust=1.1),show_guide=F)      
          }else{}
  } else{}
  
  ##highlight this water year's data
  if(wySymbol == TRUE) 
  {
          p1 <- p1 + geom_point(data=subset(plotdata, as.character(waterYear(SAMPLE_START_DT)) == as.character(waterYear(Sys.time()))),
                                aes(x=SAMPLE_START_DT,y=perc.diff),size=5,alpha = 0.5, color ="#F0E442" ,shape=19)
  }
  
  p1 <- p1 + ggtitle(maintitle)
  p1 <- p1 + geom_hline(data = hline,aes(yintercept = yint,linetype=Imbalance),show_guide=TRUE) 
  p1 <- p1 + theme_bw()
  
  if((show.smooth)==TRUE){
    p2 <- p1 + geom_smooth(data=subset(plotdata, MEDIUM_CD %in% (c("WS ","WG ")))) + theme_bw()
    if(printPlot == TRUE)
    {
    print(p2)
    } else{}
    return(p2)
  } else{
    if(printPlot == TRUE)
      {
      print(p1)
      }else {}
         return(p1)}
  
}