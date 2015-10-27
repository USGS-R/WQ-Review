#' Parameter-parameter plot
#' 
#' Takes output data object from readNWISodbc and returns a plot of parameter vs parameter.
#' @param qw.data A qw.data object generated from readNWISodbc
#' @param new.threshold The threshold value in seconds from current system time for "new" data.
#' @param site.selection A character vector of site IDs to plot
#' @param xparm Character string of parameter to plot on x axis
#' @param yparm Character string of parameter to plot on y axis
#' @param show.lm Add a linear fit to plot
#' @param highlightrecords A character vector of record numbers to highlight in plot
#' @param wySymbol Make current water-year highlighted.
#' @param printPlot Logical. Prints plot to graphics device if TRUE
#' @examples 
#' data("exampleData",package="WQReview")
#' qwparmParmPlot(qw.data = qw.data,
#'               site.selection = "06733000",
#'               xparm = "00915",
#'               yparm = "00061",
#'               facet = "multisite",
#'               new.threshold = 60*60*24*30,
#'               show.lm=FALSE,
#'               log.scaleY = FALSE,
#'               log.scaleX = FALSE,
#'               highlightrecords = NULL,
#'               wySymbol = FALSE,
#'               printPlot = TRUE)
#' @export


qwparmParmPlot <- function(qw.data,
                      site.selection,
                      xparm,
                      yparm,
                      facet = "multisite",
                      new.threshold = 60*60*24*30,
                      show.lm = FALSE,
                      log.scaleY = FALSE,
                      log.scaleX = FALSE,
                      highlightrecords = " ",
                      wySymbol = FALSE,
                      printPlot=TRUE){
  
  lm_eqn = function(df){
    m = lm(RESULT_VA_Y ~ RESULT_VA_X, df);
    eq <- paste("y =",format(coef(m)[2], digits = 2),"x +",format(coef(m)[1], digits = 2),"\nr-squared =",format(summary(m)$r.squared, digits = 3))
    
  }
  
  ## Sets color to medium code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
  medium.colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00")
  names(medium.colors) <- c("WS ","WG ","WSQ","WGQ","OAQ")
  if(length(site.selection) == 1)
  {
    maintitle <- str_wrap(unique(qw.data$PlotTable$STATION_NM[which(qw.data$PlotTable$SITE_NO == (site.selection))]), width = 25)
  } else if (length(site.selection) > 1)
  {
    maintitle <- "Multisite plot"
  } else (maintitle <- "No site selected")
  
  ###Get X and Y labels from NWIS parm name
  xlabel <- str_wrap(unique(qw.data$PlotTable$PARM_DS[which(qw.data$PlotTable$PARM_CD==(xparm))]), width = 25)
  ylabel <- str_wrap(unique(qw.data$PlotTable$PARM_DS[which(qw.data$PlotTable$PARM_CD==(yparm))]), width = 25)
  ###Subset data to parms and join by record number
  ###This is very ugly but I don't know a way to pair up the x-y data in a melted dataframe
  ###Subsetting by parm code does not work because you need the parmcodes matched up for the same record
  
  xpp.plot.data <- subset(qw.data$PlotTable,SITE_NO %in% (site.selection) & PARM_CD==(xparm))
  ypp.plot.data <- subset(qw.data$PlotTable,SITE_NO %in% (site.selection) & PARM_CD==(yparm))
  
  ###Assigned to global environment to make it work with ggplot2, I don't like doing this since it is not a persistant variable
  ###but this is hte fastest fix for now
  pp.plot.data <- join(xpp.plot.data[,c("RECORD_NO","SITE_NO","STATION_NM","MEDIUM_CD","SAMPLE_START_DT","RESULT_VA","RESULT_MD")], 
                        ypp.plot.data[,c("RECORD_NO","RESULT_VA","RESULT_MD")],by="RECORD_NO")
  names(pp.plot.data) <- c("RECORD_NO","SITE_NO","STATION_NM","MEDIUM_CD","SAMPLE_START_DT","RESULT_VA_X","RESULT_MD_X","RESULT_VA_Y","RESULT_MD_Y")
  remove(xpp.plot.data)
  remove(ypp.plot.data)
  
  pp.plot.data <- na.omit(pp.plot.data)
  ###Make the plot
  p1 <- ggplot(data=pp.plot.data)
  p1 <- p1 + geom_point(aes(x=RESULT_VA_X,y=RESULT_VA_Y,color = MEDIUM_CD),size=3)
  p1 <- p1 + ylab(paste(ylabel,"\n")) + xlab(paste("\n",xlabel))
  #Highlight records
  if(nrow(subset(pp.plot.data,RECORD_NO %in% highlightrecords)) > 0)
  {
          p1 <- p1 + geom_point(data=subset(pp.plot.data,RECORD_NO %in% highlightrecords),aes(x=RESULT_VA_X,y=RESULT_VA_Y),size=7,alpha=0.5, color = "#D55E00",shape=19)
  } else{}
  
  p1 <- p1 + scale_colour_manual("Medium code",values = medium.colors)
  if ( facet == "Facet")
  {
  p1 <- p1 + facet_wrap(~ STATION_NM, nrow = 1, scales="free") 
  }else{}
  #p1 <- p1 + stat_ellipse(aes(x=RESULT_VA_X,y=RESULT_VA_Y),level=0.999,type="t")
  if(log.scaleY == TRUE)
  {
    p1 <- p1 + scale_y_log10()
  }
  if(log.scaleX == TRUE)
  {
    p1 <- p1 + scale_x_log10()
  }

  ##Check for new samples and label them. Tried ifelse statement for hte label but it did no recognize new.threshol as a variable for some reason
  if(nrow(subset(pp.plot.data, RESULT_MD_X >= (Sys.time()-new.threshold) | RESULT_MD_Y >= (Sys.time()-new.threshold))) > 0)
  {
    p1 <- p1 + geom_text(data=subset(pp.plot.data, RESULT_MD_X >= (Sys.time()-new.threshold) | RESULT_MD_Y >= (Sys.time()-new.threshold)),
                         aes(x=RESULT_VA_X,y=RESULT_VA_Y,color = MEDIUM_CD,label="New",hjust=1.1),show_guide=F)      
  }else{}
  
  ##highlight this water year's data
  if(wySymbol == TRUE) 
  {
          p1 <- p1 + geom_point(data=subset(pp.plot.data, as.character(waterYear(SAMPLE_START_DT)) == as.character(waterYear(Sys.time()))),
                                aes(x=RESULT_VA_X,y=RESULT_VA_Y),size=7,alpha = 0.5, color = "#F0E442",shape=19)
  }
  
  p1 <- p1 + ggtitle(maintitle) + theme_bw()
  
  
  
  if((show.lm)==TRUE){
    p2 <- p1 + geom_smooth(data=subset(pp.plot.data, MEDIUM_CD %in%c("WS ","WG ")),aes(x=RESULT_VA_X,y=RESULT_VA_Y),method="lm",formula=y~x)
    p2 <- p2 + xlab(paste("\n",xlabel,"\n",lm_eqn(subset(pp.plot.data, MEDIUM_CD %in%c("WS ","WG ")))))
    
    if(printPlot == TRUE)
    {
            print(p2)
    }else{return(p2)}
    
    } else{if(printPlot == TRUE)
        {
        print(p1)
        }else{return(p1)}}  
}