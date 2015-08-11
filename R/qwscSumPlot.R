#' Sum ions vs conductance plot
#' 
#' Takes output data object from readNWISodbc and prints a plot of sum ions vs. conductance. Requires charge balance = TRUE in NWISPullR
#' @param qw.data A qw.data object generated from readNWISodbc
#' @param new.threshold The threshold value in seconds from current system time for "new" data.
#' @param site.selection A character vector of site IDs to plot
#' @param plotparm A character string of ions to plot. Options are "Cations", "Anions", "Both"
#' @param highlightrecords A character vector of record numbers to highlight in plot
#' @param printPlot Logical. Prints plot to graphics device if TRUE
#' @import stringr 
#' @import ggplot2
#' @import grid
#' @import gridExtra 
#' @import plyr
#' @export

qwscSumPlot <- function(qw.data,
                       site.selection,
                       facet = "multisite",
                       new.threshold = 60*60*24*30,
                       highlightrecords = NULL,
                       printPlot = TRUE
                       ) {
  
        ###Run ion balance function if not run already and join to qw.data$PlotTable
        #if(is.null(qw.data$PlotTable$perc.diff))
        #{
        #        tryCatch({       
        #                chargebalance.table <- ionBalance(qw.data = qw.data,wide=FALSE)
        #                
        #                ###Check that a balance was calculated
        #                ###Join charge balance table to plot table
        #                chargebalance.table <- chargebalance.table[c("RECORD_NO","sum_cat","sum_an","perc.diff","complete.chem")]
        #                qw.data$PlotTable <- join(qw.data$PlotTable,chargebalance.table[!duplicated(chargebalance.table$RECORD_NO), ],by="RECORD_NO")           
        #        }, warning = function(w) {
        #        }, error = function(e) {
        #                stop("Insufficient data to calculate charge balance. Check your qw.data$PlotTable data")
        #        })
        #        
        #} else {}
        
  ###Subset to plot data
  plotdata <- subset(qw.data$PlotTable,SITE_NO %in% site.selection & PARM_CD== "00095")
  plotdata <- melt(plotdata[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                   id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff"))
  ##New data subset for new modified samples. Used for labelling points as "NEW"
  newflagdata <- na.omit(subset(plotdata,SAMPLE_MD >= (Sys.time()-new.threshold)))
  
  ## Sets color to medium code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
  medium.colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00")
  names(medium.colors) <- c("WS ","WG ","WSQ","WGQ","OAQ")
  
  ## Sets color to medium code name, not factor level, so its consistant between all plots regardles of number of medium codes in data
  qual.shapes <- c(19,0,2,5)
  names(qual.shapes) <- c("Complete","Incomplete")
  
  
  if(length(site.selection) == 1)
  {
    maintitle <- str_wrap(unique(plotdata$STATION_NM[which(plotdata$SITE_NO == (site.selection))]), width = 25)
  } else if (length(site.selection) > 1)
  {
    maintitle <- "Multisite plot"
  } else (maintitle <- "No site selected")
    
    p1 <- ggplot(plotdata)
      
    
      ylabel <- "mEQ/l"
      #p1 <- p1 + geom_point(aes(x=RESULT_VA,y=sum_cat,shape = complete.chem, color = "Cations"),size=3)
      
      p1 <- p1 + geom_point(aes(x=RESULT_VA,y=value,shape = complete.chem, color = variable),size=3)
      
      ###Highlight records
      if(nrow(subset(plotdata, RECORD_NO %in% highlightrecords)) > 0)
      {
        p1 <- p1 + geom_point(data=subset(plotdata, RECORD_NO %in% highlightrecords),aes(x=RESULT_VA,y=value,shape = complete.chem,color = variable),size=7,alpha=0.5)
        #p1 <- p1 + geom_point(data=subset(plotdata, RECORD_NO %in% highlightrecords),aes(x=RESULT_VA,y=sum_an,shape = complete.chem,color = "Anions"),size=7,alpha=0.5)
      }
      
      ###Check for labels that have all NAs, it will crash plot
      if(nrow(newflagdata) > 0 & all(!is.na(newflagdata[c("value")])))
      {
        p1 <- p1 + geom_text(data=newflagdata,
                             aes(x=RESULT_VA,y=value,color=variable,label="New",hjust=1.1),show_guide=F)      
        #p1 <- p1 + geom_text(data=newflagdata,
         #                    aes(x=RESULT_VA,y=sum_an,label="New",hjust=1.1),show_guide=F)      
      }else{}
      
      
    p1 <- p1 + ylab(paste(ylabel,"\n")) + xlab("Specific conducatance")
    p1 <- p1 + labs(color='Sum ions')
    p1 <- p1 + scale_shape_manual("Chemistry status",values = qual.shapes)
  if ( facet == "Facet")
  {
          p1 <- p1 + facet_wrap(~ STATION_NM, nrow = 1, scales="free")
  } else {}
    ###Line for sum/conductunce ratio acceptable bounds
    #p1 <- p1 + geom_abline(aes(slope = 0.0092, intercept=0),linetype="dashed",show_guide=TRUE) 
    #p1 <- p1 + geom_abline(aes(slope = 0.0124, intercept=0),linetype="dashed" ,show_guide=TRUE) 
  p1 <- p1+geom_ribbon(data = plotdata,
                       aes(x=RESULT_VA,ymin=0.0092*RESULT_VA,ymax=0.0124*RESULT_VA,fill="gray",inherit.aes=F),
                       alpha=0.5,show_guide=TRUE)
  p1 <- p1 + scale_fill_manual(name = "Acceptable sum/Sc range",values="gray",labels="")
    ###Remove lines from symbol and color legends
    p1 <- p1 + guides(shape = guide_legend(override.aes = list(fill = NA))) + guides(color = guide_legend(override.aes = list(fill = NA)))
    #p1 <- p1 + labs(linetype="Acceptable sum(ion)/Sc range")
    
    
    p1 <- p1 + ggtitle(maintitle)
    
  if(printPlot == TRUE)
  {
    print(p1)
  } else{}
    return(p1)
}


