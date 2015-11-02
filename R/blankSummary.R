#' blankSummary Generate a summary table of blank analyses
#' 
#' Generates a summary table of blank analyses for all analytes in a qw.data list from readNWISodbc.
#' @param qw.data a qw.data list generated from readNWISodbc
#' @param STAIDS A character vector of station IDs to summarize.
#' @param begin.date Character string (yyyy-mm-dd) of beggining date to subset blank analyses. Leave blank for all analyses.
#' @param end.date Character string (yyyy-mm-dd) of ending date to subset blank analyses. Leave blank for all analyses.
#' @param multiple.levels Logical to analyze multiple reporting levels separetly. Default is TRUE.
#' @examples 
#' data("exampleData",package="WQReview")
#' blankSummaryOut <- blankSummary(qw.data = qw.data,
#'              STAIDS = unique(qw.data$PlotTable$SITE_NO),
#'              begin.date = "2009-01-01",
#'              end.date = "2013-01-01",
#'              multiple.levels = FALSE)
#' @importFrom plyr join
#' @importFrom plyr ddply
#' @importFrom plyr summarize
#' @export
#' 

blankSummary <- function(qw.data,STAIDS,begin.date = NA,end.date=NA,multiple.levels = FALSE)
{

### Various if statements to check for date range subsetting
  
  if(is.na(begin.date) & is.na(end.date))
  {
    blanks <- subset(qw.data$PlotTable, MEDIUM_CD == "OAQ" & 
                             SITE_NO %in% STAIDS &
                       PARM_SEQ_GRP_CD !="INF" &
                       PARM_SEQ_GRP_CD != "PHY")
} else if (is.na(begin.date) & !is.na(end.date))
{
  blanks <- subset(qw.data$PlotTable, MEDIUM_CD == "OAQ" &
                           SITE_NO %in% STAIDS &
                     SAMPLE_START_DT <= as.POSIXct(end.date) &
                     PARM_SEQ_GRP_CD !="INF" &
                     PARM_SEQ_GRP_CD != "PHY")
} else if (!is.na(begin.date) & is.na(end.date))
{
  blanks <- subset(qw.data$PlotTable, MEDIUM_CD = "OAQ" & 
                           SITE_NO %in% STAIDS &
                     SAMPLE_START_DT >= as.POSIXct(begin.date) &
                     PARM_SEQ_GRP_CD !="INF" &
                     PARM_SEQ_GRP_CD != "PHY")
} else 
{
  blanks <- subset(qw.data$PlotTable, MEDIUM_CD == "OAQ" & 
                           SITE_NO %in% STAIDS &
                     SAMPLE_START_DT >= as.POSIXct(begin.date) &
                     SAMPLE_START_DT <= as.POSIXct(end.date) &
                     PARM_SEQ_GRP_CD !="INF" &
                     PARM_SEQ_GRP_CD != "PHY")
}
#blankDetects <- subset(blanks,REMARK_CD != "<")
#blankDetects$flags <- ""
#blankDetects$flags[which(blankDetects$RESULT_VA >= 10*as.numeric(blankDetects$RPT_LEV_VA))] <- "Detection >= 10xLRL"

#Check that blanks exist
if(nrow(blanks) > 0)
{
  
#get rid of NAs in reporting level. NAs cause ddploy to fail because it is a grouping variable  
blanks$RPT_LEV_VA[which(is.na(blanks$RPT_LEV_VA))] <- "unknown"
  
###Summarize blanks for multiple reporting levels

if(multiple.levels)
{
  
  ###Make blank summary dataframe using pcodes
  blanksummary <- data.frame(PARM_CD = unique(blanks$PARM_CD))
  blanksummary <- join(blanksummary, blanks[c("PARM_CD","PARM_SEQ_GRP_CD","PARM_DS","RPT_LEV_VA")],by="PARM_CD")
  blanksummary <- unique(blanksummary)
  
summary <- ddply(blanks, c("PARM_CD","RPT_LEV_VA"), function(x) {
			num_blanks <- length(x$RESULT_VA)
			num_detects <- length(x$RESULT_VA[which(x$REMARK_CD == "Sample")])
      
      ###Change how the estimated code is displayed or provide better documentation
			num_e_detects <- length(x$RESULT_VA[which(x$REMARK_CD == "E")])
			
      num_detects_10xRL <- length(x$RESULT_VA[which(x$RESULT_VA >= 10*as.numeric(x$RPT_LEV_VA) & x$REMARK_CD == "Sample")])
			median_detected_value <- median(x$RESULT_VA[which(x$REMARK_CD == "Sample")])
			max_detected_value <- max(x$RESULT_VA[which(x$REMARK_CD == "Sample")],na.rm=TRUE)
			
      ##Calculate BD9090 for detections
      #if(length(x$RESULT_VA[which(x$REMARK_CD == "Sample")]) > 1)
			#{
      #sorteddetects <- sort(x$RESULT_VA[which(x$REMARK_CD == "Sample")]) 
      #BD90.90 <- sorteddetects[qbinom(0.9,length(sorteddetects),0.9)]
			#} else{BD90.90 <- NA}
      
      ##make a date frame out of the summary vectors
      data.frame(num_blanks = num_blanks,
                 num_detects=num_detects,
                 num_e_detects=num_e_detects,
                 num_detects_10xRL=num_detects_10xRL,
                 median_detected_value=median_detected_value,
                 max_detected_value=max_detected_value
                 #if(length(BD90.90 != 0))
                 #{
                 #BD90.90 = BD90.90
                 #} else (BD90.90 = NA)
                )

			})
        
##Join summary to pcode data

blanksummary <- join(blanksummary,summary,by=c("PARM_CD","RPT_LEV_VA"))
} else if (!multiple.levels)
{
  ###Make blank summary dataframe using pcodes. Omitting RPT_LEVEL_VA so its a unique by pcode
  blanksummary <- data.frame(PARM_CD = unique(blanks$PARM_CD))
  blanksummary <- join(blanksummary, blanks[c("PARM_CD","PARM_SEQ_GRP_CD","PARM_DS")],by="PARM_CD")
  blanksummary <- unique(blanksummary)
  
  ##Add in max and min reporting levels
  maxRL <- aggregate(blanks[c("PARM_CD","RPT_LEV_VA")],by=list(blanks$PARM_CD),FUN="max")
  minRL <- aggregate(blanks[c("PARM_CD","RPT_LEV_VA")],by=list(blanks$PARM_CD),FUN="min")
  names(maxRL) <- c("group","PARM_CD","MAX_RPT_LEV_VA")
  names(minRL) <- c("group","PARM_CD","MIN_RPT_LEV_VA")
  
  
  blanksummary <- join(blanksummary,maxRL[c("PARM_CD","MAX_RPT_LEV_VA")], by = "PARM_CD")
  blanksummary <- join(blanksummary,minRL[c("PARM_CD","MIN_RPT_LEV_VA")], by = "PARM_CD")
  
  
  summary <- ddply(blanks, c("PARM_CD"), function(x) {
    num_blanks <- length(x$RESULT_VA)
    num_detects <- length(x$RESULT_VA[which(x$REMARK_CD == "Sample")])
    num_e_detects <- length(x$RESULT_VA[which(x$REMARK_CD == "E")])
    num_detects_10xmaxRL <- length(x$RESULT_VA[which(x$RESULT_VA >= 10*as.numeric(max(x$RPT_LEV_VA)) & x$REMARK_CD == "Sample")])
    median_detected_value <- median(x$RESULT_VA[which(x$REMARK_CD == "Sample")])
    max_detected_value <- max(x$RESULT_VA[which(x$REMARK_CD == "Sample")],na.rm=TRUE)
    
    ##Calculate BD9090 for detections. DISABLED
   # if(length(x$RESULT_VA[which(x$REMARK_CD == "Sample")]) > 1)
  #  {
   #   sorteddetects <- sort(x$RESULT_VA[which(x$REMARK_CD == "Sample")]) 
  #    BD90.90 <- sorteddetects[qbinom(0.9,length(sorteddetects),0.9)]
  #  } else{BD90.90 <- NA}
    
    ##make a date frame out of the summary vectors

    data.frame(num_blanks = num_blanks,
               num_detects=num_detects,
               num_e_detects=num_e_detects,
               num_detects_10xRL=num_detects_10xmaxRL,
               median_detected_value=median_detected_value,
               max_detected_value=max_detected_value
               #if(length(BD90.90 != 0))
               #{
              #   BD90.90 = BD90.90
              # } else (BD90.90 = NA)
    )
    
  })
    

  
  ##Join summary to pcode data
  blanksummary <- join(blanksummary,summary,by=c("PARM_CD"))
}	

###Order blank summary by pcode group
blanksummary <-blanksummary[with(blanksummary, order(PARM_SEQ_GRP_CD, PARM_CD)), ]

###Round results to 3 decimal places

blanksummary$median_detected_value <- round(blanksummary$median_detected_value,3)
blanksummary$max_detected_value <- round(blanksummary$max_detected_value,3)
#blanksummary$BD90.90 <- round(blanksummary$BD90.90,3)

return(blanksummary)
###End of blank dataframe length check
} else{}


}

