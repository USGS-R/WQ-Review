#' Function to flag samples if values are out of range of historic data
#' @param qw.data A qw.data list generated from readNWISodbc
#' @import plyr
#' @export
#' @return A dataframe containing all samples with applicable flags

historicCheck <- function(qw.data)
{
        #subset to in review and approved data, removing information pcodes
        inReviewData <- subset(qw.data$PlotTable, DQI_CD %in% c("I","S","P") &
                                       PARM_SEQ_GRP_CD != "INF" &
                                       MEDIUM_CD %in% c("WS ","WG ", "OA "))
        approvedData <- subset(qw.data$PlotTable, DQI_CD %in% c("R","O","A") &
                                       PARM_SEQ_GRP_CD != "INF" &
                                       MEDIUM_CD %in% c("WS ","WG ", "OA "))
        
        
        #Get stats by parm for each site
        siteStats <- ddply(approvedData,c("SITE_NO","PARM_CD"),summarize,
                        min = min(RESULT_VA,na.rm=TRUE),
                        max = max(RESULT_VA,na.rm=TRUE),
                        quant99 = quantile(RESULT_VA,probs=(0.99),na.rm=TRUE),
                        quant01 = quantile(RESULT_VA,probs=(0.01),na.rm=TRUE),
                        N = length(RESULT_VA)
        )
        
        inReviewData <- join(inReviewData,siteStats, by = c("SITE_NO","PARM_CD"))
        
        ##Check if new max
        inReviewData$newMax_30.11 <- NA
        inReviewData$newMax_30.11[which(inReviewData$RESULT_VA > inReviewData$max &
                                          inReviewData$N > 4)] <-
                paste("flag",
                      inReviewData$RESULT_VA[which(inReviewData$RESULT_VA > inReviewData$max &
                                                           inReviewData$N > 4)]
                )
                   
        ##Check if new min
        inReviewData$newMin_30.12 <- NA
        inReviewData$newMin_30.12[which(inReviewData$RESULT_VA < inReviewData$min &
                                          inReviewData$N > 4)] <-
                paste("flag",
                      inReviewData$RESULT_VA[which(inReviewData$RESULT_VA < inReviewData$min &
                                                           inReviewData$N > 4)]
                )  
        
        ##Check if greater than 99th percentile
        inReviewData$greaterQuant99_30.15 <- NA
        inReviewData$greaterQuant99_30.15[which(inReviewData$RESULT_VA > inReviewData$quant99 &
                                          inReviewData$N > 4)] <-
                paste("flag",
                      inReviewData$RESULT_VA[which(inReviewData$RESULT_VA > inReviewData$quant99 &
                                                           inReviewData$N > 4)]
                )
        
        ##Check if less than 1th percentile
        inReviewData$lessQuant01_30.16 <- NA
        inReviewData$lessQuant01_30.16[which(inReviewData$RESULT_VA < inReviewData$quant01 &
                                          inReviewData$N > 4)] <-
                paste("flag",
                      inReviewData$RESULT_VA[which(inReviewData$RESULT_VA < inReviewData$quant01 &
                                                           inReviewData$N > 4)]
                )
        
        #Extract site info and flags
        flaggedSamples <- inReviewData[c("RECORD_NO",
                                         "SITE_NO",
                                         "STATION_NM",
                                         "SAMPLE_START_DT",
                                         "SAMPLE_END_DT",
                                         "MEDIUM_CD",
                                         "PARM_CD",
                                         "PARM_NM",
                                         "min",
                                         "max",
                                         "quant99",
                                         "quant01",
                                         "newMax_30.11",
                                         "newMin_30.12",
                                         "greaterQuant99_30.15",
                                         "lessQuant01_30.16"
                                         )]
}