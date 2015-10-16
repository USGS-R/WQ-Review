#' Function to flag pesticide samples
#' @param qw.data A qw.data list generated from readNWISodbc
#' @import plyr
#' @import smwrBase
#' @export
#' @return A dataframe containing all samples with applicable flags

pestCheck <- function(qw.data)
{
        #subset data to schedule 2437 parameters collected after WY2013
        pestData <- subset(qw.data$PlotTable, PARM_CD %in% schedule2437 & 
                                   waterYear(SAMPLE_START_DT) >= 2013)
        
        #Split data into approved and in review
        inReviewData <- subset(pestData, DQI_CD %in% c("I","S","P") &
                                       PARM_SEQ_GRP_CD != "INF" &
                                       MEDIUM_CD %in% c("WS ","WG ", "OA "))
        approvedData <- subset(pestData, DQI_CD %in% c("R","O","A") &
                                       PARM_SEQ_GRP_CD != "INF" &
                                       MEDIUM_CD %in% c("WS ","WG ", "OA "))
        
        
        #Get max number of hits in approved data at each site
        detects <- subset(approvedData, REMARK_CD != "<")
        siteStats <- ddply(detects,c("SITE_NO","RECORD_NO"),summarize,
                           numHits = length(RESULT_VA)
        )
        
        siteStats <- ddply(siteStats,c("SITE_NO"),summarize,
                           maxHits = max(numHits,na.rm=TRUE),
                           minHits = min(numHits,na.rm=TRUE),
                           N = length(numHits)
        )
        
        #Get number of hits for each sample
        detects <- subset(inReviewData, REMARK_CD != "<")
        sampleHits <- ddply(detects,c("SITE_NO","RECORD_NO"),summarize,
                           numHits = length(RESULT_VA)
        )
        
        #Join siteStats to sample hits for comparison
        sampleHits <- join(sampleHits,siteStats,by="SITE_NO")
        
        #Compare to max and min number of hits and flag
        sampleHits$newMaxHits_30.13 <- NA
        sampleHits$newMaxHits_30.13[which(sampleHits$numHits > sampleHits$maxHits &
                                            sampleHits$N > 4)] <- paste("flag",
                                                                        sampleHits$numHits[which(sampleHits$numHits > sampleHits$maxHits &
                                                                                                            sampleHits$N > 4)],
                                                                        "hits"
                                            )
        
        sampleHits$newMinHits_30.14 <- NA
        sampleHits$newMinHits_30.14[which(sampleHits$numHits < sampleHits$minHits &
                                            sampleHits$N > 4)] <- paste("flag",
                                                                        sampleHits$numHits[which(sampleHits$numHits < sampleHits$minHits &
                                                                                                         sampleHits$N > 4)],
                                                                        "hits"
                                            )
        sampleHits <- join(unique(pestData[c("RECORD_NO",
                                             "STATION_NM",
                                             "SAMPLE_START_DT",
                                             "SAMPLE_END_DT",
                                             "MEDIUM_CD")]),sampleHits,
                           by="RECORD_NO",
                           type="right")
        return(sampleHits)
}
        
        
