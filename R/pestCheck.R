#' Function to flag pesticide samples
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @details Compares each sample with DQI code of "I","S", or "P" to ranges of all prior approved data ("R","O","A"),
#' and flags samples that are suspisciously high or low i nnumber of hits for pesticide schedule 2437. 
#' Definitions of checks can be found at http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check30-sql.html
#' @examples 
#' data("exampleData",package="WQReview")
#' pestCheckOut <- pestCheck(qw.data=qw.data,
#'              returnAll = FALSE)
#' @importFrom plyr join
#' @importFrom plyr ddply
#' @export
#' @return A dataframe containing all samples with applicable flags

pestCheck <- function(qw.data, returnAll = FALSE)
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
        
        if(nrow(inReviewData) > 0 & nrow(approvedData) > 0)
        {
                
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
        } else {
                sampleHits <- unique(qw.data$PlotTable[c("RECORD_NO",
                                                "STATION_NM",
                                                "SAMPLE_START_DT",
                                                "SAMPLE_END_DT",
                                                "MEDIUM_CD",
                                                "SITE_NO")])
                sampleHits$numHits <- NA
                sampleHits$maxHits <- NA
                sampleHits$minHits <- NA
                sampleHits$N <- NA
                sampleHits$newMaxHits_30.13 <- NA
                sampleHits$newMinHits_30.14 <- NA
                warning("Insufficient pesticide data to auto check")}
        
        if(returnAll == FALSE)
        {
                #remove NAs from result flags
                sampleHits <- unique(sampleHits[which(!is.na(sampleHits[11]) |
                                                                      !is.na(sampleHits[12])
                                                      
                ),]) 
        } else {}
        
        return(sampleHits)
        
}
        
        
