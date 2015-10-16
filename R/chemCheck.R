#' Function to flag samples if basic chemistry is unreasonable
#' @param qw.data A qw.data list generated from readNWISodbc
#' @import plyr
#' @export
#' @return A dataframe containing all samples with applicable flags

chemCheck <- function(qw.data) {
        #Make data frame of all samples
        inReviewData <- subset(qw.data$PlotTable, DQI_CD %in% c("I","S","P"))
        flaggedSamples <- unique(inReviewData[c("RECORD_NO",
                                              "SITE_NO",
                                              "STATION_NM",
                                              "SAMPLE_START_DT",
                                              "SAMPLE_END_DT",
                                              "MEDIUM_CD")])
        #Flag samples with bad pH ranges
        
        ##Subset data to pH results
        pHData <- subset(inReviewData, PARM_CD == "00400")
        
        ##Flag samples that are too low
        ###Make empty vector for flags
        pHData$phTooLow_30.01 <- NA
        ###Flag samples
        pHData$phTooLow_30.01[which(pHData$RESULT_VA < 4.5)] <- paste("flag",
                                                                     pHData$RESULT_VA[which(pHData$RESULT_VA < 4.5)])
        ##Flag samples that are too high
        ###Make empty vector for flags
        pHData$phTooHigh_30.02 <- NA
        ###Flag samples
        pHData$phTooHigh_30.02[which(pHData$RESULT_VA > 9)] <- paste("flag",
                                                                      pHData$RESULT_VA[which(pHData$RESULT_VA > 9)])
        
        
        #Flag samples with high O2
        
        ##Subset data to O2 results
        O2Data <- subset(inReviewData, PARM_CD == "00300")
        
        ##Flag samples that are too high
        ###Make empty vector for flags
        O2Data$O2TooHigh_30.03 <- NA
        ###Flag samples
        O2Data$O2TooHigh_30.03[which(O2Data$RESULT_VA > 25)] <- paste("flag",
                                                                     O2Data$RESULT_VA[which(O2Data$RESULT_VA > 25)])
        
        
        #Flag samples with bad charge balance
        
        ##Subset to samples with SC data
        cbData <- subset(inReviewData, PARM_CD == "00095" & MEDIUM_CD != "OAQ")
        
        ##Make empty vector for flags
        cbData$BadCB_30.21 <- NA
        ###Flag samples
        cbData$BadCB_30.21[which(cbData$RESULT_VA <= 100 & 
                                         abs(cbData$perc.diff) > 15)] <- paste("flag",
                                                                               "SC=",
                                                                               cbData$RESULT_VA[which(cbData$RESULT_VA <= 100 & 
                                                                                                              abs(cbData$perc.diff) > 15)],
                                                                               "perc diff=",
                                                                               cbData$perc.diff[which(cbData$RESULT_VA <= 100 & 
                                                                                                              abs(cbData$perc.diff) > 15)],
                                                                               cbData$complete.chem[which(cbData$RESULT_VA <= 100 & 
                                                                                                              abs(cbData$perc.diff) > 15)]
                                                                               )
        cbData$BadCB_30.21[which(cbData$RESULT_VA > 100 &
                                         cbData$RESULT_VA <= 1000 &
                                         abs(cbData$perc.diff) > 10)] <- paste("flag",
                                                                               "SC=",
                                                                               cbData$RESULT_VA[which(cbData$RESULT_VA > 100 &
                                                                                                              cbData$RESULT_VA <= 1000 &
                                                                                                              abs(cbData$perc.diff) > 10)],
                                                                               "perc diff=",
                                                                               cbData$perc.diff[which(cbData$RESULT_VA > 100 &
                                                                                                              cbData$RESULT_VA <= 1000 &
                                                                                                              abs(cbData$perc.diff) > 10)],
                                                                               cbData$complete.chem[which(cbData$RESULT_VA > 100 &
                                                                                                              cbData$RESULT_VA <= 1000 &
                                                                                                              abs(cbData$perc.diff) > 10)]
                                                                               )
        cbData$BadCB_30.21[which(cbData$RESULT_VA > 1000 & 
                                         abs(cbData$perc.diff) > 5)] <- paste("flag",
                                                                              "SC=",
                                                                              cbData$RESULT_VA[which(cbData$RESULT_VA > 1000 & 
                                                                                                             abs(cbData$perc.diff) > 5)],
                                                                              "perc diff=",
                                                                               cbData$perc.diff[which(cbData$RESULT_VA > 1000 & 
                                                                                                              abs(cbData$perc.diff) > 5)],
                                                                              cbData$complete.chem[which(cbData$RESULT_VA > 1000 & 
                                                                                                             abs(cbData$perc.diff) > 5)]
                                                                              )
        #Join flagged samples together into one dataframe
        flaggedSamples <- join(flaggedSamples, 
                               pHData[c("RECORD_NO","phTooLow_30.01")], 
                               by = "RECORD_NO")
        
        flaggedSamples <- join(flaggedSamples, 
                               pHData[c("RECORD_NO","phTooHigh_30.02")], 
                               by = "RECORD_NO")
        
        flaggedSamples <- join(flaggedSamples, 
                               O2Data[c("RECORD_NO","O2TooHigh_30.03")], 
                               by = "RECORD_NO")
        flaggedSamples <- join(flaggedSamples, 
                               cbData[c("RECORD_NO","BadCB_30.21")], 
                               by = "RECORD_NO")
        flaggedSamples <- unique(flaggedSamples)
        return(flaggedSamples)
}
        
        