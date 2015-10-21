#' Function to flag samples if basic chemistry is unreasonable
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @import plyr
#' @export
#' @return A dataframe containing all samples with applicable flags

chemCheck <- function(qw.data, returnAll = FALSE) {
        #Make data frame of all samples
        inReviewData <- subset(qw.data$PlotTable, DQI_CD %in% c("I","S","P"))
        inReviewData$SC_badLabVSField <- NA
        inReviewData$phTooLow_30.01 <- NA
        inReviewData$phTooHigh_30.02 <- NA
        inReviewData$O2TooHigh_30.03 <- NA
        inReviewData$BadCB_30.21 <- NA
        
        flaggedSamples <- unique(inReviewData[c("RECORD_NO",
                                              "SITE_NO",
                                              "STATION_NM",
                                              "SAMPLE_START_DT",
                                              "SAMPLE_END_DT",
                                              "MEDIUM_CD")])
        #Flag samples with bad field vs lab SC
        ##Subset data to field and lab SC results
        fieldSCData <- subset(inReviewData, PARM_CD == "00095" & MEDIUM_CD != "OAQ")
        labSCData <- subset(inReviewData, PARM_CD == "90095" & MEDIUM_CD != "OAQ")

        SCData <- join(fieldSCData[c("RECORD_NO","RESULT_VA")], labSCData[c("RECORD_NO","RESULT_VA","SC_badLabVSField")], by="RECORD_NO")
        rm(fieldSCData)
        rm(labSCData)
        
        colnames(SCData) <- c("RECORD_NO","fieldSC","labSC","SC_badLabVSField")
        #SCData <- na.omit(SCData)
        
        SCData$perc.diff <- (SCData$labSC - SCData$fieldSC)/SCData$fieldSC * 100
     
        ###Flag samples
        SCData$SC_badLabVSField[which(abs(SCData$perc.diff) > 10)] <- paste("flag",
                                                                            "field=",SCData$fieldSC[which(abs(SCData$perc.diff) > 10)],
                                                                            "lab=",SCData$labSC[which(abs(SCData$perc.diff) > 10)],
                                                                            "percDiff=",SCData$perc.diff[which(abs(SCData$perc.diff) > 10)])
        
                              
        
        ##Subset data to pH results
        pHData <- subset(inReviewData, PARM_CD == "00400")


        ##Flag samples that are too low
        ###Make empty vector for flags
        ###Flag samples
        pHData$phTooLow_30.01[which(pHData$RESULT_VA < 4.5)] <- paste("flag",
                                                                     pHData$RESULT_VA[which(pHData$RESULT_VA < 4.5)])
        ##Flag samples that are too high
        ###Make empty vector for flags
        ###Flag samples
        pHData$phTooHigh_30.02[which(pHData$RESULT_VA > 9)] <- paste("flag",
                                                                      pHData$RESULT_VA[which(pHData$RESULT_VA > 9)])
        
        
        #Flag samples with high O2
        
        ##Subset data to O2 results
        O2Data <- subset(inReviewData, PARM_CD == "00300")
        
        ##Flag samples that are too high
        ###Make empty vector for flags
        ###Flag samples
        O2Data$O2TooHigh_30.03[which(O2Data$RESULT_VA > 25)] <- paste("flag",
                                                                     O2Data$RESULT_VA[which(O2Data$RESULT_VA > 25)])
        
        
        #Flag samples with bad charge balance
        
        ##Subset to samples with SC data
        cbData <- subset(inReviewData, PARM_CD == "00095" & MEDIUM_CD != "OAQ")
        
        ##Make empty vector for flags
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
                               SCData[c("RECORD_NO","SC_badLabVSField")], 
                               by = "RECORD_NO")
        
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
        
        if(returnAll == FALSE)
        {
                #remove NAs from result flags
                flaggedSamples <- unique(flaggedSamples[which(!is.na(flaggedSamples[7]) |
                                                                      !is.na(flaggedSamples[8]) |
                                                                      !is.na(flaggedSamples[9]) |
                                                                      !is.na(flaggedSamples[10]) |
                                                                                     !is.na(flaggedSamples[11])
                ),]) 
        } else {}
        
        return(flaggedSamples)
}
        
        