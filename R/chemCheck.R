#' chemCheck
#' Function to flag samples if basic chemistry is unreasonable
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @details Performs chemical checks for expected ranges of O2, pH, Sc, and chargebalance. Definitions of checks can be found at http://internal.cida.usgs.gov/NAWQA/data_checks/docs/files/check30-sql.html
#' @examples 
#' data("exampleData",package="WQReview")
#' chemCheckOut <- chemCheck(qw.data=qw.data,
#'              returnAll = FALSE)
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

chemCheck <- function(qw.data, returnAll = FALSE) {
        
        #Make data frame of all samples
        inReviewData <- subset(qw.data$PlotTable, DQI_CD %in% c("I","S","P"))
        
        #run ionBalance on inReview data is not present
        if(is.null(inReviewData$complete.chem)){
                ionbal <- ionBalance(qw.data)
                inReviewData <- dplyr::left_join(inReviewData, ionbal[,c("RECORD_NO","PARM_CD","meqCharge","sum_cat","sum_an","complete.chem",
                                                                  "perc.diff","element")], by = c("RECORD_NO","PARM_CD"))
        }
        
        #Check if empty rows and put in a NA row, super hacky but fixes the issue
        if(nrow(inReviewData) == 0){
          tempDF <- as.data.frame(t(rep(NA,ncol(inReviewData))))
          names(tempDF) <- names(inReviewData)
          inReviewData <- dplyr::bind_rows(inReviewData,tempDF)
        }
        
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
                                              "MEDIUM_CD",
                                              "SAMPLE_CM_TX")])
        #Flag samples with bad field vs lab SC
        ##Subset data to field and lab SC results
        fieldSCData <- subset(inReviewData, PARM_CD == "00095" & MEDIUM_CD != "OAQ")
        labSCData <- subset(inReviewData, PARM_CD == "90095" & MEDIUM_CD != "OAQ")

        SCData <- dplyr::left_join(fieldSCData[c("RECORD_NO","RESULT_VA")], labSCData[c("RECORD_NO","RESULT_VA","SC_badLabVSField")], by="RECORD_NO")
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
        #Check if charge balance was run and throw a warning if not
        if(is.null(inReviewData$complete.chem))
        {
                warning("No charge balance data. Try running ionBalance() on qw.data list input first.")
                cbData <- subset(inReviewData, PARM_CD == "00095" & MEDIUM_CD != "OAQ")
                cbData$BadCB_30.21 <- NA
        } else {
        ##Subset to samples with SC data
        cbData <- subset(inReviewData, PARM_CD == "00095" & MEDIUM_CD != "OAQ" & complete.chem == "Complete")
        
        ##Make empty vector for flags
        ###Flag samples
        cbData$BadCB_30.21[which(cbData$RESULT_VA <= 100 & 
                                         abs(cbData$perc.diff) > 15)] <- paste("flag",
                                                                               "SC=",
                                                                               cbData$RESULT_VA[which(cbData$RESULT_VA <= 100 & 
                                                                                                              abs(cbData$perc.diff) > 15)],
                                                                               "perc diff=",
                                                                               cbData$perc.diff[which(cbData$RESULT_VA <= 100 & 
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
                                                                                                              abs(cbData$perc.diff) > 10)]
                                                                               )
        cbData$BadCB_30.21[which(cbData$RESULT_VA > 1000 & 
                                         abs(cbData$perc.diff) > 5)] <- paste("flag",
                                                                              "SC=",
                                                                              cbData$RESULT_VA[which(cbData$RESULT_VA > 1000 & 
                                                                                                             abs(cbData$perc.diff) > 5)],
                                                                              "perc diff=",
                                                                               cbData$perc.diff[which(cbData$RESULT_VA > 1000 & 
                                                                                                              abs(cbData$perc.diff) > 5)]
                                                                              )
        }
        #Join flagged samples together into one dataframe
        flaggedSamples <- dplyr::left_join(flaggedSamples, 
                               SCData[c("RECORD_NO","SC_badLabVSField")], 
                               by = "RECORD_NO")
        
        flaggedSamples <- dplyr::left_join(flaggedSamples, 
                               pHData[c("RECORD_NO","phTooLow_30.01")], 
                               by = "RECORD_NO")
        
        flaggedSamples <- dplyr::left_join(flaggedSamples, 
                               pHData[c("RECORD_NO","phTooHigh_30.02")], 
                               by = "RECORD_NO")
        
        flaggedSamples <- dplyr::left_join(flaggedSamples, 
                               O2Data[c("RECORD_NO","O2TooHigh_30.03")], 
                               by = "RECORD_NO")
        flaggedSamples <- dplyr::left_join(flaggedSamples, 
                               cbData[c("RECORD_NO","BadCB_30.21")], 
                               by = "RECORD_NO")
        flaggedSamples <- unique(flaggedSamples)
        
        if(returnAll == FALSE)
        {
                #remove NAs from result flags
                flaggedSamples <- unique(flaggedSamples[which(!is.na(flaggedSamples[8]) |
                                                                      !is.na(flaggedSamples[9]) |
                                                                      !is.na(flaggedSamples[10]) |
                                                                      !is.na(flaggedSamples[11]) |
                                                                                     !is.na(flaggedSamples[12])
                ),]) 
        } else {}
        
        return(unique(flaggedSamples))
}
        
        