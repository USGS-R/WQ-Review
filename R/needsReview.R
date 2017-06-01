#' Function to return sample needing review
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param flaggedResults Optional dataframe of flagged results generated from \code {flagSummary} function
#' @details Returns list of unapproved results (DQI of I, S, or P) and if the result has been flagged anywhere
#' @examples 
#' data("exampleData",package="WQReview")
#' pestCheckOut <- pestCheck(qw.data=qw.data,
#'              returnAll = FALSE)
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom dplyr transmute
#' @export
#' @return A dataframe containing all samples with applicable flags

needsReview <- function(qw.data,flaggedResults = NULL)
{
        
        plotTable <- qw.data$PlotTable
        
        if(is.null(flaggedResults))
        {
        flaggedResults <- flagSummary(qw.data)
        }
        
        inReviewData <- plotTable[plotTable$DQI_CD %in% c("I","S","P","U"),
                               c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","PARM_CD","PARM_NM","PARM_SEQ_GRP_CD","DQI_CD","RESULT_CM_TX")]
        # inReviewData <- transmute(inReviewData,
        #                        RECORD_NO = RECORD_NO,
        #                        SITE_NO = as.factor(SITE_NO),
        #                        STATION_NM = STATION_NM,
        #                        SAMPLE_START_DT = SAMPLE_START_DT,
        #                        MEDIUM_CD = as.factor(MEDIUM_CD),
        #                        PARM_CD = as.factor(PARM_CD),
        #                        PARM_NM = as.factor(PARM_NM),
        #                        PARM_SEQ_GRP_CD = as.factor(PARM_SEQ_GRP_CD),
        #                        DQI_CD = as.factor(DQI_CD))
        
        inReviewData <- unique(inReviewData)
        inReviewData$FLAGGED <- ifelse(paste0(inReviewData$RECORD_NO,inReviewData$PARM_CD) %in%
                                               paste0(flaggedResults$RECORD_NO,flaggedResults$PARM_CD),
                                       TRUE,FALSE)
        
        return(inReviewData)

}