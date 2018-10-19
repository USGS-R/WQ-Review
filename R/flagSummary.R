#' Function to summarize samples that are not approved
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param returnAll Only return results that have been flagged somewhere
#' @details Summarizes automated check tables and displays records numbers for unapproved data (DQI of I, S, or P) and number of flags
#' @examples 
#' data("exampleData",package="WQReview")
#' pestCheckOut <- pestCheck(qw.data=qw.data,
#'              returnAll = FALSE)
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom dplyr transmute
#' @importFrom dplyr filter
#' @export
#' @return A dataframe containing all samples with applicable flags

flagSummary <- function(qw.data,returnAll = FALSE, returnAllTables = FALSE)
{

        repTable <- try(suppressWarnings(repTabler(qw.data)))
        blankTable <- try(blankSummary(qw.data))
        wholevpartTable <- try(suppressWarnings(wholevPart(qw.data)))
        chemFlagTable <- try(suppressWarnings(chemCheck(qw.data)))
        pestFlagTable <- try(suppressWarnings(pestCheck(qw.data)))
        resultFlagTable <- try(suppressWarnings(historicCheck(qw.data,returnAll=FALSE)))
        
        
        plotTable <- qw.data$PlotTable
        flaggedSamples <- plotTable[plotTable$DQI_CD %in% c("I","S","P"),
                                    c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","PARM_CD","PARM_NM","PARM_SEQ_GRP_CD","DQI_CD")]
        # flaggedSamples <- transmute(flaggedSamples,
        #                             RECORD_NO = RECORD_NO,
        #                             SITE_NO = as.factor(SITE_NO),
        #                             STATION_NM = STATION_NM,
        #                             SAMPLE_START_DT = SAMPLE_START_DT,
        #                             MEDIUM_CD = as.factor(MEDIUM_CD),
        #                             PARM_CD = as.factor(PARM_CD),
        #                             PARM_NM = as.factor(PARM_NM),
        #                             PARM_SEQ_GRP_CD = as.factor(PARM_SEQ_GRP_CD),
        #                             DQI_CD = as.factor(DQI_CD))
        
        flaggedSamples <- unique(flaggedSamples)
        
        temp <- dplyr::summarize(dplyr::group_by(flaggedSamples,RECORD_NO,PARM_CD),
                                 chemFlags = length(RECORD_NO[RECORD_NO %in% chemFlagTable$RECORD_NO]),
                                 repFlags = length(PARM_CD[PARM_CD %in% repTable$PARM_CD[repTable$flags=="RPD > 10% and > RPT_LEV"]]),
                                 resultFlag = ifelse(paste0(RECORD_NO,PARM_CD) %in% 
                                                             paste0(resultFlagTable$RECORD_NO,resultFlagTable$PARM_CD),
                                                     "TRUE","FALSE"),
                                 wholevPartFlag = ifelse(paste0(RECORD_NO,PARM_CD) %in% 
                                                                 paste0(wholevpartTable$RECORD_NO,wholevpartTable$PARM_CD),
                                                         "TRUE","FALSE")
        )
        
        flaggedSamples <- dplyr::left_join(flaggedSamples,temp,by=c("RECORD_NO","PARM_CD"))
        
        if(returnAll == FALSE) {
                flaggedSamples <- dplyr::filter(flaggedSamples,chemFlags > 0 |
                                                 repFlags > 0 |
                                                 resultFlag == TRUE |
                                                 wholevPartFlag == TRUE)
        } 
        
        return(flaggedSamples)
        
        
        
}