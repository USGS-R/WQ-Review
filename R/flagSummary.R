#' Function to summarize data flags
#' @param qw.data A qw.data list generated from readNWISodbc
#' @import plyr
#' @export
#' @return A dataframe containing only flagged samples

flagSummary <- function(qw.data)
{
        #Get sample info
        flagSummary <- qw.data$PlotTable[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_END_DT","MEDIUM_CD")]
        
        #Run flagging functions
        
        chemFlags <- chemCheck(qw.data)
        pestFlags <- pestCheck(qw.data)
        
        #Join tables
        flagSummary <- join(flagSummary, chemFlags[c(1,7:10)],by="RECORD_NO")
        flagSummary <- join(flagSummary,pestFlags[c(1,11,12)],by="RECORD_NO")

        #remove NAs
        flagSummary <- unique(flagSummary[which(!is.na(flagSummary[7]) |
                                                 !is.na(flagSummary[8]) |
                                                 !is.na(flagSummary[9]) |
                                                 !is.na(flagSummary[10]) |
                                                 !is.na(flagSummary[11]) |
                                                 !is.na(flagSummary[12])
        ),])
        
        return(flagSummary)
}
        


                    
                                     