#' flipDQI Flip DQI codes for given sites and parameters
#' 
#' Generates batch files for input into QWData with user-specified DQI codes.
#' 
#' @param STAIDS A vector of station IDs
#' @param records A vector of record numbers for which DQI codes are to be flipped. Must be in the format as retrieved using readNWISodbc of record number with coorseponding database number appended with an underscore. E.g. "123456_01
#' @param parameters A vector of parameter codes of the same length as records for which DQI codes are to be flipped.See details
#' @param dqiCodes A vector of specified DQI codes of the same length as records.See details
#' @param DSN A character string containing the DSN for your local server
#' @param env.db Environmental database number
#' @param qa.db QA database number
#' @details This function allows the user to generate batch files to change DQI codes for custom sets of parameters and record numbers.
#' The input vectors must be the same length, but record numbers can and often will be repeated. See example below for a demonstration of input.
#' @examples 
#' \dontrun{
#' #Will not run unless connected to NWISCO
#' STAIDS <- c("391517106223801","391500106224901")
#' records <- c("00306540_01","00306540_01","01305854_01","01305854_01")
#' parameters <- c("00400","00095","01106","01056")
#' dqiCodes <- c("R","R","Q","R")
#' flipDQI(STAIDS = STAIDS,
#' records = records,
#' parameter = parameters,
#' dqiCodes = dqiCodes,
#' DSN = "NWISCO",
#' env.db = "01",
#' qa.db = "02")
#' }
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @import RODBC 
#' @export
#' 
flipDQI <- function(STAIDS,
                    records,
                    parameters,
                    dqiCodes,
                    DSN = "NWISCO",
                    env.db = "01",
                    qa.db = "02") {
        
         #STAIDS <- c("391517106223801","391500106224901")
         #records <- c("00306540_01","00306540_01","01305854_01","01305854_01")
         #parameters <- c("00400","00095","01106","01056")
         #dqiCodes <- c("R","R","Q","R")
         
         dqiData <- data.frame(RECORD_NO = records,
                               PARM_CD = parameters,
                               DQI_CD = dqiCodes,
                               stringsAsFactors = FALSE)

        if(is.null(records)){
                print("You must enter atleast one record number")
                stop("You must enter atleast one record number")
        }
        if(is.null(parameters)){
                print("You must enter atleast one parameter code")
                stop("You must enter atleast one parameter code")
        }
        if(is.null(dqiCodes)){
                print("You must enter atleast one dqi code")
                stop("You must enter atleast one dqi code")
        }
        if(!all.equal(length(records),length(parameters),length(dqiCodes)))
        {
                print("record, parameter, and dqi code vectors are not of the same length")
                stop("record, parameter, and dqi code vectors are not of the same length")
        }
        ########
        #Get the data
        qwData <- readNWISodbc(DSN = "NWISCO",
                               STAIDS = STAIDS,
                               dl.parms = parameters,
                               parm.group.check=FALSE)$PlotTable
        qwData <- qwData[qwData$RECORD_NO %in% records,]
        
        #########
        ###THIS MAY NOT BE NECESSARY IF DWDATA DOES NOT OVERWRITE THESE FIELDS WHEN LEFT NULL
        ###Populate sample comment fields
        #Get sample comments
        #sampleComments <- qwData[c("RECORD_NO","PARM_CD","SAMPLE_CM_TX","SAMPLE_CM_TP")]
        #fieldSampleComments <- filter(sampleComments,SAMPLE_CM_TP == "F")
        #labSampleComments <- filter(sampleComments,SAMPLE_CM_TP == "L")
        
        #Get result comments
        #resultComments <- qwData[c("RECORD_NO","PARM_CD","RESULT_CM_TX","RESULT_CM_TP")]
        #fieldResultComments <- filter(resultComments,RESULT_CM_TP == "F")
        #labResultComments <- filter(resultComments,RESULT_CM_TP == "L")
        
        #########
        #Get value qualifer codes
        valCodes <- qwData[c("RECORD_NO","PARM_CD","VAL_QUAL_CD")]
        valCodes <- na.omit(valCodes)
        valCodes <- unique(valCodes)
        ##Aggregate into a pasted string of codes
        valCodes <- dplyr::group_by(valCodes,RECORD_NO,PARM_CD)
        valCodes <- dplyr::summarize(valCodes,
                                     VAL_QUAL_CD = paste0(VAL_QUAL_CD,collapse=""))
        ###########
        #Make the QWResult file
        
        #Get the data
        qwResult <- unique(qwData[c("RECORD_NO",
                                    "PARM_CD",
                                    "RESULT_VA",
                                    "REMARK_CD",
                                    "METH_CD",
                                    "RESULT_RD",
                                    "RPT_LEV_VA",
                                    "RPT_LEV_CD",
                                    "NULL_VAL_QUAL_CD",
                                    "PREP_SET_NO",
                                    "ANL_SET_NO",
                                    "ANL_DT",
                                    "PREP_DT",
                                    "LAB_STD_DEV_VA",
                                    "ANL_ENT_CD")])
        
        #Add in blank columns and empty comment columns
        qwResult$LAB_RESULT_COM <- NA
        qwResult$FIELD_RESULT_COM <- NA
        qwResult$blankCol <- NA
        
        #Join in value qualifier codes
        qwResult <- dplyr::left_join(qwResult,valCodes, by=c("RECORD_NO","PARM_CD"))
        
        #Join to dqiData to drop unwanted results/records and flip DQIs
        qwResult <- dplyr::left_join(dqiData,qwResult, by = c("RECORD_NO","PARM_CD"))

        
        #Reorder dataframe to proper order for qwdata
        qwResult <- qwResult[c("RECORD_NO",
                               "PARM_CD",
                               "RESULT_VA",
                               "REMARK_CD",
                               "blankCol",
                               "METH_CD",
                               "RESULT_RD",
                               "VAL_QUAL_CD",
                               "RPT_LEV_VA",
                               "RPT_LEV_CD",
                               "DQI_CD",
                               "NULL_VAL_QUAL_CD",
                               "PREP_SET_NO",
                               "ANL_SET_NO",
                               "ANL_DT",
                               "PREP_DT",
                               "LAB_RESULT_COM",
                               "FIELD_RESULT_COM",
                               "LAB_STD_DEV_VA",
                               "ANL_ENT_CD")]
        
        ###########
        #Make the QWSample file
        qwSample <- unique(data.frame(qwData$RECORD_NO,
                                      "*UNSPECIFIED*",
                                      qwData$AGENCY_CD,
                                      qwData$SITE_NO,
                                      qwData$SAMPLE_START_DT,
                                      qwData$SAMPLE_END_DT,
                                      qwData$MEDIUM_CD,
                                      qwData$LAB_NO,
                                      qwData$PROJECT_CD,
                                      qwData$AQFR_CD,
                                      qwData$SAMP_TYPE_CD,
                                      qwData$ANL_STAT_CD,
                                      "",
                                      qwData$HYD_COND_CD,
                                      qwData$HYD_EVENT_CD,
                                      qwData$TU_ID,
                                      qwData$BODY_PART_ID,
                                      "",
                                      "",
                                      "",
                                      "",
                                      qwData$COLL_ENT_CD,
                                      qwData$SAMPLE_START_TZ_CD,
                                      qwData$SAMPLE_START_LOCAL_TM_FG,
                                      stringsAsFactors=FALSE))
        
        qwsampleheader <- c("sample.integer",  "user.code",  "agency",  "site.no",  "start.date",  "end.date",  "medium",  "labid",
                            "project.code",  "aquifer.code",	"sample.type",	"analysis.status",	"analysis.source",	"hydrologic.cond",
                            "hydrologic.event",	"tissue",	"body.part",	"lab.comment.",	"field.comment",	"time.datum",	"datum.reliability",
                            "collecting.agency.code","time.zone","std.time.code")
        
        colnames(qwSample)<-qwsampleheader
        
        ##Format times into GMT and correct of daylight savings offset according to location
        ##Weather or not to apply daylight savings is in the std.time.code column, which is from the SAMPLE_START_LOCAL_TM_FG NWIS parameter
        ##e.g. in Colorado, SAMPLE_START_LOCAL_TM_FG = Y, timezone = MDT, SAMPLE_START_LOCAL_TM_FG = N, timezone = MST
        qwSample$start.date <- as.POSIXct(qwSample$start.date, tz="GMT")
        
        qwSample$offset <- ifelse (qwSample$std.time.code == "Y", 60*60,0)
        qwSample$start.date.offset <- qwSample$start.date + qwSample$offset
        ###Format times from GMT to appropriate time zone
        ###Using a loop because I could not figure out how to vectorize it, perhaps "mapply" would work, but don't know
        for ( i in 1:nrow(qwSample))
        {
                ###Converts to time zone
                qwSample$start.date.adj[i] <- format(qwSample$start.date.offset[i],"%Y%m%d%H%M", tz=as.character(qwSample$time.zone[i]))
        }
        
        qwSample$start.date <- qwSample$start.date.adj 
        qwSample$start.date.adj <- NULL
        qwSample$offset <- NULL
        qwSample$start.date.offset <- NULL
        qwSample$time.zone <- NULL
        ###Remove extra empty character space from medium to make it match medium in data file of 2-3 char
        #qwSample$medium <- (gsub(" ", "", qwSample$medium))
        qwSample$RECORD_NO <- qwSample$sample.integer
        qwSample$sample.integer <- seq(1:nrow(qwSample))
        
        ###Merge in sample integer and drop UID and recordno columns
        qwResult <- dplyr::left_join(qwSample[c("RECORD_NO","sample.integer")],qwResult,by="RECORD_NO")
        
        ###Drop old RECORD_NO columns
        qwResult$RECORD_NO <- NULL
        qwSample$RECORD_NO <- NULL
        
        return(list(qwsample = qwSample,
                    qwresult = qwResult))
}
