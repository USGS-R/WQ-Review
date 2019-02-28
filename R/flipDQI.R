#' flipDQI Flip DQI codes for given sites and parameters
#' 
#' Generates batch files for input into QWData with user-specified DQI codes.
#' 
#' @param STAIDS A vector of station IDs
#' @param records A vector of record numbers for which DQI codes are to be flipped. Must be in the format as retrieved using readNWISodbc of record number with coorseponding database number appended with an underscore. E.g. "123456_01
#' @param parameters A vector of parameter codes of the same length as records for which DQI codes are to be flipped.See details
#' @param dqiCodes A vector of specified DQI codes of the same length as records.See details
#' @param comments An optional vector of comments for each result of the same length as records.See details
#' @param commentType An optional vector of comment types of the same length as records. Default is "F" for field. See details

#' @param DSN A character string containing the DSN for your local server
#' @param env.db Environmental database number
#' @param qa.db QA database number
#' @param writeFiles write out qwresult and qwsample files to text files in working directory
#' @param keepRecordNo Logical. Retain record numbers in qwresult and qwsample. If TRUE, RECORD_NO column in qwresult and qwsample dataframes will need to be removed prior to batch upload to NWIS 
#' @param qwresultname Name of qwresult file
#' @param qwsamplename Name of qwsample file
#' @details This function allows the user to generate batch files to change DQI codes for custom sets of parameters and record numbers.
#' The input vectors must be the same length, but record numbers can and often will be repeated. See example below for a demonstration of input.
#' @export
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

#' 
flipDQI <- function(STAIDS,
                    records,
                    parameters,
                    dqiCodes,
                    comments = NULL,
                    commentType = NULL,
                    DSN = "NWISCO",
                    env.db = "01",
                    qa.db = "02",
                    keepRecordNo = FALSE,
                    writeFiles = FALSE,
                    qwresultname="qwresult",
                    qwsamplename="qwsample") {
  
  # STAIDS <- c("391517106223801","391500106224901")
  # records <- c("00306540_01","00306540_01","01305854_01","01305854_01")
  # parameters <- c("00400","00095","01106","01056")
  # dqiCodes <- c("I","I","I","I")
  # comments <- c("t1","t2","t3","t4")
  # commentType <- c("F","F","F","L")
  
  
  
  
  if(is.null(records)){
    print("You must enter at least one record number")
    stop("You must enter at least one record number")
  }
  if(is.null(parameters)){
    print("You must enter at least one parameter code")
    stop("You must enter at least one parameter code")
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
  
  if(is.null(comments))
  {
    dqiData <- data.frame(RECORD_NO = records,
                          PARM_CD = parameters,
                          DQI_CD = dqiCodes,
                          RESULT_CM_TX = NA,
                          stringsAsFactors = FALSE)
  } else {
    if(is.null(commentType)) {
      commentType <- rep("F",length(comments))
    }
    if(!all.equal(length(records),
                  length(parameters),
                  length(dqiCodes),
                  length(comments),
                  length(commentType)))
    {
      print("record, parameter, dqi code, comment, and comment type vectors are not of the same length")
      stop("record, parameter, dqi code, comment, and comment type vectors are not of the same length")
    }
    
    dqiData <- data.frame(RECORD_NO = records,
                          PARM_CD = parameters,
                          DQI_CD = dqiCodes,
                          RESULT_CM_TX = comments,
                          RESULT_CM_TP = commentType,
                          stringsAsFactors = FALSE)
  }
  ########
  #Get the data
  qwData <- readNWISodbc(DSN = DSN,
                         env.db=env.db,
                         qa.db = qa.db,
                         STAIDS = unique(STAIDS),
                         dl.parms = unique(parameters),
                         parm.group.check=FALSE,
                         resultAsText = TRUE)$PlotTable
  qwData <- qwData[qwData$RECORD_NO %in% records,]
  

  ###2017-03-28
  ###Leave columns blank and it will not do a transaction
  ###THIS ^ IS NOT CORRECT, IT WILL NULL ALL VALUES
  
  
  #########
  ###THIS MAY NOT BE NECESSARY IF DWDATA DOES NOT OVERWRITE THESE FIELDS WHEN LEFT NULL
  ###Populate sample comment fields
  #Get sample comments
  #sampleComments <- qwData[c("RECORD_NO","PARM_CD","SAMPLE_CM_TX","SAMPLE_CM_TP")]
  #fieldSampleComments <- sampleComments[sampleComments$SAMPLE_CM_TP == "F",]
  #labSampleComments <- sampleComments[sampleComments$SAMPLE_CM_TP == "L",]
  
  #Get result comments
  resultComments <- qwData[c("RECORD_NO","PARM_CD","RESULT_CM_TX","RESULT_CM_TP")]
  fieldResultComments <- resultComments[resultComments$RESULT_CM_TP == "F",]
  fieldResultComments <- dplyr::rename(fieldResultComments,FIELD_RESULT_CM = RESULT_CM_TX)
  
  labResultComments <- resultComments[resultComments$RESULT_CM_TP == "L",]
  labResultComments <- dplyr::rename(labResultComments,LAB_RESULT_CM = RESULT_CM_TX)
  
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
  qwResult$blankCol <- NA
  
  #Join in value qualifier codes
  qwResult <- dplyr::left_join(qwResult,valCodes, by=c("RECORD_NO","PARM_CD"))
  
  
  #Join in field comments
  qwResult <- dplyr::left_join(qwResult,fieldResultComments[c("RECORD_NO","PARM_CD","FIELD_RESULT_CM")],by=c("RECORD_NO","PARM_CD"))
  
  #Join in lab comments
  qwResult <- dplyr::left_join(qwResult,labResultComments[c("RECORD_NO","PARM_CD","LAB_RESULT_CM")],by=c("RECORD_NO","PARM_CD"))
  
  #Join to dqiData to drop unwanted results/records and flip DQIs
  qwResult <- dplyr::left_join(dqiData,qwResult, by = c("RECORD_NO","PARM_CD"))
  
  qwResult$FIELD_RESULT_CM[qwResult$RESULT_CM_TP == "F"] <- paste(qwResult$FIELD_RESULT_CM[qwResult$RESULT_CM_TP == "F"],
                                                                  qwResult$RESULT_CM_TX[qwResult$RESULT_CM_TP == "F"],
                                                                  sep="_")
  qwResult$FIELD_RESULT_CM <- gsub("NA_"," ",qwResult$FIELD_RESULT_CM)
  qwResult$FIELD_RESULT_CM <- gsub("_NA"," ",qwResult$FIELD_RESULT_CM)
  
  qwResult$LAB_RESULT_CM[qwResult$RESULT_CM_TP == "L"] <- paste(qwResult$FIELD_RESULT_CM[qwResult$RESULT_CM_TP == "L"],
                                                                qwResult$RESULT_CM_TX[qwResult$RESULT_CM_TP == "L"],
                                                                sep="_")
  qwResult$LAB_RESULT_CM <- gsub("NA_"," ",qwResult$LAB_RESULT_CM)
  qwResult$LAB_RESULT_CM <- gsub("_NA"," ",qwResult$LAB_RESULT_CM)
  
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
                         "LAB_RESULT_CM",
                         "FIELD_RESULT_CM",
                         "LAB_STD_DEV_VA",
                         "ANL_ENT_CD")]
  
  #Get rid of invalid remark codes
  qwResult$REMARK_CD[qwResult$REMARK_CD == "Sample"] <- NA
  
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
                                qwData$SAMPLE_START_TZ_CD,
                                qwData$TM_DATUM_RLBLTY_CD,
                                qwData$COLL_ENT_CD,
                                qwData$SAMPLE_ID,
                                qwData$SIDNO_PARTY_CD,
                                stringsAsFactors=FALSE))
  
  qwsampleheader <- c("sample.integer",  "user.code",  "agency",  "site.no",  "start.date",  "end.date",  "medium",  "lab.id",
                      "project.code",  "aquifer.code",	"sample.type",	"analysis.status", "BLANKcol", "hydrologic.cond",
                      "hydrologic.event", "tissue.id",	"body.part",	"lab.comment.",	"field.comment", "time.datum",	"time.datum.reliability",
                      "collecting.agency.code","sample.id", "SIDNO.party.cd")
  
  colnames(qwSample)<-qwsampleheader
  
  
  ###Remove extra empty character space from medium to make it match medium in data file of 2-3 char
  #qwSample$medium <- (gsub(" ", "", qwSample$medium))
  qwSample$RECORD_NO <- qwSample$sample.integer
  qwSample$sample.integer <- seq(1:nrow(qwSample))
  
  ###Format time to character
  qwSample$start.date <- format(qwSample$start.date,format="%Y%m%d%H%M")
  
  #Per QWDATA user manual sect. 3.8, it is suggested that the time datum be left blank
  # see http://nwis.usgs.gov/nwisdocs5_2/qw/QW_Sect3.8.pdf
  qwSample$time.datum <- NA
  ###Merge in sample integer and drop UID and recordno columns
  qwResult <- dplyr::left_join(qwSample[c("RECORD_NO","sample.integer")],qwResult,by="RECORD_NO")
  
  
  #Drop duplicates
  qwResult <- unique(qwResult)
  
  
  ###Drop old RECORD_NO columns
  if(keepRecordNo == FALSE)
  {
    qwResult$RECORD_NO <- NULL
    qwSample$RECORD_NO <- NULL
  }
  
  ###NULL out all values except DQI code and field result comment
  
  # qwResult[c(3,4,5,6,7,8,9,10,12,13,14,15,16,17,19,20)] <- NA
  # qwSample[c(18,19)] <- NA
  
  ###Format columns to work with QWDATA
  
  ###Add back in the leading 0 before decimal
  qwResult$RESULT_VA <- sub('^(-)?[.]', '\\10.', qwResult$RESULT_VA)
  qwResult$RPT_LEV_VA <- sub('^(-)?[.]', '\\10.', qwResult$RPT_LEV_VA)
  qwResult$LAB_STD_DEV_VA <- sub('^(-)?[.]', '\\10.', qwResult$LAB_STD_DEV_VA)
  
  #Format date times
  qwResult$ANL_DT <- format(as.Date(qwResult$ANL_DT),format="%Y%m%d")
  qwResult$ANL_DT[qwResult$ANL_DT=="NA"] <- NA
  qwResult$PREP_DT <- format(as.Date(qwResult$PREP_DT),format="%Y%m%d")
  qwResult$PREP_DT[qwResult$PREP_DT=="NA"] <- NA
  
  
  if(writeFiles == TRUE)
  {
    write.table(qwSample,file=qwsamplename,sep="\t", col.names = F, row.names = F,na="", quote = FALSE)
    write.table(qwResult,file=qwresultname,sep="\t", col.names = F, row.names = F, na="",quote = FALSE)
  }
  
  return(list(qwsample = qwSample,
              qwresult = qwResult))
}
