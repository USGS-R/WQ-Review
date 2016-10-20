#' dataUpload formats files for upload into QWData
#' 
#' Assists in formatting data for upload into QWData. Use templateExport to generate spreadsheet templates to create labfile and pcode file.
#' 
#' @param qwsampletype Numeric of type of qwsample file. Options 1: Pull qwsample form NWIS, 2:QWSample from file, 3: batch qwsample file
#' @param labfile Character string containing path to the labfile, must be .csv format
#' @param pcodefile Character string containing path to the pcodefile, must be .csv format
#' @param DSN Character string containing local NWIS server name
#' @param env.db Environmental database number
#' @param qa.db QA database number
#' @param writeFiles write out qwresult and qwsample files to text files in working directory
#' @param qwresultname Name of qwresult file
#' @param qwsamplename Name of qwsample file
#' @param qwsamplefile Character string containing path to the qwsample file if using qwsampletype = 2, must be .csv format
#' @param qwsample.begin.date Character string (yyyy-mm-dd) of beggining date to subset qwsample file to
#' @param qwsample.end.date Character string (yyyy-mm-dd) of ending date to subset qwsample file to
#' @param censor Logical. If TRUE results are censored to lrl provided in pcode file
#' @param agencycode Manual qwsample entry
#' @param labid Manual qwsample entry
#' @param projectcode Manual qwsample entry
#' @param aquifercode Manual qwsample entry
#' @param analysisstatus Manual qwsample entry
#' @param analysissource Manual qwsample entry
#' @param hydrologiccond Manual qwsample entry
#' @param hydrologicevent Manual qwsample entry
#' @param tissue Manual qwsample entry
#' @param bodypart Manual qwsample entry
#' @param labcomment Manual qwsample entry   
#' @param fieldcomment Manual qwsample entry
#' @param timedatum Manual qwsample entry
#' @param datumreliability Manual qwsample entry
#' @param colagencycode Manual qwsample entry
#' @importFrom reshape2 melt
#' @importFrom reshape2 dcast
#' @importFrom dplyr left_join
#' @import RODBC 
#' @export
#' 
dataUpload <- function(qwsampletype,
                       labfile,
                       pcodefile,
                       DSN = "NWISCO",
                       env.db = "01",
                       qa.db = "02",
                       writeFiles = FALSE,
                       qwresultname="",
                       qwsamplename="",
                       qwsamplefile = "",
                       qwsample.begin.date = NA,
                       qwsample.end.date = NA,
                       censor = FALSE,
                       agencycode = "USGS",
                       labid = NA,
                       projectcode = NA,
                       aquifercode = NA,
                       analysisstatus = NA,
                       analysissource = NA,
                       hydrologiccond = "A",
                       hydrologicevent = 9,
                       tissue = NA,
                       bodypart = NA,
                       labcomment = NA,    
                       fieldcomment = NA,
                       timedatum = NA,
                       datumreliability = NA,
                       colagencycode = "USGS"){
  
  ###Convert times to POSIXct, this will put it in GMT so it might not match exactly with what user wants, need to provide a day buffer on either side
  if(!is.na(qwsample.begin.date) && !is.na(qwsample.end.date)) 
  {
  qwsample.begin.date <- as.POSIXct(qwsample.begin.date) - 60*60*24
  qwsample.end.date <- as.POSIXct(qwsample.end.date) + 60*60*24
  } else{}
  
  data<-read.csv(labfile,header=TRUE,
                 colClasses = c("character","character","character","character","character","character"),na.strings = "NA")
  numdatacols <- ncol(data)-6  
  

  ##Pad times with leading 0s if truncated by excel
  if(!(is.null(data$Sample.time..hhmm.[which(nchar(data$Sample.time..hhmm.) == 3)])))
  {
  data$Sample.time..hhmm.[which(nchar(data$Sample.time..hhmm.) == 3)] <- paste("0",data$Sample.time..hhmm.[which(nchar(data$Sample.time..hhmm.) == 3)],sep="")
  }
  
  pcodedata <- read.csv(pcodefile,header=TRUE,
                        colClasses = "character",na.strings = "NA")
  names(pcodedata) <- c("pcode",
                        "qa.code",
                        "method.code",
                        "rounding.code",
                        "qualifiers",
                        "reporting.level",
                        "reporting.level.type",
                        "dqi.code",
                        "null.value.qualifier",
                        "prep.set.no",
                        "analytical.set.no",
                        "analysis.date",
                        "prep.date",
                        "lab.comment",
                        "field.comment",
                        "lab.stdev",
                        "analyzing.entity.code"
  )
  STAID <- data[[1]]
  
  #Change to a list that SQL can understand. SQL requires a parenthesized list of expressions, so must look like c('05325000', '05330000') for example
  STAID.list <- paste("'", STAID, "'", sep="", collapse=",")
  
  if (qwsampletype == 1)
  {
    # Open the odbc connection
    Chan1 <- odbcConnect(DSN)
    
    ##################
    ###DATABASE 1#####
    ##################
    
    #get the record numbers
    Query <- paste("select * from ", DSN, ".QW_SAMPLE_",env.db," where site_no IN (", STAID.list, ")", sep="")
    Samples <- sqlQuery(Chan1, Query, as.is=T)

    ##Format times into GMT and correct of daylight savings offset according to location
    ##Weather or not to apply daylight savings is in the std.time.code column, which is from the SAMPLE_START_LOCAL_TM_FG NWIS parameter
    ##e.g. in Colorado, SAMPLE_START_LOCAL_TM_FG = Y, timezone = MDT, SAMPLE_START_LOCAL_TM_FG = N, timezone = MST
    
    Samples$SAMPLE_START_DT <- convertTime(datetime = Samples$SAMPLE_START_DT,
                                           timezone = Samples$SAMPLE_START_TZ_CD,
                                           daylight = Samples$SAMPLE_START_LOCAL_TM_FG)
   
    ###SAMPLE TIMES PULLED THIS WAY ARE NOT THE ACTUAL SAMPLE TIME. THESE MUST BE CONVERTED USING THE SAMPLE_START_TZ_CD OR THEY WILL NOT MATCH ACTUAL SAMPLE TIMES
    ###THERE IS CODE BELOW TO DO THIS
    
    
      QWSample1 <- data.frame(Samples$RECORD_NO,
                            "*UNSPECIFIED*",
                            Samples$AGENCY_CD,
                            Samples$SITE_NO,
                            Samples$SAMPLE_START_DT,
                            Samples$SAMPLE_END_DT,
                            Samples$MEDIUM_CD,
                            Samples$LAB_NO,
                            Samples$PROJECT_CD,
                            Samples$AQFR_CD,
                            Samples$SAMP_TYPE_CD,
                            Samples$ANL_STAT_CD,
                            "",
                            Samples$HYD_COND_CD,
                            Samples$HYD_EVENT_CD,
                            Samples$TU_ID,
                            Samples$BODY_PART_ID,
                            "",
                            "",
                            "",
                            "",
                            Samples$COLL_ENT_CD,
                            Samples$SAMPLE_ID,
                            Samples$SIDNO_PARTY_CD,
                            stringsAsFactors=FALSE)
    ##################
    ###DATABASE 2#####
    ##################
    
    #get the record numbers
    Query <- paste("select * from ", DSN, ".QW_SAMPLE_",qa.db," where site_no IN (", STAID.list, ")", sep="")
    Samples <- sqlQuery(Chan1, Query, as.is=T)

    ##Format times into GMT and correct of daylight savings offset according to location
    ##Weather or not to apply daylight savings is in the std.time.code column, which is from the SAMPLE_START_LOCAL_TM_FG NWIS parameter
    ##e.g. in Colorado, SAMPLE_START_LOCAL_TM_FG = Y, timezone = MDT, SAMPLE_START_LOCAL_TM_FG = N, timezone = MST
    
    Samples$SAMPLE_START_DT <- convertTime(datetime = Samples$SAMPLE_START_DT,
                                       timezone = Samples$SAMPLE_START_TZ_CD,
                                       daylight = Samples$SAMPLE_START_LOCAL_TM_FG)
    ####Close ODBC Connection
    odbcClose(Chan1)
    ######################
    
    QWSample2 <- data.frame(Samples$RECORD_NO,
                            "*UNSPECIFIED*",
                            Samples$AGENCY_CD,
                            Samples$SITE_NO,
                            Samples$SAMPLE_START_DT,
                            Samples$SAMPLE_END_DT,
                            Samples$MEDIUM_CD,
                            Samples$LAB_NO,
                            Samples$PROJECT_CD,
                            Samples$AQFR_CD,
                            Samples$SAMP_TYPE_CD,
                            Samples$ANL_STAT_CD,
                            "",
                            Samples$HYD_COND_CD,
                            Samples$HYD_EVENT_CD,
                            Samples$TU_ID,
                            Samples$BODY_PART_ID,
                            "",
                            "",
                            "",
                            "",
                            Samples$COLL_ENT_CD,
                            Samples$SAMPLE_ID,
                            Samples$SIDNO_PARTY_CD,
                            stringsAsFactors=FALSE)
    
    qwsample <- rbind(QWSample1,QWSample2)
    qwsampleheader <- c("sample.integer",  
                        "user.code",  
                        "agency",  
                        "site.no",  
                        "start.date",  
                        "end.date",  
                        "medium",  
                        "labid",
                        "project.code",  
                        "aquifer.code",	
                        "sample.type",	
                        "analysis.status",
                        "BLANK",
                        "hydrologic.cond",
                        "hydrologic.event",
                        "tissue",
                        "body.part",
                        "lab.comment.",
                        "field.comment",
                        "time.datum",
                        "datum.reliability",
                        "collecting.agency.code",
                        "sample.id",
                        "sidno.party.cd")
    colnames(qwsample)<-qwsampleheader
    

    

    if(!is.na(qwsample.begin.date) && !is.na(qwsample.end.date)) 
    {
      qwsample <- subset(qwsample, start.date >= qwsample.begin.date & start.date <= qwsample.end.date)
    } else{}

    
    qwsample$start.date <- format(qwsample$start.date,"%Y%m%d%H%M")

    qwsample$sample.integer <- seq(1:nrow(qwsample))
    qwsample$UID <- paste(qwsample$site.no,qwsample$start.date,qwsample$medium,sep="")
    qwsample$UID <- gsub(" ","",qwsample$UID)
    #qwsample$sample.id <- NA
    #qwsample$sidno.party.cd <- NA

    
    
    
  }else if (qwsampletype == 2)
  {
    qwsample <- read.delim(file = qwsamplefile,header=FALSE, sep = "\t" )
    qwsampleheader <- c("sample.integer",  "user.code",  "agency",  "site.no",  "start.date",  "end.date",  "medium",  "labid",
                        "project.code",  "aquifer.code",	"sample.type",	"analysis.status",	"analysis.source",	"hydrologic.cond",
                        "hydrologic.event",	"tissue",	"body.part",	"lab.comment.",	"field.comment",	"time.datum",	"datum.reliability",
                        "collecting.agency.code")
    colnames(qwsample)<-qwsampleheader
    qwsample$UID <- paste(qwsample$site.no,qwsample$start.date,qwsample$medium,sep="")
  } else if (qwsampletype == 3)
  {
    
    ###Manual QWSample generator
    
    qwsample <- matrix(nrow=nrow(data),ncol=22)
    qwsample <- (as.data.frame(qwsample))
    names(qwsample) <-  c("sample.integer",  
                          "user.code",  
                          "agency",  
                          "site.no",  
                          "start.date",  
                          "end.date",  
                          "medium",  
                          "labid",
                          "project.code",  
                          "aquifer.code",	
                          "sample.type",	
                          "analysis.status",
                          "BLANK",
                          "hydrologic.cond",
                          "hydrologic.event",
                          "tissue",
                          "body.part",
                          "lab.comment.",
                          "field.comment",
                          "time.datum",
                          "datum.reliability",
                          "collecting.agency.code",
                          "sample.id",
                          "sidno.party.cd") 
    qwsample$sample.integer <- seq(1:nrow(qwsample))
    qwsample$user.code <- "*UNSPECIFIED*"
    qwsample$agency <- agencycode
    qwsample$site.no <- data$USGS.SID
    qwsample$start.date <- paste(data$Sample.start.date..yyyymmdd.,data$Sample.start.time..hhmm.,sep="")
    qwsample$end.date <- paste(data$Sample.end.date..yyyymmdd.,data$Sample.end.time..hhmm.,sep="")
    qwsample$medium <- data$medium.code
    qwsample$labid <- labid
    qwsample$project.code <- projectcode
    qwsample$aquifer.code <- aquifercode
    ###Get proper coding for sample type###
    ##Asign temporary UIDs for qwsample and data , THESE ARE NOT TRULY UNIQUE SINCE JUST ID and DATE
    data$UID <- paste(data$USGS.SID, data$Sample.start.date..yyyymmdd.)
    qwsample$UID <- paste(data$USGS.SID,data$Sample.start.date..yyyymmdd.)
    ###Go through data and check for reps
    for (i in 1:nrow(data))
    {
      if(qwsample$medium[i] == "OAQ")
      {
        qwsample$sample.type[i] <- 2
      } else if (qwsample$medium[i] == "WSQ" | qwsample$medium[i] == "WGQ")
      {
        qwsample$sample.type[i] <- 7
      } else if(qwsample$medium[i] == "WS" | qwsample$medium[i] == "WG")
      {
        ###Asign coding of 7 to an evnironmental sample with a rep.
        ###This will not always work if there are multiple reps and environmental samples collected at the same site on the same day
        if (qwsample$UID[i] %in% qwsample$UID[which(qwsample$medium == "WSQ" | qwsample$medium == "WGQ")] )
        {
          qwsample$sample.type[i] <- 7
        } else (qwsample$sample.type[i] <- 9)
      }
    }
    ###Get rid of temp UIDs
    data$UID <- NULL
    qwsample$UID <- NULL
    ############################################
    qwsample$analysis.status <- analysisstatus
    qwsample$analysis.source <- analysissource
    qwsample$hydrologic.cond <- hydrologiccond
    qwsample$hydrologic.event <- hydrologicevent
    qwsample$tissue <- tissue
    qwsample$body.part <- bodypart
    qwsample$lab.comment. <- labcomment         
    qwsample$field.comment <- fieldcomment
    qwsample$time.datum <- timedatum
    qwsample$datum.reliability <- datumreliability
    qwsample$collecting.agency.code <- colagencycode
    qwsample$UID <- paste(qwsample$site.no,qwsample$start.date,qwsample$medium,sep="")
    
  }
  #####################
  #######QWRESULT######
  #####################
  
  ###Rearrange data frame into qwresult format
  meltdata<-melt(data,id.vars=colnames(data[,1:6]))
  meltdata$variable <- (gsub("X", "", meltdata$variable))
  colnames(meltdata) <- c("USGS.SID" , "Sample.start.date..yyyymmdd.",	"Sample.start.time..hhmm.","medium","Sample.end.date..yyyymmdd.",  "Sample.end.time..hhmm.",	"pcode",	"result")
  ###Make a unique sample ID out of ID date time medium code
  meltdata$UID <- paste(meltdata$USGS.SID,meltdata$Sample.start.date..yyyymmdd.,meltdata$Sample.start.time..hhmm.,meltdata$medium,sep="")
  ###Join the lab data to the pcode meta data in the pcode file
  mergeddata <- (dplyr::left_join(meltdata, pcodedata, by = 'pcode'))
  ###Make an empty QWresult dataframe
  qwresult <- matrix(nrow=nrow(mergeddata),ncol=20)
  qwresult <- (as.data.frame(qwresult))
  
  names(qwresult) <- c("UID",
                       "pcode",
                       "result",
                       "qual.code",
                       "qa.code",
                       "method.code",
                       "rounding.code",
                       "qualifiers",
                       "reporting.level",
                       "reporting.level.type",
                       "dqi.code",
                       "null.value.qualifier",
                       "prep.set.no",
                       "analytical.set.no",
                       "analysis.date",
                       "prep.date",
                       "lab.comment",
                       "field.comment",
                       "lab.stdev",
                       "analyzing.entity.code")
  
  ###Populate qwresult
  qwresult$UID<-mergeddata$UID
  qwresult$UID <- gsub(" ","",qwresult$UID)
  qwresult$pcode<-mergeddata$pcode
  qwresult$result<-mergeddata$result
  
  ###Check for < and strip out and put in proper column, then convert to numeric
  qwresult$qual.code[grep("<",qwresult$result)] <- "<"
  qwresult$result <- gsub("<","",qwresult$result)
  qwresult$result <- gsub(" ","",qwresult$result)
  ###Check for e and strip out and put in proper column, then convert to numeric
  qwresult$qual.code[grep("e",qwresult$result)] <- "e"
  qwresult$result <- gsub("e","",qwresult$result)
  qwresult$result <- gsub(" ","",qwresult$result)
  
  ###Format result as numeric for censoring
  qwresult$result <- as.numeric(qwresult$result)

  ###Populate rest of QWResult
  qwresult$qa.code<-mergeddata$qa.code
  qwresult$method.code<-mergeddata$method.code
  qwresult$rounding.code<-mergeddata$rounding.code
  qwresult$qualifiers<-mergeddata$qualifiers
  qwresult$reporting.level<-as.numeric(mergeddata$reporting.level)
  qwresult$reporting.level.type<-mergeddata$reporting.level.type
  qwresult$dqi.code<-mergeddata$dqi.code
  qwresult$null.value.qualifier<-mergeddata$null.value.qualifier
  qwresult$prep.set.no<-mergeddata$prep.set.no
  qwresult$analytical.set.no<-mergeddata$analytical.set.no
  qwresult$analysis.date<-mergeddata$analysis.date
  qwresult$prep.date<-mergeddata$prep.date
  qwresult$lab.comment<-mergeddata$lab.comment
  qwresult$field.comment<-mergeddata$field.comment
  qwresult$lab.stdev<-mergeddata$lab.stdev
  qwresult$analyzing.entity.code<-mergeddata$analyzing.entity.code
  
  ###Censor data if applicable and add a < code
  if (censor){
    qwresult$qual.code[which(qwresult$result <= qwresult$reporting.level)] <- "<"
    qwresult$result[which(qwresult$result <= qwresult$reporting.level)] <- qwresult$reporting.level[which(qwresult$result <= qwresult$reporting.level)]
    
  }else{}
  
  ###Matchup sample integers between the qwsample and qwresult file
  qwresult <- dplyr::left_join(qwresult, qwsample[c("UID","sample.integer")], by = "UID")
  ###Fill in missing sample integers with UID to track down missing/mislabeled samples
  qwresult$sample.integer[which(is.na(qwresult$sample.integer))] <- qwresult$UID[which(is.na(qwresult$sample.integer))]
  
  qwresult$UID <- qwresult$sample.integer
  qwresult$sample.integer <- NULL
  qwsample$UID <- NULL
  colnames(qwresult)[1] <- "sample.integer"

  
  ###Remove rows with missing results to avoid annoying watlist errors from NWIS
  qwresult <- qwresult[qwresult$result != "",]
  
  ###Wire qwresult and qwsample files
  if(writeFiles == TRUE)
  {
  write.table(qwsample,file=qwsamplename,sep="\t", col.names = F, row.names = F,na="", quote = FALSE)
  write.table(qwresult,file=qwresultname,sep="\t", col.names = F, row.names = F, na="",quote = FALSE)
  }
  return(list(qwsample=qwsample,qwresult=qwresult))
}
