#' Function to pull data from NWIS
#' 
#' Pulls data from NWIS internal servers using ODBC connection and returns a list containing dataframes.
#' @param DSN A character string containing the DSN for your local server
#' @param env.db A character string containing the database number of environmental samples
#' @param qa.db A character string containing the database number of QA samples
#' @param STAIDS A character vector of stations IDs. Agency code defaults to "USGS" unless appended to the beginning of station ID with a dash, e.g. "USGS-123456". 
#' @param dl.parms A character vector of pcodes to pull data, e.g. c("00300","00915") or NWIS parameter groups specified in details section.
#' @param parm.group.check A logical of weather or not to use NWIS parameter groups. If TRUE, must use NWIS parameter group names in dl.parms
#' @param begin.date Character string containing beginning date of data pull (yyyy-mm-dd)
#' @param end.date Character string containing ending date of data pull (yyyy-mm-dd)
#' @return Returns a list containing two dataframes: PlotTable and DataTable. 
#' PlotTable contains all data pulled from NWIS along with all assosciated metadata in by-result format. 
#' DataTable contains all data pulled from NWIS in wide (sample-result) format, an easier format for import into spreadsheet programs.
#' Dataframe names will be changed to more appropriate values in future package updates.
#' @details 
#' NWIS parameter groups are as follows: All = "All",
#'physical = "PHY",
#'cations = "INM",
#'anions = "INN",
#'nutrients = "NUT",
#'microbiological = "MBI",
#'biological = "BIO",
#'metals = "IMM",
#'nonmetals = "IMN",
#'pesticides = "TOX",
#'pcbs="OPE",
#'other organics = "OPC",
#'radio chemicals = "RAD",
#'stable isotopes = "XXX",
#'sediment = "SED",
#'population/community = "POP"
#' @examples
#' \dontrun{
#' #Will not run unless connected to NWISCO
#' qw.data <- readNWISodbc(DSN="NWISCO",
#'                              env.db = "01",
#'                              qa.db = "02",
#'                              STAIDS = c("06733000","09067005"),
#'                              dl.parms="All",
#'                              parm.group.check=TRUE,
#'                              begin.date = "2005-01-01",
#'                              end.date = "2015-10-27")
#'                              }
#' @import RODBC
#' @importFrom reshape2 dcast
#' @importFrom reshape2 melt
#' @importFrom dplyr left_join
#' @importFrom lubridate yday
#' @export

readNWISodbc <- function(DSN,
                         env.db = "01",
                         qa.db = "02",
                         STAIDS ,
                         dl.parms = "All",
                         parm.group.check = TRUE,
                         begin.date = NA,
                         end.date = NA)
{
        ##Check that the 32 bit version of r is running
        if(Sys.getenv("R_ARCH") != "/i386"){
                print("You are not running 32 bit R. This function requires R be run in 32 bit mode for the ODBC driver to function properly. Please restart R in 32bit mode.")
                stop("You are not running 32 bit R. This function requires R be run in 32 bit mode for the ODBC driver to function properly. Please restart R in 32bit mode.")
        }
        
        ###Check for valid inputs
        if(is.null(DSN)){
                print("A valid datasource name must be entered for the ODBC connection")
                stop("A valid datasource name must be entered for the ODBC connection")
        }
        if(is.null(STAIDS)){
                print("A valid datasource name must be entered for the ODBC connection")
                stop("You must enter atleast one site number to pull data")
        }
        
  odbcCloseAll()
  

          ##parse out agency code from station ID
          agencySTAIDS <- read.delim(text=STAIDS,header=FALSE,sep="-",fill=TRUE,colClasses = "character")
          
          
          if(ncol(agencySTAIDS) > 1)
          {
                  
          ###Fill in empty fields with USGS agency code if ommitted
          agencySTAIDS[which(agencySTAIDS[2] == ""),2] <- NA
          agencySTAIDS[is.na(agencySTAIDS[2]),2] <- as.character(agencySTAIDS[is.na(agencySTAIDS[2]),1])
          agencySTAIDS[!is.na(suppressWarnings(as.numeric(agencySTAIDS[,1]))),1] <- "USGS"
          
          ###Pad Station IDs with spaces to make 15 characters long
          agencySTAIDS[,2] <-  as.character(
                  lapply(agencySTAIDS[,2],FUN=function(x){
                  pad <- rep(" ",15-nchar(x))
                  pad <- paste(pad,sep="",collapse="")
                  x <- paste(x,pad,sep="")
                  return(x)
                  })
          )
                  
                uniqueSTAIDS <- paste0(agencySTAIDS[,1],agencySTAIDS[,2])
                
                
                #Change to a list that SQL can understand. SQL requires a parenthesized list of expressions, so must look like c('05325000', '05330000') for example
                STAID.list <- paste("'", agencySTAIDS[,2], "'", sep="", collapse=",")
                
          } else{STAIDS <-  as.character(
                  lapply(STAIDS,FUN=function(x){
                          pad <- rep(" ",15-nchar(x))
                          pad <- paste(pad,sep="",collapse="")
                          x <- paste(x,pad,sep="")
                          return(x)
                  })
                  )
                  uniqueSTAIDS <- paste0("USGS",STAIDS)
                  
                  #Change to a list that SQL can understand. SQL requires a parenthesized list of expressions, so must look like c('05325000', '05330000') for example
                  STAID.list <- paste("'", STAIDS, "'", sep="", collapse=",")
                  }


  
  
  
  #############################################################################
  Chan1 <- odbcConnect(DSN)###Start of ODBC connection
  #############################################################################
  
  ##################
  ###Env Database###
  ##################
  # First get the site info--need column SITE_ID
  Query <- paste("select * from ", DSN, ".SITEFILE_",env.db," where site_no IN (", STAID.list, ")",sep="")
  SiteFile <- sqlQuery(Chan1, Query, as.is=T)
  
  #Make unique AgencyCd/sitefile key
  SiteFile$agencySTAID <- gsub(" ","",paste0(SiteFile$AGENCY_CD,SiteFile$SITE_NO))
  
  ##Subset SiteFile to unique agency code site ID pair
  SiteFile <- subset(SiteFile,agencySTAID %in% gsub(" ","",uniqueSTAIDS))
  
  ##Check for samples and throw warning if none
  if(nrow(SiteFile) == 0){
          print("Site does not exist in sitefile, check site number input")
          warning("Site does not exist in environmantal database sitefile, check site number input")
  }
  
  #get the QWSample file
  Query <- paste("select * from ", DSN, ".QW_SAMPLE_",env.db," where site_no IN (", STAID.list, ")", sep="")
  Samples <- sqlQuery(Chan1, Query, as.is=T)
  
  ##Make unique AgencyCd/sitefile key
  Samples$agencySTAID <- gsub(" ","",paste0(Samples$AGENCY_CD,Samples$SITE_NO))
  
  ##Subset Samples to unique agency code site ID pair
  Samples <- subset(Samples,agencySTAID %in% gsub(" ","",uniqueSTAIDS))
  
  ##Check if samples were pulled and quit if no
  if(nrow(Samples) == 0) {
          #print("No samples exist in your local NWIS database for site number specified, check data criteria")
          warning("No samples exist in your local environmantal NWIS database for site number specified, check data criteria")
  }
  
  
  #Subset records to date range, times are in GMT, which is the universal NWIS time so that you can have a consistant date-range accross timezones.
  #Time is corrected to local sample timezone before plotting
  
  Samples$SAMPLE_START_DT <- as.POSIXct(Samples$SAMPLE_START_DT, tz="GMT")
  if(!is.na(begin.date) && !is.na(end.date)) {
    Samples <- subset(Samples, SAMPLE_START_DT >= as.POSIXct(begin.date) & SAMPLE_START_DT <= as.POSIXct(end.date))
  }else {} 
  
  if(nrow(Samples) == 0) {
          print("No samples exist in your local NWIS database for the date range specified, check data criteria")
          stop("No samples exist in your local NWIS database for the date range specified, check data criteria")
  }
  #get the QWResult file using the record numbers
  ##SQL is limited to 1000 entries in querry
  ##Get the number of 1000 bins in querry
  breaks <- ceiling(length(Samples$RECORD_NO)/1000)
  
  ##Run SQL queries
  for(i in 1:breaks)
  {
    j <- i-1
    ###Get the 1st 1000
    if(i == 1)
    {
      ####Get the results
      records.list <- paste("'", Samples$RECORD_NO[1:1000], "'", sep="", collapse=",")
      Query <- paste("select * from ", DSN, ".QW_RESULT_",env.db," where record_no IN (", records.list, ")", sep="")
      Results <- sqlQuery(Chan1, Query, as.is=T)
      ####Get result level commments
      Query <- paste("select * from ", DSN, ".QW_RESULT_CM_",env.db," where record_no IN (", records.list, ")", sep="")
      resultComments <- sqlQuery(Chan1, Query, as.is=T)
      ####Get sample level comments
      Query <- paste("select * from ", DSN, ".QW_SAMPLE_CM_",env.db," where record_no IN (", records.list, ")", sep="")
      sampleComments <- sqlQuery(Chan1, Query, as.is=T)
      ####Get qualifier codes
      Query <- paste("select * from ", DSN, ".QW_VAL_QUAL_",env.db," where record_no IN (", records.list, ")", sep="")
      resultQualifiers <- sqlQuery(Chan1, Query, as.is=T)
    } else if(i > 1 & (j*1000+1000) < length(Samples$RECORD_NO))
      ###Get the middle ones
    {
      j <- j * 1000+1
      records.list <- paste("'", Samples$RECORD_NO[j:(j+999)], "'", sep="", collapse=",")
      Query <- paste("select * from ", DSN, ".QW_RESULT_",env.db," where record_no IN (", records.list, ")", sep="")
      Results <- rbind(Results,sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_RESULT_CM_",env.db," where record_no IN (", records.list, ")", sep="")
      resultComments <- rbind(resultComments,sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_SAMPLE_CM_",env.db," where record_no IN (", records.list, ")", sep="")
      sampleComments <- rbind(sampleComments,sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_VAL_QUAL_",env.db," where record_no IN (", records.list, ")", sep="")
      resultQualifiers <- rbind(resultQualifiers,sqlQuery(Chan1, Query, as.is=T))
    } else if (i > 1 && (j*1000+1000) > length(Samples$RECORD_NO))
    {
      ###Get the last ones
      j <- j * 1000+1
      records.list <- paste("'", Samples$RECORD_NO[j:length(Samples$RECORD_NO)], "'", sep="", collapse=",")
      Query <- paste("select * from ", DSN, ".QW_RESULT_",env.db," where record_no IN (", records.list, ")", sep="")
      Results <- rbind(Results,sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_RESULT_CM_",env.db," where record_no IN (", records.list, ")", sep="")
      resultComments <- rbind(resultComments,sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_SAMPLE_CM_",env.db," where record_no IN (", records.list, ")", sep="")
      sampleComments <- rbind(sampleComments,sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_VAL_QUAL_",env.db," where record_no IN (", records.list, ")", sep="")
      resultQualifiers <- rbind(resultQualifiers,sqlQuery(Chan1, Query, as.is=T))
    } else{}
  }
  ###Join comments to results
  Results <- dplyr::left_join(Results,sampleComments,by="RECORD_NO")
  Results <- dplyr::left_join(Results,resultComments,by=c("RECORD_NO","PARM_CD"))
  Results <- dplyr::left_join(Results,resultQualifiers,by=c("RECORD_NO","PARM_CD"))
  
  Results$Val_qual <- paste(Results$RESULT_VA,Results$REMARK_CD, sep = " ")
  Results$Val_qual <- gsub("NA","",Results$Val_qual)
  
  #Get list of parm names
  #SQL is limited to 1000 entries in querry
  ##Get the number of 1000 bins in querry
  breaks <- ceiling(length(unique(Results$PARM_CD))/1000)
  
  ##Run SQL queries
  for(i in 1:breaks)
  {
    j <- i-1
    ###Get the 1st 1000
    if(i == 1)
    {
      parms.list <- paste("'", unique(Results$PARM_CD)[1:1000], "'", sep="", collapse=",")
      Query <- paste("select * from ", DSN, ".PARM where PARM_CD IN (", parms.list, ")", sep="")
      parms <- sqlQuery(Chan1, Query, as.is=T)
    } else if(i > 1 && (j*1000+1000) < length(unique(Results$PARM_CD)))
      ###Get the middle ones
    {
      j <- j * 1000+1
      parms.list <- paste("'", unique(Results$PARM_CD)[j:(j+999)], "'", sep="", collapse=",")
      Query <- paste("select * from ", DSN, ".PARM where PARM_CD IN (", parms.list, ")", sep="")
      parms <- rbind(parms,sqlQuery(Chan1, Query, as.is=T))
    } else if (i > 1 && (j*1000+1000) > length(unique(Results$PARM_CD)))
    {
      ###Get the last ones
      j <- j * 1000+1
      parms.list <- paste("'", unique(Results$PARM_CD)[j:length(unique(Results$PARM_CD))], "'", sep="", collapse=",")
      Query <- paste("select * from ", DSN, ".PARM where PARM_CD IN (", parms.list, ")", sep="")
      parms <- rbind(parms,sqlQuery(Chan1, Query, as.is=T))
    } else{}
  }
  parms <- parms[c("PARM_CD","PARM_SEQ_GRP_CD","PARM_DS","PARM_NM","PARM_SEQ_NU")]
  
  
  
  #station names and dates
  name_num <- SiteFile[c("SITE_NO","STATION_NM","DEC_LAT_VA","DEC_LONG_VA","HUC_CD")]
  Sample_meta <- dplyr::left_join(Samples, name_num,by="SITE_NO")
  Sample_meta$RECORD_NO <- paste(Sample_meta$RECORD_NO,env.db,sep="_")
  
  ###Reorder sampel meta columns
  Sample_meta <- Sample_meta[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_END_DT","MEDIUM_CD",
                               "SAMP_TYPE_CD","AGENCY_CD","PROJECT_CD","AQFR_CD","LAB_NO","HYD_EVENT_CD",
                               "SAMPLE_CR","SAMPLE_CN","SAMPLE_MD","SAMPLE_MN",
                               "DEC_LAT_VA","DEC_LONG_VA","HUC_CD",
                               "SAMPLE_START_SG","SAMPLE_START_TZ_CD","SAMPLE_START_LOCAL_TM_FG",
                               "SAMPLE_END_SG","SAMPLE_END_TZ_CD","SAMPLE_END_LOCAL_TM_FG","SAMPLE_ID",
                               "TM_DATUM_RLBLTY_CD","ANL_STAT_CD","HYD_COND_CD",
                               "TU_ID","BODY_PART_ID","COLL_ENT_CD","SIDNO_PARTY_CD")]
  
  #Sample_meta <- Sample_meta[c("SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_START_TZ_CD","SAMPLE_START_LOCAL_TM_FG","SAMPLE_END_DT","MEDIUM_CD","RECORD_NO","LAB_NO","PROJECT_CD","AQFR_CD")]
  
  #join tables so parm names are together
  Results<- dplyr::left_join(Results,parms,by="PARM_CD")
  
  #Paste database number ot record number to make unique
  Results$RECORD_NO <- paste(Results$RECORD_NO, env.db,sep="_")
  
  #Subset results to selected parmeters
  if (parm.group.check == TRUE) 
  {
    if(!("All" %in% dl.parms))
    {
      Results <- subset(Results, PARM_SEQ_GRP_CD %in% dl.parms)
    } else{ Results<- dplyr::left_join(Results,parms,by="PARM_CD")} 
  } else {Results <- subset(Results, PARM_CD %in% dl.parms)}
  
  if(nrow(Results) == 0) {
          print("No valid parameter codes specified. Check input criteria")
          stop("No valid parameter codes specified. Check input criteria")
  }
  
  
  ###Make dataframe as record number and pcode. MUST HAVE ALL UNIQUE PCODE NAMES
  ##Remove duplicate results from data table
  DataTable1 <- subset(Results, !(DQI_CD %in% c("Q","X")))
  DataTable1 <- DataTable1[!(duplicated(DataTable1[c("RECORD_NO","PARM_CD")])),]
  DataTable1 <- dcast(DataTable1, RECORD_NO ~ PARM_CD,value.var = "Val_qual")
  #rename pcodes to parm names
  parmNames <- as.data.frame(names(DataTable1))
  names(parmNames) <- "PARM_CD"
  parmNames <- dplyr::left_join(parmNames,unique(Results[c("PARM_CD","PARM_NM")]),by="PARM_CD")
  names(DataTable1) <- c("RECORD_NO", na.omit(parmNames$PARM_NM))
  
  #fill in record number meta data (station ID, name, date, time, etc)
  DataTable1 <- dplyr::left_join(DataTable1,Sample_meta, by="RECORD_NO")
  
  #reorder columns so meta data is at front
  parmcols <- seq(from =2, to =ncol(DataTable1)-ncol(Sample_meta)+1)
  metacols <- seq(from = ncol(DataTable1)-(ncol(Sample_meta)-2), to =ncol(DataTable1))
  DataTable1 <- DataTable1[c(1,metacols[1:7],parmcols,metacols[8:length(metacols)])]
  PlotTable1 <- dplyr::left_join(Results,Sample_meta,by="RECORD_NO")
  
  ##################
  ###QA Database####
  ##################
  # First get the site info--need column SITE_ID
  Query <- paste("select * from ", DSN, ".SITEFILE_",qa.db," where site_no IN (", STAID.list, ")", sep="")
  QASiteFile <- sqlQuery(Chan1, Query, as.is=T)
  
  ##Make unique AgencyCd/sitefile key
  QASiteFile$agencySTAID <- gsub(" ","",paste0(SiteFile$AGENCY_CD,SiteFile$SITE_NO))
  
  ##Subset SiteFile to unique agency code site ID pair
  QASiteFile <- subset(SiteFile,agencySTAID %in% gsub(" ","",uniqueSTAIDS))
  
  #get the record numbers
  Query <- paste("select * from ", DSN, ".QW_SAMPLE_",qa.db," where site_no IN (", STAID.list, ")", sep="")
  QASamples <- sqlQuery(Chan1, Query, as.is=T)
  
  ##Make unique AgencyCd/sitefile key
  Samples$agencySTAID <- gsub(" ","",paste0(Samples$AGENCY_CD,Samples$SITE_NO))
  
  ##Subset Samples to unique agency code site ID pair
  Samples <- subset(Samples,agencySTAID %in% gsub(" ","",uniqueSTAIDS))
  
  if(nrow(SiteFile) == 0 & nrow(QASiteFile) == 0){
          print("Site does not exist in sitefile, check site number input")
          stop("Site does not exist in environmantal or QA database sitefile, check site number input")
  }
  

  ##Check if samples were pulled and quit if no
  if(nrow(Samples) == 0 & nrow(QASamples) == 0) {
          #print("No samples exist in your local NWIS database for site number specified, check data criteria")
          stop("No samples exist in your local environmantal or QA NWIS database for site number specified, check data criteria")
  }
  
  ###Rename to SiteFile and Samples variable names, QA distinction was coded in later and this 
  ###Makes it so I do not have to change the code below
  
  SiteFile <- QASiteFile
  Samples <- QASamples
  
  
  if(nrow(SiteFile) > 0 & nrow(Samples) > 0)
  {
    #Subset records to date range, times are in GMT, which is the universal NWIS time so that you can have a consistant date-range accross timezones.
    #Time is corrected to local sample timezone before plotting
    Samples$SAMPLE_START_DT <- as.POSIXct(Samples$SAMPLE_START_DT, tz="GMT")
    if(!is.na(begin.date) && !is.na(end.date)) {
      Samples <- subset(Samples, SAMPLE_START_DT >= begin.date & SAMPLE_START_DT <= end.date)
    }else {} 
    
    ###Check for samples. If no samples then skip QA database
    if(nrow(Samples) > 0)
    {
    
    #get the QWResult file using the record numbers
    ##SQL is limited to 1000 entries in querry
    ##Get the number of 1000 bins in querry
    breaks <- ceiling(length(Samples$RECORD_NO)/1000)
    
    ##Run SQL queries
    for(i in 1:breaks)
    {
      j <- i-1
      ###Get the 1st 1000
      if(i == 1)
      {
        ####Get the results
        records.list <- paste("'", Samples$RECORD_NO[1:1000], "'", sep="", collapse=",")
        Query <- paste("select * from ", DSN, ".QW_RESULT_",qa.db," where record_no IN (", records.list, ")", sep="")
        Results <- sqlQuery(Chan1, Query, as.is=T)
        ####Get result level commments
        Query <- paste("select * from ", DSN, ".QW_RESULT_CM_",qa.db," where record_no IN (", records.list, ")", sep="")
        resultComments <- sqlQuery(Chan1, Query, as.is=T)
        ####Get sample level comments
        Query <- paste("select * from ", DSN, ".QW_SAMPLE_CM_",qa.db," where record_no IN (", records.list, ")", sep="")
        sampleComments <- sqlQuery(Chan1, Query, as.is=T)
        ####Get qualifier codes
        Query <- paste("select * from ", DSN, ".QW_VAL_QUAL_",qa.db," where record_no IN (", records.list, ")", sep="")
        resultQualifiers <- sqlQuery(Chan1, Query, as.is=T)
      } else if(i > 1 & (j*1000+1000) < length(Samples$RECORD_NO))
        ###Get the middle ones
      {
        j <- j * 1000+1
        records.list <- paste("'", Samples$RECORD_NO[j:(j+999)], "'", sep="", collapse=",")
        Query <- paste("select * from ", DSN, ".QW_RESULT_",qa.db," where record_no IN (", records.list, ")", sep="")
        Results <- rbind(Results,sqlQuery(Chan1, Query, as.is=T))
        Query <- paste("select * from ", DSN, ".QW_RESULT_CM_",qa.db," where record_no IN (", records.list, ")", sep="")
        resultComments <- rbind(resultComments,sqlQuery(Chan1, Query, as.is=T))
        Query <- paste("select * from ", DSN, ".QW_SAMPLE_CM_",qa.db," where record_no IN (", records.list, ")", sep="")
        sampleComments <- rbind(sampleComments,sqlQuery(Chan1, Query, as.is=T))
        Query <- paste("select * from ", DSN, ".QW_VAL_QUAL_",qa.db," where record_no IN (", records.list, ")", sep="")
        resultQualifiers <- rbind(resultQualifiers,sqlQuery(Chan1, Query, as.is=T))
      } else if (i > 1 && (j*1000+1000) > length(Samples$RECORD_NO))
      {
        ###Get the last ones
        j <- j * 1000+1
        records.list <- paste("'", Samples$RECORD_NO[j:length(Samples$RECORD_NO)], "'", sep="", collapse=",")
        Query <- paste("select * from ", DSN, ".QW_RESULT_",qa.db," where record_no IN (", records.list, ")", sep="")
        Results <- rbind(Results,sqlQuery(Chan1, Query, as.is=T))
        Query <- paste("select * from ", DSN, ".QW_RESULT_CM_",qa.db," where record_no IN (", records.list, ")", sep="")
        resultComments <- rbind(resultComments,sqlQuery(Chan1, Query, as.is=T))
        Query <- paste("select * from ", DSN, ".QW_SAMPLE_CM_",qa.db," where record_no IN (", records.list, ")", sep="")
        sampleComments <- rbind(sampleComments,sqlQuery(Chan1, Query, as.is=T))
        Query <- paste("select * from ", DSN, ".QW_VAL_QUAL_",qa.db," where record_no IN (", records.list, ")", sep="")
        resultQualifiers <- rbind(resultQualifiers,sqlQuery(Chan1, Query, as.is=T))
      } else{}
    }
    ###Join comments to results
    Results <- dplyr::left_join(Results,sampleComments,by="RECORD_NO")
    Results <- dplyr::left_join(Results,resultComments,by=c("RECORD_NO","PARM_CD"))
    Results <- dplyr::left_join(Results,resultQualifiers,by=c("RECORD_NO","PARM_CD"))
    
    Results$Val_qual <- paste(Results$RESULT_VA,Results$REMARK_CD, sep = " ")
    Results$Val_qual <- gsub("NA","",Results$Val_qual)
    
    #Get list of parm names
    #SQL is limited to 1000 entries in querry
    ##Get the number of 1000 bins in querry
    breaks <- ceiling(length(unique(Results$PARM_CD))/1000)
    
    ##Run SQL queries
    for(i in 1:breaks)
    {
      j <- i-1
      ###Get the 1st 1000
      if(i == 1)
      {
        parms.list <- paste("'", unique(Results$PARM_CD)[1:1000], "'", sep="", collapse=",")
        Query <- paste("select * from ", DSN, ".PARM where PARM_CD IN (", parms.list, ")", sep="")
        parms <- sqlQuery(Chan1, Query, as.is=T)
      } else if(i > 1 && (j*1000+1000) < length(unique(Results$PARM_CD)))
        ###Get the middle ones
      {
        j <- j * 1000+1
        parms.list <- paste("'", unique(Results$PARM_CD)[j:(j+999)], "'", sep="", collapse=",")
        Query <- paste("select * from ", DSN, ".PARM where PARM_CD IN (", parms.list, ")", sep="")
        parms <- rbind(parms,sqlQuery(Chan1, Query, as.is=T))
      } else if (i > 1 && (j*1000+1000) > length(unique(Results$PARM_CD)))
      {
        ###Get the last ones
        j <- j * 1000+1
        parms.list <- paste("'", unique(Results$PARM_CD)[j:length(unique(Results$PARM_CD))], "'", sep="", collapse=",")
        Query <- paste("select * from ", DSN, ".PARM where PARM_CD IN (", parms.list, ")", sep="")
        parms <- rbind(parms,sqlQuery(Chan1, Query, as.is=T))
      } else{}
    }
    parms <- parms[c("PARM_CD","PARM_SEQ_GRP_CD","PARM_DS","PARM_NM","PARM_SEQ_NU")]
    
    
    
    #############################################################################
    odbcClose(Chan1)###End of ODBC connection
    #############################################################################
    
    #station names and dates
    name_num <- SiteFile[c("SITE_NO","STATION_NM")]
    Sample_meta <- dplyr::left_join(Samples, name_num,by="SITE_NO")
    Sample_meta$RECORD_NO <- paste(Sample_meta$RECORD_NO,qa.db,sep="_")
    
    Sample_meta <- Sample_meta[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_END_DT","MEDIUM_CD",
                                 "SAMP_TYPE_CD","AGENCY_CD","PROJECT_CD","AQFR_CD","LAB_NO","HYD_EVENT_CD",
                                 "SAMPLE_CR","SAMPLE_CN","SAMPLE_MD","SAMPLE_MN",
                                 "SAMPLE_START_SG","SAMPLE_START_TZ_CD","SAMPLE_START_LOCAL_TM_FG",
                                 "SAMPLE_END_SG","SAMPLE_END_TZ_CD","SAMPLE_END_LOCAL_TM_FG","SAMPLE_ID",
                                 "TM_DATUM_RLBLTY_CD","ANL_STAT_CD","HYD_COND_CD",
                                 "TU_ID","BODY_PART_ID","COLL_ENT_CD","SIDNO_PARTY_CD")]
    
    #Sample_meta <- Sample_meta[c("SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_START_TZ_CD","SAMPLE_START_LOCAL_TM_FG","SAMPLE_END_DT","MEDIUM_CD","RECORD_NO","LAB_NO","PROJECT_CD","AQFR_CD")]
    
    
    #join tables so parm names are together
    Results<- dplyr::left_join(Results,parms,by="PARM_CD")
    
    #Paste database number ot record number to make unique
    Results$RECORD_NO <- paste(Results$RECORD_NO, qa.db,sep="_")
    
    #Subset results to selected parmeters
    if (parm.group.check == TRUE) 
    {
      if(!("All" %in% dl.parms))
      {
        Results <- subset(Results, PARM_SEQ_GRP_CD %in% dl.parms)
      } else{Results<- dplyr::left_join(Results,parms,by="PARM_CD")} 
    } else {Results <- subset(Results, PARM_CD %in% dl.parms)}
    
    #Make dataframe as record number and pcode. MUST HAVE ALL UNIQUE PCODE NAMES
    if(nrow(Results) != 0)
    {
      ###Make dataframe as record number and pcode. MUST HAVE ALL UNIQUE PCODE NAMES
      ##Remove duplicate results from data table
      DataTable2 <- subset(Results, !(DQI_CD %in% c("Q","X")))
      DataTable2 <- DataTable2[!(duplicated(DataTable2[c("RECORD_NO","PARM_CD")])),]
      DataTable2 <- dcast(DataTable2, RECORD_NO ~ PARM_CD,value.var = "Val_qual")
      #rename pcodes to parm names
      parmNames <- data.frame(PARM_CD = names(DataTable2))
      parmNames <- dplyr::left_join(parmNames,unique(Results[c("PARM_CD","PARM_NM")]),by="PARM_CD")
      names(DataTable2) <- c("RECORD_NO", na.omit(parmNames$PARM_NM))
      
      #fill in record number meta data (station ID, name, date, time, etc)
      DataTable2 <- dplyr::left_join(DataTable2,Sample_meta, by="RECORD_NO")
      
      #reorder columns so meta data is at front
      parmcols <- seq(from =2, to =ncol(DataTable2)-ncol(Sample_meta)+1)
      metacols <- seq(from = ncol(DataTable2)-(ncol(Sample_meta)-2), to =ncol(DataTable2))
      DataTable2 <- DataTable2[c(1,metacols[1:7],parmcols,metacols[8:length(metacols)])]
      PlotTable2 <- dplyr::left_join(Results,Sample_meta,by="RECORD_NO")
    }else{}
    
    }
  } else{}
  ###Check that data was pulled from Database 2
  if(exists("DataTable2"))
  {
    DataTable <- rbind.fill(DataTable1,DataTable2)
    PlotTable <- rbind.fill(PlotTable1,PlotTable2)
  } else{
    DataTable <- DataTable1
    PlotTable <-PlotTable1
  }
  
  remarkCodes <- c("<",">","A","E","M","N","R","S","U","V")
  
  ###Replace NAs with "Sample"
  #PlotTable$REMARK_CD[is.na(PlotTable$REMARK_CD)] <- "Sample"
  PlotTable$REMARK_CD[which(!(PlotTable$REMARK_CD %in% remarkCodes))] <- "Sample"
  
  ###Set remark code as factor for plotting
  #PlotTable$REMARK_CD <- as.factor(PlotTable$REMARK_CD)
  ###Set factors levels
  #PlotTable$REMARK_CD = factor(PlotTable$REMARK_CD,levels(PlotTable$REMARK_CD)[c(4,1:3)])
  ###Make result a numeric
  PlotTable$RESULT_VA <- as.numeric(PlotTable$RESULT_VA)
  
  
  ##Format date times to local sample collection timezone
  #SAMPLE_START_DT
  PlotTable$SAMPLE_START_DT <- convertTime(datetime = PlotTable$SAMPLE_START_DT,
                                           timezone = PlotTable$SAMPLE_START_TZ_CD,
                                           daylight = PlotTable$SAMPLE_START_LOCAL_TM_FG)
  
  
  DataTable$SAMPLE_START_DT <- convertTime(datetime = DataTable$SAMPLE_START_DT,
                                           timezone = DataTable$SAMPLE_START_TZ_CD,
                                           daylight = DataTable$SAMPLE_START_LOCAL_TM_FG)
  #RESULT_CR
  PlotTable$RESULT_CR <- convertTime(datetime = PlotTable$RESULT_CR,
                                     timezone = PlotTable$SAMPLE_START_TZ_CD,
                                     daylight = PlotTable$SAMPLE_START_LOCAL_TM_FG)
  
  
  #RESULT_MD
  PlotTable$RESULT_MD <- convertTime(datetime = PlotTable$RESULT_MD,
                                     timezone = PlotTable$SAMPLE_START_TZ_CD,
                                     daylight = PlotTable$SAMPLE_START_LOCAL_TM_FG)
  #SAMPLE_CR
  PlotTable$SAMPLE_CR <- convertTime(datetime = PlotTable$SAMPLE_CR,
                                     timezone = PlotTable$SAMPLE_START_TZ_CD,
                                     daylight = PlotTable$SAMPLE_START_LOCAL_TM_FG)
  
  #SAMPLE_MD
  PlotTable$SAMPLE_MD <- convertTime(datetime = PlotTable$SAMPLE_MD,
                                     timezone = PlotTable$SAMPLE_START_TZ_CD,
                                     daylight = PlotTable$SAMPLE_START_LOCAL_TM_FG)
  
  #SAMPLE_END_DT
  PlotTable$SAMPLE_END_DT <- convertTime(datetime = PlotTable$SAMPLE_END_DT,
                                         timezone = PlotTable$SAMPLE_START_TZ_CD,
                                         daylight = PlotTable$SAMPLE_START_LOCAL_TM_FG)
  
  
  DataTable$SAMPLE_END_DT <- convertTime(datetime = DataTable$SAMPLE_END_DT,
                                         timezone = DataTable$SAMPLE_START_TZ_CD,
                                         daylight = DataTable$SAMPLE_START_LOCAL_TM_FG)
  
  ###Get month for seasonal plots and reorder factor levels to match water-year order
  PlotTable$SAMPLE_MONTH <-  factor(format(PlotTable$SAMPLE_START_DT,"%b"),levels=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))
  ##Convert ddata table start date to character string so displays in gui tables
  #DataTable$SAMPLE_START_DT <- as.character(DataTable$SAMPLE_START_DT)
  
  ###Format columns to proper class
  PlotTable$PARM_SEQ_NU <- as.numeric(gsub(" ","",PlotTable$PARM_SEQ_NU))
  PlotTable$RECORD_NO <- as.character(PlotTable$RECORD_NO)
  PlotTable$RPT_LEV_VA <- as.character(PlotTable$RPT_LEV_VA)
  PlotTable$REMARK_CD <- as.character(PlotTable$REMARK_CD)
  
  ###limit medium codes in plot table
  PlotTable <- subset(PlotTable, MEDIUM_CD %in% c("WS ","WG ","OA ","WSQ","WGQ","OAQ"))
  
  ###Add in day of year to PlotTable
  PlotTable$DOY <- yday(PlotTable$SAMPLE_START_DT)
  
  ###Remove trailing spaces on site IDs
  
  PlotTable$SITE_NO <- gsub(" ","",PlotTable$SITE_NO)
  DataTable$SITE_NO <- gsub(" ","",DataTable$SITE_NO)
  
  ###Reorder columns
  PlotTable <- PlotTable[c("RECORD_NO" ,"SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_END_DT","MEDIUM_CD","PROJECT_CD",
                           "PARM_CD","PARM_NM","METH_CD","RESULT_VA","REMARK_CD","VAL_QUAL_CD","RPT_LEV_VA","DQI_CD", 
                           "DEC_LAT_VA","DEC_LONG_VA",
                           "SAMPLE_CM_TX","SAMPLE_CM_CR","SAMPLE_CM_CN","SAMPLE_CM_MD","SAMPLE_CM_MN",
                           "RESULT_CM_TX","RESULT_CM_CR","RESULT_CM_CN","RESULT_CM_MD","RESULT_CM_MN",
                           "SAMPLE_CR","SAMPLE_CN","SAMPLE_MD","SAMPLE_MN",
                           "RESULT_CR","RESULT_CN","RESULT_MD","RESULT_MN",
                           "PREP_DT","ANL_DT","LAB_NO","PREP_SET_NO","ANL_SET_NO","ANL_ENT_CD","LAB_STD_DEV_VA" ,"LAB_STD_DEV_SG",
                           "RESULT_SG","RESULT_RD","RPT_LEV_SG","RPT_LEV_CD","NULL_VAL_QUAL_CD",  
                           "SAMPLE_CM_TP",
                           "RESULT_CM_TP",
                           "VAL_QUAL_NU","Val_qual","PARM_SEQ_GRP_CD","PARM_DS",
                           "PARM_SEQ_NU","AGENCY_CD",
                           "SAMPLE_START_SG","SAMPLE_START_TZ_CD",
                           "SAMPLE_START_LOCAL_TM_FG","SAMPLE_END_SG","SAMPLE_END_TZ_CD",
                           "SAMPLE_END_LOCAL_TM_FG","SAMPLE_ID","TM_DATUM_RLBLTY_CD","ANL_STAT_CD",
                           "HYD_COND_CD","SAMP_TYPE_CD","HYD_EVENT_CD",
                           "AQFR_CD","TU_ID","BODY_PART_ID",
                           "COLL_ENT_CD","SIDNO_PARTY_CD","PARM_DS",
                           "HUC_CD","SAMPLE_MONTH","DOY")]

  ###Remove reviewed and rejected results
  PlotTable <- subset(PlotTable,!(DQI_CD %in% c("Q","X"))) 
  
  return(list(DataTable=DataTable,PlotTable=PlotTable))
  
}