# Function to pull data from NWIS
# IN PROGRESS, NOT EXPORTED
# Pulls data from NWIS WEB servers using functions from the dataRetrieval package.
# @param STAIDS A character vector of stations IDs to pull data
# @param dl.parms A character vector of pcodes to pull data
# @param parm.group.check A logical of weather or not to use NWIS parameter groups. If TRUE, must use NWIS parameter group names in dl.parms
# @param begin.date Character string containing beginning date of data pull (yyyy-mm-dd)
# @param end.date Character string containing ending date of data pull (yyyy-mm-dd)
# @import dataRetrieval 
# @import reshape2 
# @import plyr
# @import lubridate

publicData <- function(STAIDS,dl.parms = "All",parm.group.check = TRUE,begin.date ="",end.date="")
{

  ###Convert NAs from svalue to empty "" for dataRetrieval
  if(is.na(begin.date))
  {
    begin.date <- ""
  } else {}
  
  if(is.na(end.date))
  {
    end.date <- ""
  } else {}
###Get available data for the site
availableData <- whatNWISdata(STAIDS, service = "qw")

#Subset results to selected parmeters
if (parm.group.check == TRUE) 
{
  if(dl.parms != "All")
  {
    pCodes <- unique(availableData$parm_cd[
      which(availableData$parm_grp_cd == dl.parms)])
  } else{ pCodes <- unique(availableData$parm_cd)} 
} else {pCodes <- unique(availableData$parm_cd[
  which(availableData$parm_cd %in% dl.parms)])}
###
pCodes <- pCodes[which(pCodes != "")]
###Pull data into long data frame
PlotTable <- readNWISqw(STAIDS,pCodes,begin.date,end.date)

##Format date times to posixct with timezone from sample_start_time_datum_cd
offsetLibrary <- setNames(c(5, 4, 6, 5, 7, 6, 8, 7, 9, 
                            8, 10, 10), c("EST", "EDT", "CST", "CDT", "MST", 
                                          "MDT", "PST", "PDT", "AKST", "AKDT", "HAST", "HST"))
offset <- offsetLibrary[PlotTable$sample_start_time_datum_cd] * 60 * 60
PlotTable$startDateTime <- PlotTable$startDateTime - offset
###Bring in meta data
PlotTable <- join(PlotTable, unique(availableData[c("site_no","station_nm","dec_lat_va","dec_long_va","huc_cd")]),by= "site_no")
PlotTable <- join(PlotTable, unique(availableData[c("parm_cd","parameter_nm","parm_grp_cd")]),by= "parm_cd")

###Rename to same as internal Plot Table
names(PlotTable) <- c("AGENCY_CD","SITE_NO","SAMPLE_DT",
"SAMPLE_TM","SAMPLE_END_DT","SAMPLE_END_TM",
"SAMPLE_START_TIME_DATUM_CD","TM_DATUM_RLBTY_CD","COLL_ENT_CD",
"MEDIUM_CD","TU_ID","BODY_PART_ID",
"PARM_CD","REMARK_CD","RESULT_VA",
"VAL_QUAL_TX","METH_CD","DQI_CD",
"RPT_LEV_VA","RPT_LEV_CD","LAB_STD_VA",
"ANL_ENT_CD","SAMPLE_START_DT","STATION_NM",
"DEC_LAT_VA","DEC_LONG_VA","HUC_CD",
"PARM_NM","PARM_SEQ_GRP_CD")

###Add in RESULT_MD as SAMPLE_START_DT
PlotTable$RESULT_MD <- PlotTable$SAMPLE_START_DT

##Add in phony PARM_SEQ_NU
PlotTable$PARM_SEQ_NU <- ""

##Add in phony PARM_DS
PlotTable$PARM_DS <- PlotTable$PARM_NM

###Add scpae to medium code to make compatible with plots
PlotTable$MEDIUM_CD[which(PlotTable$MEDIUM_CD %in% c("WS","WG"))] <- paste(PlotTable$MEDIUM_CD[which(PlotTable$MEDIUM_CD %in% c("WS","WG"))]," ",sep="")

###Add in sample remark code
PlotTable$REMARK_CD[which(PlotTable$REMARK_CD == "")] <- "Sample"

###Addd in sample month
PlotTable$SAMPLE_MONTH <-  factor(format(PlotTable$SAMPLE_START_DT,"%b"),levels=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))###Make Data Table

###Add in unique "RECORD_NO"
PlotTable$RECORD_NO <- paste(PlotTable$SITE_NO,PlotTable$SAMPLE_START_DT,PlotTable$MEDIUM_CD,sep="")
  
DataTable <- PlotTable
DataTable$VAL_QUAL <- paste(DataTable$RESULT_VA,DataTable$REMARK_CD,sep = " ")
DataTable <- dcast(DataTable,SITE_NO + 
                     STATION_NM +
                     SAMPLE_DT +
                     SAMPLE_TM +
                     SAMPLE_END_DT +
                     SAMPLE_END_TM +
                    MEDIUM_CD ~ PARM_NM,value.var="VAL_QUAL")
return(list(DataTable=DataTable,PlotTable=PlotTable))
}
