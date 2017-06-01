# qualitySummary - IN PROGRESS, NOT EXPORTED
# 
# Takes output data object from readNWISodbc and generates a summary of data quality.
# @param qw.data A qw.data object generated from readNWISodbc
#
#

qualitySummary <- function(qw.data,reports = NULL) 
{
  ###Get tables
  if(is.null(reports))
  {
  wholevpartTable <- wholevPart(qw.data)
  repTable <- repTabler(qw.data)
  ionBalanceOut <- ionBalance(qw.data = .guiEnv$qw.data,wide=TRUE)
  balanceTable <- ionBalanceOut$chargebalance.table
  } else {
    wholevpartTable <- reports$wholevpartTable
    repTable <- reports$repTable
    balanceTable <- reports$balanceTable
    
  }
  ###Summarize whole v part
  if(!is.null(wholevpartTable))
  {
  wholevpartSummary <- ddply(wholevpartTable,"RECORD_NO",summarise,
                             Fil.greater.Unf = length(flags[which(flags == "Filtered > unfiltered")]),
                             Fil.greater.Unf.10perc = length(flags[which(flags == "Filtered > unfiltered and > 10% diff.")]))
  }else{}
  
  ###Summarize Reps
  if(exists("repTable"))
  {
  repSummary <- ddply(repTable,c("Env_RECORD_NO"),summarise,
                                           RPD.greater.10perc = length(flags[which(flags == "RPD > 10%")]),
                                           RPD.greater.10perc.DL = length(flags[which(flags == "RPD > 10% and > RPT_LEV")]))
  } else{}
  
  if(!is.null(repSummary))
  {
  colnames(repSummary)[1] <- "RECORD_NO"
  } else{}
  
  ###Summarize charge balance
  
  if(exists("balanceTable"))
  {
  balanceSummary <- ddply(balanceTable,"RECORD_NO",summarise,
                                           imbalance.greater.10perc = (perc.diff > 10 | perc.diff < -10),
                                           imbalance.greater.10perc.complete = (perc.diff > 10 & complete.chem == "Complete" | perc.diff < -10 & complete.chem == "Complete"))
  } else {}
  
  
  ###Join tables
  summaryTable <- unique(qw.data$PlotTable[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD")])
  
  #if(exists("balanceSummary"))
  #{
  try(summaryTable <- join(summaryTable,balanceSummary,by="RECORD_NO"),silent=TRUE)
  #} else {}
  
  #if(exists("repSummary"))
  #{
  try(summaryTable <- join(summaryTable,repSummary,by="RECORD_NO"),silent=TRUE)
  #} else {}
 
  
  #if(exists("wholevpartSummary"))
  #{
  try(summaryTable <- join(summaryTable,wholevpartSummary,by="RECORD_NO"),silent=TRUE)
  #} else{}
  
  #summaryTable <- join(unique(qw.data$PlotTable[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD")]),summaryTable,
  #                by="RECORD_NO",type="right")
  
  ###Clean out NAs
  summaryTable[is.na(summaryTable)] <- ""
	summaryTable <- unique(summaryTable)
return(summaryTable)
}


