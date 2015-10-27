#' Look for missing data for a set of samples
#' 
#' Searches for missing data and returns a dataframe of missing data by record number.
#' @param qw.data A qw.data object generater by readNWISodbc
#' @param searchParms A character vector of parameter codes to search for if groups = FALSE, otherwise a dataframe of parameter codes in character format to search for grouped by named columns if groups= TRUE. Maximum number of columns(groups) is 10
#' @param begin.date Subset data to begin date
#' @param end.date Subset data to end date.
#' @param groups Logical. Search for searchParms by groups instead of one long vector. 
#' @examples 
#' data("exampleData",package="WQReview")
#' searchParms <- data.frame(cations = c("00915","00930","00925","00935","01123"),
#'                           anions = c("00945","00940","99220",NA,NA),
#'                           nutrients = c("00608","00631",NA,NA,NA),
#'                           stringsAsFactors = FALSE)
#' whatData(qw.data = qw.data,
#'         searchParms = searchParms,
#'         begin.date = NULL,
#'         end.date = NULL,
#'         groups=TRUE)
#' @import plyr
#' @export


whatData <- function(qw.data,searchParms,begin.date = NULL,end.date = NULL,groups=FALSE)
	{
  
	##global aignments to make them work with ddply, sucks but don't know other way around it
	checkTable <<- qw.data$PlotTable
  searchParms <<- searchParms 
	
  ##subset to date range
  if(!is.null(begin.date) & !is.null(end.date))
  {  
    checkTable <- subset(checkTable,SAMPLE_START_DT > as.POSIXct(begin.date) & SAMPLE_START_DT < as.POSIXct(end.date))
  } else if(!is.null(begin.date) & is.null(end.date))
  {
    checkTable <- subset(checkTable,SAMPLE_START_DT > as.POSIXct(begin.date))
  } else if(is.null(begin.date) & !is.null(end.date))
  {
    checkTable <- subset(checkTable,SAMPLE_START_DT < as.POSIXct(end.date))
  } else{}
  
  ###Check for missing searchParms and create list of missing parms by record
  if(groups)
  {
  	###Check for missing searchParms by group and create list of missing parms by record
  	
  	
  	###Brute forcing it for groups. I do not know how to create a dataframe that depends on the number of columns of searchParms. I.e. the number of groups.
  	###For some reason plyr fails when you don't put the ""plyr" library in front of the function
  	if(ncol(searchParms)==1)
  	{
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(searchParms[which(!(searchParms[,1] %in% PARM_CD)),1],collapse = ","))
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} 
  	else if(ncol(searchParms) == 2)
  	{
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,1]) %in% PARM_CD)),1]),collapse = ","),
  												 group2 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,2]) %in% PARM_CD)),2]),collapse = ","))
  		
  		
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} else if(ncol(searchParms) == 3)
  	{
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,1]) %in% PARM_CD)),1]),collapse = ","),
  												 group2 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,2]) %in% PARM_CD)),2]),collapse = ","),
  												 group3 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,3]) %in% PARM_CD)),3]),collapse = ","))
  		
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} else if(ncol(searchParms) == 4)
  	{
  		
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,1]) %in% PARM_CD)),1]),collapse = ","),
  												 group2 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,2]) %in% PARM_CD)),2]),collapse = ","),
  												 group3 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,3]) %in% PARM_CD)),3]),collapse = ","),
  												 group4 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,4]) %in% PARM_CD)),4]),collapse = ","))
  		
  		
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} else if(ncol(searchParms) == 5)
  	{
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,1]) %in% PARM_CD)),1]),collapse = ","),
  												 group2 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,2]) %in% PARM_CD)),2]),collapse = ","),
  												 group3 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,3]) %in% PARM_CD)),3]),collapse = ","),
  												 group4 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,4]) %in% PARM_CD)),4]),collapse = ","),
  												 group5 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,5]) %in% PARM_CD)),5]),collapse = ","))
  		
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} else if(ncol(searchParms) == 6)
  	{
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,1]) %in% PARM_CD)),1]),collapse = ","),
  												 group2 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,2]) %in% PARM_CD)),2]),collapse = ","),
  												 group3 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,3]) %in% PARM_CD)),3]),collapse = ","),
  												 group4 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,4]) %in% PARM_CD)),4]),collapse = ","),
  												 group5 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,5]) %in% PARM_CD)),5]),collapse = ","),
  												 group6 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,6]) %in% PARM_CD)),6]),collapse = ","))
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} else if(ncol(searchParms) == 7)
  	{
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,1]) %in% PARM_CD)),1]),collapse = ","),
  												 group2 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,2]) %in% PARM_CD)),2]),collapse = ","),
  												 group3 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,3]) %in% PARM_CD)),3]),collapse = ","),
  												 group4 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,4]) %in% PARM_CD)),4]),collapse = ","),
  												 group5 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,5]) %in% PARM_CD)),5]),collapse = ","),
  												 group6 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,6]) %in% PARM_CD)),6]),collapse = ","),
  												 group7 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,7]) %in% PARM_CD)),7]),collapse = ","))
  		
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} else if(ncol(searchParms) == 8)
  	{
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,1]) %in% PARM_CD)),1]),collapse = ","),
  												 group2 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,2]) %in% PARM_CD)),2]),collapse = ","),
  												 group3 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,3]) %in% PARM_CD)),3]),collapse = ","),
  												 group4 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,4]) %in% PARM_CD)),4]),collapse = ","),
  												 group5 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,5]) %in% PARM_CD)),5]),collapse = ","),
  												 group6 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,6]) %in% PARM_CD)),6]),collapse = ","),
  												 group7 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,7]) %in% PARM_CD)),7]),collapse = ","),
  												 group8 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,8]) %in% PARM_CD)),8]),collapse = ","))
  		
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} else if(ncol(searchParms) == 9)
  	{
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,1]) %in% PARM_CD)),1]),collapse = ","),
  												 group2 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,2]) %in% PARM_CD)),2]),collapse = ","),
  												 group3 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,3]) %in% PARM_CD)),3]),collapse = ","),
  												 group4 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,4]) %in% PARM_CD)),4]),collapse = ","),
  												 group5 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,5]) %in% PARM_CD)),5]),collapse = ","),
  												 group6 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,6]) %in% PARM_CD)),6]),collapse = ","),
  												 group7 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,7]) %in% PARM_CD)),7]),collapse = ","),
  												 group8 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,8]) %in% PARM_CD)),8]),collapse = ","),
  												 group9 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,9]) %in% PARM_CD)),9]),collapse = ","))
  		
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} else if(ncol(searchParms) == 10)
  	{
  		missingData <- ddply(checkTable,"RECORD_NO", summarise,
  												 group1 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,1]) %in% PARM_CD)),1]),collapse = ","),
  												 group2 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,2]) %in% PARM_CD)),2]),collapse = ","),
  												 group3 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,3]) %in% PARM_CD)),3]),collapse = ","),
  												 group4 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,4]) %in% PARM_CD)),4]),collapse = ","),
  												 group5 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,5]) %in% PARM_CD)),5]),collapse = ","),
  												 group6 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,6]) %in% PARM_CD)),6]),collapse = ","),
  												 group7 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,7]) %in% PARM_CD)),7]),collapse = ","),
  												 group8 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,8]) %in% PARM_CD)),8]),collapse = ","),
  												 group9 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,9]) %in% PARM_CD)),9]),collapse = ","),
  												 group10 = paste(na.omit(searchParms[which(!(na.omit(searchParms[,10]) %in% PARM_CD)),10]),collapse = ","))
  		
  		names(missingData) <- c("RECORD_NO",names(searchParms))
  	} else(print("To many or to few parameter groups, please check parameter input"))
  
  } else { 
  	missingData <- ddply(checkTable,"RECORD_NO", summarise,
  											 missingParms = paste(searchParms[which(!(searchParms %in% PARM_CD))],collapse = ","))
  }
    
  
###Bring in sample meta data

missingData <- join(unique(checkTable[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","PROJECT_CD")]),missingData,by="RECORD_NO")
rm(checkTable,envir=.GlobalEnv)
rm(searchParms,envir=.GlobalEnv)
return(missingData)
    
}
    
    
    
    
    
    
   