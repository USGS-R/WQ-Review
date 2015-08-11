#' ionBalance
#' 
#' Takes output list from readNWISodbc and checks whole vs part concentrations
#' @param qw.data A qw.data object generated from readNWISodbc
#' @import plyr
#' @export
#' 


wholevPart <- function(qw.data)
{
  ###Read in qwdata filtered unfiltered data table
  #filunfTable <- read.csv("data/filtered_unfiltered.csv",header=T,colClasses = "character")
  
  ###Subset results to filtered
  filteredData <- subset(qw.data$PlotTable,PARM_CD %in% filunfTable$PARM_CD[which(filunfTable$filter_cd == "filtered")])
  
  ###subset results to unfiltered
  unfilteredData <- subset(qw.data$PlotTable,PARM_CD %in% filunfTable$PARM_CD[which(filunfTable$filter_cd == "unfiltered")])
  
  if(nrow(filteredData) > 0 & nrow(unfilteredData) > 0 )
  {
  
  ###bring in index number for pair
  filteredData <- join(filteredData, filunfTable,by="PARM_CD")
  ###Rename to indicate filtered
  names(filteredData) <- paste("Fil_",names(filteredData),sep="")
  ###bring in index number for pair
  unfilteredData <- join(unfilteredData, filunfTable,by="PARM_CD")
  ###Rename to indicate filtered
  names(unfilteredData) <- paste("Unf_",names(unfilteredData),sep="")
  
  
  ###Join tables by record number and index to pair up filtered unfiltered results
  filteredData <- rename(filteredData,c("Fil_RECORD_NO" = "RECORD_NO"))
  filteredData <- rename(filteredData,c("Fil_index" = "index"))
  unfilteredData <- rename(unfilteredData,c("Unf_RECORD_NO" = "RECORD_NO"))
  unfilteredData <- rename(unfilteredData,c("Unf_index" = "index"))
  pairedData <- join(filteredData,unfilteredData, by = c("RECORD_NO","index"))
  
  ###Remove extra columns and NAs
  pairedData <- (pairedData[c("RECORD_NO",
                              "Fil_SITE_NO",
                           "Fil_STATION_NM",
                         "Fil_SAMPLE_START_DT",
                         "Fil_SAMPLE_END_DT",
                         "Fil_MEDIUM_CD",
                         "Fil_LAB_NO","Unf_LAB_NO",
                         "Fil_PARM_CD","Fil_PARM_DS", 
                         "Unf_PARM_CD","Unf_PARM_DS", 
                         "Fil_RESULT_VA","Fil_REMARK_CD",
                         "Unf_RESULT_VA","Unf_REMARK_CD",
                         "Fil_RPT_LEV_VA","Unf_RPT_LEV_VA")])
  ###Rename some columns
  colnames(pairedData) <- c("RECORD_NO",
                              "SITE_NO",
                              "STATION_NM",
                              "SAMPLE_START_DT",
                              "SAMPLE_END_DT",
                              "MEDIUM_CD",
                              "Fil_LAB_NO","Unf_LAB_NO",
                              "Fil_PARM_CD","Fil_PARM_DS", 
                              "Unf_PARM_CD","Unf_PARM_DS", 
                              "Fil_RESULT_VA","Fil_REMARK_CD",
                              "Unf_RESULT_VA","Unf_REMARK_CD",
                              "Fil_RPT_LEV_VA","Unf_RPT_LEV_VA")
  ###Fill in missing reporting levels with 0s
  pairedData$Fil_RPT_LEV_VA[which(is.na(pairedData$Fil_RPT_LEV_VA))] <- 0
  pairedData$Unf_RPT_LEV_VA[which(is.na(pairedData$Unf_RPT_LEV_VA))] <- 0
  
  ###Remove results where both fil and unf are censored.
  pairedData <- pairedData[which(pairedData$Fil_REMARK_CD != "<" | pairedData$Unf_REMARK_CD != "<"),]
  
  ###Flag results where filtered is greater than unfiltered by more than the highest LRL
  pairedData$flags <- ""
  pairedData$flags[which(
    ((pairedData$Fil_RESULT_VA - pairedData$Unf_RESULT_VA) > 0 &
       (pairedData$Fil_RESULT_VA - pairedData$Unf_RESULT_VA) > pmax(pairedData$Fil_RPT_LEV_VA,pairedData$Unf_RPT_LEV_VA)) &
                           ((pairedData$Fil_RESULT_VA - pairedData$Unf_RESULT_VA)/((pairedData$Fil_RESULT_VA + pairedData$Unf_RESULT_VA)/2) <= 0.1)
                         
                         )] <- "Filtered > unfiltered"
  pairedData$flags[which(
    ((pairedData$Fil_RESULT_VA - pairedData$Unf_RESULT_VA) > 0 &
       (pairedData$Fil_RESULT_VA - pairedData$Unf_RESULT_VA) > pmax(pairedData$Fil_RPT_LEV_VA,pairedData$Unf_RPT_LEV_VA)) &
      ((pairedData$Fil_RESULT_VA - pairedData$Unf_RESULT_VA)/((pairedData$Fil_RESULT_VA + pairedData$Unf_RESULT_VA)/2) > 0.1)
    
  )] <- "Filtered > unfiltered and > 10% diff."
  
  pairedData$flags[which(pairedData$Fil_REMARK_CD == "<" | pairedData$Unf_REMARK_CD == "<")] <-
    paste(pairedData$flags[which(pairedData$Fil_REMARK_CD == "<" | pairedData$Unf_REMARK_CD == "<")],"calculations effected by remark code",sep="")
  wholevpartTable <- pairedData
  #wholevpartTable$Fil_SAMPLE_START_DT <- as.character(wholevpartTable$Fil_SAMPLE_START_DT)
  return(wholevpartTable)
} else{}

}