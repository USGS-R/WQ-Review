#' Summarize SRS sample agreement
#' 
#' Summarizes standard reference sample (SRS) results and automatically flags bad SRS results.
#' @param srsValues A dataframe containing the SRS values. The first column contains the SRS sample ID,the second column contains the SRS analyte parameter code, the third column contains the SRS analyte name, and the fourth column contains the SRS MPV
#' @param srsResults A dataframe containing the results of the SRS analyses from the lab. The first column containssample descriptions (e.g. date and time), the second column contains the SRS sample ID, and the remaining columns containthe SRS results. SRS Result columns must be named with the cooresponding paramter code.
#' @param reportingLevels A dataframe containing the reporting levels for each analyte. The first column contains the parameter code and the second column contains hte associated reporting level
#' @import plyr
#' @import reshape2
#' @export
#' 



srsSummary <- function(srsValues,srsResults,reportingLevels) {
        
        ###Rename dataframe columns
        names(srsValues) <- c("srsID","PARM_CD","PARM_NM","MPV")
        names(srsResults)[1:2] <- c("description","srsID")
        names(reportingLevels) <- c("PARM_CD","lrl")
        
 
        ###Melt the data
        srsResults <- melt(srsResults,id.vars = c("description","srsID"))
        
        #remove < and e from data results
        srsResults$value <- (gsub("<", "", srsResults$value))
        srsResults$value <- (gsub(">", "", srsResults$value))
        srsResults$value <- (gsub("e", "", srsResults$value))
        srsResults$value <- (gsub("E", "", srsResults$value))
        
        ###Rename melted dataframe
        names(srsResults) <- c("description","srsID","PARM_CD","value")
        
        ###Remove Xs from parmcd
        srsResults$PARM_CD <- (gsub("X", "", srsResults$PARM_CD))
        
        ###Join results to mpvs
        srsSummary <- join(srsResults,srsValues,by=c("srsID","PARM_CD"))
        
        ###bring in lrls
        srsSummary <- join(srsSummary,reportingLevels,by="PARM_CD")
        
        ###Make columns numeric
        srsSummary$value <- as.numeric(srsSummary$value)
        srsSummary$MPV <- as.numeric(srsSummary$MPV)
        srsSummary$lrl <- as.numeric(srsSummary$lrl)
        
        
        ###Make calculations
        srsSummary$Diff <- srsSummary$value - srsSummary$MPV
        srsSummary$RPD <- srsSummary$Diff/((srsSummary$value + srsSummary$MPV)/2)*100
        
        ###remove non SRS samples
        srsSummary <- na.omit(srsSummary)
        
        ###Make summary flags
        srsSummary$flags <- ""
        srsSummary$flags[which(abs(srsSummary$RPD) > 10 & abs(srsSummary$Diff) > srsSummary$lrl)] <- "RPD > 10%"
        srsSummary$flags[which(abs(srsSummary$RPD) > 10 & abs(srsSummary$Diff) > 3*srsSummary$lrl)] <- "RPD > 10% & > 3x lrl"
        srsSummary$flags[which(abs(srsSummary$RPD) > 10 & abs(srsSummary$Diff) > 10*srsSummary$lrl)] <- "RPD > 10% & > 10x lrl"
        
return(srsSummary)
}
        
        
        