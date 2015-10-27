#' Summary stats function
#' 
#' Takes output data object from readNWISodbc and generates a table of summary statistcs
#' @param qw.data A qw.data object generated from readNWISodbc
#' @param STAIDS A character vector of sites to summarize. Defaults to all sites in qw.data if unspecified.
#' @param parameterCd A character vector of parameter codes to summarize. Defaults to all pcodes if unspecified.
#' @param begin.date A character string of optional begin date range to summarize over in "YYYY-MM-DD" format. Defaults to all if unspecified.
#' @param end.date A character string of optional end date range to summarize over in "YYYY-MM-DD" format. Defaults to all if unspecified.
#' @examples 
#' data("exampleData",package="WQReview")
#' summaryStats(qw.data = qw.data,
#'              STAIDS=NULL, 
#'              parameterCd = NULL, 
#'              begin.date = "2010-01-01", 
#'              end.date = "2011-12-31")
#' @import plyr
#' @export
#' 
summaryStats <- function(qw.data, STAIDS=NULL, parameterCd = NULL, begin.date = NA, end.date = NA) {
        
        ###Subset to STIADS
        if(is.null(STAIDS))
        {
                data <- qw.data$PlotTable
        } else{data <- subset(qw.data$PlotTable, SITE_NO %in% STAIDS)}
        
        ###Subset to parm codes
        if(is.null(parameterCd))
        {
        data <- qw.data$PlotTable
        } else{data <- subset(qw.data$PlotTable, PARM_CD %in% parameterCd)}
        
        ###Subset to date range
        if(!is.na(begin.date) && !is.na(end.date)) {
                data <- subset(qw.data$PlotTable, SAMPLE_START_DT >= as.POSIXct(begin.date) & SAMPLE_START_DT <= as.POSIXct(end.date))
        }else {} 

        if(nrow(data == 0))
        {
                stop("No data to summarize. Check parameter codes and data pull criteria.")
        } else{}
 
  sumStats<- ddply(data,c("SITE_NO","PARM_CD"),summarize,
                          N = length(na.omit(RESULT_VA)),
                          min = min(na.omit(RESULT_VA)),
                          max = max(na.omit(RESULT_VA)),
                          mean = mean(na.omit(RESULT_VA)),
                          median = median(na.omit(RESULT_VA)),
                   quant99 = quantile(na.omit(RESULT_VA),probs=c(0.99)),
                          quant90 = quantile(na.omit(RESULT_VA),probs=c(0.9)),
                          quant10 = quantile(na.omit(RESULT_VA),probs=c(0.1)),
                          quant95 = quantile(na.omit(RESULT_VA),probs=c(0.95)),
                          quant05 = quantile(na.omit(RESULT_VA),probs=c(0.05)),
                   quant01 = quantile(na.omit(RESULT_VA),probs=c(0.01))
                          )
  return(sumStats)
  
}
