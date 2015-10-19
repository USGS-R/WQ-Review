#' Sum ions vs conductance plot
#' 
#' Takes output data object from readNWISodbc and prints a plot of sum ions vs. conductance. Requires charge balance = TRUE in NWISPullR
#' @param qw.data A qw.data object generated from readNWISodbc
#' @param parameterCd A character vector of parameter codes to summarize. Defaults to all pcodes if unspecified.
#' @import plyr
#' @export
#' 
summaryStats <- function(qw.data, parameterCd = NULL) {
  data <- qw.data$PlotTable
  
 
  sumStats<- ddply(data,c("SITE_NO","PARM_CD"),summarise,
                          N = length(na.omit(RESULT_VA)),
                          min = min(na.omit(RESULT_VA)),
                          max = max(na.omit(RESULT_VA)),
                          mean = mean(na.omit(RESULT_VA)),
                          median = median(na.omit(RESULT_VA)),
                          quant90 = quantile(na.omit(RESULT_VA),probs=c(0.9)),
                          quant10 = quantile(na.omit(RESULT_VA),probs=c(0.1)),
                          quant95 = quantile(na.omit(RESULT_VA),probs=c(0.95)),
                          quant05 = quantile(na.omit(RESULT_VA),probs=c(0.05))
                          )
}
