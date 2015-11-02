#'Stolen from smwrBase to remove dependency on that package
#' Takes a date and returns the water year
#' @param x a vector of dates in yyyy-mm-dd format
#' @examples 
#' waterYear("2015-01-01")
#' @import plyr
#' @export
#' @return A dataframe containing all samples with applicable flags
waterYear <- function (x, numeric = FALSE) 
{
        x <- as.POSIXlt(x)
        yr <- x$year + 1900L
        mn <- x$mon + 1L
        yr <- yr + ifelse(mn < 10L, 0L, 1L)
        if (numeric) 
                return(yr)
        ordered(yr)
}