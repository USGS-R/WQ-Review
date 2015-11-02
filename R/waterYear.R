#'Stolen from smwrBase to remove dependency on that package
#' Takes a date and returns the water year
#' @param x an object of class "Date" or "POSIXt." Missing values are permitted and result in corresponding missing values in the output.
#' @param numeric numeric a logical value that indicates whether the returned values should be numeric TRUE or an ordered factor FALSE. The default value is FALSE.
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