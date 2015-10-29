#'Stolen from smwrBase to remove dependency on that package

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