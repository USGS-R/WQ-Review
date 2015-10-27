# Companion function to readNWISodbc to convert dates to proper time zone
convertTime <- function(datetime,timezone,daylight)
{

datetime <- as.POSIXct(datetime, tz="GMT")
##Format date times to posixct with timezone, NWIS has 4 slots for characters followed by a tab, so for 3 letters you need 1 space and 1 tab after
offsetLibrary <- setNames(c(5, 4, 6, 5, 7, 6, 8, 7, 9, 
                            8, 10, 10), c("EST   ", "EDT   ", "CST   ", "CDT   ", "MST   ", 
                                          "MDT   ", "PST   ", "PDT   ", "AKST  ", "AKDT  ", "HAST  ", "HST   "))
offset <- offsetLibrary[timezone] * 60 * 60
daylight.offset <- ifelse (daylight == "Y", 60*60,0)

date.adj <- datetime - offset + daylight.offset
return(date.adj)
}
