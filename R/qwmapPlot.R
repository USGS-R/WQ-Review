# Parameter matrix plot - IN PROGRESS NOT EXPORTED
# 
# Takes output data object from readNWISodbc and prints a parameter matrix plot
# @param qw.data A qw.data object generated from readNWISodbc
# @param site.selection A character vector of site IDs to plot
# @param plotparm A character vector of parameters to plot
# @param map A ggmap object to plot data on, not required
# @param begin.date Data range to subset data to
# @param end.date Data range to subset data to


qwmapPlot <- function(qw.data,
                      site.selection,
                      plotparm,
                      map = NULL,
                      begin.date = NULL,
                      end.date = NULL) {
        
        if(!(is.null(begin.date)) & !(is.null(end.date)))
        {
                plotData <- subset(qw.data$PlotTable, SAMPLE_START_DT >= as.POSIXct(begin.date) &
                           SAMPLE_START_DT <= as.POSIXct(end.date))
        } else {plotData <- qw.data$PlotTable}

if(is.null(map))
{
map <- get_map(location = c(lon = as.numeric(median(plotData$DEC_LONG_VA[!duplicated(plotData$DEC_LONG_VA)],na.rm=TRUE)), 
                            lat = as.numeric(median(plotData$DEC_LAT_VA[!duplicated(plotData$DEC_LAT_VA)],na.rm=TRUE))), 
               zoom = "auto",
               maptype = "terrain", 
               scale = "auto")
}else{}
# plotting the map with some points on it
ggmap(map) +
        geom_point(data=plotData,aes(x = as.numeric(DEC_LONG_VA), y = as.numeric(DEC_LAT_VA),size=RESULT_VA,shape=SITE_NO,fill=length(RESULT_VA),alpha=0.8)) +
        geom_text(data=plotData,aes(x = as.numeric(DEC_LONG_VA), y = as.numeric(DEC_LAT_VA),label=STATION_NM),vjust=0,hjust=0,size=2)  


}