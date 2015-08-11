#' Parameter matrix plot
#' 
#' Takes output data object from readNWISodbc and prints a parameter matrix plot
#' @param qw.data A qw.data object generated from readNWISodbc
#' @param site.selection A character vector of site IDs to plot
#' @param plotparm A character vector of parameters to plot
#' @export

qwmatrixPlot <- function(qw.data,
                        site.selection,
                       plotparm){

  
  
###Functions for matrix plot
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray", ...)
}

## put correlations on the upper panels

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y,use="na.or.complete"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt,cex=2)
}

data <- subset(qw.data$PlotTable,SITE_NO %in% (site.selection) & PARM_CD%in%(plotparm))
plotdata <- na.omit(dcast(data,RECORD_NO ~ PARM_CD, max,value.var = "RESULT_VA" ))

if (length(site.selection) == 1)
{
  maintitle <- str_wrap(unique(data$STATION_NM), width = 25)
  maintitle <- paste("\n",maintitle)
  } else (maintitle <- "Multisite plot \n")


pairs(plotdata[2:ncol(plotdata)], diag.panel = panel.hist, upper.panel=panel.cor,cex.labels = 2, font.labels = 1,main=maintitle)

}