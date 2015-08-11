#' Function to open the WQReview GUI
#' 
#' @export
#' @import ggplot2
#' @import shinydashboard
#' @import shiny
#' @import stringr
#' @import lubridate
#' @import DT
#' @import RODBC
#' @import plyr
#' @import reshape2

WQReviewGUI <- function() {
  appDir <- system.file("shiny", "WQReviewGUI",package = "WQReview")
  if (appDir == "") {
    stop("Could not find GUI directory. Try re-installing `WQReview`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}