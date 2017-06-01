#' Function to open the WQReview GUI
#' @examples
#' \dontrun{ 
#' WQReviewGUI()
#' }
#' @export
#' @import shiny
#' @import excel.link
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableOutput
WQReviewGUI <- function() {
  appDir <- system.file("shiny", "WQReviewGUI",package = "WQReview")
  if (appDir == "") {
    stop("Could not find GUI directory. Try re-installing `WQReview`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}