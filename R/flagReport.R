#' flagReport
#' 
#' Pulls data from internal NWIS server and runs automated data checks, generates tables, and produces
#' an indexed notebook of flagged samples, tables, and plots for all sites entered.
#' @param DSN A character string containing the DSN for your local server
#' @param env.db A character string containing the database number of environmental samples
#' @param qa.db A character string containing the database number of QA samples
#' @param STAIDS A character vector of stations IDs 
#' @param begin.date Character string containing beginning date of data pull (yyyy-mm-dd). Default is 10 years from current date.
#' @param end.date Character string containing ending date of data pull (yyyy-mm-dd). Default is current date.
#' @param outputFile Filename of report output.
#' @param outputDir Directory to store output files.
#' @export


flagReport <- function(DSN,
                       env.db = "01",
                       qa.db = "02",
                       STAIDS,
                       begin.date = Sys.Date()-365*10,
                       end.date = Sys.Date(),
                       outputFile = "flagReport.pdf",
                       outputDir = "output"){
        
        dir.create("output")
        
        rmarkdown::render("markdown/flagReport.rmd",params = list(DSN=DSN,
                                                     env.db = env.db,
                                                     qa.db=qa.db,
                                                     STAIDS=STAIDS,
                                                     begin.date=begin.date,
                                                     end.date=end.date
                                                     ), 
                          output_file=outputFile,
                          output_dir=outputDir
                          )
        
}


