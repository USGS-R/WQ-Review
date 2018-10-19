#' @name qw.data
#' @title Output qw.data object for the Big Thompson River generated using readNWISodbc.
#' @description This contains all data for the Big Thompson River site number 06733000 pulled from NWIS using the readNWISodbc function. 
#' This dataset is used for all run examples.
#' @docType data
#' @usage qw.data
#' @source Colorado NWIS server
#' @author Joe Mills, 2015-11-02
NULL

#' @name reports
#' @title Output reports object for the Big Thompson River generated using various report functions.
#' @description This contains all data for the Big Thompson River site number 06733000 pulled from NWIS using the readNWISodbc function. 
#' This dataset is used for all run examples.
#' @docType data
#' @usage qw.data
#' @source Colorado NWIS server
#' @author Joe Mills, 2015-11-02
NULL

.onAttach <- function(libname, pkgname) {
        packageStartupMessage("
                              Disclaimer: This software has been approved for release by the U.S. Geological Survey
                              (USGS). Although the software has been subjected to rigorous review, the USGS
                              reserves the right to update the software as needed pursuant to further analysis
                              and review. No warranty, expressed or implied, is made by the USGS or the U.S.
                              Government as to the functionality of the software and related material nor
                              shall the fact of release constitute any such warranty. Furthermore, the
                              software is released on condition that neither the USGS nor the U.S. Government
                              shall be held liable for any damages resulting from its authorized or
                              unauthorized use.")
}