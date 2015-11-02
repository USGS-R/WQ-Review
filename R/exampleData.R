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
        packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}