#' templateExport exports templates for use with the dataUpload function
#' 
#' exports templates for use with the nwisupload function. Files exported to working directory as tab-delimited files.
#' @examples 
#' templateExport()
#' @export
#' 

templateExport <- function(){
  write.table(labdata,file="labfile.txt",sep="\t",row.names=FALSE,quote=FALSE)
  write.table(pcodedata,file="pcodefile.txt",sep="\t",row.names=FALSE,quote=FALSE,na="")
  
}