#########################################################################
###This downloads the excel templates stored in the QWToolbox package####
#########################################################################

output$templateExport <- downloadHandler(
        filename = function() {"uploadTemplates.xlsx"},
        content = function(file) {
                file.rename(paste0(path.package("QWToolbox"), "/shiny/uploadTemplates.xlsx"),
                            file)
        }
)
outputOptions(output, 'templateExport', suspendWhenHidden=FALSE)

#########################################################################
###This runs the dataUpload function and generates table output###
#########################################################################

    observeEvent(input$formatFiles, {
            
            labFile <- input$labFile
            pcodeFile <- input$pcodeFile
            qwsampleFile <- input$qwsampleFile
            
            if(is.null(qwsampleFile))
            {
                    qwsampleFile <- ""
            } else (qwsampleFile <- qwsample$datapath)
            
            withProgress(message="Formatting files",detail="Pulling data QWSample from NWIS",value=0.5,{
                    uploadFiles <<- NULL
        uploadFiles <<- dataUpload(
                    qwsampletype = as.numeric(input$qwsampleType),
                    #DSN = input$uploadDSN,
                    #env.db = input$uploadenv.db,
                    #qa.db = input$uploadqa.db,
                    labfile = labFile$datapath,
                    pcodefile = pcodeFile$datapath,
                    #qwsamplefile = qwsampleFile,
                    #qwresultname = input$qwresultName,
                    #qwsamplename = input$qwsampleName,
                    qwsample.begin.date = as.character(input$qwsampleBeginDate),
                    qwsample.end.date = as.character(input$qwsampleEndDate),
                    censor = input$censor
            )
            })
            output$qwsample <- DT::renderDataTable(
                    uploadFiles$qwsample,
                    server=TRUE,
                    options = list(
                            scrollX=TRUE,
                            autoWidth=TRUE)
            )
            output$qwresult <- DT::renderDataTable(
                    uploadFiles$qwresult,
                    server=TRUE,
                    options = list(
                            scrollX=TRUE,
                            autoWidth=TRUE)
            )
    })
         
#########################################################################
###This downloads the qwsample and qwresult files####
#########################################################################

output$qwsampleExport <- downloadHandler(
        filename = function() {"qwsample"},
        content = function(file) {
                write.table(uploadFiles$qwsample,sep="\t", col.names = F, row.names = F,na="", quote = FALSE,
                            file)
        }
)

outputOptions(output, 'qwsampleExport', suspendWhenHidden=FALSE)      


output$qwresultExport <- downloadHandler(
        filename = function() {"qwresult"},
        content = function(file) {
                write.table(uploadFiles$qwresult,sep="\t", col.names = F, row.names = F,na="", quote = FALSE,
                            file)
        }
)

outputOptions(output, 'qwresultExport', suspendWhenHidden=FALSE)    