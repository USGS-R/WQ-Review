#########################################################################
###This downloads the excel templates stored in the QWToolbox package####
#########################################################################

output$srsTemplate <- downloadHandler(
        filename = function() {"srsTemplate.xlsx"},
        content = function(file) {
                file.rename(paste0(path.package("WQReview"), "/shiny/srsTemplate.xlsx"),
                            file)
        }
)
outputOptions(output, 'srsTemplate', suspendWhenHidden=FALSE)

#########################################################################
###This runs the dataUpload function and generates table output###
#########################################################################

observeEvent(input$srsSummary, {
        
        srsValues <- input$srsValues
        srsResults <- input$srsResults
        reportingLevels <- input$srsLRLs
        

        output$srsSummary <- DT::renderDataTable(

                srsSummary(srsValues= read.csv(srsValues$datapath,header=T,colClasses = "character"),
                           srsResults = read.csv(srsResults$datapath,header=T,colClasses = "character"),
                           reportingLevels = read.csv(reportingLevels$datapath,header=T,colClasses = "character")
                           ),
                server=TRUE,
                options = list(
                        scrollX=TRUE,
                        autoWidth=TRUE)
        
        )

})

