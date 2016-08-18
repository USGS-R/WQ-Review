###This subsets the qw.data dataframe to selected sites and results

selData_blank <- reactive({
        if(input$siteSel_blank == "All")
        {
                plotTable[plotTable$PARM_CD %in% input$parmSel_blank &
                                  plotTable$MEDIUM_CD %in% c("OAQ","OA"),]
        } else {
                plotTable[plotTable$SITE_NO %in% input$siteSel_blank &
                                  plotTable$PARM_CD %in% input$parmSel_blank &
                                  plotTable$MEDIUM_CD %in% c("OAQ","OA"),]
        }
})


#######################################
###This does the timeseries plotting###
#######################################

output$qwblankPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_blank) & !is.null(input$parmSel_blank),
                      "No site or parameter selected"))
        
        if(input$siteSel_blank == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_blank)
        }
        
        qwblankPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = sites,
                 plotparm = as.character(input$parmSel_blank),
                 facet = input$facetSel_blank,
                 highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                      reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_blank))]),
                 printPlot=FALSE
        ) + theme_bw()  
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwblankPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges_blank$x), "Select area in upper plot to zoom"))
        
        
        if(input$siteSel_seasonal == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_blank)
        }
        
        qwblankPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = sites,
                 plotparm = as.character(input$parmSel_blank),
                 facet = input$facetSel_blank,
                 highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                      reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_blank))]),
                 printPlot = FALSE) + theme_bw() +  
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = as.POSIXct(ranges_blank$x,origin="1970-01-01 00:00.00 UTC"), ylim = ranges_blank$y)
})

#########################################
###This does the plotting interactions###
#########################################

##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_blank <- "SAMPLE_START_DT"
yvar_blank <- "RESULT_VA"
##################################################

###This sets the ranges_blank variables for brushin
ranges_blank <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush_blank
        if (!is.null(brush)) {
                ranges_blank$x <- c(brush$xmin, brush$xmax)
                ranges_blank$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges_blank$x <- NULL
                ranges_blank$y <- NULL
        }
})

blank_clickReactive <- reactive({
        nearPoints(df=selData_blank(),
                   coordinfo = input$plot_click_seasonal,
                   xvar=xvar_seasonal,
                   yvar=yvar_seasonal)
})

###This outputs the data tables for clicked and brushed points

output$blank_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=selData_blank(),
                                 coordinfo = input$plot_click_blank,
                                 xvar=xvar_blank,
                                 yvar=yvar_blank),
                      
                      options=list(scrollX=TRUE)
        )
})


output$blank_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=selData_blank(),
                                    brush=input$plot_brush_blank,
                                    xvar=xvar_blank,
                                    yvar=yvar_blank),
                      
                      options=list(scrollX=TRUE)
        )
})

###This prints info about the hovered point. It is very messy with the code in places. 
###Basically it uses nearPoints() to get the dataframe and then extracts the information 
###from that dataframe. For the flag results, it pulls the record number from the 
###nearPoints() dataframe and then uses that ot subset the flag table.
###It then returns the column names of columns that have flags in them.

output$blank_hoverinfo <- renderPrint({
        
        hoverTable <- nearPoints(df=selData_blank(),
                                 coordinfo = input$plot_hover,
                                 xvar=xvar_blank,
                                 yvar=yvar_blank)
        
        cat("Record #:",unique(hoverTable$RECORD_NO),
            "\n"
        );
        
        cat("Site #:",unique(hoverTable$SITE_NO),
            "\n");
        
        cat("Station:",unique(hoverTable$STATION_NM),
            "\n");
        cat("Date/time:",format(unique(hoverTable$SAMPLE_START_DT,"%Y-%m-%d %H:%M")),
            "\n");
        cat("Chemical flags:",
            names(subset(reports$chemFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[7:10])[which(sapply(subset(reports$chemFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[7:10], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Pesticide flags:",
            names(subset(reports$pestFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[11:12])[which(sapply(subset(reports$pestFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[11:12], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Result flags:",
            names(subset(reports$resultFlagTable,PARM_CD == input$parmSel_blank & RECORD_NO == unique(hoverTable$RECORD_NO))[14:17])[which(sapply(subset(reports$resultFlagTable,PARM_CD == input$parmSel_blank & RECORD_NO == unique(hoverTable$RECORD_NO))[14:17], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        
        
})

###This creates a new entry in the marked record table
observeEvent(input$blank_addRecord, {
        try({
                newEntry <- getEntry(qw.data,
                                     flaggedRecord = input$blank_flaggedRecord,
                                     whereFlagged = "blank",
                                     flaggedStatus = input$blank_flaggedStatus,
                                     flaggedComment = input$blank_flaggedComment,
                                     parmSel = input$parmSel_blank)
                
                markedRecords <<- rbind(markedRecords,newEntry)
                
                updateTextInput(session, 
                                "blank_flaggedRecord",
                                value = " "
                )
                
                updateTextInput(session, 
                                "blank_flaggedComment",
                                value = " "
                )
                
                
        })
})