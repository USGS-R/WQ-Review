###This subsets the qw.data dataframe to selected sites and results

selData_seasonal <- reactive({
        if(input$siteSel_seasonal == "All")
        {
                plotTable[plotTable$PARM_CD %in% input$parmSel_seasonal,]
        } else {
                plotTable[plotTable$SITE_NO %in% input$siteSel_seasonal &
                                  plotTable$PARM_CD %in% input$parmSel_seasonal,]
        }
})



#######################################
###This does the seasonal plotting###
#######################################




output$qwseasonalPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_seasonal) & !is.null(input$parmSel_seasonal),
                      "No site or parameter selected"))
        
        if(input$siteSel_seasonal == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_seasonal)
        }
        
        qwseasonalPlot(qw.data = qw.data,
                       new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                       site.selection = sites,
                       plotparm = as.character(input$parmSel_seasonal),
                       facet = input$facetSel_seasonal,
                       labelDQI = input$labelDQI_seasonal,
                       show.smooth = input$fit_seasonal,
                       highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                            reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_seasonal))]),
                       print = FALSE)
        
})


output$qwseasonalPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges_seasonal$x), "Select area in upper plot to zoom"))
        
        if(input$siteSel_seasonal == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_seasonal)
        }
        
        qwseasonalPlot(qw.data = qw.data,
                       new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                       site.selection = sites,
                       plotparm = as.character(input$parmSel_seasonal),
                       facet = input$facetSel_seasonal,
                       labelDQI = input$labelDQI_seasonal,
                       show.smooth = input$fit_seasonal,
                       highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                            reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_seasonal))]),
                       print = FALSE) + 
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = ranges_seasonal$x, ylim = ranges_seasonal$y)
})

#########################################
###This does the plotting interactions###
#########################################


##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_seasonal <- "DOY"
yvar_seasonal <- "RESULT_VA"
##################################################


###This sets the ranges_seasonal variables for brushin
ranges_seasonal <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush_seasonal
        if (!is.null(brush)) {
                ranges_seasonal$x <- c(brush$xmin, brush$xmax)
                ranges_seasonal$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges_seasonal$x <- NULL
                ranges_seasonal$y <- NULL
        }
})


###This outputs the data tables for clicked and brushed points
#Make this table reactive so that the values can be used to populate the review notes on click
seasonal_clickReactive <- reactive({
        nearPoints(df=selData_seasonal(),
                   coordinfo = input$plot_click_seasonal,
                   xvar=xvar_seasonal,
                   yvar=yvar_seasonal)
})

output$seasonal_clickinfo <- DT::renderDataTable(
        seasonal_clickReactive(),
        extensions = list(FixedColumns = list(leftColumns = 1)),
        server=TRUE,
        #selection = 'single',
        
        #rownames= FALSE,
        options = list(
                scrollX=TRUE,
                autoWidth=TRUE)
        # nearPoints() also works with hover and dblclick events
)

observeEvent(input$seasonal_popNotes, {
        updateTextInput(session, 
                        "sidebar_flaggedRecord",
                        value = seasonal_clickReactive()$RECORD_NO[as.numeric(input$seasonal_clickinfo_rows_selected)]
        )
        updateTextInput(session, 
                        "parmSel_sidebar",
                        value = seasonal_clickReactive()$PARM_CD[as.numeric(input$seasonal_clickinfo_rows_selected)]
        )
        updateSelectInput(session,
                          "sidebar_dqiCode",
                          selected=NA)
        updateRadioButtons(session,
                           "sidebar_flaggedStatus",
                           selected="No selection")
        updateTextInput(session, 
                        "sidebar_flaggedComment",
                        value = " "
        )
})
###This outputs the data tables for brushed points

output$seasonal_brushinfo <- DT::renderDataTable({
        if(is.null(selData_seasonal()))
        {
                return()
        } else {
        # With base graphics, need to tell it what the x and y variables are.
        DT::datatable(brushedPoints(df=selData_seasonal(),
                                    brush=input$plot_brush_seasonal,
                                    xvar=xvar_seasonal,
                                    yvar=yvar_seasonal),
                      
                      options=list(scrollX=TRUE)
        )
        }
        # nearPoints() also works with hover and dblclick events
})

###This prints info about the hovered point. It is very messy with the code in places. 
###Basically it uses nearPoints() to get the dataframe and then extracts the information 
###from that dataframe. For the flag results, it pulls the record number from the 
###nearPoints() dataframe and then uses that ot subset the flag table.
###It then returns the column names of columns that have flags in them.

output$seasonal_hoverinfo <- renderPrint({
        if(is.null(selData_seasonal()))
        {
                return()
        } else {
                hoverTable <- nearPoints(df=selData_seasonal(),
                                         coordinfo = input$plot_hover,
                                         xvar=xvar_seasonal,
                                         yvar=yvar_seasonal)
                
                
                
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
                    names(subset(reports$chemFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[7:11])[which(sapply(subset(reports$chemFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[7:11], function(x)all(is.na(x))) == FALSE)],
                    "\n");
                
                cat("Pesticide flags:",
                    names(subset(reports$pestFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[11:12])[which(sapply(subset(reports$pestFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[11:12], function(x)all(is.na(x))) == FALSE)],
                    "\n");
                
                cat("Result flags:",
                    names(subset(reports$resultFlagTable,PARM_CD == input$parmSel_seasonal & RECORD_NO == unique(hoverTable$RECORD_NO))[14:17])[which(sapply(subset(reports$resultFlagTable,PARM_CD == input$parmSel_seasonal & RECORD_NO == unique(hoverTable$RECORD_NO))[14:17], function(x)all(is.na(x))) == FALSE)],
                    "\n");
        }
})

###This creates a new entry in the marked record table
observeEvent(input$seasonal_addRecord, {
        try({
                newEntry <- getEntry(qw.data,
                                     flaggedRecord = input$seasonal_flaggedRecord,
                                     whereFlagged = "seasonal",
                                     flaggedStatus = input$seasonal_flaggedStatus,
                                     flaggedComment = input$seasonal_flaggedComment,
                                     parmSel = input$parmSel_seasonal)
                
                markedRecords <<- rbind(markedRecords,newEntry)
                
                updateTextInput(session, 
                                "seasonal_flaggedRecord",
                                value = " "
                )
                
                updateTextInput(session, 
                                "seasonal_flaggedComment",
                                value = " "
                )
                
                
        })
})
