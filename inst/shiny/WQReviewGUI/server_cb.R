###This subsets the qw.data dataframe to selected sites and results

selData_cb <- reactive({
        if(input$siteSel_cb == "All")
        {
                unique(plotTable[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","perc.diff","sum_an","sum_cat")])
        } else {
                unique(plotTable[plotTable$SITE_NO %in% input$siteSel_cb,
                                 c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","perc.diff","sum_an","sum_cat")])
        }
})


#######################################
###This does the timeseries plotting###
#######################################

output$qwcbPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_cb),
                      "No site selected"))
        
        if(input$siteSel_cb == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_cb)
        }
        
        qwcbPlot(qw.data = qw.data,
                 site.selection = sites,
                 facet = input$facetSel_cb,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 show.smooth = FALSE,
                 highlightrecords = reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                 labelDQI = input$labelDQI_cb,
                 printPlot = FALSE)
        
        
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwcbPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges_cb$x), "Select area in upper plot to zoom"))
        
        if(input$siteSel_cb == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_cb)
        }
        
        qwcbPlot(qw.data = qw.data,
                 site.selection = as.character(input$siteSel_cb),
                 facet = input$facetSel_cb,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 show.smooth = FALSE,
                 highlightrecords = reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                 labelDQI = input$labelDQI_cb,
                 printPlot = FALSE) +  
                ###This resecb the axes to zoomed area, must specify origin because brushedPoincb returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = as.POSIXct(ranges_cb$x,origin="1970-01-01 00:00.00 UTC"), ylim = ranges_cb$y)
})

#########################################
###This does the plotting interactions###
#########################################

##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_cb <- "SAMPLE_START_DT"
yvar_cb <- "perc.diff"
##################################################

###This sets the ranges_cb variables for brushin
ranges_cb <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush_cb
        if (!is.null(brush)) {
                ranges_cb$x <- c(brush$xmin, brush$xmax)
                ranges_cb$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges_cb$x <- NULL
                ranges_cb$y <- NULL
        }
})

###This outputs the data tables for clicked and brushed points

output$cb_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=selData_cb(),
                                 coordinfo = input$plot_click_cb,
                                 xvar=xvar_cb,
                                 yvar=yvar_cb),
                      
                      options=list(scrollX=TRUE)
        )
})


output$cb_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=selData_cb(),
                                    brush=input$plot_brush_cb,
                                    xvar=xvar_cb,
                                    yvar=yvar_cb),
                      
                      options=list(scrollX=TRUE)
        )
})

###This prints info about the hovered point. It is very messy with the code in places. 
###Basically it uses nearPoints() to get the dataframe and then extracts the information 
###from that dataframe. For the flag results, it pulls the record number from the 
###nearPoints() dataframe and then uses that ot subset the flag table.
###It then returns the column names of columns that have flags in them.

output$cb_hoverinfo <- renderPrint({
        
        hoverTable <- nearPoints(df=selData_cb(),
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
        cat("Date/time:",format(hoverTable$SAMPLE_START_DT,"%Y-%m-%d %H:%M"),
            "\n");
        cat("Chemical flags:",
            names(subset(reports$chemFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[7:11])[which(sapply(subset(reports$chemFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[7:11], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Pesticide flags:",
            names(subset(reports$pestFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[11:12])[which(sapply(subset(reports$pestFlagTable,RECORD_NO == unique(hoverTable$RECORD_NO))[11:12], function(x)all(is.na(x))) == FALSE)],
            "\n");
})


