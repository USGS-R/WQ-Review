#######################################
###This does the timeseries plotting###
#######################################

output$qwblankPlot <- renderPlot({
        qwblankPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_blank),
                 plotparm = as.character(input$parmSel_blank),
                 facet = input$facetSel_blank,
                 highlightrecords = c(reports$sampleFlagTable$RECORD_NO,
                                      reports$resultFlagTable$RECORD_NO),
                 print=FALSE
        ) + theme_bw()  
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwblankPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges$x), "Select area in upper plot to zoom"))
        qwblankPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_blank),
                 plotparm = as.character(input$parmSel_blank),
                 facet = input$facetSel_blank,
                 highlightrecords = c(reports$sampleFlagTable$RECORD_NO,
                                      reports$resultFlagTable$RECORD_NO),
                 printPlot = FALSE) + theme_bw() +  
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = as.POSIXct(ranges$x,origin="1970-01-01 00:00.00 UTC"), ylim = ranges$y)
})

#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections <- reactiveValues(siteSel = NULL, parmSel = NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections$siteSel <- input$siteSel_blank
        dataSelections$parmSel <- input$parmSel_blank
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_blank <- "SAMPLE_START_DT"
yvar_blank <- "RESULT_VA"
##################################################

###This sets the ranges variables for brushin
ranges <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush
        if (!is.null(brush)) {
                ranges$x <- c(brush$xmin, brush$xmax)
                ranges$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges$x <- NULL
                ranges$y <- NULL
        }
})

###This outputs the data tables for clicked and brushed points

output$blank_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel & MEDIUM_CD %in% c("OAQ","OA")),
                                 coordinfo = input$plot_click,
                                 xvar=xvar_blank,
                                 yvar=yvar_blank),
                      
                      options=list(scrollX=TRUE)
        )
})


output$blank_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel & MEDIUM_CD %in% c("OAQ","OA")),
                                    brush=input$plot_brush,
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
        
        cat("Record #:",unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                          coordinfo = input$plot_hover,
                                          xvar=xvar_blank,
                                          yvar=yvar_blank)$RECORD_NO),
            "\n"
        );
        
        cat("Site #:",unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                        coordinfo = input$plot_hover,
                                        xvar=xvar_blank,
                                        yvar=yvar_blank)$SITE_NO),
            "\n");
        
        cat("Station:",unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                         coordinfo = input$plot_hover,
                                         xvar=xvar_blank,
                                         yvar=yvar_blank)$STATION_NM),
            "\n");
        cat("Date/time:",format(unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                           coordinfo = input$plot_hover,
                                           xvar=xvar_blank,
                                           yvar=yvar_blank)$SAMPLE_START_DT,"%Y-%m-%d %H:%M")),
            "\n");
        cat("Chemical flags:",
            names(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                                                              coordinfo = input$plot_hover,
                                                                              xvar=xvar_blank,
                                                                              yvar=yvar_blank)$RECORD_NO))[7:10])[which(sapply(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                                                                                                                                                                              coordinfo = input$plot_hover,
                                                                                                                                                                                              xvar=xvar_blank,
                                                                                                                                                                                              yvar=yvar_blank)$RECORD_NO))[7:10], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Pesticide flags:",
            names(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                                                              coordinfo = input$plot_hover,
                                                                              xvar=xvar_blank,
                                                                              yvar=yvar_blank)$RECORD_NO))[11:12])[which(sapply(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                                                                                                                                                                               coordinfo = input$plot_hover,
                                                                                                                                                                                               xvar=xvar_blank,
                                                                                                                                                                                               yvar=yvar_blank)$RECORD_NO))[11:12], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Result flags:",
            names(subset(reports$resultFlagTable,PARM_CD == dataSelections$parmSel & RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                                                                                                    coordinfo = input$plot_hover,
                                                                                                                    xvar=xvar_blank,
                                                                                                                    yvar=yvar_blank)$RECORD_NO))[14:17])[which(sapply(subset(reports$resultFlagTable,PARM_CD == dataSelections$parmSel & RECORD_NO == unique(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel),
                                                                                                                                                                                                                                                                           coordinfo = input$plot_hover,
                                                                                                                                                                                                                                                                           xvar=xvar_blank,
                                                                                                                                                                                                                                                                           yvar=yvar_blank)$RECORD_NO))[14:17], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        
        
})

