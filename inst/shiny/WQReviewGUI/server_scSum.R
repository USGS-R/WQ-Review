#######################################
###This does the timeseries plotting###
#######################################

output$qwscSumPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_scSum),
                      "No site selected"))
        qwscSumPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_scSum),
                 facet = input$facetSel_scSum,
                 highlightrecords = c(reports$sampleFlagTable$RECORD_NO),
                 print=FALSE)
})

output$tableOut <- renderPrint(input$wideDataTable_rows_selected)

output$qwscSumPlot_zoom <- renderPlot({
        validate(need(!is.null(ranges_scSum$x), "Select area in upper plot to zoom"))
        qwscSumPlot(qw.data = qw.data,
                 new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                 site.selection = as.character(input$siteSel_scSum),
                 facet = input$facetSel_scSum,
                 highlightrecords = c(reports$sampleFlagTable$RECORD_NO,
                                      reports$resultFlagTable$RECORD_NO),
                 printPlot = FALSE) + 
                ###This resescSum the axes to zoomed area, must specify origin because brushedPoinscSum returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = ranges_scSum$x, ylim = ranges_scSum$y)
})

#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections_scSum <- reactiveValues(siteSel = NULL, parmSel = NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections_scSum$siteSel <- input$siteSel_scSum
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_scSum <- "RESULT_VA"
yvar_scSum <- "value"
##################################################

###This sets the ranges_scSum variables for brushin
ranges_scSum <- reactiveValues(x = NULL, y = NULL)

observe({
        brush <- input$plot_brush_scSum
        if (!is.null(brush)) {
                ranges_scSum$x <- c(brush$xmin, brush$xmax)
                ranges_scSum$y <- c(brush$ymin, brush$ymax)
                
        } else {
                ranges_scSum$x <- NULL
                ranges_scSum$y <- NULL
        }
})

###This outputs the data tables for clicked and brushed points

output$scSum_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                         id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                 coordinfo = input$plot_click_scSum,
                                 xvar=xvar_scSum,
                                 yvar=yvar_scSum),
                      
                      options=list(scrollX=TRUE)
        )
})


output$scSum_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                            id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                    brush=input$plot_brush_scSum,
                                    xvar=xvar_scSum,
                                    yvar=yvar_scSum),
                      
                      options=list(scrollX=TRUE)
        )
})

###This prints info about the hovered point. It is very messy with the code in places. 
###Basically it uses nearPoints() to get the dataframe and then extracts the information 
###from that dataframe. For the flag results, it pulls the record number from the 
###nearPoints() dataframe and then uses that ot subset the flag table.
###It then returns the column names of columns that have flags in them.

output$scSum_hoverinfo <- renderPrint({
        
        cat("Record #:",unique(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                                  id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                          coordinfo = input$plot_hover_scSum,
                                          xvar=xvar_scSum,
                                          yvar=yvar_scSum)$RECORD_NO),
            "\n"
        );
        
        cat("Site #:",unique(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                                id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                        coordinfo = input$plot_hover_scSum,
                                        xvar=xvar_scSum,
                                        yvar=yvar_scSum)$SITE_NO),
            "\n");
        
        cat("Station:",unique(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                                 id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                         coordinfo = input$plot_hover_scSum,
                                         xvar=xvar_scSum,
                                         yvar=yvar_scSum)$STATION_NM),
            "\n");
        cat("Date/time:",format(unique(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                                          id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                                  coordinfo = input$plot_hover_scSum,
                                                  xvar=xvar_scSum,
                                                  yvar=yvar_scSum)$SAMPLE_START_DT,"%Y-%m-%d %H:%M")),
            "\n");
        cat("Chemical flags:",
            names(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                                                                      id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                                                              coordinfo = input$plot_hover_scSum,
                                                                              xvar=xvar_scSum,
                                                                              yvar=yvar_scSum)$RECORD_NO))[7:11])[which(sapply(subset(reports$chemFlagTable,RECORD_NO == unique(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                                                                                                                                                                                   id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                                                                                                                                                                           coordinfo = input$plot_click_scSum,
                                                                                                                                                                                           xvar=xvar_scSum,
                                                                                                                                                                                           yvar=yvar_scSum)$RECORD_NO))[7:11], function(x)all(is.na(x))) == FALSE)],
            "\n");
        
        cat("Pesticide flags:",
            names(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                                                                      id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                                                              coordinfo = input$plot_hover_scSum,
                                                                              xvar=xvar_scSum,
                                                                              yvar=yvar_scSum)$RECORD_NO))[11:12])[which(sapply(subset(reports$pestFlagTable,RECORD_NO == unique(nearPoints(df=melt(subset(qw.data$PlotTable,SITE_NO %in% dataSelections_scSum$siteSel & PARM_CD== "00095")[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","sum_cat","sum_an","complete.chem","perc.diff")],
                                                                                                                                                                                                    id.vars=c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_MD","MEDIUM_CD","RESULT_VA","complete.chem","perc.diff")),
                                                                                                                                                                                            coordinfo = input$plot_click_scSum,
                                                                                                                                                                                            xvar=xvar_scSum,
                                                                                                                                                                                            yvar=yvar_scSum)$RECORD_NO))[11:12], function(x)all(is.na(x))) == FALSE)],
            "\n");
        

        
        
})