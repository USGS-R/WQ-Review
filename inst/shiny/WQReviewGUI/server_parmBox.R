###This subsets the qw.data dataframe to selected sites and results

selData_parmBox <- reactive({
        if(input$siteSel_parmBox == "All")
        {
                plotTable[plotTable$PARM_CD %in% input$parmSel_parmBox & 
                                  plotTable$MEDIUM_CD %in%(c("WG ","WS ")),]
        } else {
                plotTable[plotTable$SITE_NO %in% input$siteSel_parmBox &
                                  plotTable$PARM_CD %in% input$parmSel_parmBox & 
                                  plotTable$MEDIUM_CD %in%(c("WG ","WS ")),]
        }
})


#######################################
###This does the parameter boxPlot plotting###
#######################################


output$qwparmBoxPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_parmBox) & !is.null(input$parmSel_parmBox),
                      "No site or parameter selected"))
        
        if(input$siteSel_parmBox == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_parmBox)
        }
        
        qwparmBoxPlot(qw.data = qw.data,
                      new.threshold = Sys.time()-as.POSIXct(input$newThreshold),
                      site.selection = sites,
                      plotparm = as.character(input$parmSel_parmBox),
                      log.scale = input$axes_parmBox,
                      facet = input$facetSel_parmBox,
                      show.points = input$showpoints_parmBox,
                      labelDQI = input$labelDQI_parmBox,
                      highlightrecords = c(reports$chemFlagTable$RECORD_NO[which(!is.na(reports$chemFlagTable$BadCB_30.21))],
                                           reports$resultFlagTable$RECORD_NO[which(reports$resultFlagTable$PARM_CD == as.character(input$parmSel_parmBox))]),
                      printPlot=FALSE)+ theme_bw()  
})


#########################################
###This does the plotting interactions###
#########################################


output$parmBox_sumStats <- DT::renderDataTable({
        DT::datatable(
                        dplyr::summarize(dplyr::group_by(selData_parmBox(),
                                                         PARM_CD,PARM_NM),
                                         
                                         Quant95 = quantile(na.omit(RESULT_VA),0.90),
                                         Quant05 = quantile(na.omit(RESULT_VA),0.10),
                                         Median = median(na.omit(RESULT_VA)),
                                         Mean = mean(na.omit(RESULT_VA)),
                                         Min = min(na.omit(RESULT_VA)),
                                         Max = max(na.omit(RESULT_VA))
                        ),
                options=list(scrollX=TRUE))
        
})






