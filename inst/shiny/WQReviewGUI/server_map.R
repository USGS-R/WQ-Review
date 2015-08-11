#######################################
###This does the spatial plotting###
#######################################
observeEvent(input$getMap, {
        siteMap <<- get_map(location = c(lon = as.numeric(median(qw.data$PlotTable$DEC_LONG_VA[!duplicated(qw.data$PlotTable$DEC_LONG_VA)],na.rm=TRUE)), 
                                     lat = as.numeric(median(qw.data$PlotTable$DEC_LAT_VA[!duplicated(qw.data$PlotTable$DEC_LAT_VA)],na.rm=TRUE))), 
                        zoom = "auto",
                        maptype = "terrain", 
                        scale = "auto")
})

output$qwmapPlot <- renderPlot({
        qwmapPlot(qw.data = qw.data,
                  map = siteMap,
                    site.selection = as.character(input$siteSel_map),
                    plotparm = as.character(input$parmSel_map),
        ) 
})

output$qwmapPlot_zoom <- renderPlot({
        qwmapPlot(qw.data = qw.data,
                  map=siteMap,
                    site.selection = as.character(input$siteSel_map),
                    plotparm = as.character(input$parmSel_map),
                    ) +  
                ###This resets the axes to zoomed area, must specify origin because brushedPoints returns time in seconds from origin, not hte posixCT "yyyy-mm-dd" format
                coord_cartesian(xlim = ranges$x, ylim = ranges$y)
})

#########################################
###This does the plotting interactions###
#########################################

###These are the values to subset the data by for dataTable ouput
dataSelections <- reactiveValues(siteSel = NULL, parmSel = NULL)

##################################################
###CHANGE these to the respective sidebar element
observe({
        dataSelections$siteSel <- input$siteSel_map
        dataSelections$parmSel <- input$parmSel_map
})
##################################################
##################################################
###CHANGE these to the respective plot variables
xvar_map <- "DEC_LONG_VA"
yvar_map <- "DEC_LAT_VA"
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

output$map_clickinfo <- DT::renderDataTable({
        DT::datatable(nearPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel & MEDIUM_CD %in% c("OAQ","OA")),
                                 coordinfo = input$plot_click,
                                 xvar=xvar_map,
                                 yvar=yvar_map),
                      
                      options=list(scrollX=TRUE)
        )
})


output$map_brushinfo <- DT::renderDataTable({
        DT::datatable(brushedPoints(df=subset(qw.data$PlotTable,SITE_NO %in% dataSelections$siteSel & PARM_CD %in% dataSelections$parmSel & MEDIUM_CD %in% c("OAQ","OA")),
                                    brush=input$plot_brush,
                                    xvar=xvar_map,
                                    yvar=yvar_map),
                      
                      options=list(scrollX=TRUE)
        )
})
