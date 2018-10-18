output$qwblankPlot <- renderPlot({
        validate(
                need(!is.null(input$siteSel_br) &
                             !is.null(input$parmSel_br),
                     "No site selected"))
        
        if(input$siteSel_br == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_br)
        }
        qwblankPlot(qw.data = qw.data,
                    plotparm = as.character(input$parmSel_br),
                    site.selection = sites,
                    facet = input$facetSel_br,
                    printPlot=FALSE) 
})

output$qwrepPlot <- renderPlot({
        validate(
                need(!is.null(input$siteSel_br) &
                             !is.null(input$parmSel_br),
                     "No site selected"))
        
        if(input$siteSel_br == "All")
        {
                sites <- unique(qw.data$PlotTable$SITE_NO)
        } else {
                sites <- as.character(input$siteSel_br)
        }
        qwrepPlot(qw.data = qw.data,
                  plotparm = as.character(input$parmSel_br),
                  site.selection = sites,
                  facet = input$facetSel_br,
                  printPlot=FALSE)
})