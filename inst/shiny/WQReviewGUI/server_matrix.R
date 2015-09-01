#######################################
###This does the matrixPlot plotting###
#######################################


output$qwmatrixPlot <- renderPlot({
        validate(need(!is.null(input$siteSel_matrix) & !is.null(input$parmSel_matrix),
                      "No site or parameter selected"))
        validate(need(!is.null(input$siteSel_matrix) & length(input$parmSel_matrix) > 1,
                      "More than 1 parameter required for a mtrix plot"))
        qwmatrixPlot(qw.data = qw.data,
                      site.selection = as.character(input$siteSel_matrix),
                      plotparm = as.character(input$parmSel_matrix)
        ) + theme_bw()  
})


