#######################################
###This does the matrixPlot plotting###
#######################################


output$qwmatrixPlot <- renderPlot({
        qwmatrixPlot(qw.data = qw.data,
                      site.selection = as.character(input$siteSel_matrix),
                      plotparm = as.character(input$parmSel_matrix)
        ) + theme_bw()  
})


