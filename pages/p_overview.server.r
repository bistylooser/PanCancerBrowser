
output$p_overview_dataset <- renderText({
  
  names(selectable.data)[ which(selectable.data==input$dataset_select) ]
  
})
  
  
output$p_overview_textpanel <- renderTable({
  
  ret <- matrix( c( "Number of samples: ", length(envA()$group.labels),
            "Number of subgroups: ", length(unique(envA()$group.labels)),
            "Number of genes: ", length(envA()$gene.info$ids),
            "Dimension of the SOM: ", paste(envA()$preferences$dim.1stLvlSom, "x", envA()$preferences$dim.1stLvlSom ),
            "Date of calculation: ", envA()$preferences$started,
            "Analyst: ", envA()$preferences$system.info["user"],
            "oposSOM version: ", envA()$preferences$session.info$otherPkgs$oposSOM$Version ), ncol=2, byrow = T )

}, colnames = FALSE, width = "100%" )
  



