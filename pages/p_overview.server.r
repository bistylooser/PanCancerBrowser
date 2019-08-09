
output$p_overview_dataset <- renderText({
  
  names(selectable.data)[ which(selectable.data==input$dataset_select) ]
  
})
  
  
output$p_overview_textpanel <- renderTable({
  
  ret <- matrix( c( "Number of samples: ", length(env()$group.labels),
            "Number of subgroups: ", length(unique(env()$group.labels)),
            "Number of genes: ", length(env()$gene.info$ids),
            "Dimension of the SOM: ", paste(env()$preferences$dim.1stLvlSom, "x", env()$preferences$dim.1stLvlSom ),
            "Date of calculation: ", env()$preferences$started,
            "Analyst: ", env()$preferences$system.info["user"],
            "oposSOM version: ", env()$preferences$session.info$otherPkgs$oposSOM$Version ), ncol=2, byrow = T )

}, colnames = FALSE, width = "100%" )
  



