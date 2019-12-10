
output$p_overview_textpanel <- renderTable({
  
  name.envA <- names(selectable.data)[ which(selectable.data==input$dataset_select) ]
  name.envB <- names(selectable.data)[ which(selectable.data==input$dataset_selectB) ]
  
  link.envA <- paste0("<a href='",  info.links[name.envA,"Url"], "' target='_blank'>", info.links[name.envA,"Publication"], "</a>")
  link.envB <- paste0("<a href='",  info.links[name.envB,"Url"], "' target='_blank'>", info.links[name.envB,"Publication"], "</a>")

  ret <- matrix( c(
    "", "Data set 1", "Data set 2",
    "Name:", name.envA, name.envB,
    "Publication:", link.envA, link.envB,
    "Number of samples: ", length(envA()$group.labels), length(envB()$group.labels),
    "Number of subgroups: ", length(unique(envA()$group.labels)), length(unique(envB()$group.labels)),
    "Number of genes: ", length(envA()$gene.info$ids), length(envB()$gene.info$ids),
    "Dimension of the SOM: ", paste(envA()$preferences$dim.1stLvlSom, "x", envA()$preferences$dim.1stLvlSom ),
                              paste(envB()$preferences$dim.1stLvlSom, "x", envB()$preferences$dim.1stLvlSom ),
    "Date of calculation: ", envA()$preferences$started, envB()$preferences$started,
    "Analyst: ", envA()$preferences$system.info["user"], envB()$preferences$system.info["user"],
    "oposSOM version: ", envA()$preferences$session.info$otherPkgs$oposSOM$Version,
                        envA()$preferences$session.info$otherPkgs$oposSOM$Version),
    ncol=3, byrow = T)

}, sanitize.text.function = function(x) x, colnames = FALSE, width = "100%" )
  



