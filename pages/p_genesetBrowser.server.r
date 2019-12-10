

functionTable <- reactive({
  if (input$dataset_select != input$dataset_selectB) {
    
  g.all.intersect <- intersect(envA()$gene.info$ids, envB()$gene.info$ids)
  g1 <- sapply(envA()$gs.def.list, function(x){intersect(x$Genes, g.all.intersect)}) 
  g2 <- sapply(envB()$gs.def.list, function(x){intersect(x$Genes, g.all.intersect)}) 
  Common_Functions  <- intersect(names(g1), names(g2))
 
  # # matrix p-values for genes in each common function
  # data.p <- sapply(Common_Functions, function(x){
  #   # Contingency table
  #   a <- length(intersect(g2[x][[1]],g1[x][[1]]))
  #   b <- length(setdiff(g2[x][[1]], intersect(g2[x][[1]],g1[x][[1]]))) #b <- length(g2)-a
  #   c <- length(setdiff(g1[x][[1]], intersect(g2[x][[1]],g1[x][[1]]))) #c <- length(g1)-a
  #   d <- length(g.all.intersect) - a - b - c
  #   data.fisher <- matrix(c(a,c,b,d),2)
  #   #Fisher´s exact test
  #   p <- fisher.test(data.fisher, alternative = "greater")
  #   p$p.value 
  # })
  # # Sort table according to p-value
  # data.p <- sort(data.p)
  
  
  # Sort table according to Common_Genes for each common function
  Common_Genes <- sapply(Common_Functions, function(x) length(intersect(g2[x][[1]],g1[x][[1]])))
  Common_Genes <- sort(Common_Genes)
  Common_Genes <- sort(Common_Genes, decreasing = TRUE)
  
  Genset1 <- sapply(g1, function(x) length(x))
  Genset2 <- sapply(g2, function(x) length(x))
  
  p_genesetBrowser_genesetTableCommon <- 
    data.frame(
      Name = names(Common_Genes),
      #p_value= format.pval(data.p),
      Common_Genes = Common_Genes,
      Genset1 = Genset1[names(Common_Genes)],
      Genset2 = Genset2[names(Common_Genes)])
  
  # Functions only in EnvA or EnvB
  only.A <- setdiff(names(envA()$gs.def.list), Common_Functions)
  only.B <- setdiff(names(envB()$gs.def.list), Common_Functions)
  
  p_genesetBrowser_genesetTableA <- 
    data.frame(
      Name = only.A,
      #p_value= "-",
      Common_Genes = "-",
      Genset1 = Genset1[only.A],
      Genset2 = "-")
  
  p_genesetBrowser_genesetTableB <- 
    data.frame(
      Name = only.B,
      #p_value= "-",
      Common_Genes = "-",
      Genset1 = "-",
      Genset2 = Genset2[only.B])
  
# rbind output Table
  p_genesetBrowser_genesetTable <- rbind(p_genesetBrowser_genesetTableCommon, 
                                         p_genesetBrowser_genesetTableA,
                                         p_genesetBrowser_genesetTableB)
  
  } else {
    dataA <- envA()$gs.def.list[which(!duplicated(names(envA()$gs.def.list)))]
    p_genesetBrowser_genesetTable <-
      data.frame(
        Name = names(dataA),
        #p_value= "-",
        Common_Genes = "",
        Genset1 = sapply(dataA, function(x) length(x$Genes)),
        Genset2 = "",
        stringsAsFactors = FALSE)
  }
  
  
  return(
    p_genesetBrowser_genesetTable
  )
})


output$p_genesetBrowser_genesetTable <- renderDataTable({
  return(
    functionTable()
  )
}, rownames=F, selection = 'single')



output$p_genesetBrowser_geneProfileA <- renderPlotly({
  
  clicked.name <- functionTable()$Name[input$p_genesetBrowser_genesetTable_row_last_clicked]
  clicked.row.envA <- first(which(rownames(envA()$samples.GSZ.scores) == clicked.name)) # first() added due to few duplicates in genset function name
  
  if (!is.null( input$p_genesetBrowser_genesetTable_row_last_clicked ))
  {
    if (clicked.name %in% rownames(envA()$samples.GSZ.scores))
    {
      df <- data.frame(sample=1:length(envA()$samples.GSZ.scores[clicked.row.envA,]), 
                       expression =round(envA()$samples.GSZ.scores[clicked.row.envA,], 4), 
                       group = "", 
                       stringsAsFactors = FALSE)
      df$group <- as.factor(envA()$group.labels[rownames(df)])
      df$group <- ordered(df$group, levels=names(envA()$groupwise.group.colors))
      
      if( input$p_genesetBrowser_checkboxA )
      {
        p <- ggplot(df, aes(group, expression)) + 
          theme_light() +
          geom_boxplot(aes(fill = group), outlier.shape=2, size = 0.2)+
          scale_fill_manual(values = envA()$groupwise.group.colors[df$group]) +
          labs(title= rownames(envA()$samples.GSZ.scores)[clicked.row.envA], x = "", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envA()$samples.GSZ.scores), max(envA()$samples.GSZ.scores)))+
          theme(legend.position = "none",
                plot.title = element_text(size=11, hjust = 0.5),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                axis.text.x= element_text(angle=20))
        ggplotly(p) %>%
          config(displayModeBar = FALSE)
        
      } else
      {
        p <- ggplot(df, aes(sample, expression)) + 
          theme_light() +
          geom_bar(aes(fill = group, text = paste("group:", group)), stat = "identity")+
          scale_fill_manual(values = envA()$groupwise.group.colors[df$group])+
          labs(title= rownames(envA()$samples.GSZ.scores)[clicked.row.envA], x = "samples", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envA()$samples.GSZ.scores), max(envA()$samples.GSZ.scores)))+
          theme(legend.position = "none",
                plot.title = element_text(size=11, hjust = 0.5),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        ggplotly(p) %>%
          config(displayModeBar = FALSE)
        
      }
    }
  }
  
})



output$p_genesetBrowser_geneProfileB <- renderPlotly({
  
  clicked.name <- functionTable()$Name[input$p_genesetBrowser_genesetTable_row_last_clicked]
  clicked.row.envB <- first(which(rownames(envB()$samples.GSZ.scores) == clicked.name)) # first() added due to few duplicates in genset/function name
  
  if (!is.null( input$p_genesetBrowser_genesetTable_row_last_clicked ))
  {
    if (clicked.name %in% rownames(envB()$samples.GSZ.scores))
    {
      df <- data.frame(sample=1:length(envB()$samples.GSZ.scores[clicked.row.envB,]), 
                       expression =round(envB()$samples.GSZ.scores[clicked.row.envB,], 4), 
                       group = "", 
                       stringsAsFactors = FALSE)
      df$group <- as.factor(envB()$group.labels[rownames(df)])
      df$group <- ordered(df$group, levels=names(envB()$groupwise.group.colors))
      
      if( input$p_genesetBrowser_checkboxB )
      {
        p <- ggplot(df, aes(group, expression)) + 
          theme_light() +
          geom_boxplot(aes(fill = group), outlier.shape=2, size = 0.2)+
          scale_fill_manual(values = envB()$groupwise.group.colors[df$group]) +
          labs(title= rownames(envB()$samples.GSZ.scores)[clicked.row.envB], x = "", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envB()$samples.GSZ.scores), max(envB()$samples.GSZ.scores)))+
          theme(legend.position = "none",
                plot.title = element_text(size=11, hjust = 0.5),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                axis.text.x= element_text(angle=20))
        ggplotly(p) %>%
          config(displayModeBar = FALSE)
        
      } else
      {
        p <- ggplot(df, aes(sample, expression)) + 
          theme_light() +
          geom_bar(aes(fill = group, text = paste("group:", group)), stat = "identity")+
          scale_fill_manual(values = envB()$groupwise.group.colors[df$group])+
          labs(title= rownames(envB()$samples.GSZ.scores)[clicked.row.envB], x = "samples", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envB()$samples.GSZ.scores), max(envB()$samples.GSZ.scores)))+
          theme(legend.position = "none",
                plot.title = element_text(size=11, hjust = 0.5),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        ggplotly(p) %>%
          config(displayModeBar = FALSE)
        
      }
    }
  }
  
})
