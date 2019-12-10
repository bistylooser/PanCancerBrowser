
geneTable <- reactive({
  p_geneBrowser_geneTableA <- 
    data.frame(
      Name=envA()$gene.info$names,
      Dataset1 = "x", 
      Dataset2 = "",
      Ensemble=(envA()$gene.info$ids),
      Chromosome=paste(envA()$gene.info$chr.name,envA()$gene.info$chr.band),
      Description=envA()$gene.info$descriptions,
      stringsAsFactors = FALSE) 
  
  p_geneBrowser_geneTableB <- data.frame(
    Name=envB()$gene.info$names,
    Dataset1 = "", 
    Dataset2 = "x", 
    Ensemble=envB()$gene.info$ids,
    Chromosome=paste(envB()$gene.info$chr.name,envB()$gene.info$chr.band),
    Description=envB()$gene.info$descriptions,
    stringsAsFactors = FALSE) 
  
  # combine TableA and TableB to p_geneBrowser_geneTable
  p_geneBrowser_geneTable.all <- rbind( p_geneBrowser_geneTableA, p_geneBrowser_geneTableB )
  p_geneBrowser_geneTable <- p_geneBrowser_geneTable.all[ which(!duplicated( p_geneBrowser_geneTable.all$Ensemble ) ), ] 
  
  # Mark Ensemble present in TableA and TableB with x 
  p_geneBrowser_geneTable[which(p_geneBrowser_geneTable$Ensemble %in% p_geneBrowser_geneTableA$Ensemble),"Dataset1"] <- "x" 
  p_geneBrowser_geneTable[which(p_geneBrowser_geneTable$Ensemble %in% p_geneBrowser_geneTableB$Ensemble),"Dataset2"] <- "x" 
  
  return(
    p_geneBrowser_geneTable
  )
})


output$p_geneBrowser_geneTable <- renderDataTable({
  return(
    geneTable()
  )
}, rownames=F, selection = 'single')



output$p_geneBrowser_geneProfileA <- renderPlotly({
  
  clicked.ensemble <- geneTable()$Ensemble[input$p_geneBrowser_geneTable_row_last_clicked]
  clicked.rows.envA <- which(envA()$gene.info$ids == clicked.ensemble)
  
  if (!is.null( input$p_geneBrowser_geneTable_row_last_clicked ))
  {
    if (clicked.ensemble %in% envA()$gene.info$ids)
      {
     
      if (length(clicked.rows.envA) > 1){
        expression.average <- colMeans(envA()$indata[clicked.rows.envA,])
      } else {
        expression.average <- envA()$indata[clicked.rows.envA,]
      }
      
      df <- data.frame(sample=1:length(expression.average), expression =round(expression.average, 4), group = "", stringsAsFactors = FALSE)
      df$group <- as.factor(envA()$group.labels[rownames(df)])
      df$group <- ordered(df$group, levels=names(envA()$groupwise.group.colors))
      
      if( input$p_geneBrowser_checkboxA )
      {
        p <- ggplot(df, aes(group, expression)) + 
          theme_light() +
          geom_boxplot(aes(fill = group), outlier.shape=2, size = 0.2)+
          scale_fill_manual(values = envA()$groupwise.group.colors[df$group]) +
          labs(title= paste(clicked.ensemble,"/", envA()$gene.info$names[first(clicked.rows.envA)]),
               x = "", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envA()$indata), max(envA()$indata)))+
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
         labs(title= paste(clicked.ensemble,"/", envA()$gene.info$names[first(clicked.rows.envA)]),
              x = "samples", y = "expression")+
         scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envA()$indata), max(envA()$indata)))+
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


output$p_geneBrowser_geneProfileB <- renderPlotly({
  
  clicked.ensemble <- geneTable()$Ensemble[input$p_geneBrowser_geneTable_row_last_clicked]
  clicked.rows.envB <- which(envB()$gene.info$ids == clicked.ensemble)
  
  if (!is.null( input$p_geneBrowser_geneTable_row_last_clicked ))
  {
    if (clicked.ensemble %in% envB()$gene.info$ids)
    {
      if (length(clicked.rows.envB) > 1){
        expression.average <- colMeans(envB()$indata[clicked.rows.envB,])
      } else {
        expression.average <- envB()$indata[clicked.rows.envB,]
      }
      
      df <- data.frame(sample=1:length(expression.average), expression =round(expression.average, 4), group = "", stringsAsFactors = FALSE)
      df$group <- as.factor(envB()$group.labels[rownames(df)])
      df$group <- ordered(df$group, levels=names(envB()$groupwise.group.colors))
      
      if( input$p_geneBrowser_checkboxB )
      {
        p <- ggplot(df, aes(group, expression)) + 
          theme_light() +
          geom_boxplot(aes(fill = group), outlier.shape=2, size = 0.2)+
          scale_fill_manual(values = envB()$groupwise.group.colors[df$group]) +
          labs(title= paste(clicked.ensemble,"/", envB()$gene.info$names[first(clicked.rows.envB)]),
               x = "", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envB()$indata), max(envB()$indata)))+
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
          labs(title= paste(clicked.ensemble,"/", envB()$gene.info$names[first(clicked.rows.envB)]),
               x = "samples", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envB()$indata), max(envB()$indata)))+
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


