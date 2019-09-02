functionTable <- reactive({
  p_genesetBrowser_genesetTableA <- 
    data.frame(
      Name = names( envA()$gs.def.list ),
      Genes_dataset1 = sapply( envA()$gs.def.list, function(x) length(x$Genes)),
      Genes_dataset2 = "",
      stringsAsFactors = FALSE)
  
  p_genesetBrowser_genesetTableB <- 
    data.frame(
      Name = names( envB()$gs.def.list ),
      Genes_dataset1 = "",
      Genes_dataset2 = sapply( envB()$gs.def.list, function(x) length(x$Genes)), 
      stringsAsFactors = FALSE )
  
  # combine TableA and TableB to p_genesetBrowser_genesetTable
  p_genesetBrowser_genesetTable.all <- rbind( p_genesetBrowser_genesetTableA, p_genesetBrowser_genesetTableB )
  p_genesetBrowser_genesetTable <- p_genesetBrowser_genesetTable.all[ which(!duplicated( p_genesetBrowser_genesetTable.all$Name ) ), ] 
  p_genesetBrowser_genesetTable <-p_genesetBrowser_genesetTable[order(p_genesetBrowser_genesetTable$Name),]
  
  # Add Number of Genes for dataset 1 and 2
  p_genesetBrowser_genesetTable$Genes_dataset1 <- p_genesetBrowser_genesetTableA[p_genesetBrowser_genesetTable$Name,"Genes_dataset1"]
  p_genesetBrowser_genesetTable$Genes_dataset2 <- p_genesetBrowser_genesetTableB[p_genesetBrowser_genesetTable$Name,"Genes_dataset2"]
  
  
  return(
    p_genesetBrowser_genesetTable
  )
})

output$p_genesetBrowser_genesetTable <- renderDataTable({
  return(
    functionTable()
  )
}, rownames=F, selection = 'single')
#options = list(columnDefs = list(list(className = 'dt-center', targets = 2:3)))

output$p_genesetBrowser_geneProfileA <- renderPlotly({
  
  clicked.name <- functionTable()$Name[input$p_genesetBrowser_genesetTable_row_last_clicked]
  clicked.row.envA <- which(rownames(envA()$samples.GSZ.scores) == clicked.name)
  
  if (!is.null( input$p_genesetBrowser_genesetTable_row_last_clicked ))
  {
    if (clicked.name %in% rownames(envA()$samples.GSZ.scores))
    {
      #session$sendCustomMessage("element_visible", message=list(id="#p_genesetBrowser_checkbox_div", state="visible"))
      
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
          labs(title= paste( "data set 1:", names(envA()$gs.def.list)[clicked.row.envA]), x = "", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envA()$samples.GSZ.scores), max(envA()$samples.GSZ.scores)))+
          theme(legend.position = "none",
                plot.title = element_text(size=11, margin = margin(10, 0, 10, 0)),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                axis.text.x= element_text(angle=20))
        p
        
      } else
      {
        p <- ggplot(df, aes(sample, expression)) + 
          theme_light() +
          geom_bar(aes(fill = group, text = paste("group:", group)), stat = "identity")+
          scale_fill_manual(values = envA()$groupwise.group.colors[df$group])+
          labs(title= paste( "data set 1:", names(envA()$gs.def.list)[clicked.row.envA]), x = "samples", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envA()$samples.GSZ.scores), max(envA()$samples.GSZ.scores)))+
          theme(legend.position = "none",
                plot.title = element_text(size=11, margin = margin(10, 0, 10, 0)),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        p
        
      }
    }
  }
  
})


output$p_genesetBrowser_geneProfileB <- renderPlotly({
  
  clicked.name <- functionTable()$Name[input$p_genesetBrowser_genesetTable_row_last_clicked]
  clicked.row.envB <- which(rownames(envB()$samples.GSZ.scores) == clicked.name)
  
  if (!is.null( input$p_genesetBrowser_genesetTable_row_last_clicked ))
  {
    if (clicked.name %in% rownames(envB()$samples.GSZ.scores))
    {
      #session$sendCustomMessage("element_visible", message=list(id="#p_genesetBrowser_checkbox_div", state="visible"))
      
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
          labs(title= paste( "data set 2:", names(envB()$gs.def.list)[clicked.row.envB]), x = "", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envB()$samples.GSZ.scores), max(envB()$samples.GSZ.scores)))+
          theme(legend.position = "none",
                plot.title = element_text(size=11, margin = margin(10, 0, 10, 0)),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                axis.text.x= element_text(angle=20))
        p
        
      } else
      {
        p <- ggplot(df, aes(sample, expression)) + 
          theme_light() +
          geom_bar(aes(fill = group, text = paste("group:", group)), stat = "identity")+
          scale_fill_manual(values = envB()$groupwise.group.colors[df$group])+
          labs(title= paste( "data set 2:", names(envB()$gs.def.list)[clicked.row.envB]), x = "samples", y = "expression")+
          scale_y_continuous(breaks = scales::pretty_breaks(n=5), limits = c(min(envB()$samples.GSZ.scores), max(envB()$samples.GSZ.scores)))+
          theme(legend.position = "none",
                plot.title = element_text(size=11, margin = margin(10, 0, 10, 0)),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        p
        
      }
    }
  }
  
})




# output$p_genesetBrowser_geneProfileA <- renderPlot({
# 
#   if( !is.null( input$p_genesetBrowser_genesetTable_row_last_clicked ) )
#   {
#     session$sendCustomMessage("element_visible", message=list(id="#p_genesetBrowser_checkboxA_div", state="visible"))
# 
#     if( input$p_genesetBrowser_checkboxA )
#     {
#       group.expression <- tapply( envA()$samples.GSZ.scores[input$p_genesetBrowser_genesetTable_row_last_clicked,], envA()$group.labels, c )[unique(envA()$group.labels)]
# 
#       par(mar=c(5,4,2,1))
#       boxplot( group.expression, col=envA()$groupwise.group.colors,
#                names = NA, ylim=range(envA()$samples.GSZ.scores), ylab="GSZ", las=2 )
# 
#       text(x=c(1:length(group.expression)), y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), #par("usr")[3] - 0.3,
#            labels=unique(envA()$group.labels), srt=40, adj=1, xpd=TRUE, cex=1)
# 
#       title( main=names( envA()$gs.def.list )[ input$p_genesetBrowser_genesetTable_row_last_clicked ], line=1)
#       box()
# 
#     } else
#     {
#       par(mar=c(2,4,2,1))
#       barplot( envA()$samples.GSZ.scores[input$p_genesetBrowser_genesetTable_row_last_clicked,],
#                col=envA()$group.colors, border = NA, names.arg = NA,
#                ylim=range(envA()$samples.GSZ.scores), ylab="GSZ", las=2 )
#       title( main=names( envA()$gs.def.list )[ input$p_genesetBrowser_genesetTable_row_last_clicked ], line=1)
#       mtext("samples",1)
#       box()
#     }
#   }
# 
# })
# 
