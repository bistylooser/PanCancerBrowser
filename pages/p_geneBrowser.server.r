
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
  #p_geneBrowser_geneTable <-p_geneBrowser_geneTable[order(p_geneBrowser_geneTable$Name),]
  
  # Mark Ensemble present in TableA and TableB with x 
  #Ensemble.intersect <- intersect(p_geneBrowser_geneTableB$Ensemble, p_geneBrowser_geneTableA$Ensemble)
  #p_geneBrowser_geneTable[which(p_geneBrowser_geneTable$Ensemble %in% Ensemble.intersect),c("Dataset1", "Dataset2")] <- "x" 
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
      #session$sendCustomMessage("element_visible", message=list(id="#p_geneBrowser_checkbox_div", state="visible"))
      
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
        p
      
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
       p
      
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
      #session$sendCustomMessage("element_visible", message=list(id="#p_geneBrowser_checkbox_div", state="visible"))
      
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
        p
        
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
        p
        
      }
    }
  }
  
})


# output$p_geneBrowser_geneProfileA <- renderPlot({
#   
#   clicked.ensemble <- geneTable()$Ensemble[input$p_geneBrowser_geneTable_row_last_clicked]
#   clicked.rows.envA <- which(envA()$gene.info$ids == clicked.ensemble)
#   
#   if (!is.null( input$p_geneBrowser_geneTable_row_last_clicked ))
#   {
#     if (clicked.ensemble %in% envA()$gene.info$ids)
#     {
#       #session$sendCustomMessage("element_visible", message=list(id="#p_geneBrowser_checkbox_div", state="visible"))
#       if (length(clicked.rows.envA) > 1){
#         expression.average <- colMeans(envA()$indata[clicked.rows.envA,])
#       } else {
#         expression.average <- envA()$indata[clicked.rows.envA,]
#       }
#       if( input$p_geneBrowser_checkboxA )
#       {
#         group.expression <- tapply(expression.average, envA()$group.labels, c )[unique(envA()$group.labels)]
#         
#         par(mar=c(5,4,2,1))
#         boxplot( group.expression, col=envA()$groupwise.group.colors,
#                  names = NA, ylim=range(envA()$indata), ylab="expression", las=2 )
#         
#         text(x=c(1:length(group.expression)), par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), #y=par("usr")[3] - 0.3,
#              labels=unique(envA()$group.labels), srt=40, adj=1, xpd=TRUE, cex=1)
#         
#         title( main=paste( "data set 1", envA()$gene.info$names[first(clicked.rows.envA)], sep="  /  " ), line=1)
#         box()
#         
#       } else
#       {
#         par(mar=c(2,4,2,1))
#         barplot( expression.average,
#                  col=envA()$group.colors, border = NA, names.arg = NA,
#                  ylim=range(envA()$indata), ylab="expression", las=2 )
#         title( main=paste( "data set 1", envA()$gene.info$names[first(clicked.rows.envA)], sep="  /  " ), line=1)
#         mtext("samples",1)
#         box()
#         
#       }
#     }
#   }
#   
# })


# output$p_geneBrowser_geneProfileB <- renderPlot({
#   
#   clicked.ensemble <- geneTable()$Ensemble[input$p_geneBrowser_geneTable_row_last_clicked]
#   clicked.rows.envB <- which(envB()$gene.info$ids == clicked.ensemble)
#   
#   if (!is.null( input$p_geneBrowser_geneTable_row_last_clicked ))
#   {
#     if (clicked.ensemble %in% envB()$gene.info$ids)
#     {
#       #session$sendCustomMessage("element_visible", message=list(id="#p_geneBrowser_checkbox_div", state="visible"))
#       if (length(clicked.rows.envB) > 1){
#         expression.average <- colMeans(envB()$indata[clicked.rows.envB,])
#       } else {
#         expression.average <- envB()$indata[clicked.rows.envB,]
#       }
#       if( input$p_geneBrowser_checkboxB )
#       {
#         group.expression <- tapply(expression.average, envB()$group.labels, c )[unique(envB()$group.labels)]
#         
#         par(mar=c(5,4,2,1))
#         boxplot( group.expression, col=envB()$groupwise.group.colors,
#                  names = NA, ylim=range(envB()$indata), ylab="expression", las=2 )
#         
#         text(x=c(1:length(group.expression)), par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), #y=par("usr")[3] - 0.3,
#              labels=unique(envB()$group.labels), srt=40, adj=1, xpd=TRUE, cex=1)
#         
#         title( main=paste( "data set 2", envB()$gene.info$names[first(clicked.rows.envB)], sep="  /  " ), line=1)
#         box()
#         
#       } else
#       {
#         par(mar=c(2,4,2,1))
#         barplot( expression.average,
#                  col=envB()$group.colors, border = NA, names.arg = NA,
#                  ylim=range(envB()$indata), ylab="expression", las=2 )
#         title( main=paste( "data set 2", envB()$gene.info$names[first(clicked.rows.envB)], sep="  /  " ), line=1)
#         mtext("samples",1)
#         box()
#         
#       }
#     }
#   }
#   
# })
# 
# 


