
output$p_geneBrowser_geneTable <- renderDataTable({
  
  return( data.frame(
    Name=env()$gene.info$names,
    Ensemble=env()$gene.info$ids,
    ID=names(env()$gene.info$ids),
    Chromosome=paste(env()$gene.info$chr.name,env()$gene.info$chr.band),
    Description=env()$gene.info$descriptions
  ) )
	
}, rownames=F, selection = 'single')


output$p_geneBrowser_geneProfile <- renderPlot({
  
  if( !is.null( input$p_geneBrowser_geneTable_row_last_clicked ) ) 
  {
    session$sendCustomMessage("element_visible", message=list(id="#p_geneBrowser_checkbox_div", state="visible"))
    
    if( input$p_geneBrowser_checkbox )
    {
      group.expression <- tapply( env()$indata[input$p_geneBrowser_geneTable_row_last_clicked,], env()$group.labels, c )[unique(env()$group.labels)]

      par(mar=c(5,4,2,1))      
      boxplot( group.expression, col=env()$groupwise.group.colors,
               names = NA, ylim=range(env()$indata), ylab="expression", las=2 )
      
      text(x=c(1:length(group.expression)), par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), #y=par("usr")[3] - 0.3,
           labels=unique(env()$group.labels), srt=40, adj=1, xpd=TRUE, cex=1)
      
      title( main=paste( env()$gene.info$names[input$p_geneBrowser_geneTable_row_last_clicked], names(env()$gene.info$ids)[input$p_geneBrowser_geneTable_row_last_clicked], sep="  /  " ), line=1)
      box()
      
    } else
    {
      par(mar=c(2,4,2,1))
      barplot( env()$indata[input$p_geneBrowser_geneTable_row_last_clicked,], 
               col=env()$group.colors, border = NA, names.arg = NA,
               ylim=range(env()$indata), ylab="expression", las=2 )
      title( main=paste( env()$gene.info$names[input$p_geneBrowser_geneTable_row_last_clicked], names(env()$gene.info$ids)[input$p_geneBrowser_geneTable_row_last_clicked], sep="  /  " ), line=1)
      mtext("samples",1)
      box()
      
    }
  }
  
})

output$p_geneBrowser_geneMapping <- renderPlot({
  
  if( !is.null( input$p_geneBrowser_geneTable_row_last_clicked ) ) 
  {
    par(mar=c(2,2,2,0))
    
    spot.background <- get(paste("spot.list.",env()$preferences$standard.spot.modules,sep=""),envir = env())$overview.mask
      
    image( x=c(1:env()$preferences$dim.1stLvlSom), y=c(1:env()$preferences$dim.1stLvlSom),
           z=matrix(spot.background, env()$preferences$dim.1stLvlSom), col="gray90", axes=F, xlab="", ylab="" )
    box()
    axis( 1, las=1 ); axis( 1, 1, 1, las=1 )
    axis( 2, las=1 ); axis( 2, 1, 1, las=1 )
    
    mtext( paste("gene localization:", env()$gene.info$coordinates[input$p_geneBrowser_geneTable_row_last_clicked]) ,3)
    
    coords <- as.numeric( strsplit( env()$gene.info$coordinates[input$p_geneBrowser_geneTable_row_last_clicked], " x " )[[1]] )
    
    points( coords[1], coords[2], pch=4, cex=2, col="blue" )
    points( coords[1], coords[2], pch=18, cex=2, col="blue" )
  }
  
})