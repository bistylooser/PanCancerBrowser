
output$p_genesetBrowser_genesetTable <- renderDataTable({
  
  return( data.frame(
    Name = names( env()$gs.def.list ),
    Genes = sapply( env()$gs.def.list, function(x) length(x$Genes) )
  ) )
  
}, rownames=F, selection = 'single')


output$p_genesetBrowser_geneProfile <- renderPlot({

  if( !is.null( input$p_genesetBrowser_genesetTable_row_last_clicked ) )
  {
    session$sendCustomMessage("element_visible", message=list(id="#p_genesetBrowser_checkbox_div", state="visible"))

    if( input$p_genesetBrowser_checkbox )
    {
      group.expression <- tapply( env()$samples.GSZ.scores[input$p_genesetBrowser_genesetTable_row_last_clicked,], env()$group.labels, c )[unique(env()$group.labels)]

      par(mar=c(5,4,2,1))
      boxplot( group.expression, col=env()$groupwise.group.colors,
               names = NA, ylim=range(env()$samples.GSZ.scores), ylab="GSZ", las=2 )

      text(x=c(1:length(group.expression)), y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), #par("usr")[3] - 0.3,
           labels=unique(env()$group.labels), srt=40, adj=1, xpd=TRUE, cex=1)

      title( main=names( env()$gs.def.list )[ input$p_genesetBrowser_genesetTable_row_last_clicked ], line=1)
      box()

    } else
    {
      par(mar=c(2,4,2,1))
      barplot( env()$samples.GSZ.scores[input$p_genesetBrowser_genesetTable_row_last_clicked,],
               col=env()$group.colors, border = NA, names.arg = NA,
               ylim=range(env()$samples.GSZ.scores), ylab="GSZ", las=2 )
      title( main=names( env()$gs.def.list )[ input$p_genesetBrowser_genesetTable_row_last_clicked ], line=1)
      mtext("samples",1)
      box()
    }
  }

})

output$p_genesetBrowser_geneMapping <- renderPlot({

  if( !is.null( input$p_genesetBrowser_genesetTable_row_last_clicked ) )
  {
    par(mar=c(2,2,2,0))

    spot.background <- get(paste("spot.list.",env()$preferences$standard.spot.modules,sep=""),envir = env())$overview.mask

    image( x=c(1:env()$preferences$dim.1stLvlSom), y=c(1:env()$preferences$dim.1stLvlSom),
           z=matrix(spot.background, env()$preferences$dim.1stLvlSom), col="gray90", axes=F, xlab="", ylab="" )
    box()
    axis( 1, las=1 ); axis( 1, 1, 1, las=1 )
    axis( 2, las=1 ); axis( 2, 1, 1, las=1 )

    

    
    n.map <- matrix(0,env()$preferences$dim.1stLvlSom,env()$preferences$dim.1stLvlSom)
    gs.nodes <- env()$som.result$feature.BMU[names(env()$gene.info$ids)[which(env()$gene.info$ids %in% env()$gs.def.list[[input$p_genesetBrowser_genesetTable_row_last_clicked]]$Genes)]]
    n.map[as.numeric(names(table(gs.nodes)))] <- table(gs.nodes)
    n.map[which(n.map==0)] <- NA
    n.map <- matrix(n.map, env()$preferences$dim.1stLvlSom)
    
    lim <- c(1,env()$preferences$dim.1stLvlSom) + env()$preferences$dim.1stLvlSom * 0.01 * c(-1, 1)
    colr <- env()$color.palette.heatmaps(1000)[(na.omit(as.vector(n.map)) - min(n.map,na.rm=TRUE)) /
                                           max(1, (max(n.map,na.rm=TRUE) - min(n.map,na.rm=TRUE))) *
                                           999 + 1]
    cex <- 0.5 + na.omit(as.vector(n.map)) / 10 * 2.8
    cex <- pmin(cex,3.3)
    
    par(new=T)
    plot(which(!is.na(n.map), arr.ind=TRUE), xlim=lim, ylim=lim, pch=16, axes=FALSE,
         xlab="",ylab="", xaxs="i", yaxs="i", col=colr,
         cex=cex)
    
    mtext( paste0("gene localization (maximum: ",max(n.map,na.rm=T)," genes)"),3)
  }
})