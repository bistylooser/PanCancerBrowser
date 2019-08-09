
output$p_psfBrowser_menu_div <- renderUI(
  absolutePanel( id="p_psfBrowser_menu", draggable=TRUE,
    column( 6, selectInput("p_psfBrowser_selectPSF", label = "Select pathway",
                           choices = sort( intersect( names(env()$psf.results.groups), names(kegg.collection) ) ),
                           selected = 1, width = 300) ),
    column( 6, selectInput("p_psfBrowser_selectClass", label = "Select class",
                           choices = unique(env()$group.labels),
                           selected = 1, width = 300) )
  )
)

circle <- function(x, y, r, ...) {
  xx <- c()
  yy <- c()
  for(i in 1:100 ) {
    xx[i] <- cos(i*6.28/100)
    yy[i] <- sin(i*6.28/100)
  }
  polygon( r * xx + x, r * yy + y, ...)
}


plot.psf.pathway <- function( kegg.pathway, signal.values, signal.values.lim )
{
  width = ncol(kegg.pathway$pathway.img)
  height = nrow(kegg.pathway$pathway.img)
  max.xy = max(width,height)

  par(mar = c(0, 0, 0, 0))

  plot( 0, xlim=c(0,width), ylim=c(0,height), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", axes=F)
  rasterImage(kegg.pathway$pathway.img, 0, 0, width, height, interpolate = T)
  box()

  title.node <- which( sapply( kegg.pathway$pathway.info, function(x) grepl("TITLE:", x$graphics$label.long ) ) )
  if(length(title.node)>0)
  {
    rect( kegg.pathway$pathway.info[[title.node]]$graphics$x-kegg.pathway$pathway.info[[title.node]]$graphics$width*0.6,
          height-kegg.pathway$pathway.info[[title.node]]$graphics$y+kegg.pathway$pathway.info[[title.node]]$graphics$height*0.6,
          kegg.pathway$pathway.info[[title.node]]$graphics$x+kegg.pathway$pathway.info[[title.node]]$graphics$width*0.6,
          height-kegg.pathway$pathway.info[[title.node]]$graphics$y-kegg.pathway$pathway.info[[title.node]]$graphics$height*0.6,
          col="gray90", lwd=0.5 )

    text( kegg.pathway$pathway.info[[title.node]]$graphics$x, height - kegg.pathway$pathway.info[[title.node]]$graphics$y, strsplit(kegg.pathway$pathway.info[[title.node]]$graphics$label.short,":")[[1]][2],
          cex = 1.6, col = "black", family="sans" )
  }

  for( i in which( sapply(kegg.pathway$pathway.info,"[[", "type" ) == "gene" ) )
  {
    node.col = colorRampPalette(c("blue4","blue","gray90","orange","red4"))(1000)[999*(signal.values[i]-signal.values.lim[1])/(signal.values.lim[2]-signal.values.lim[1])+1]

    rect( kegg.pathway$pathway.info[[i]]$graphics$x-kegg.pathway$pathway.info[[i]]$graphics$width*0.5,
          height-kegg.pathway$pathway.info[[i]]$graphics$y+kegg.pathway$pathway.info[[i]]$graphics$height*0.5,
          kegg.pathway$pathway.info[[i]]$graphics$x+kegg.pathway$pathway.info[[i]]$graphics$width*0.5+2,
          height-kegg.pathway$pathway.info[[i]]$graphics$y-kegg.pathway$pathway.info[[i]]$graphics$height*0.5,
          col=node.col, lwd=0.5 )

    text( kegg.pathway$pathway.info[[i]]$graphics$x, height - kegg.pathway$pathway.info[[i]]$graphics$y, kegg.pathway$pathway.info[[i]]$graphics$label.short,
          cex = 1-0.02*nchar(kegg.pathway$pathway.info[[i]]$graphics$label.short), col = "black", family="sans" )
  }

  for( i in which( sapply(kegg.pathway$pathway.info,"[[", "type" ) == "compound" ) )
  {
    node.col = colorRampPalette(c("blue4","blue","gray90","orange","red4"))(1000)[999*(signal.values[i]-signal.values.lim[1])/(signal.values.lim[2]-signal.values.lim[1])+1]

    circle( kegg.pathway$pathway.info[[i]]$graphics$x, height-kegg.pathway$pathway.info[[i]]$graphics$y,
            kegg.pathway$pathway.info[[i]]$graphics$width*0.75,
            col=node.col, lwd=0.5 )
  }

  map.nodes <- which( sapply(kegg.pathway$pathway.info,"[[", "type" ) == "map" )
  map.nodes <- setdiff( map.nodes, title.node )
  map.nodes <- map.nodes[ which( sapply( kegg.pathway$pathway.info[map.nodes], function(x) sub("path:", "", x$id, fixed=T ) ) %in% sapply( kegg.collection, "[[", "id" ) ) ]

  for( i in map.nodes )
  {
    rect( kegg.pathway$pathway.info[[i]]$graphics$x-kegg.pathway$pathway.info[[i]]$graphics$width*0.5-1,
          height-kegg.pathway$pathway.info[[i]]$graphics$y+kegg.pathway$pathway.info[[i]]$graphics$height*0.5+1,
          kegg.pathway$pathway.info[[i]]$graphics$x+kegg.pathway$pathway.info[[i]]$graphics$width*0.5+2,
          height-kegg.pathway$pathway.info[[i]]$graphics$y-kegg.pathway$pathway.info[[i]]$graphics$height*0.5-3,
          col = "#00000030", lwd=0.5 )
  }

}


selectedPathway <- reactive({
  
  # catch call when selectors are still empty
  if( is.null(input$p_psfBrowser_selectPSF) || is.null(input$p_psfBrowser_selectClass) ) return( NULL )
  
  # catch race conditions when dataset is switched and choices are outdated
  if( !input$p_psfBrowser_selectPSF %in% intersect( names(env()$psf.results.groups), names(kegg.collection) ) ) return( NULL )
  if( !input$p_psfBrowser_selectClass %in% unique(env()$group.labels) ) return( NULL )
  
  # load respective kegg pathway data
  load( paste0("data/", kegg.collection[[input$p_psfBrowser_selectPSF]]$id, ".RData") )
  return(kegg.data)

})




output$p_psfBrowser_plotframe <- renderPlot({

  if( is.null(selectedPathway()) ) return()
    
  plot.psf.pathway( kegg.pathway = selectedPathway(),
                    signal.values = log10( env()$psf.results.groups[[input$p_psfBrowser_selectPSF]][[input$p_psfBrowser_selectClass]]$signal.at.nodes ),
                    signal.values.lim = c(-1,1)*max( abs( log10( sapply( env()$psf.results.groups[[input$p_psfBrowser_selectPSF]], function(x) x$signal.at.nodes ) ) ) )
  )

  },
  height = function() { input$contentHeight }

)



p_psfBrowser_hoverId <- reactive({

  if( is.null(input$p_psfBrowser_plotframe_hover) ) return(NULL)
  x <- input$p_psfBrowser_plotframe_hover$x
  y <- input$p_psfBrowser_plotframe_hover$y
  height <- nrow(selectedPathway()$pathway.img)

  coord.matrix <- sapply(selectedPathway()$pathway.info, function(x) c( x$graphics$x-0.5*x$graphics$width, height-(x$graphics$y+0.5*x$graphics$height), x$graphics$x+0.5*x$graphics$width, height-(x$graphics$y-0.5*x$graphics$height) ) )
  coord.fit <- x>=coord.matrix[1,] & y>=coord.matrix[2,] & x<=coord.matrix[3,] & y<=coord.matrix[4,]

  if( any(coord.fit) ) return(names(which(coord.fit)))
  else return(NULL)
})
p_psfBrowser_clickId <- reactive({

  if( is.null(input$p_psfBrowser_plotframe_click) || is.null(selectedPathway()) ) return(NULL)
  x <- input$p_psfBrowser_plotframe_click$x
  y <- input$p_psfBrowser_plotframe_click$y
  height <- nrow(selectedPathway()$pathway.img)

  coord.matrix <- sapply(selectedPathway()$pathway.info, function(x) c( x$graphics$x-0.5*x$graphics$width, height-(x$graphics$y+0.5*x$graphics$height), x$graphics$x+0.5*x$graphics$width, height-(x$graphics$y-0.5*x$graphics$height) ) )
  coord.fit <- x>=coord.matrix[1,] & y>=coord.matrix[2,] & x<=coord.matrix[3,] & y<=coord.matrix[4,]
  
  if( any(coord.fit) ) return(names(which(coord.fit)))
  else return(NULL)
})


output$p_psfBrowser_hoverbox <- renderPlot({

  hoverFeatures <- c()
  try({
    hoverFeatures <- p_psfBrowser_hoverId()
    hoverFeatures <- hoverFeatures[ which( sapply( selectedPathway()$pathway.info[hoverFeatures], "[[", "type" ) == "gene" ) ]
    hoverFeatures <- sub("...","",strsplit(selectedPathway()$pathway.info[[hoverFeatures[1]]]$graphics$label.long,", ")[[1]],fixed=T)
    hoverFeatures <- names(env()$gene.info$names)[ which( env()$gene.info$names %in% hoverFeatures ) ]
  }, silent = T)

  if( length(hoverFeatures) == 0 )
  {
    session$sendCustomMessage("element_visible",
                              message=list(id="#p_psfBrowser_hoverbox", state="hidden"))
    return()
  }

  par(mar=c(.1,.1,.1,.1))

  spot.background <- get(paste("spot.list.",env()$preferences$standard.spot.modules,sep=""),envir = env())$overview.mask
  image( x=c(1:env()$preferences$dim.1stLvlSom), y=c(1:env()$preferences$dim.1stLvlSom),
         z=matrix(spot.background, env()$preferences$dim.1stLvlSom), col="gray90", axes=F, xlab="", ylab="" )
  box()

  coords <- strsplit( env()$gene.info$coordinates[hoverFeatures], " x " )
  for( i in 1:length(coords) )
  {
    x <- as.numeric(coords[[i]][1])
    y <- as.numeric(coords[[i]][2])
    points( x, y, pch=4, cex=2, col="blue" )
    points( x, y, pch=18, cex=2, col="blue" )
  }

  session$sendCustomMessage("element_visible",
                            message=list(id="#p_psfBrowser_hoverbox", state="visible"))

})


observeEvent(input$p_psfBrowser_clickbox_close, {
  session$sendCustomMessage("element_visible", message=list(id="#p_psfBrowser_clickbox", state="hidden"))
  session$sendCustomMessage("element_visible", message=list(id="#p_psfBrowser_clickbox_goto", state="hidden"))
})

observeEvent(input$p_psfBrowser_clickbox_goto, {

  gotoPathway <- sub("path:", "", selectedPathway()$pathway.info[[p_psfBrowser_clickId()]]$id, fixed=T )
  gotoPathway <- names( which( sapply( kegg.collection, "[[", "id" ) == gotoPathway ) )

  updateSelectInput( session, "p_psfBrowser_selectPSF", selected = gotoPathway )

  session$sendCustomMessage("element_visible", message=list(id="#p_psfBrowser_clickbox", state="hidden"))
  session$sendCustomMessage("element_visible", message=list(id="#p_psfBrowser_clickbox_goto", state="hidden"))
})



get.clickId.info <- function(id)
{
  clickFeatures <- p_psfBrowser_clickId()
  if( is.null(clickFeatures) ) return(NULL)

  clickFeatures.node <- clickFeatures[ which( sapply( selectedPathway()$pathway.info[clickFeatures], "[[", "type" ) %in% c("gene","compound") ) ]
  clickFeatures.map <- clickFeatures[ which( sapply( selectedPathway()$pathway.info[clickFeatures], "[[", "type" ) %in% c("map") &
                                              sub("path:", "", selectedPathway()$pathway.info[[clickFeatures[1]]]$id, fixed=T ) %in% sapply( kegg.collection, "[[", "id" )  )]

  if( length(clickFeatures.node) >=1 ) return( clickFeatures.node[1] )
  if( length(clickFeatures.map) ==1 ) return( clickFeatures.map )
  return(NULL)
}

output$p_psfBrowser_clickbox_info <- renderText({

  clickFeature <- get.clickId.info( p_psfBrowser_clickId() )

  if( !is.null(clickFeature) )
  {

    session$sendCustomMessage("element_visible", message=list(id="#p_psfBrowser_clickbox", state="visible"))
    session$sendCustomMessage("element_visible", message=list(id="#p_psfBrowser_clickbox_goto", state=ifelse( grepl("path:",clickFeature), "visible", "hidden" )))

    return( paste0( selectedPathway()$pathway.info[[clickFeature]]$type,": ", selectedPathway()$pathway.info[[clickFeature]]$graphics$label.long ) )

  }

  session$sendCustomMessage("element_visible", message=list(id="#p_psfBrowser_clickbox", state="hidden"))
  session$sendCustomMessage("element_visible", message=list(id="#p_psfBrowser_clickbox_goto", state="hidden"))
})



output$p_psfBrowser_clickbox_profile <- renderPlot({

  clickFeature <- get.clickId.info( p_psfBrowser_clickId() )

  if( !is.null(clickFeature) && !grepl("path:",clickFeature) )
  {
    par(mar=c(4.5,4,0,0))
    signals <- sapply( env()$psf.results.groups[[input$p_psfBrowser_selectPSF]], function(x) x$signal.at.nodes[clickFeature[1]] )
    signal.values.lim <- c(-1,1)*max( abs( log10( sapply( env()$psf.results.groups[[input$p_psfBrowser_selectPSF]], function(x) x$signal.at.nodes ) ) ) )

    b <- barplot( log10(signals), col=env()$groupwise.group.colors, names.arg=NA, las=1, ylab="signal", ylim=c(signal.values.lim) )
    box()

    text(x=b, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),
         labels=unique(env()$group.labels), srt=40, adj=1, xpd=TRUE, cex=1)

  }

  if( !is.null(clickFeature) && grepl("path:",clickFeature) )
  {
    clickPathway <- which( sapply( kegg.collection, "[[", "id" ) ==
                             sub("path:", "", selectedPathway()$pathway.info[[clickFeature]]$id, fixed=T ) )

    par(mar=c(4.5,4,0,0))
    if( !is.null(env()$psf.results.groups[[ clickPathway ]][[1]]$signal.at.sinks) )
    {
      signals <- sapply( env()$psf.results.groups[[ clickPathway ]], "[[", "signal.at.sinks" )
      boxplot( log10(signals), col=env()$groupwise.group.colors, names=NA, las=1, ylab="signal at sinks" )
      text(x=1:ncol(signals), y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),
           labels=unique(env()$group.labels), srt=40, adj=1, xpd=TRUE, cex=1)
    }
  }


})





