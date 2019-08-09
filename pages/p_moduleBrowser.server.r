

#### --------------------------- Global Variables ------------------------------- ####
   
sel.spotlist <- reactiveVal(NULL) 
click.metagene <- reactiveVal(NULL) 


#### --------------------------- Side - Panel ----------------------------- ####

output$p_moduleBrowser_side_OE <- renderPlot({
  par(mar = c(0.1, 0.1, 1.2, 0.1))
  image( matrix( env()$spot.list.overexpression$overview.map,
                 env()$preferences$dim.1stLvlSom ),col=env()$color.palette.portraits(1000), axes=F, main = "Overexpression", cex.main=.8)
  box()
}, height = function() session$clientData$output_p_moduleBrowser_side_OE_width )

output$p_moduleBrowser_side_UE <- renderPlot({
  par(mar = c(0.1, 0.1, 1.2, 0.1))
  image( matrix( env()$spot.list.underexpression$overview.map,
                 env()$preferences$dim.1stLvlSom ),col=env()$color.palette.portraits(1000), axes=F, main = "Underexpression", cex.main=.8)
  box()
}, height = function() session$clientData$output_p_moduleBrowser_side_UE_width )

output$p_moduleBrowser_side_KM <- renderPlot({
  par(mar = c(0.1, 0.1, 1.2, 0.1))
  image( matrix( env()$spot.list.kmeans$overview.map,
                 env()$preferences$dim.1stLvlSom ),col=env()$color.palette.portraits(1000), axes=F, main = "k-Means", cex.main=.8)
  box()
}, height = function() session$clientData$output_p_moduleBrowser_side_KM_width )

output$p_moduleBrowser_side_CO <- renderPlot({
  par(mar = c(0.1, 0.1, 1.2, 0.1))
  image( matrix( env()$spot.list.correlation$overview.map,
                 env()$preferences$dim.1stLvlSom ),col=env()$color.palette.portraits(1000), axes=F, main = "Correlation", cex.main=.8)
  box()
}, height = function() session$clientData$output_p_moduleBrowser_side_CO_width )

output$p_moduleBrowser_side_DM <- renderPlot({
  par(mar = c(0.1, 0.1, 1.2, 0.1))
  image( matrix( env()$spot.list.dmap$overview.map,
                 env()$preferences$dim.1stLvlSom ),col=colorRampPalette(c("blue2","white","red2"))(1000), axes=F, main = "D-Map", cex.main=.8)
  box()
}, height = function() session$clientData$output_p_moduleBrowser_side_DM_width )

output$p_moduleBrowser_side_GR <- renderPlot({
  par(mar = c(0.1, 0.1, 1.2, 0.1))
  image( matrix( env()$spot.list.group.overexpression$overview.map,
                 env()$preferences$dim.1stLvlSom ),col=env()$color.palette.portraits(1000), axes=F, main = "Group overexpr.", cex.main=.8)
  box()
}, height = function() session$clientData$output_p_moduleBrowser_side_GR_width )

output$p_moduleBrowser_side_AG <- renderPlot({
  if(is.null(env()$pheno.maps$age)) return()
  par(mar = c(0.1, 0.1, 1.2, 0.1))
  image( matrix(env()$pheno.maps$age,env()$preferences$dim.1stLvlSom),
         col=env()$color.palette.heatmaps(1000), axes=F, main = "Age", cex.main=.8)
  box()
}, height = function() session$clientData$output_p_moduleBrowser_side_AG_width )

output$p_moduleBrowser_side_SX <- renderPlot({
  if(is.null(env()$pheno.maps$sex)) return()
  par(mar = c(0.1, 0.1, 1.2, 0.1))
  image( matrix(env()$pheno.maps$sex,env()$preferences$dim.1stLvlSom),
         zlim=max(abs(range(env()$pheno.maps$sex)))*c(-1,1), col=env()$color.palette.heatmaps(1000), axes=F, main = "Sex", cex.main=.8)
  box()
}, height = function() session$clientData$output_p_moduleBrowser_side_SX_width )

output$p_moduleBrowser_side_HR <- renderPlot({
  if(is.null(env()$pheno.maps$HR)) return()
  par(mar = c(0.1, 0.1, 1.2, 0.1))
  image( matrix(env()$pheno.maps$HR,env()$preferences$dim.1stLvlSom),
         col=env()$color.palette.heatmaps(1000), axes=F , main = "Survival", cex.main=.8)
  box()
}, height = function() session$clientData$output_p_moduleBrowser_side_HR_width )


#### --------------------------- Side - Panel (Function) ------------------------- ####
     
observeEvent(input$p_moduleBrowser_side_OE_click, {
  sel.spotlist("spot.list.overexpression")
  click.metagene(NULL)
})

observeEvent(input$p_moduleBrowser_side_GR_click, {
  sel.spotlist("spot.list.group.overexpression")
  click.metagene(NULL)
})

observeEvent(input$p_moduleBrowser_side_UE_click, {
  sel.spotlist("spot.list.underexpression")    
  click.metagene(NULL)
})

observeEvent(input$p_moduleBrowser_side_DM_click, {
  sel.spotlist("spot.list.dmap") 
  click.metagene(NULL)
})

observeEvent(input$p_moduleBrowser_side_KM_click, {
  sel.spotlist("spot.list.kmeans")
  click.metagene(NULL)
})

observeEvent(input$p_moduleBrowser_side_CO_click, {
  sel.spotlist("spot.list.correlation")   
  click.metagene(NULL)
})

observeEvent(input$p_moduleBrowser_side_AG_click, {
  if(is.null(env()$pheno.maps$age)) return()
  sel.spotlist("age") 
  click.metagene(NULL)
})

observeEvent(input$p_moduleBrowser_side_SX_click, {
  if(is.null(env()$pheno.maps$sex)) return()
  sel.spotlist("sex") 
  click.metagene(NULL)
})

observeEvent(input$p_moduleBrowser_side_HR_click, {
  if(is.null(env()$pheno.maps$HR)) return()
  sel.spotlist("HR") 
  click.metagene(NULL)
})


#### --------------------------- Main Panel -------------------------------------- ####
  
output$p_moduleBrowser_plotframe <- renderPlot({

  if( is.null(sel.spotlist()) )
    sel.spotlist( paste0("spot.list.",env()$preferences$standard.spot.modules) )

  
  mains = c( spot.list.overexpression="Overexpression modules", spot.list.underexpression="Underexpression modules",
             spot.list.kmeans="k-Means modules", spot.list.correlation="Correlation modules",
             spot.list.dmap="D-Map modules", spot.list.group.overexpression="Group overexpression modules",
             "age"="Age map","sex"="Sex map","HR"="Survival map" )

    
  if( grepl("spot.list",sel.spotlist() ) )  ### one of the spotmaps selected?
  {
    par(mar=c(0.1,0.1,2.2,0.1))
    image( matrix( get(sel.spotlist(),envir = env() )$overview.map, env()$preferences$dim.1stLvlSom), axes=F, main=mains[sel.spotlist()],
           col = if( sel.spotlist() != "spot.list.dmap" ) env()$color.palette.portraits(1000) else colorRampPalette(c("blue2","white","red2"))(1000) )
    box()
    
    if( input$p_moduleBrowser_checkbox == TRUE &&
        any(is.na(  get(sel.spotlist(),envir = env() )$overview.mask  )) )
    {
      mask = matrix( get(sel.spotlist(),envir = env() )$overview.mask, env()$preferences$dim.1stLvlSom )
      mask = is.na(mask)
      mask[which(mask==F)] = NA
      
      par(new=T)
      image( mask, col="#B0B0B0BB", axes=F )
    }
    
    session$sendCustomMessage("element_visible", 
                              message=list(id="#p_moduleBrowser_checkbox_div", state="visible"))
    
  } else
  {
    par(mar=c(0.1,0.1,2.2,0.1))
    image( matrix(env()$pheno.maps[[sel.spotlist()]], env()$preferences$dim.1stLvlSom), main=mains[sel.spotlist()],
           col=env()$color.palette.heatmaps(1000), axes=F )
    box()
          
    session$sendCustomMessage("element_visible", 
                              message=list(id="#p_moduleBrowser_checkbox_div", state="hidden"))
    
  }
   
}, height = function() session$clientData$output_p_moduleBrowser_plotframe_width )

  
output$p_moduleBrowser_hoverbox <- renderText({
   
  if( is.null(input$p_moduleBrowser_plotframe_hover) ) 
  {
    session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_hoverbox",
                                           state="hidden"))
    return()
  }
  
  session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_hoverbox",
                                         state="visible"))
  
  x <- ceiling((input$p_moduleBrowser_plotframe_hover$x - input$p_moduleBrowser_plotframe_hover$domain$left)/(input$p_moduleBrowser_plotframe_hover$domain$right - input$p_moduleBrowser_plotframe_hover$domain$left)* env()$preferences$dim.1stLvlSom )
  y <- ceiling((input$p_moduleBrowser_plotframe_hover$y - input$p_moduleBrowser_plotframe_hover$domain$bottom)/(input$p_moduleBrowser_plotframe_hover$domain$top - input$p_moduleBrowser_plotframe_hover$domain$bottom)* env()$preferences$dim.1stLvlSom )
  xy_id <- x + (y-1) * env()$preferences$dim.1stLvlSom
  out <- paste( "Coordinate: (", x, ",", y, ")")
     
  if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
  {
    spot <- names(which(sapply(get(sel.spotlist(),envir = env() )$spots, function(x) xy_id %in% x$metagenes)))
    out <- if(length(spot) == 0) paste( "Coordinate: (", x, ",", y, ")<br>Spot: none")
           else paste( "Coordinate: (", x, ",", y, ")<br>Spot: ", spot)
  }
  
  return(out)

})

observeEvent(input$p_moduleBrowser_plotframe_click, {
  
  if( is.null(input$p_moduleBrowser_plotframe_click) ) return()
  
  x <- ceiling((input$p_moduleBrowser_plotframe_click$x - input$p_moduleBrowser_plotframe_click$domain$left)/(input$p_moduleBrowser_plotframe_click$domain$right - input$p_moduleBrowser_plotframe_click$domain$left)* env()$preferences$dim.1stLvlSom )
  y <- ceiling((input$p_moduleBrowser_plotframe_click$y - input$p_moduleBrowser_plotframe_click$domain$bottom)/(input$p_moduleBrowser_plotframe_click$domain$top - input$p_moduleBrowser_plotframe_click$domain$bottom)* env()$preferences$dim.1stLvlSom )
  xy_id <- x + (y-1) * env()$preferences$dim.1stLvlSom
       
  click.metagene( xy_id )
  
  # session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_tabsetpanel",state="visible"))
  
})

output$p_moduleBrowser_bottomframe <- renderPlot({

  if( is.null(click.metagene()) ) return()
  
  if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
  {
    spot <- names(which(sapply(get(sel.spotlist(),envir = env() )$spots, function(x) click.metagene() %in% x$metagenes)))
       
    if (length(spot) > 0) 
    {
      par(mar=c(2.2, 4.2, 0.1, 0.1))
      barplot( get(sel.spotlist(),envir = env() )$spotdata[spot,], border = NA, col=env()$group.colors, names.arg = NA)
      mtext("samples", 1, line = 1)
      mtext("expression", 2, line = 2)
      
      session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_bottomframe",state="visible"))
      
    } else
      session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_bottomframe",state="hidden"))
    
  } else
  {
    metagene.samples <- lapply( c(1:nrow(env()$metadata)), function(i) names( which(env()$metadata[i,]>0 ) ) )
    
    samples.with.metagene <- metagene.samples[[click.metagene()]]
    samples.without.metagene <- setdiff(colnames(env()$metadata), samples.with.metagene)
    
    if( sel.spotlist() == "age")
    {
      par(mar=c(2.2, 4.2, 2.1, 0.1))
      hist( env()$patient.data[samples.with.metagene,"age"], main="Age distribution", xlab = "age", xlim=c(0,100), ylab = "frequency", col="gray90" )
      
    } else
    if( sel.spotlist() == "sex")
    {
      gender.table <- table(env()$patient.data[samples.with.metagene,"sex"])[c("female","male","na")]
      gender.table <- gender.table[which(!is.na(gender.table))]

      par(mar=c(1.2, 4.2, 2.1, 0.1))
      pie(gender.table, main="Sex distribution", col=c("lightpink","cornflowerblue","gray80")[match(names(gender.table), c("female","male","na"))],
          labels = paste0(round(prop.table(gender.table)*100, 2), "%"))
      legend(.9, .3, c("female","male","N/A"), cex = 0.7, fill = c("lightpink","cornflowerblue","gray80"))
      
    } else
    if( sel.spotlist() == "HR")
    {
      # par(mar=c(4.2, 6.2, 1.1, 0.1))
      # image(matrix(1:100, 1, 100), col = env()$color.palette.heatmaps(1000), axes=FALSE)
      # axis(2, round(c(min(env()$pheno.maps$HR,na.rm=T), max(env()$pheno.maps$HR,na.rm=T)),1), at=c(0, 1), las=2, tick=FALSE, pos=-0.5, cex.axis=1.4)
      # box()
    
      plot(0, type="n", xlim=c(0,10), ylim=c(0,1), las=1, xlab="years", ylab="% overall survival", cex.axis=1, cex.lab=1)
      mtext("Overall survival curves", 3)
      
      fu.time <- env()$survival.data[ "os", intersect(samples.with.metagene, colnames(env()$survival.data)) ]
      event.state <- env()$survival.data[ "event", intersect(samples.with.metagene, colnames(env()$survival.data)) ]
      event.state = event.state[order(fu.time)]
      fu.time = fu.time[order(fu.time)]
      plot.kaplan.meier.curve(fu.time, event.state, "blue", "with")

      fu.time <- env()$survival.data[ "os", intersect(samples.without.metagene, colnames(env()$survival.data)) ]
      event.state <- env()$survival.data[ "event", intersect(samples.without.metagene, colnames(env()$survival.data)) ]
      event.state = event.state[order( fu.time )]
      fu.time = fu.time[order(fu.time)]
      plot.kaplan.meier.curve(fu.time, event.state, "red", "without")
    }

    session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_bottomframe",state="visible"))
  }
  
}, height = function() session$clientData$output_p_moduleBrowser_bottomframe_width )

#### --------------------------- Side - Panel (Genes) ---------------------------- #####
   
output$p_moduleBrowser_tabGenes <- renderDataTable({
  
  if( is.null(click.metagene()) ) return()
  
  if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
  {
    spot <- names(which(sapply(get(sel.spotlist(),envir = env() )$spots, function(x) click.metagene() %in% x$metagenes)))
    
    if (length(spot) > 0) 
    {
      spot.genes <- get(sel.spotlist(),envir = env() )$spots[[spot]]$genes
      df <- data.frame(ensemble = spot.genes, name = env()$gene.info$names[spot.genes],
                       chr = paste( env()$gene.info$chr.name[spot.genes], env()$gene.info$chr.band[spot.genes] ), 
                       description = sub( "\\[.*\\]", "", env()$gene.info$descriptions[spot.genes])   )
      
      df <- df[order(df$name),]
      df <- df[c(which(df$name!=""),which(df$name=="")),]
      rownames(df)<-NULL
      colnames(df)<-c("ID","Name", "Chr.", "Description")
      
      return( datatable( df, rownames=FALSE, options = list(paging=FALSE) ) )
      # caption = htmltools::tags$caption( style = 'color: black; font-weight: bold;','Module genes:'), 
    }
  }
 
})

output$p_moduleBrowser_tabFunc <- renderDataTable({
  
  if( is.null(click.metagene()) ) return()
  
  if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
  {
    spot <- names(which(sapply(get(sel.spotlist(),envir = env() )$spots, function(x) click.metagene() %in% x$metagenes)))
    
    if (length(spot) > 0) 
    {
      top.gs.p <- sort( get(sel.spotlist(),envir = env() )$spots[[spot]]$Fisher.p[names(which(sapply(env()$gs.def.list, function(x) x$Type) != "Chromatin states" ))])
      top.gs.p <- top.gs.p[ which(top.gs.p<0.05) ]
      spot.genes <- get(sel.spotlist(),envir = env() )$spots[[spot]]$genes
      
      df = data.frame(  format(top.gs.p, digits=1), 
                        paste (sapply(env()$gs.def.list[names(top.gs.p)], function(x)
                            {length(intersect(x$Genes, env()$gene.info$ids[spot.genes]))}), "/",
                            sapply(env()$gs.def.list[names(top.gs.p)], function(x)
                            {length(x$Genes)})), 
                        names(top.gs.p)
                      )

      rownames(df) <- NULL
      colnames (df) = c("p-value", "#in/all", "Geneset")

      return( datatable( df, rownames=FALSE, options = list(paging=FALSE) ) )
    }
  }
  
})
  
  

output$p_moduleBrowser_tabClass <- renderDataTable({
  
  if( is.null(click.metagene()) ) return()
  
  if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
  {
    spot <- names(which(sapply(get(sel.spotlist(),envir = env() )$spots, function(x) click.metagene() %in% x$metagenes)))
    
    if (length(spot) > 0) 
    {
      samples.with.spot <- names(which( get(sel.spotlist(),envir = env() )$spotdata[spot,] > sd(get(sel.spotlist(),envir = env() )$spotdata) ))

      group.table <- table(env()$group.labels[samples.with.spot])[unique(env()$group.labels)]
      group.table <- group.table[which(!is.na(group.table))]
 
      group.table <- t( sapply( seq_along(group.table), function(g)
      {
        c( names(group.table)[g],
           group.table[g],
           round(100 * group.table[g]/sum(env()$group.labels == names(group.table)[g]), 1)
        )
      } ) )
      colnames(group.table) = c("subtype","#","%")
      
      return( datatable( group.table, rownames=FALSE, options = list(paging=FALSE,searching=FALSE) ) )     
    }
    
  } else
  {
    
    
  }
  
# {
  #   if(selected_map() >= 7 && !is.null(samples_with())){
  #   group.table <-
  #     table(env$group.labels[samples_with()])[unique(env$group.labels)]
  #   group.table <- group.table[which(!is.na(group.table))]
  # 
  #   group.table <- t( sapply( seq_along(group.table), function(g)
  #   {
  #     c( names(group.table)[g],
  #        group.table[g],
  #        round(100 * group.table[g]/sum(env$group.labels ==
  #                                         names(group.table)[g]), 1)
  #     )
  #   } ) )
  #   colnames(group.table) = c("subtype","#","%")
  # 
  #   datatable(
  #     group.table, options = list(dom = 't')
  #   )
  # }
  # else{
  #   if(selected_map() >= 7){
  #     infotext <- "For more details click the spot on the map!"
  #     df <- matrix(infotext)
  #     datatable(
  #       df, options = list(dom = 't')
  #     )
  #   }
  # }
  # })
  
})

