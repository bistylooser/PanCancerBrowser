

# #### --------------------------- Global Variables ------------------------------- ####
#    
# sel.spotlist <- reactiveVal(NULL) 
#click.metagene <- reactiveVal(NULL) 
# 
# 



# #### --------------------------- Main Panel -------------------------------------- ####


## heatmap for common genes of each module of data set 1 and 2 
output$p_moduleBrowser_plotframe <- renderPlotly ({
  
  # list of unique Ensemble IDs for each module of envA
  envA_genes <- sapply(envA()$spot.list.group.overexpression$spots, function(x){
    a <- unique(envA()$gene.info$ids[x$genes])
    a[which(a != "")]
  })
  # list of unique Ensemble IDs for each module of envB
  envB_genes <- sapply(envB()$spot.list.group.overexpression$spots, function(x){
    b <- unique(envB()$gene.info$ids[x$genes])
    b[which(b != "")]
  })
  
  # matrix - counts of genes (Ensemble IDs) for each module combination
  data <- sapply(envB_genes, function(y){
    sapply(envA_genes, function(x){
      length(which(x %in% y))
    })
  })
  
  ## matrix - p-values for each combination
  g.all.intersect <- intersect(envA()$gene.info$ids, envB()$gene.info$ids)
  g1 <- sapply(envA_genes, function(x){intersect(x, g.all.intersect)})
  g2 <- sapply(envB_genes, function(x){intersect(x, g.all.intersect)})
  
  data.p <- sapply(g2, function(y){
    sapply(g1, function(x){
      # Contingency table
      a <- length(intersect(y,x))
      b <- length(setdiff(y, intersect(y,x))) #b <- length(g2)-a
      c <- length(setdiff(x, intersect(y,x))) #c <- length(g1)-a
      d <- length(g.all.intersect) - a - b - c
      data.fisher <- matrix(c(a,c,b,d),2)
      #Fisher´s exact test
      p <- fisher.test(data.fisher, alternative = "greater")
      p$p.value 
    })
  })
  
  # df
  x <- names(envA()$spot.list.group.overexpression$spots)
  y <- names(envB()$spot.list.group.overexpression$spots)
  df <- expand.grid(X=x, Y=y)
  df$Genes <- as.vector(data)
  df$x.total <- lengths(g1[df$X])
  df$y.total <- lengths(g2[df$Y])
  df$p <- as.vector(data.p)
  
  #text for tooltip:
  df$text <- paste0("Module 1: ", df$X, " (n = ", df$x.total, ")", "\n", "Module 2: ", df$Y, " (n = ", df$y.total, ")", "\n", "Common genes: ",df$Genes, "\n", "p-value: ",format.pval(df$p))
  
  # heatplot, ggplot, with text in aes
  p <- ggplot(df, aes(X, Y, fill= Genes, text=text)) + 
    geom_tile()+
    scale_fill_distiller(palette = "Blues") +
    labs(x = "Data set 1 / expression modules",
         y = "Data set 2 / expression modules")
  #theme(legend.title=element_blank())
  #scale_color_discrete(name="genes")
  p <- ggplotly(p, tooltip="text") %>%
    config(p, displayModeBar = FALSE)
  p
})


# #### --------------------------- Main Panel - bottom-------------------------------------- ####

## Gene Table
genes_common <- reactive({
  
  clicked <- event_data("plotly_click")
  clicked.A <- clicked$x
  clicked.B <- clicked$y
  if (!is.null(clicked) && 
      (!is.na(rownames(envA()$spot.list.group.overexpression$spotdata)[clicked.A])) &&
      (!is.na(rownames(envB()$spot.list.group.overexpression$spotdata)[clicked.B])))
  { 
    envA_genes <- unique(envA()$gene.info$ids[envA()$spot.list.group.overexpression$spots[[clicked.A]][["genes"]]])
    envA_genes <- envA_genes[which(envA_genes != "")]
    envB_genes <- unique(envB()$gene.info$ids[envB()$spot.list.group.overexpression$spots[[clicked.B]][["genes"]]])
    envB_genes <- envB_genes[which(envB_genes != "")]
    
    envA_common_genes <- intersect(envA_genes, envB_genes)
    envA_common_genes <- envA()$gene.info$ids[envA()$gene.info$ids %in% envA_common_genes]
    envA_common_genes <- envA_common_genes[!duplicated(envA_common_genes)]
  
    genes_commonTable <- 
      data.frame(
        Name = envA()$gene.info$names[names(envA_common_genes)],
        Ensemble = envA_common_genes,
        Chromosome = paste(envA()$gene.info$chr.name[names(envA_common_genes)],envA()$gene.info$chr.band[names(envA_common_genes)]),
        Description = envA()$gene.info$descriptions[names(envA_common_genes)],
        stringsAsFactors = FALSE)
  
  return(
    genes_commonTable
  )}
})

output$p_moduleBrowser_geneTable <- renderDataTable({
    return(
      genes_common()
    )
  }, rownames=F, selection = 'single')


## Function Tables

functionTable_data1 <- reactive({
  clicked <- event_data("plotly_click")
  if (!is.null(clicked) && !is.null(genes_common()))
  { 
  # matrix p-values for each combination
  g.all.intersect <- intersect(envA()$gene.info$ids, envB()$gene.info$ids)
  g1 <- sapply(envA()$gs.def.list, function(x){intersect(x$Genes, g.all.intersect)}) # Gene in jeweiligem Genset von dataset 1
  g2 <- genes_common()$Ensemble # common genes - bleibt gleich
  
  data.p <- sapply(g1, function(x){
    # Contingency table
    a <- length(intersect(g2,x))
    b <- length(setdiff(g2, intersect(g2,x))) #b <- length(g2)-a
    c <- length(setdiff(x, intersect(g2,x))) #c <- length(g1)-a
    d <- length(g.all.intersect) - a - b - c
    data.fisher <- matrix(c(a,c,b,d),2)
    #Fisher´s exact test
    p <- fisher.test(data.fisher, alternative = "greater")
    p$p.value 
  })
  
  # Sort table according to p-value
  data.p <- sort(data.p)
  
  # Additional infos
  Genes <- sapply(g1, function(x) length(intersect(g2,x)))
  Genset <- sapply(g1, function(x) length(x))
  
  p_moduleBrowser_functionTable <- 
    data.frame(
      Name = names(data.p),
      p_value= format.pval(data.p),
      Genes = Genes[names(data.p)],
      Genset = Genset[names(data.p)])

  return(
    p_moduleBrowser_functionTable
  )}
})

output$p_moduleBrowser_functionTable_data1 <- renderDataTable({
    return(
      functionTable_data1()
    )
  }, rownames=F, selection = 'single')



functionTable_data2 <- reactive({
  clicked <- event_data("plotly_click")
  if (!is.null(clicked) && !is.null(genes_common()))
  { 
    # matrix p-values for each combination
    g.all.intersect <- intersect(envA()$gene.info$ids, envB()$gene.info$ids)
    g1 <- sapply(envB()$gs.def.list, function(x){intersect(x$Genes, g.all.intersect)}) # Gene in jeweiligem Genset von dataset 1
    g2 <- genes_common()$Ensemble # common genes - bleibt gleich
    
    data.p <- sapply(g1, function(x){
      # Contingency table
      a <- length(intersect(g2,x))
      b <- length(setdiff(g2, intersect(g2,x))) #b <- length(g2)-a
      c <- length(setdiff(x, intersect(g2,x))) #c <- length(g1)-a
      d <- length(g.all.intersect) - a - b - c
      data.fisher <- matrix(c(a,c,b,d),2)
      #Fisher´s exact test
      p <- fisher.test(data.fisher, alternative = "greater")
      p$p.value 
    })
    
    # Sort table according to p-value
    data.p <- sort(data.p)
    
    # Additional infos
    Genes <- sapply(g1, function(x) length(intersect(g2,x)))
    Genset <- sapply(g1, function(x) length(x))
    
    p_moduleBrowser_functionTable <- 
      data.frame(
        Name = names(data.p),
        p_value= format.pval(data.p),
        Genes = Genes[names(data.p)],
        Genset = Genset[names(data.p)])
    
    return(
      p_moduleBrowser_functionTable
    )}
})

output$p_moduleBrowser_functionTable_data2 <- renderDataTable({
    return(
      functionTable_data2()
    )
  }, rownames=F, selection = 'single')





# #### --------------------------- Side - Panel ----------------------------- ####

## Module Text output
#invisible()
output$clickA <- renderPrint({
  d <- event_data("plotly_click")
  d <- d$x
  if (is.null(d)) {
    "Click on the map to select"
  } else if (is.na(rownames(envA()$spot.list.group.overexpression$spotdata)[d])) {
    "invalid selection"
  } else {
    paste("Module", rownames(envA()$spot.list.group.overexpression$spotdata)[d])
    }
})

output$clickB <- renderPrint({
  d <- event_data("plotly_click")
  d <- d$y
  if (is.null(d)) {
    "Click on the map to select"
  } else if (is.na(rownames(envB()$spot.list.group.overexpression$spotdata)[d])) {
    "invalid selection"
  } else {
    paste("Module", rownames(envB()$spot.list.group.overexpression$spotdata)[d])
  }
})


## Module plots
output$p_moduleBrowser_side_GR_A <- renderPlotly({
  
  clicked <- event_data("plotly_click")
  clicked.A <- clicked$x
  if (!is.null(clicked) && (!is.na(rownames(envA()$spot.list.group.overexpression$spotdata)[clicked.A])))
  {
    clicked.spot <-rownames(envA()$spot.list.group.overexpression$spotdata)[clicked.A]
    
    # df
    x <- 1:envA()$preferences$dim.1stLvlSom
    y <- 1:envA()$preferences$dim.1stLvlSom
    df <- expand.grid(X=x, Y=y)
    df$Z <- envA()$spot.list.group.overexpression$overview.map
    #Modules
    spots <- names(envA()$spot.list.group.overexpression$spots)
    df$Spots <- spots[envA()$spot.list.group.overexpression$overview.mask]
    # Mask
    df$Mask <- df$Spots == clicked.spot
    df$Mask[which(df$Mask==T)] = 1
    df$Mask[is.na(df$Mask)] = 0
    #text for tooltip:
    df$text <- paste0("Coordinate: (", df$X, ",", df$Y, ")", "\n", "Modul: ",df$Spots)
    
    
    if( input$p_moduleBrowser_checkbox_maskA == TRUE) # &&
        #any(is.na(envA()$spot.list.group.overexpression$overview.mask)) )
    {
      # heatplot, ggplot, with text in aes
      p <- ggplot(df, aes(X, Y, text=text)) + 
        #geom_tile(aes(fill=Z, alpha=Mask))+
        #geom_tile()+  # fill=df$fill
        geom_point(aes(color=df$Z, alpha=df$Mask), shape=15, size = 1)+
        scale_colour_gradientn(colours=envA()$color.palette.portraits(1000))+
        #scale_fill_gradientn(colors=envA$color.palette.portraits(1000))+
        scale_alpha(range=c(0.03,0.99))+
        theme(legend.title=element_blank(),
              axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              legend.position = "none")
      ggplotly(p, tooltip="text") %>%
        config(displayModeBar = FALSE) %>%
        layout(margin = list(l = 10, r = 10, b = 10, t = 0, pad = 4))
    }
    else
    {
      # heatplot, ggplot, with text in aes
      p <- ggplot(df, aes(X, Y, fill= Z, text=text)) + 
        geom_tile()+
        scale_fill_gradientn(colors=envA()$color.palette.portraits(1000))+
        theme(legend.title=element_blank(),
              axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              legend.position = "none")
      #scale_color_discrete(name="genes")
      ggplotly(p, tooltip="text") %>%
        config(displayModeBar = FALSE) %>%
        layout(margin = list(l = 10, r = 10, b = 10, t = 0, pad = 4)) 
    }
    
  }})
#  ,height = function() {
#    session$clientData$output_p_moduleBrowser_side_GR_A_width
#})



output$p_moduleBrowser_side_GR_B <- renderPlotly({
  clicked <- event_data("plotly_click")
  clicked.B <- clicked$y
  if (!is.null(clicked) && (!is.na(rownames(envB()$spot.list.group.overexpression$spotdata)[clicked.B])))
  {
    clicked.spot <-rownames(envB()$spot.list.group.overexpression$spotdata)[clicked.B]
    
  # df
  x <- 1:envB()$preferences$dim.1stLvlSom
  y <- 1:envB()$preferences$dim.1stLvlSom
  df <- expand.grid(X=x, Y=y)
  df$Z <- envB()$spot.list.group.overexpression$overview.map
  # Module names
  spots <- names(envB()$spot.list.group.overexpression$spots)
  df$Spots <- spots[envB()$spot.list.group.overexpression$overview.mask]
  # Mask
  df$Mask <- df$Spots == clicked.spot
  df$Mask[which(df$Mask==T)] = 2
  df$Mask[is.na(df$Mask)] = 0
  #text for tooltip:
  df$text <- paste0("Coordinate: (", df$X, ",", df$Y, ")", "\n", "Module: ",df$Spots)
  
  
  if( input$p_moduleBrowser_checkbox_maskB == TRUE) # &&
    #any(is.na(envB()$spot.list.group.overexpression$overview.mask)) )
  {
    # heatplot, ggplot, with text in aes
    p <- ggplot(df, aes(X, Y, text=text)) + 
      #geom_tile(aes(fill=Z, alpha=Mask))+
      #geom_tile()+  # fill=df$fill
      geom_point(aes(color=df$Z, alpha=df$Mask), shape=15, size = 1)+
      scale_colour_gradientn(colours=envB()$color.palette.portraits(1000))+
      #scale_fill_gradientn(colors=envA$color.palette.portraits(1000))+
      scale_alpha(range=c(0.03,0.99))+
      theme(legend.title=element_blank(),
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.position = "none")
    ggplotly(p, tooltip="text") %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 10, r = 10, b = 10, t = 0, pad = 4))
  }
  else
  {
    # heatplot, ggplot, with text in aes
    p <- ggplot(df, aes(X, Y, fill= Z, text=text)) + 
      geom_tile()+
      scale_fill_gradientn(colors=envB()$color.palette.portraits(1000))+
      theme(legend.title=element_blank(),
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.position = "none")
    #scale_color_discrete(name="genes")
    ggplotly(p, tooltip="text") %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 10, r = 10, b = 10, t = 0, pad = 4))  
  }
  
  }})
#  ,height = function() {
#    session$clientData$output_p_moduleBrowser_side_GR_B_width
#})


# #### --------------------------- Side - Panel - bottom----------------------------- ####

## Expression profiles
output$p_moduleBrowser_bottomframeA <- renderPlotly({
  
  clicked <- event_data("plotly_click")
  clicked.A <- clicked$x
  
  if (!is.null(clicked) && (!is.na(rownames(envA()$spot.list.group.overexpression$spotdata)[clicked.A])))
  {
    #session$sendCustomMessage("element_visible", message=list(id="#p_genesetBrowser_checkbox_div", state="visible"))
    
    df <- data.frame(sample=1:length(envA()$spot.list.group.overexpression$spotdata[clicked.A,]), 
                     expression =round(envA()$spot.list.group.overexpression$spotdata[clicked.A,], 4), 
                     group = "", 
                     stringsAsFactors = FALSE)
    df$group <- as.factor(envA()$group.labels[rownames(df)])
    df$group <- ordered(df$group, levels=names(envA()$groupwise.group.colors))
    
    if( input$p_moduleBrowser_checkbox_boxA )
    {
      p <- ggplot(df, aes(group, expression)) +
        theme_light() +
        geom_boxplot(aes(fill = group), outlier.shape=2, size = 0.2)+
        scale_fill_manual(values = envA()$groupwise.group.colors[df$group]) +
        labs(
          #title= paste("Module", rownames(envA()$spot.list.group.overexpression$spotdata)[clicked.A]), 
          x = "", 
          y = "expression")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=5),
                           limits = c(min(envA()$spot.list.group.overexpression$spotdata), max(envA()$spot.list.group.overexpression$spotdata)))+
        theme(legend.position = "none",
              plot.title = element_text(size=11, hjust = 0.5),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.text.x= element_text(angle=20)) 
      ggplotly(p) %>%
        config(displayModeBar = FALSE) %>%
        layout(margin = list(l = 10, r = 10, b = 10, t = 0, pad = 4))
      
    } else
    {
      p <- ggplot(df, aes(sample, expression)) + 
        theme_light() +
        geom_bar(aes(fill = group, text = paste("group:", group)), stat = "identity")+
        scale_fill_manual(values = envA()$groupwise.group.colors[df$group])+
        labs(
          #title= paste("Module", rownames(envA()$spot.list.group.overexpression$spotdata)[clicked.A]), 
          x = "samples", 
          y = "expression")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=5), 
                           limits = c(min(envA()$spot.list.group.overexpression$spotdata), max(envA()$spot.list.group.overexpression$spotdata)))+
        theme(legend.position = "none",
              plot.title = element_text(size=11, hjust = 0.5),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) 
      ggplotly(p) %>%
        config(displayModeBar = FALSE) %>%
        layout(margin = list(l = 10, r = 10, b = 10, t = 0, pad = 4))
      
    }
  }
})



output$p_moduleBrowser_bottomframeB <- renderPlotly({
  
  clicked <- event_data("plotly_click")
  clicked.B <- clicked$y
  
  if (!is.null(clicked) && (!is.na(rownames(envB()$spot.list.group.overexpression$spotdata)[clicked.B])))
  {
    #session$sendCustomMessage("element_visible", message=list(id="#p_genesetBrowser_checkbox_div", state="visible"))
    
    df <- data.frame(sample=1:length(envB()$spot.list.group.overexpression$spotdata[clicked.B,]), 
                     expression =round(envB()$spot.list.group.overexpression$spotdata[clicked.B,], 4), 
                     group = "", 
                     stringsAsFactors = FALSE)
    df$group <- as.factor(envB()$group.labels[rownames(df)])
    df$group <- ordered(df$group, levels=names(envB()$groupwise.group.colors))
    
    if( input$p_moduleBrowser_checkbox_boxB )
    {
      p <- ggplot(df, aes(group, expression)) +
        theme_light() +
        geom_boxplot(aes(fill = group), outlier.shape=2, size = 0.2)+
        scale_fill_manual(values = envB()$groupwise.group.colors[df$group]) +
        labs(
          #title= paste("Module", rownames(envB()$spot.list.group.overexpression$spotdata)[clicked.B]), 
          x = "", 
          y = "expression")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=5),
                           limits = c(min(envB()$spot.list.group.overexpression$spotdata), max(envB()$spot.list.group.overexpression$spotdata)))+
        theme(legend.position = "none",
              plot.title = element_text(size=11, hjust = 0.5),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.text.x= element_text(angle=20))
      ggplotly(p) %>%
        config(displayModeBar = FALSE) %>%
        layout(margin = list(l = 1, r = 1, b = 1, t = 0, pad = 1))
      
    } else
    {
      p <- ggplot(df, aes(sample, expression)) + 
        theme_light() +
        geom_bar(aes(fill = group, text = paste("group:", group)), stat = "identity")+
        scale_fill_manual(values = envB()$groupwise.group.colors[df$group])+
        labs(
          #title= paste("Module", rownames(envB()$spot.list.group.overexpression$spotdata)[clicked.B]), 
          x = "samples", 
          y = "expression")+
        scale_y_continuous(breaks = scales::pretty_breaks(n=5), 
                           limits = c(min(envB()$spot.list.group.overexpression$spotdata), max(envB()$spot.list.group.overexpression$spotdata)))+
        theme(legend.position = "none",
              plot.title = element_text(size=11, hjust = 0.5),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
      ggplotly(p) %>%
        config(displayModeBar = FALSE) %>%
        layout(margin = list(l = 1, r = 1, b = 1, t = 0, pad = 1))
      
    }
  }
})













# 
# output$p_moduleBrowser_side_OE <- renderPlot({
#   par(mar = c(0.1, 0.1, 1.2, 0.1))
#   image( matrix( envA()$spot.list.overexpression$overview.map,
#                  envA()$preferences$dim.1stLvlSom ),col=envA()$color.palette.portraits(1000), axes=F, main = "Overexpression", cex.main=.8)
#   box()
# }, height = function() session$clientData$output_p_moduleBrowser_side_OE_width )
# 
# output$p_moduleBrowser_side_UE <- renderPlot({
#   par(mar = c(0.1, 0.1, 1.2, 0.1))
#   image( matrix( envA()$spot.list.underexpression$overview.map,
#                  envA()$preferences$dim.1stLvlSom ),col=envA()$color.palette.portraits(1000), axes=F, main = "Underexpression", cex.main=.8)
#   box()
# }, height = function() session$clientData$output_p_moduleBrowser_side_UE_width )
# 
# output$p_moduleBrowser_side_KM <- renderPlot({
#   par(mar = c(0.1, 0.1, 1.2, 0.1))
#   image( matrix( envA()$spot.list.kmeans$overview.map,
#                  envA()$preferences$dim.1stLvlSom ),col=envA()$color.palette.portraits(1000), axes=F, main = "k-Means", cex.main=.8)
#   box()
# }, height = function() session$clientData$output_p_moduleBrowser_side_KM_width )
# 
# output$p_moduleBrowser_side_CO <- renderPlot({
#   par(mar = c(0.1, 0.1, 1.2, 0.1))
#   image( matrix( envA()$spot.list.correlation$overview.map,
#                  envA()$preferences$dim.1stLvlSom ),col=envA()$color.palette.portraits(1000), axes=F, main = "Correlation", cex.main=.8)
#   box()
# }, height = function() session$clientData$output_p_moduleBrowser_side_CO_width )
# 
# output$p_moduleBrowser_side_DM <- renderPlot({
#   par(mar = c(0.1, 0.1, 1.2, 0.1))
#   image( matrix( envA()$spot.list.dmap$overview.map,
#                  envA()$preferences$dim.1stLvlSom ),col=colorRampPalette(c("blue2","white","red2"))(1000), axes=F, main = "D-Map", cex.main=.8)
#   box()
# }, height = function() session$clientData$output_p_moduleBrowser_side_DM_width )
# 
# output$p_moduleBrowser_side_GR_A <- renderPlot({
#   #par(mar = c(0.1, 0.1, 1.2, 0.1))
#   image( matrix( envA()$spot.list.group.overexpression$overview.map,
#                  envA()$preferences$dim.1stLvlSom ),col=envA()$color.palette.portraits(1000), axes=F, main = "Group overexpr.", cex.main=.8)
#   box()
# }) #, height = function() session$clientData$output_p_moduleBrowser_side_GR_width )

# output$p_moduleBrowser_side_AG <- renderPlot({
#   if(is.null(envA()$pheno.maps$age)) return()
#   par(mar = c(0.1, 0.1, 1.2, 0.1))
#   image( matrix(envA()$pheno.maps$age,envA()$preferences$dim.1stLvlSom),
#          col=envA()$color.palette.heatmaps(1000), axes=F, main = "Age", cex.main=.8)
#   box()
# }, height = function() session$clientData$output_p_moduleBrowser_side_AG_width )
# 
# output$p_moduleBrowser_side_SX <- renderPlot({
#   if(is.null(envA()$pheno.maps$sex)) return()
#   par(mar = c(0.1, 0.1, 1.2, 0.1))
#   image( matrix(envA()$pheno.maps$sex,envA()$preferences$dim.1stLvlSom),
#          zlim=max(abs(range(envA()$pheno.maps$sex)))*c(-1,1), col=envA()$color.palette.heatmaps(1000), axes=F, main = "Sex", cex.main=.8)
#   box()
# }, height = function() session$clientData$output_p_moduleBrowser_side_SX_width )
# 
# output$p_moduleBrowser_side_HR <- renderPlot({
#   if(is.null(envA()$pheno.maps$HR)) return()
#   par(mar = c(0.1, 0.1, 1.2, 0.1))
#   image( matrix(envA()$pheno.maps$HR,envA()$preferences$dim.1stLvlSom),
#          col=envA()$color.palette.heatmaps(1000), axes=F , main = "Survival", cex.main=.8)
#   box()
# }, height = function() session$clientData$output_p_moduleBrowser_side_HR_width )
# 
# 
# #### --------------------------- Side - Panel (Function) ------------------------- ####
#      
# observeEvent(input$p_moduleBrowser_side_OE_click, {
#   sel.spotlist("spot.list.overexpression")
#   click.metagene(NULL)
# })
# 
# observeEvent(input$p_moduleBrowser_side_GR_click, {
#   sel.spotlist("spot.list.group.overexpression")
#   click.metagene(NULL)
# })
# 
# observeEvent(input$p_moduleBrowser_side_UE_click, {
#   sel.spotlist("spot.list.underexpression")    
#   click.metagene(NULL)
# })
# 
# observeEvent(input$p_moduleBrowser_side_DM_click, {
#   sel.spotlist("spot.list.dmap") 
#   click.metagene(NULL)
# })
# 
# observeEvent(input$p_moduleBrowser_side_KM_click, {
#   sel.spotlist("spot.list.kmeans")
#   click.metagene(NULL)
# })
# 
# observeEvent(input$p_moduleBrowser_side_CO_click, {
#   sel.spotlist("spot.list.correlation")   
#   click.metagene(NULL)
# })
# 
# observeEvent(input$p_moduleBrowser_side_AG_click, {
#   if(is.null(envA()$pheno.maps$age)) return()
#   sel.spotlist("age") 
#   click.metagene(NULL)
# })
# 
# observeEvent(input$p_moduleBrowser_side_SX_click, {
#   if(is.null(envA()$pheno.maps$sex)) return()
#   sel.spotlist("sex") 
#   click.metagene(NULL)
# })
# 
# observeEvent(input$p_moduleBrowser_side_HR_click, {
#   if(is.null(envA()$pheno.maps$HR)) return()
#   sel.spotlist("HR") 
#   click.metagene(NULL)
# })
# 
# 


 
# output$p_moduleBrowser_plotframe <- renderPlot({
# 
#   if( is.null(sel.spotlist()) )
#     sel.spotlist( paste0("spot.list.",envA()$preferences$standard.spot.modules) )
# 
#   
#   mains = c( spot.list.overexpression="Overexpression modules", spot.list.underexpression="Underexpression modules",
#              spot.list.kmeans="k-Means modules", spot.list.correlation="Correlation modules",
#              spot.list.dmap="D-Map modules", spot.list.group.overexpression="Group overexpression modules",
#              "age"="Age map","sex"="Sex map","HR"="Survival map" )
# 
#     
#   if( grepl("spot.list",sel.spotlist() ) )  ### one of the spotmaps selected?
#   {
#     par(mar=c(0.1,0.1,2.2,0.1))
#     image( matrix( get(sel.spotlist(),envir = envA() )$overview.map, envA()$preferences$dim.1stLvlSom), axes=F, main=mains[sel.spotlist()],
#            col = if( sel.spotlist() != "spot.list.dmap" ) envA()$color.palette.portraits(1000) else colorRampPalette(c("blue2","white","red2"))(1000) )
#     box()
#     
#     if( input$p_moduleBrowser_checkbox == TRUE &&
#         any(is.na(  get(sel.spotlist(),envir = envA() )$overview.mask  )) )
#     {
#       mask = matrix( get(sel.spotlist(),envir = envA() )$overview.mask, envA()$preferences$dim.1stLvlSom )
#       mask = is.na(mask)
#       mask[which(mask==F)] = NA
#       
#       par(new=T)
#       image( mask, col="#B0B0B0BB", axes=F )
#     }
#     
#     session$sendCustomMessage("element_visible", 
#                               message=list(id="#p_moduleBrowser_checkbox_div", state="visible"))
#     
#   } else
#   {
#     par(mar=c(0.1,0.1,2.2,0.1))
#     image( matrix(envA()$pheno.maps[[sel.spotlist()]], envA()$preferences$dim.1stLvlSom), main=mains[sel.spotlist()],
#            col=envA()$color.palette.heatmaps(1000), axes=F )
#     box()
#           
#     session$sendCustomMessage("element_visible", 
#                               message=list(id="#p_moduleBrowser_checkbox_div", state="hidden"))
#     
#   }
#    
# }, height = function() session$clientData$output_p_moduleBrowser_plotframe_width )
# 
#   
# output$p_moduleBrowser_hoverbox <- renderText({
#    
#   if( is.null(input$p_moduleBrowser_plotframe_hover) ) 
#   {
#     session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_hoverbox",
#                                            state="hidden"))
#     return()
#   }
#   
#   session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_hoverbox",
#                                          state="visible"))
#   
#   x <- ceiling((input$p_moduleBrowser_plotframe_hover$x - input$p_moduleBrowser_plotframe_hover$domain$left)/(input$p_moduleBrowser_plotframe_hover$domain$right - input$p_moduleBrowser_plotframe_hover$domain$left)* envA()$preferences$dim.1stLvlSom )
#   y <- ceiling((input$p_moduleBrowser_plotframe_hover$y - input$p_moduleBrowser_plotframe_hover$domain$bottom)/(input$p_moduleBrowser_plotframe_hover$domain$top - input$p_moduleBrowser_plotframe_hover$domain$bottom)* envA()$preferences$dim.1stLvlSom )
#   xy_id <- x + (y-1) * envA()$preferences$dim.1stLvlSom
#   out <- paste( "Coordinate: (", x, ",", y, ")")
#      
#   if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
#   {
#     spot <- names(which(sapply(get(sel.spotlist(),envir = envA() )$spots, function(x) xy_id %in% x$metagenes)))
#     out <- if(length(spot) == 0) paste( "Coordinate: (", x, ",", y, ")<br>Spot: none")
#            else paste( "Coordinate: (", x, ",", y, ")<br>Spot: ", spot)
#   }
#   
#   return(out)
# 
# })
# 
# observeEvent(input$p_moduleBrowser_plotframe_click, {
#   if( is.null(input$p_moduleBrowser_plotframe_click) ) return()
#   click.metagene <- input$p_moduleBrowser_plotframe_click$x
# })



# observeEvent(input$p_moduleBrowser_plotframe_click, {
#   
#   if( is.null(input$p_moduleBrowser_plotframe_click) ) return()
#   
#   x <- ceiling((input$p_moduleBrowser_plotframe_click$x - input$p_moduleBrowser_plotframe_click$domain$left)/(input$p_moduleBrowser_plotframe_click$domain$right - input$p_moduleBrowser_plotframe_click$domain$left)* envA()$preferences$dim.1stLvlSom )
#   y <- ceiling((input$p_moduleBrowser_plotframe_click$y - input$p_moduleBrowser_plotframe_click$domain$bottom)/(input$p_moduleBrowser_plotframe_click$domain$top - input$p_moduleBrowser_plotframe_click$domain$bottom)* envA()$preferences$dim.1stLvlSom )
#   xy_id <- x + (y-1) * envA()$preferences$dim.1stLvlSom
#        
#   click.metagene( xy_id )
#   
#   # session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_tabsetpanel",state="visible"))
#   
# })
# 





# output$p_moduleBrowser_bottomframe <- renderPlot({
# 
#   if( is.null(click.metagene()) ) return()
#   
#   if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
#   {
#     spot <- names(which(sapply(get(sel.spotlist(),envir = envA() )$spots, function(x) click.metagene() %in% x$metagenes)))
#        
#     if (length(spot) > 0) 
#     {
#       par(mar=c(2.2, 4.2, 0.1, 0.1))
#       barplot( get(sel.spotlist(),envir = envA() )$spotdata[spot,], border = NA, col=envA()$group.colors, names.arg = NA)
#       mtext("samples", 1, line = 1)
#       mtext("expression", 2, line = 2)
#       
#       session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_bottomframe",state="visible"))
#       
#     } else
#       session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_bottomframe",state="hidden"))
#     
#   } else
#   {
#     metagene.samples <- lapply( c(1:nrow(envA()$metadata)), function(i) names( which(envA()$metadata[i,]>0 ) ) )
#     
#     samples.with.metagene <- metagene.samples[[click.metagene()]]
#     samples.without.metagene <- setdiff(colnames(envA()$metadata), samples.with.metagene)
#     
#     if( sel.spotlist() == "age")
#     {
#       par(mar=c(2.2, 4.2, 2.1, 0.1))
#       hist( envA()$patient.data[samples.with.metagene,"age"], main="Age distribution", xlab = "age", xlim=c(0,100), ylab = "frequency", col="gray90" )
#       
#     } else
#     if( sel.spotlist() == "sex")
#     {
#       gender.table <- table(envA()$patient.data[samples.with.metagene,"sex"])[c("female","male","na")]
#       gender.table <- gender.table[which(!is.na(gender.table))]
# 
#       par(mar=c(1.2, 4.2, 2.1, 0.1))
#       pie(gender.table, main="Sex distribution", col=c("lightpink","cornflowerblue","gray80")[match(names(gender.table), c("female","male","na"))],
#           labels = paste0(round(prop.table(gender.table)*100, 2), "%"))
#       legend(.9, .3, c("female","male","N/A"), cex = 0.7, fill = c("lightpink","cornflowerblue","gray80"))
#       
#     } else
#     if( sel.spotlist() == "HR")
#     {
#       # par(mar=c(4.2, 6.2, 1.1, 0.1))
#       # image(matrix(1:100, 1, 100), col = envA()$color.palette.heatmaps(1000), axes=FALSE)
#       # axis(2, round(c(min(envA()$pheno.maps$HR,na.rm=T), max(envA()$pheno.maps$HR,na.rm=T)),1), at=c(0, 1), las=2, tick=FALSE, pos=-0.5, cex.axis=1.4)
#       # box()
#     
#       plot(0, type="n", xlim=c(0,10), ylim=c(0,1), las=1, xlab="years", ylab="% overall survival", cex.axis=1, cex.lab=1)
#       mtext("Overall survival curves", 3)
#       
#       fu.time <- envA()$survival.data[ "os", intersect(samples.with.metagene, colnames(envA()$survival.data)) ]
#       event.state <- envA()$survival.data[ "event", intersect(samples.with.metagene, colnames(envA()$survival.data)) ]
#       event.state = event.state[order(fu.time)]
#       fu.time = fu.time[order(fu.time)]
#       plot.kaplan.meier.curve(fu.time, event.state, "blue", "with")
# 
#       fu.time <- envA()$survival.data[ "os", intersect(samples.without.metagene, colnames(envA()$survival.data)) ]
#       event.state <- envA()$survival.data[ "event", intersect(samples.without.metagene, colnames(envA()$survival.data)) ]
#       event.state = event.state[order( fu.time )]
#       fu.time = fu.time[order(fu.time)]
#       plot.kaplan.meier.curve(fu.time, event.state, "red", "without")
#     }
# 
#     session$sendCustomMessage("element_visible", message=list(id="#p_moduleBrowser_bottomframe",state="visible"))
#   }
#   
# }, height = function() session$clientData$output_p_moduleBrowser_bottomframe_width )
# 
# #### --------------------------- Side - Panel (Genes) ---------------------------- #####




#    
# output$p_moduleBrowser_tabGenes <- renderDataTable({
#   
#   if( is.null(click.metagene()) ) return()
#   
#   if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
#   {
#     spot <- names(which(sapply(get(sel.spotlist(),envir = envA() )$spots, function(x) click.metagene() %in% x$metagenes)))
#     
#     if (length(spot) > 0) 
#     {
#       spot.genes <- get(sel.spotlist(),envir = envA() )$spots[[spot]]$genes
#       df <- data.frame(ensemble = spot.genes, name = envA()$gene.info$names[spot.genes],
#                        chr = paste( envA()$gene.info$chr.name[spot.genes], envA()$gene.info$chr.band[spot.genes] ), 
#                        description = sub( "\\[.*\\]", "", envA()$gene.info$descriptions[spot.genes])   )
#       
#       df <- df[order(df$name),]
#       df <- df[c(which(df$name!=""),which(df$name=="")),]
#       rownames(df)<-NULL
#       colnames(df)<-c("ID","Name", "Chr.", "Description")
#       
#       return( datatable( df, rownames=FALSE, options = list(paging=FALSE) ) )
#       # caption = htmltools::tags$caption( style = 'color: black; font-weight: bold;','Module genes:'), 
#     }
#   }
#  
# })
# 
# output$p_moduleBrowser_tabFunc <- renderDataTable({
#   
#   if( is.null(click.metagene()) ) return()
#   
#   if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
#   {
#     spot <- names(which(sapply(get(sel.spotlist(),envir = envA() )$spots, function(x) click.metagene() %in% x$metagenes)))
#     
#     if (length(spot) > 0) 
#     {
#       top.gs.p <- sort( get(sel.spotlist(),envir = envA() )$spots[[spot]]$Fisher.p[names(which(sapply(envA()$gs.def.list, function(x) x$Type) != "Chromatin states" ))])
#       top.gs.p <- top.gs.p[ which(top.gs.p<0.05) ]
#       spot.genes <- get(sel.spotlist(),envir = envA() )$spots[[spot]]$genes
#       
#       df = data.frame(  format(top.gs.p, digits=1), 
#                         paste (sapply(envA()$gs.def.list[names(top.gs.p)], function(x)
#                             {length(intersect(x$Genes, envA()$gene.info$ids[spot.genes]))}), "/",
#                             sapply(envA()$gs.def.list[names(top.gs.p)], function(x)
#                             {length(x$Genes)})), 
#                         names(top.gs.p)
#                       )
# 
#       rownames(df) <- NULL
#       colnames (df) = c("p-value", "#in/all", "Geneset")
# 
#       return( datatable( df, rownames=FALSE, options = list(paging=FALSE) ) )
#     }
#   }
#   
# })
#   
#   
# 
# output$p_moduleBrowser_tabClass <- renderDataTable({
#   
#   if( is.null(click.metagene()) ) return()
#   
#   if( grepl("spot.list",sel.spotlist() ) )   ### one of the spotmaps selected?
#   {
#     spot <- names(which(sapply(get(sel.spotlist(),envir = envA() )$spots, function(x) click.metagene() %in% x$metagenes)))
#     
#     if (length(spot) > 0) 
#     {
#       samples.with.spot <- names(which( get(sel.spotlist(),envir = envA() )$spotdata[spot,] > sd(get(sel.spotlist(),envir = envA() )$spotdata) ))
# 
#       group.table <- table(envA()$group.labels[samples.with.spot])[unique(envA()$group.labels)]
#       group.table <- group.table[which(!is.na(group.table))]
#  
#       group.table <- t( sapply( seq_along(group.table), function(g)
#       {
#         c( names(group.table)[g],
#            group.table[g],
#            round(100 * group.table[g]/sum(envA()$group.labels == names(group.table)[g]), 1)
#         )
#       } ) )
#       colnames(group.table) = c("subtype","#","%")
#       
#       return( datatable( group.table, rownames=FALSE, options = list(paging=FALSE,searching=FALSE) ) )     
#     }
#     
#   } else
#   {
#     
#     
#   }
  
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
  
#})

