
##### --------------------------- Main Panel -------------------------------------- ####


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

  p <- ggplotly(p, tooltip="text") %>%
    config(p, displayModeBar = FALSE)
  p
})


##### --------------------------- Main Panel - bottom-------------------------------------- ####

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
  g1 <- sapply(envA()$gs.def.list, function(x){intersect(x$Genes, g.all.intersect)}) # Genes in respective gene set of data set 1
  g2 <- genes_common()$Ensemble # common genes - remains the same
  
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
    g1 <- sapply(envB()$gs.def.list, function(x){intersect(x$Genes, g.all.intersect)}) # Genes in respective gene set of data set 1
    g2 <- genes_common()$Ensemble # common genes - remains the same
    
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
    {
      # heatplot, ggplot, with text in aes
      p <- ggplot(df, aes(X, Y, text=text)) + 
        geom_point(aes(color=df$Z, alpha=df$Mask), shape=15, size = 1)+
        scale_colour_gradientn(colours=envA()$color.palette.portraits(1000))+
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
  {
    # heatplot, ggplot, with text in aes
    p <- ggplot(df, aes(X, Y, text=text)) + 
      geom_point(aes(color=df$Z, alpha=df$Mask), shape=15, size = 1)+
      scale_colour_gradientn(colours=envB()$color.palette.portraits(1000))+
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
    ggplotly(p, tooltip="text") %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 10, r = 10, b = 10, t = 0, pad = 4))  
  }
  
  }})
#  ,height = function() {
#    session$clientData$output_p_moduleBrowser_side_GR_B_width
#})



##### --------------------------- Side - Panel - bottom----------------------------- ####

## Expression profiles
output$p_moduleBrowser_bottomframeA <- renderPlotly({
  
  clicked <- event_data("plotly_click")
  clicked.A <- clicked$x
  
  if (!is.null(clicked) && (!is.na(rownames(envA()$spot.list.group.overexpression$spotdata)[clicked.A])))
  {
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
