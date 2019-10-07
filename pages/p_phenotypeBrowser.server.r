

kaplan.meier.df <- function(fu.time,event.state)
{
  S.lefttail <- 1
  n.i <- length(fu.time)
  
  S <- rep( NA, n.i )
  for( t.i in 1:n.i )
  {
    S[t.i] <- S.lefttail * ( ( n.i-event.state[t.i] ) / n.i )
    S.lefttail <- S[t.i]
    n.i <- n.i - 1
  }
  S <- c( 1, S )
  names(S) <- c( 0, fu.time )
  
  
  S.stairway <- 1
  t.stariway <- 0
  i=2
  for( i in 2:length(S) )
  {
    if( S[i] == S[i-1] )
    {
      S.stairway <- c( S.stairway, S[i] )
      t.stariway <- c( t.stariway, fu.time[i-1] )
      
    } else
    {
      S.stairway <- c( S.stairway, S[i-1] )
      t.stariway <- c( t.stariway, fu.time[i-1] )			
      
      S.stairway <- c( S.stairway, S[i] )
      t.stariway <- c( t.stariway, fu.time[i-1] )			
    }
  }
  return(data.frame(cbind(Years = round(t.stariway, 1), Probability = round(S.stairway, 2))))
  #lines( t.stariway, S.stairway, type="l", col=col, lwd=3 )	
  #points( fu.time[ which( event.state == 0 ) ], S[ which( event.state == 0 )+1 ], pch=3, col=col )
  
  #text( tail(t.stariway,1)+0.1, tail(S.stairway,1), caption, cex=0.9, adj=0 )
  
}



pheno.sel <- reactive({
  if(input$p_phenotypeBrowser_selectPheno == "Age"){
    selA <- (info.age[input$dataset_select])
    selB <- (info.age[input$dataset_selectB])
  }
  
  if(input$p_phenotypeBrowser_selectPheno == "Sex"){
    selA <- (info.sex[input$dataset_select])
    selB <- (info.sex[input$dataset_selectB])
  }

  if(input$p_phenotypeBrowser_selectPheno == "Histology"){
    selA <- (info.histology[input$dataset_select])
    selB <- (info.histology[input$dataset_selectB])
  }

  if(input$p_phenotypeBrowser_selectPheno == "Molecular type"){
    selA <- (info.molecular[input$dataset_select])
    selB <- (info.molecular[input$dataset_selectB])
  }

return( list( p_phenotypeBrowser_selectPhenoA=selA, p_phenotypeBrowser_selectPhenoB=selB ))
})



pheno.info <- reactive({
  
  # # catch call when selectPheno is still empty
  #if( is.null(input$p_phenotypeBrowser_selectPhenoA) ) return( NULL )
  #if( is.null(input$p_phenotypeBrowser_selectPhenoB) ) return( NULL ) 
  
  # # catch call when selectPheno2 is empty and needs to be rendered
  # if( is.null(input$p_phenotypeBrowser_selectPheno2)  )
  # {
  #   updateSelectInput( session, "p_phenotypeBrowser_selectPheno2", selected = "all" )
  #   return(NULL)
  # }
  
  # # catch race conditions when 'all' is picked and selectPheno2 needs update
  # if( length(input$p_phenotypeBrowser_selectPheno2) > 1 &&
  #     "all" %in% input$p_phenotypeBrowser_selectPheno2 ) return( NULL )
  
  # # catch race conditions when dataset is switched and choices are outdated
  # if( !input$p_phenotypeBrowser_selectPheno %in%    
  #     unique( sapply( strsplit( colnames(env()$pheno.table), "_" ), head, 1 ) ) ) return( NULL )
  
  
  classesA <- grep( paste0("^", pheno.sel()$p_phenotypeBrowser_selectPhenoA,"_"), colnames(envA()$pheno.table), value=T )
  classesA <- classesA[order(classesA)]
  classesB <- grep( paste0("^", pheno.sel()$p_phenotypeBrowser_selectPhenoB,"_"), colnames(envB()$pheno.table), value=T )
  classesB <- classesB[order(classesB)]
  
  # if( !"all" %in% input$p_phenotypeBrowser_selectPheno2 ) 
  # {
  #   
  #   if( !paste( input$p_phenotypeBrowser_selectPheno, 
  #               input$p_phenotypeBrowser_selectPheno2[1], sep="_") %in% classes ) return(NULL)
  #   
  #   classes <- paste( input$p_phenotypeBrowser_selectPheno, 
  #                     input$p_phenotypeBrowser_selectPheno2, sep="_")
  # }
  
  # samples (schreibt bei jedem sample die class rein)
  samplesA <- apply( envA()$pheno.table[,classesA,drop=FALSE], 1, function(x)
  {
    x = unlist(x)
    if(any(!is.na(x))&&any(x)) classesA[which(x)] else ""
  })
  
  samplesB <- apply( envB()$pheno.table[,classesB,drop=FALSE], 1, function(x)
  {
    x = unlist(x)
    if(any(!is.na(x))&&any(x)) classesB[which(x)] else ""
  })

  return( list( classesA=classesA, samplesA=samplesA, classesB=classesB, samplesB=samplesB ) )
})


## plotly used to render graph
output$p_phenotypeBrowser_survivalCurves <- renderPlotly({

  # if( is.null(pheno.info()) || (is.null(envA()$survival.data) && is.null(envB()$survival.data) )) return()
  
  # ggplot 
  p <- ggplot() + 
    theme_light() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())+
    scale_x_continuous(name = "Years", expand=c(0,0), limits = c(0,10)) +
    scale_y_continuous(name = "Probability (OS)",limits = c(0,1))
  
  #loop for every class in envA
  colfunc <- colorRampPalette(c("cornflowerblue", "darkblue"))
  info.colA <- colfunc(length(pheno.info()$classesA))
  i <- 1
  
  if(!is.null(envA()$survival.data) && any(input$p_phenotypeBrowser_selectdata == "data set 1")){
    for( gr.i in pheno.info()$classesA )
      {
      gr.samples <- intersect(names(which(pheno.info()$samplesA==gr.i)), colnames(envA()$survival.data) )
      fu.time <- envA()$survival.data[1,gr.samples]
      event.state <- envA()$survival.data[2,gr.samples]
    
      event.state = event.state[order( fu.time )]
      fu.time = fu.time[order( fu.time )]
    
      if(length(fu.time)>0)
        data = data.frame(kaplan.meier.df(fu.time, event.state), Class = strsplit( gr.i, "_" )[[1]][2])
      p <- p + geom_line(data = data, 
                          aes(x = Years, y = Probability, text = paste(input$p_phenotypeBrowser_selectPheno,":", Class)), 
                          color = info.colA[i])
      i <- i+1
    }
    #p <- p + geom_text() + annotate("text", label = "data set 1", x=0.4, y=0.1, colour = "cornflowerblue", size = 3.5)
  } 
  
  # loop for every class in envB
  colfunc <- colorRampPalette(c("gold", "gold4"))
  info.colB <- colfunc(length(pheno.info()$classesB))
  i <- 1
  
  if(!is.null(envB()$survival.data) && any(input$p_phenotypeBrowser_selectdata == "data set 2")){
    for( gr.i in pheno.info()$classesB )
    {
      gr.samples <- intersect( names(which(pheno.info()$samplesB==gr.i)), colnames(envB()$survival.data) )
      fu.time <- envB()$survival.data[1,gr.samples]
      event.state <- envB()$survival.data[2,gr.samples]
      
      event.state = event.state[order( fu.time )]
      fu.time = fu.time[order( fu.time )]
      
      if(length(fu.time)>0)
        data = data.frame(kaplan.meier.df(fu.time, event.state), Class = strsplit( gr.i, "_" )[[1]][2])
      p <- p + geom_line(data = data, 
                          aes(x = Years, y = Probability, text = paste(input$p_phenotypeBrowser_selectPheno,":", Class)), 
                          color = info.colB[i])
      i <- i+1
    }
    #p <- p + geom_text() + annotate("text", label = "data set 2", x = 0.4, y=0.05, colour = "gold", size = 3.5)
  } 
  ggplotly(p) %>%
    config(displayModeBar = FALSE)
})

