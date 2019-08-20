

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


# get.id <- function(x,y)
# {
#   id <- NA
#   if( x > -1.095 && x < 1.095 && y > -1.095 && y < 1.095 )
#   {
#     x.i <- 1+ round( ( x+1.1 )*100 )
#     y.i <- 1+ round( ( y+1.1 )*100 )
#     id <- envA()$cn.graph.nodehover[ x.i, y.i ]
#   }
#   return( id )
# }

#hover.id <- reactive({
  #if( is.null(input$p_phenotypeBrowser_correlationNetwork_hover) ) return(NA)
  #x <- input$p_phenotypeBrowser_correlationNetwork_hover$x
  #y <- input$p_phenotypeBrowser_correlationNetwork_hover$y
  #return( get.id(x,y) )
#})


#click.id <- reactive({
  #if( is.null(input$p_phenotypeBrowser_correlationNetwork_click) ) return(NA)
  #x <- input$p_phenotypeBrowser_correlationNetwork_click$x
  #y <- input$p_phenotypeBrowser_correlationNetwork_click$y
  #return( get.id(x,y) )
#})

## Jetzt in phenotypeBrowser ui
# output$p_phenotypeBrowser_sideMenu <- renderUI(
#   div( class="side_menu",
#     h4("Select phenotype"),
#     fluidRow(
#       column( 6,
#         selectInput("p_phenotypeBrowser_selectPheno", label = NULL,
#                      #choices = unique( sapply( strsplit( colnames(envA()$pheno.table), "_" ), head, 1 ) ),
#                      choices = c("Overall", "Age", "Gender", "Histology", "Molecular type"), selected = "Age")
#       )
#     )
#   )
# )
      
# # choices aus daten envA()$pheno.table geholt - bei dieser Version vorgegeben = nicht nötig
# observeEvent(input$p_phenotypeBrowser_selectPheno, {
# 
#   choices <- grep( paste0("^",input$p_phenotypeBrowser_selectPheno,"_"), colnames(envA()$pheno.table), value=T )
#   choices <- sapply( strsplit( choices, "_" ), tail, 1 )
#   choices <- c( "all", choices )
#   updateSelectInput( session, "p_phenotypeBrowser_selectPheno2", choices = choices, selected = "all" )
# 
# })

#observeEvent(input$p_phenotypeBrowser_selectPheno2, { 
  
  #if( tail( input$p_phenotypeBrowser_selectPheno2, 1 ) == "all" )
  #{
    #updateSelectInput( session, "p_phenotypeBrowser_selectPheno2", selected = "all" )
  #}
  #else if( "all" %in% input$p_phenotypeBrowser_selectPheno2 &&
           # length(input$p_phenotypeBrowser_selectPheno2) > 1 )
  #{
    #selected <- setdiff( input$p_phenotypeBrowser_selectPheno2, "all" )
    #updateSelectInput( session, "p_phenotypeBrowser_selectPheno2", selected = selected )
  #}

#})

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
  
  
  classesA <- grep( paste0(pheno.sel()$p_phenotypeBrowser_selectPhenoA,"_"), colnames(envA()$pheno.table), value=T )
  classesA <- classesA[order(classesA)]
  classesB <- grep( paste0(pheno.sel()$p_phenotypeBrowser_selectPhenoB,"_"), colnames(envB()$pheno.table), value=T )
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




output$p_phenotypeBrowser_survivalCurves <- renderPlotly({

  if( is.null(pheno.info()) || (is.null(envA()$survival.data) && is.null(envB()$survival.data) )) return()
  
  # ggplt plot 
  p <- ggplot() + 
    theme_light() +
    #ggtitle ("Survival curves")+
    #theme(plot.margin = margin(10, 0, 10, 0))+
    scale_x_continuous(name = "Years", limits = c(0,10)) +
    scale_y_continuous(name = "Probability (OS)", limits = c(0,1))
  
  #loop for every class data envA
  colfunc <- colorRampPalette(c("cornflowerblue", "darkblue"))
  info.colA <- colfunc(length(pheno.info()$classesA))
  i <- 1
  if(!is.null(envA()$survival.data)){
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
    p
  } 
  
  # loop for every class data envB
  colfunc <- colorRampPalette(c("gold", "gold4"))
  info.colB <- colfunc(length(pheno.info()$classesB))
  i <- 1
  if(!is.null(envB()$survival.data)){
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
    p
  } else {
    return()
  }
})




##### Ende
# output$p_phenotypeBrowser_correlationNetwork <- renderPlot({
#   
#   if( is.null(pheno.info()) ) return()
# 
#   vertex.size <- 10
#   if(vcount(envA()$cn.graph)>100) vertex.size <- 8
#   if(vcount(envA()$cn.graph)>500) vertex.size <- 6
# 
#   par(mar=c(0,0,1,0))
#   frame()
#   legend("topright", sapply(strsplit( pheno.info()$classes, "_" ),tail,1), pch=16,col=envA()$pheno.table.colors[pheno.info()$classes] )
# 
#   par(new=T,mar=c(0,0,1,2))
#   plot(envA()$cn.graph, vertex.size=vertex.size, vertex.label=NA,
#        vertex.color = envA()$pheno.table.colors[ pheno.info()$samples[ V(envA()$cn.graph)$name ] ] )
#   title( main="Sample landscape", line=-1, cex.main=2)
# 
#  }
#  , height = function(){ min( 600, session$clientData$output_p_phenotypeBrowser_correlationNetwork_width ) })
# 






# output$p_phenotypeBrowser_correlationNetwork_hoverbox <- renderText({
# 
#   if(!is.na(hover.id()))
#   {
#     session$sendCustomMessage("element_visible", 
#         message=list(id="#p_phenotypeBrowser_correlationNetwork_hoverbox", state="visible"))
#     
#     return( paste("ID:",hover.id()) )
#     
#   } else
#   {
#     session$sendCustomMessage("element_visible", 
#                               message=list(id="#p_phenotypeBrowser_correlationNetwork_hoverbox", state="hidden"))
#   }
# })


# output$p_phenotypeBrowser_correlationNetwork_clickbox_info <- renderText({
# 
#   if(!is.na(click.id()))
#   {
#     session$sendCustomMessage("element_visible",
#                               message=list(id="#p_phenotypeBrowser_correlationNetwork_clickbox", state="visible"))
#     
#     info.text = paste0( "ID: ", click.id(), "\n", "Class: ", envA()$group.labels[click.id()] )
#     if( "age" %in% names(envA()$patient.data) ) info.text = paste0( info.text, "\nAge: ", envA()$patient.data[click.id(),"age"] )
#     if( "sex" %in% names(envA()$patient.data) ) info.text = paste0( info.text, "\nSex: ", envA()$patient.data[click.id(),"sex"] )
#     return( info.text )
#     
#   } else
#   {
#     session$sendCustomMessage("element_visible",
#                               message=list(id="#p_phenotypeBrowser_correlationNetwork_clickbox", state="hidden"))
#   }
# })


# output$p_phenotypeBrowser_correlationNetwork_clickbox_portrait <- renderPlot({
# 
#   if(!is.na(click.id()))
#   {
#     par(mar=c(0,0,0,0))
#     image(matrix(envA()$metadata[,click.id()],envA()$preferences$dim.1stLvlSom),
#           col=envA()$color.palette.portraits(1000), axes=F )
#     box()
    
#  }
  
#})




# 
# output$info <- renderText({
#   
#   
# })


