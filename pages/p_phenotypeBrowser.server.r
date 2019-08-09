
plot.kaplan.meier.curve <- function(fu.time,event.state,col,caption)
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
  
  lines( t.stariway, S.stairway, type="l", col=col, lwd=3 )	
  points( fu.time[ which( event.state == 0 ) ], S[ which( event.state == 0 )+1 ], pch=3, col=col )
  
  text( tail(t.stariway,1)+0.1, tail(S.stairway,1), caption, cex=0.9, adj=0 )
  
}


get.id <- function(x,y)
{
  id <- NA
  if( x > -1.095 && x < 1.095 && y > -1.095 && y < 1.095 )
  {
    x.i <- 1+ round( ( x+1.1 )*100 )
    y.i <- 1+ round( ( y+1.1 )*100 )
    id <- env()$cn.graph.nodehover[ x.i, y.i ]
  }
  return( id )
}

hover.id <- reactive({
  if( is.null(input$p_phenotypeBrowser_correlationNetwork_hover) ) return(NA)
  x <- input$p_phenotypeBrowser_correlationNetwork_hover$x
  y <- input$p_phenotypeBrowser_correlationNetwork_hover$y
  return( get.id(x,y) )
})


click.id <- reactive({
  if( is.null(input$p_phenotypeBrowser_correlationNetwork_click) ) return(NA)
  x <- input$p_phenotypeBrowser_correlationNetwork_click$x
  y <- input$p_phenotypeBrowser_correlationNetwork_click$y
  return( get.id(x,y) )
})


output$p_phenotypeBrowser_sideMenu <- renderUI(
  div( class="side_menu",
    h4("Select phenotype"),
    fluidRow(
      column( 6,
        selectInput("p_phenotypeBrowser_selectPheno", label = NULL,
                     choices = unique( sapply( strsplit( colnames(env()$pheno.table), "_" ), head, 1 ) ),
                     selected = 1)
      ),
      column( 6,
        selectInput("p_phenotypeBrowser_selectPheno2", label = NULL, choices = "", multiple = TRUE )
      )
    )
  )
)


observeEvent(input$p_phenotypeBrowser_selectPheno, { 
  
  choices <- grep( paste0("^",input$p_phenotypeBrowser_selectPheno,"_"), colnames(env()$pheno.table), value=T )
  choices <- sapply( strsplit( choices, "_" ), tail, 1 )
  choices <- c( "all", choices )
  updateSelectInput( session, "p_phenotypeBrowser_selectPheno2", choices = choices, selected = "all" )

})

observeEvent(input$p_phenotypeBrowser_selectPheno2, { 
  
  if( tail( input$p_phenotypeBrowser_selectPheno2, 1 ) == "all" )
  {
    updateSelectInput( session, "p_phenotypeBrowser_selectPheno2", selected = "all" )
  }
  else if( "all" %in% input$p_phenotypeBrowser_selectPheno2 &&
            length(input$p_phenotypeBrowser_selectPheno2) > 1 )
  {
    selected <- setdiff( input$p_phenotypeBrowser_selectPheno2, "all" )
    updateSelectInput( session, "p_phenotypeBrowser_selectPheno2", selected = selected )
  }

})





pheno.info <- reactive({

  # catch call when selectPheno is still empty
  if( is.null(input$p_phenotypeBrowser_selectPheno) ) return( NULL ) 

  # catch call when selectPheno2 is empty and needs to be rendered
  if( is.null(input$p_phenotypeBrowser_selectPheno2)  )
  {
    updateSelectInput( session, "p_phenotypeBrowser_selectPheno2", selected = "all" )
    return(NULL)
  }

  # catch race conditions when 'all' is picked and selectPheno2 needs update
  if( length(input$p_phenotypeBrowser_selectPheno2) > 1 &&
      "all" %in% input$p_phenotypeBrowser_selectPheno2 ) return( NULL )

  # catch race conditions when dataset is switched and choices are outdated
  if( !input$p_phenotypeBrowser_selectPheno %in%    
      unique( sapply( strsplit( colnames(env()$pheno.table), "_" ), head, 1 ) ) ) return( NULL )
  
  
  classes <- grep( paste0("^",input$p_phenotypeBrowser_selectPheno,"_"), colnames(env()$pheno.table), value=T )

  if( !"all" %in% input$p_phenotypeBrowser_selectPheno2 ) 
  {

    if( !paste( input$p_phenotypeBrowser_selectPheno, 
                input$p_phenotypeBrowser_selectPheno2[1], sep="_") %in% classes ) return(NULL)
    
    classes <- paste( input$p_phenotypeBrowser_selectPheno, 
                 input$p_phenotypeBrowser_selectPheno2, sep="_")
  }
  
  samples <- apply( env()$pheno.table[,classes,drop=FALSE], 1, function(x)
  {
    x = unlist(x)
    if(any(!is.na(x))&&any(x)) classes[which(x)] else ""
  })
  
  return( list( classes=classes, samples=samples ) )
})

output$p_phenotypeBrowser_survivalCurves <- renderPlot({

  if( is.null(pheno.info()) || is.null(env()$survival.data) ) return()

  par(mar=c(5,4,4,0))
  plot( 0, type="n", xlim=c(0,ceiling(max(env()$survival.data[1,]))), ylim=c(0,1), las=1, xlab="Years", ylab="Probability (OS)", cex.axis=1, cex.lab=1)
  title( main="Survival curves", cex.main=2)
  for( gr.i in pheno.info()$classes )
  {
    gr.samples <- intersect( names(which(pheno.info()$samples==gr.i)), colnames(env()$survival.data) )
    fu.time <- env()$survival.data[1,gr.samples]
    event.state <- env()$survival.data[2,gr.samples]

    event.state = event.state[order( fu.time )]
    fu.time = fu.time[order( fu.time )]

    if(length(fu.time)>0)
      plot.kaplan.meier.curve( fu.time, event.state, env()$pheno.table.colors[gr.i], strsplit( gr.i, "_" )[[1]][2] )
  }
})


output$p_phenotypeBrowser_correlationNetwork <- renderPlot({
  
  if( is.null(pheno.info()) ) return()

  vertex.size <- 10
  if(vcount(env()$cn.graph)>100) vertex.size <- 8
  if(vcount(env()$cn.graph)>500) vertex.size <- 6

  par(mar=c(0,0,1,0))
  frame()
  legend("topright", sapply(strsplit( pheno.info()$classes, "_" ),tail,1), pch=16,col=env()$pheno.table.colors[pheno.info()$classes] )

  par(new=T,mar=c(0,0,1,2))
  plot(env()$cn.graph, vertex.size=vertex.size, vertex.label=NA,
       vertex.color = env()$pheno.table.colors[ pheno.info()$samples[ V(env()$cn.graph)$name ] ] )
  title( main="Sample landscape", line=-1, cex.main=2)

 }
 , height = function(){ min( 600, session$clientData$output_p_phenotypeBrowser_correlationNetwork_width ) })







output$p_phenotypeBrowser_correlationNetwork_hoverbox <- renderText({

  if(!is.na(hover.id()))
  {
    session$sendCustomMessage("element_visible", 
        message=list(id="#p_phenotypeBrowser_correlationNetwork_hoverbox", state="visible"))
    
    return( paste("ID:",hover.id()) )
    
  } else
  {
    session$sendCustomMessage("element_visible", 
                              message=list(id="#p_phenotypeBrowser_correlationNetwork_hoverbox", state="hidden"))
  }
})


output$p_phenotypeBrowser_correlationNetwork_clickbox_info <- renderText({

  if(!is.na(click.id()))
  {
    session$sendCustomMessage("element_visible",
                              message=list(id="#p_phenotypeBrowser_correlationNetwork_clickbox", state="visible"))
    
    info.text = paste0( "ID: ", click.id(), "\n", "Class: ", env()$group.labels[click.id()] )
    if( "age" %in% names(env()$patient.data) ) info.text = paste0( info.text, "\nAge: ", env()$patient.data[click.id(),"age"] )
    if( "sex" %in% names(env()$patient.data) ) info.text = paste0( info.text, "\nSex: ", env()$patient.data[click.id(),"sex"] )
    return( info.text )
    
  } else
  {
    session$sendCustomMessage("element_visible",
                              message=list(id="#p_phenotypeBrowser_correlationNetwork_clickbox", state="hidden"))
  }
})


output$p_phenotypeBrowser_correlationNetwork_clickbox_portrait <- renderPlot({

  if(!is.na(click.id()))
  {
    par(mar=c(0,0,0,0))
    image(matrix(env()$metadata[,click.id()],env()$preferences$dim.1stLvlSom),
          col=env()$color.palette.portraits(1000), axes=F )
    box()
    
  }
  
})





output$info <- renderText({
  
  
})


