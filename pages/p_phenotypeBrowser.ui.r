

p_phenotypeBrowser.ui <- fluidPage( id="p_phenotypeBrowser", 
  
  fluidRow(
    
    column( 5,
      uiOutput( "p_phenotypeBrowser_sideMenu" ),
      plotOutput("p_phenotypeBrowser_survivalCurves")
      # verbatimTextOutput("info",placeholder=T)
      
    ),

    column( 7, plotOutput("p_phenotypeBrowser_correlationNetwork",
                     hover=hoverOpts(id = "p_phenotypeBrowser_correlationNetwork_hover", delay = 0, delayType = "throttle" ),
                     click = "p_phenotypeBrowser_correlationNetwork_click" )
                      
    ),
    
    uiOutput("p_phenotypeBrowser_correlationNetwork_hoverbox"),
    absolutePanel( id="p_phenotypeBrowser_correlationNetwork_clickbox", draggable=FALSE,
                    verbatimTextOutput("p_phenotypeBrowser_correlationNetwork_clickbox_info",placeholder=T),
                    plotOutput("p_phenotypeBrowser_correlationNetwork_clickbox_portrait", width=100, height=100 )

    ),

    
    ### hover / click script ####
    tags$script('
              $(document).ready(function()
              { 
                $("#p_phenotypeBrowser_correlationNetwork").mousemove(function(e)
                {
                  $("#p_phenotypeBrowser_correlationNetwork_hoverbox").css({
                    top: (e.pageY + 5) + "px",
                    left: (e.pageX + 5) + "px"
                  });
                });

                $("#p_phenotypeBrowser_correlationNetwork").click(function(e)
                {
                  $("#p_phenotypeBrowser_correlationNetwork_clickbox").css({
                    top: e.pageY + "px",
                    left: e.pageX + "px"
                  });
                });
              });     ')
        
  )
)

