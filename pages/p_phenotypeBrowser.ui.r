

p_phenotypeBrowser.ui <- fluidPage( id="p_phenotypeBrowser", 
  
  fluidRow(
    column( 5,   div( class="side_menu",
                      h4("Select phenotype"),
                      fluidRow(
                        column( 6,
                                selectInput("p_phenotypeBrowser_selectPheno", label = NULL,
                                            #choices = unique( sapply( strsplit( colnames(envA()$pheno.table), "_" ), head, 1 ) ),
                                            choices = c("Age", "Sex", "Histology", "Molecular type"), 
                                            selected = "Age")
                        )
                      )
    )
    )),
  fluidRow(
    column(12, plotOutput("p_phenotypeBrowser_survivalCurves")))
)



#    column( 7, plotOutput("p_phenotypeBrowser_correlationNetwork",
#                     hover=hoverOpts(id = "p_phenotypeBrowser_correlationNetwork_hover", delay = 0, delayType = "throttle" ),
#                     click = "p_phenotypeBrowser_correlationNetwork_click" )
#                      
#    ),
#    
#    uiOutput("p_phenotypeBrowser_correlationNetwork_hoverbox"),
#    absolutePanel( id="p_phenotypeBrowser_correlationNetwork_clickbox", draggable=FALSE,
#                    verbatimTextOutput("p_phenotypeBrowser_correlationNetwork_clickbox_info",placeholder=T),
#                    plotOutput("p_phenotypeBrowser_correlationNetwork_clickbox_portrait", width=100, height=100 )
#
#    ),

    
    ### hover / click script ####
#    tags$script('
#              $(document).ready(function()
#              { 
#                $("#p_phenotypeBrowser_correlationNetwork").mousemove(function(e)
#                {
#                  $("#p_phenotypeBrowser_correlationNetwork_hoverbox").css({
#                    top: (e.pageY + 5) + "px",
#                    left: (e.pageX + 5) + "px"
#                  });
#                });
#
#                $("#p_phenotypeBrowser_correlationNetwork").click(function(e)
#                {
#                  $("#p_phenotypeBrowser_correlationNetwork_clickbox").css({
#                    top: e.pageY + "px",
#                    left: e.pageX + "px"
#                  });
#                });
#              });     ')
        
#  )
#)

