


p_moduleBrowser.ui <- fluidPage( id="p_moduleBrowser",
  
   uiOutput("p_moduleBrowser_hoverbox"),
   
  #Navigator
  fluidRow(
    column(3,
           fluidRow(
             column(6, plotOutput("p_moduleBrowser_side_OE", height = "40%", click = clickOpts("p_moduleBrowser_side_OE_click", clip = TRUE))),
             column(6, plotOutput("p_moduleBrowser_side_GR", height = "40%", click = clickOpts("p_moduleBrowser_side_GR_click", clip = TRUE)))),
           fluidRow(
             column(6, plotOutput("p_moduleBrowser_side_UE", height = "40%", click = clickOpts("p_moduleBrowser_side_UE_click", clip = TRUE))),
             column(6, plotOutput("p_moduleBrowser_side_DM", height = "40%", click = clickOpts("p_moduleBrowser_side_DM_click", clip = TRUE)))),
           fluidRow(
             column(6, plotOutput("p_moduleBrowser_side_KM", height = "40%", click = clickOpts("p_moduleBrowser_side_KM_click", clip = TRUE))),
             column(6, plotOutput("p_moduleBrowser_side_CO", height = "40%", click = clickOpts("p_moduleBrowser_side_CO_click", clip = TRUE)))),
           fluidRow(
             column(6, plotOutput("p_moduleBrowser_side_AG", height = "40%", click = clickOpts("p_moduleBrowser_side_AG_click", clip = TRUE))),
             column(6, plotOutput("p_moduleBrowser_side_SX", height = "40%", click = clickOpts("p_moduleBrowser_side_SX_click", clip = TRUE)))),
           fluidRow(
             column(6, plotOutput("p_moduleBrowser_side_HR", height = "40%", click = clickOpts("p_moduleBrowser_side_HR_click", clip = TRUE))))
    ),
            
    #Main Panel
    column(5, align = "center",
           plotOutput("p_moduleBrowser_plotframe", height = "60%", click = "p_moduleBrowser_plotframe_click", hover = hoverOpts(id = "p_moduleBrowser_plotframe_hover", delay = 1, delayType ="throttle")),
           
           div( id="p_moduleBrowser_checkbox_div",
             checkboxInput("p_moduleBrowser_checkbox", "Enable/Disable mask", TRUE)
           ),
           
           plotOutput("p_moduleBrowser_bottomframe", height = "80%")
    ),
    
    #Genelist
    column(4,
      # div(id="p_moduleBrowser_clickText","For more details click a spot on the map."),
      tabsetPanel(id="p_moduleBrowser_tabsetpanel",
        tabPanel("Genes", dataTableOutput("p_moduleBrowser_tabGenes") ), # tags$head(tags$style("max-height: 90vh; overflow-y: auto;")) ),
        tabPanel("Function", dataTableOutput("p_moduleBrowser_tabFunc") ),
        tabPanel("Classes", dataTableOutput("p_moduleBrowser_tabClass") )
      )
    ),

    tags$script('
           $(document).ready(function()
           {
             $("#p_moduleBrowser_plotframe").mousemove(function(e)
             {
               $("#p_moduleBrowser_hoverbox").css({
                 top: (e.pageY + 5) + "px",
                 left: (e.pageX + 5) + "px"
               });
             });
           });
       ')
    
  )
)
