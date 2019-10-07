


p_moduleBrowser.ui <- fluidPage( id="p_moduleBrowser",
                                 #tags$head(tags$script(src = "message-handler.js")),
                                 # fluidRow(column(12, 
                                 #                 tabsetPanel(id="p_moduleBrowser_tabsetpanel",
                                 #                             tabPanel("Common genes", dataTableOutput("p_moduleBrowser_genetable_clickbox") ), # tags$head(tags$style("max-height: 90vh; overflow-y: auto;")) ),
                                 #                             tabPanel("Function")))), #dataTableOutput("p_moduleBrowser_functiontable_clickbox") )),
                                 fluidRow(
                                    #Main Panel
                                   column(6, align = "center", h4("Number of common genes"), h5("for expression modules of different data sets"),
                                        fluidRow(column(12, plotlyOutput("p_moduleBrowser_plotframe")))),
                                        #fluidRow(column(12, align = "left", actionButton("p_moduleBrowser_genetable_button", label= "Get gene table"))), 
                                        #fluidRow(column(12, align = "left", actionButton("p_moduleBrowser_functiontable_button", label = "Get function table")))),
                                   #Side Panel
                                   column(3, align = "center",h4("Data set 1:"),
                                          fluidRow(column(12, verbatimTextOutput(outputId = "clickA"))), 
                                          fluidRow(column(12, align = "center", checkboxInput("p_moduleBrowser_checkbox_maskA", label="Enable/Disable mask", value=TRUE))),
                                          fluidRow(plotlyOutput("p_moduleBrowser_side_GR_A", height = 200, width = 200)),
                                          fluidRow(checkboxInput("p_moduleBrowser_checkbox_boxA", label="class boxplots", value=FALSE )),
                                          fluidRow(plotlyOutput("p_moduleBrowser_bottomframeA", height = 250))),
                                   column(3,align = "center", h4("Data set 2:"), verbatimTextOutput(outputId = "clickB"),
                                          fluidRow(checkboxInput("p_moduleBrowser_checkbox_maskB", label="Enable/Disable mask", value=TRUE)),
                                          fluidRow(plotlyOutput("p_moduleBrowser_side_GR_B", height = 200, width = 200)),
                                          fluidRow(checkboxInput("p_moduleBrowser_checkbox_boxB", label="class boxplots", value=FALSE )),
                                          fluidRow(plotlyOutput("p_moduleBrowser_bottomframeB", height = 250)))
                                 ),
                                   #div( id="p_moduleBrowser_clickbox",
                                        #actionButton("p_moduleBrowser_genetable_clickbox_close","close"),
                                        #actionButton("p_psfBrowser_clickbox_goto","go to pathway"),
                                        #h4( textOutput("p_psfBrowser_clickbox_info") ),
                                        #dataTableOutput("p_moduleBrowser_genetable_clickbox")
                                 fluidRow((column(12, style = "color: white", h4("margin")))),
                                 fluidRow(column(12, 
                                      tabsetPanel(id="p_moduleBrowser_tabsetpanel",
                                      tabPanel("Common genes", dataTableOutput("p_moduleBrowser_geneTable") ), # tags$head(tags$style("max-height: 90vh; overflow-y: auto;")) ),
                                      tabPanel("Gene functions / based on data set 1", dataTableOutput("p_moduleBrowser_functionTable_data1")),
                                      tabPanel("Gene functions / based on data set 2", dataTableOutput("p_moduleBrowser_functionTable_data2"))))),
                                 fluidRow((column(12, style = "color: white", h4("margin"))))#dataTableOutput("p_moduleBrowser_functiontable_clickbox") ))
                                 )
                                                   
                                                   
                                                  
                                   
                                   
                                   # column(7,
                                   #        fluidRow(
                                   #          column(12, 
                                   #             fluidRow(h4("Data set 1:"),
                                   #               column(5, checkboxInput("p_moduleBrowser_checkbox_maskA", label="Enable/Disable mask", value=TRUE )),
                                   #               column(7, checkboxInput("p_moduleBrowser_checkbox_boxA", label="class boxplots", value=FALSE ))),
                                   #             fluidRow(
                                   #               column(5, plotOutput("p_moduleBrowser_side_GR_A", height = 150, width = 150)),
                                   #               column(7, plotlyOutput("p_moduleBrowser_bottomframeA", height = 250))
                                   #            ))),
                                   #              
                                   #        fluidRow(
                                   #          column(12, 
                                   #                 fluidRow(h4("Data set 2:"),
                                   #                   column(5, checkboxInput("p_moduleBrowser_checkbox_maskB", label="Enable/Disable mask", value=TRUE )),
                                   #                   column(7, checkboxInput("p_moduleBrowser_checkbox_boxB", label="class boxplots", value=FALSE ))),
                                   #                 fluidRow(
                                   #                   column(5, plotOutput("p_moduleBrowser_side_GR_B", height = 150, width = 150)),
                                   #                   column(7, plotlyOutput("p_moduleBrowser_bottomframeB", height = 250))
                                   #        )))
                                   #        )))
                                   # 
                                
  # uiOutput("p_moduleBrowser_hoverbox"),
   
  #Navigator
  #fluidRow(
    # column(3,
    #        fluidRow(
    #          column(6, plotOutput("p_moduleBrowser_side_OE", height = "40%", click = clickOpts("p_moduleBrowser_side_OE_click", clip = TRUE))),
    #          column(6, plotOutput("p_moduleBrowser_side_GR", height = "40%", click = clickOpts("p_moduleBrowser_side_GR_click", clip = TRUE)))),
    #        fluidRow(
    #          column(6, plotOutput("p_moduleBrowser_side_UE", height = "40%", click = clickOpts("p_moduleBrowser_side_UE_click", clip = TRUE))),
    #          column(6, plotOutput("p_moduleBrowser_side_DM", height = "40%", click = clickOpts("p_moduleBrowser_side_DM_click", clip = TRUE)))),
    #        fluidRow(
    #          column(6, plotOutput("p_moduleBrowser_side_KM", height = "40%", click = clickOpts("p_moduleBrowser_side_KM_click", clip = TRUE))),
    #          column(6, plotOutput("p_moduleBrowser_side_CO", height = "40%", click = clickOpts("p_moduleBrowser_side_CO_click", clip = TRUE)))),
    #        fluidRow(
    #          column(6, plotOutput("p_moduleBrowser_side_AG", height = "40%", click = clickOpts("p_moduleBrowser_side_AG_click", clip = TRUE))),
    #          column(6, plotOutput("p_moduleBrowser_side_SX", height = "40%", click = clickOpts("p_moduleBrowser_side_SX_click", clip = TRUE)))),
    #        fluidRow(
    #          column(6, plotOutput("p_moduleBrowser_side_HR", height = "40%", click = clickOpts("p_moduleBrowser_side_HR_click", clip = TRUE))))
    # ),
            
   

                      #, height = "60%", click = "p_moduleBrowser_plotframe_click", hover = hoverOpts(id = "p_moduleBrowser_plotframe_hover", delay = 1, delayType ="throttle")),
           # 
           # div( id="p_moduleBrowser_checkbox_div",
           #   checkboxInput("p_moduleBrowser_checkbox", "Enable/Disable mask", TRUE))
           
           #plotOutput("p_moduleBrowser_bottomframe", height = "80%")
    
#     #Side Panel
#     column(7,
#       # div(id="p_moduleBrowser_clickText","For more details click a spot on the map."),
#       fluidRow(
#         column(12, h4("Data set 1:"), height = "50%",
#                fluidRow(#id="p_geneBrowser_checkbox_div", class="side_menu",
#                  column(5, checkboxInput("p_moduleBrowser_checkbox_maskA", label="Enable/Disable mask", value=TRUE )),
#                  column(7, checkboxInput("p_moduleBrowser_checkbox_boxA", label="class boxplots", value=FALSE ))),
#                fluidRow(
#                  column(5, plotOutput("p_moduleBrowser_side_GR_A")),
#                  column(7, plotlyOutput("p_moduleBrowser_bottomframeA"))))),
#       
#       fluidRow(
#         column(12, h4("Data set 2:"), height = "50%",
#                fluidRow(#id="p_geneBrowser_checkbox_div", class="side_menu",
#                  column(5, checkboxInput("p_moduleBrowser_checkbox_maskB", label="Enable/Disable mask", value=TRUE )),
#                  column(7, checkboxInput("p_moduleBrowser_checkbox_boxB", label="class boxplots", value=FALSE ))),
#                fluidRow(
#                  column(5, plotOutput("p_moduleBrowser_side_GR_B")),
#                  column(7, plotlyOutput("p_moduleBrowser_bottomframeB"))))) 
#       
#       # fluidRow(
#       #   column(12, verbatimTextOutput("click"))))
#     )
#   )
# )
    
    
      # tabsetPanel(id="p_moduleBrowser_tabsetpanel",
      #   tabPanel("Genes", dataTableOutput("p_moduleBrowser_tabGenes") ), # tags$head(tags$style("max-height: 90vh; overflow-y: auto;")) ),
      #   tabPanel("Function", dataTableOutput("p_moduleBrowser_tabFunc") ),
      #   tabPanel("Classes", dataTableOutput("p_moduleBrowser_tabClass") )
      # )


    # tags$script('
    #        $(document).ready(function()
    #        {
    #          $("#p_moduleBrowser_plotframe").mousemove(function(e)
    #          {
    #            $("#p_moduleBrowser_hoverbox").css({
    #              top: (e.pageY + 5) + "px",
    #              left: (e.pageX + 5) + "px"
    #            });
    #          });
    #        });
    #    ')

