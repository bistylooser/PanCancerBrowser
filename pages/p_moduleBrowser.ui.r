


p_moduleBrowser.ui <- fluidPage( id="p_moduleBrowser",
                                 fluidRow(
                                    #Main Panel
                                   column(6, align = "center", h4("Number of common genes"), h5("for expression modules of different data sets"),
                                        fluidRow(column(12, plotlyOutput("p_moduleBrowser_plotframe")))),
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
                                 fluidRow((column(12, style = "color: white", h4("margin")))),
                                 fluidRow(column(12, 
                                      tabsetPanel(id="p_moduleBrowser_tabsetpanel",
                                      tabPanel("Common genes", dataTableOutput("p_moduleBrowser_geneTable") ), 
                                      tabPanel("Gene functions / based on data set 1", dataTableOutput("p_moduleBrowser_functionTable_data1")),
                                      tabPanel("Gene functions / based on data set 2", dataTableOutput("p_moduleBrowser_functionTable_data2"))))),
                                 fluidRow((column(12, style = "color: white", h4("margin"))))
                                 )
                                                   
