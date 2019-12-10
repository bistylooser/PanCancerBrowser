

p_genesetBrowser.ui <- fluidPage( id="p_genesetBrowser",
  
  dataTableOutput("p_genesetBrowser_genesetTable"),

  fluidRow(
    column(6, h4("Data set 1:")),
    column(6, h4("Data set 2:"))
  ),
  fluidRow(
    column(6, 
           checkboxInput("p_genesetBrowser_checkboxA", label="class boxplots", value=FALSE )),
    column(6, checkboxInput("p_genesetBrowser_checkboxB", label="class boxplots", value=FALSE ))
  ),
  
  fluidRow( id="p_genesetBrowser_geneInfos",
            column( 6,
                    plotlyOutput("p_genesetBrowser_geneProfileA")
            ),
            column( 6,
                    plotlyOutput("p_genesetBrowser_geneProfileB")
    )
  )

)
