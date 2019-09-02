

p_genesetBrowser.ui <- fluidPage( id="p_genesetBrowser",
  
  dataTableOutput("p_genesetBrowser_genesetTable"),

  # div( id="p_genesetBrowser_checkbox_div", class="side_menu",
  #   checkboxInput("p_genesetBrowser_checkbox", label="class boxplots", value=FALSE )
  # ),
  fluidRow(#id="p_genesetBrowser_checkbox_div", class="side_menu",
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



