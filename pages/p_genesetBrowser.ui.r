

p_genesetBrowser.ui <- fluidPage( id="p_genesetBrowser",

  dataTableOutput("p_genesetBrowser_genesetTable"),

  div( id="p_genesetBrowser_checkbox_div", class="side_menu",
    checkboxInput("p_genesetBrowser_checkbox", label="class boxplots", value=FALSE )
  ),

  fluidRow( id="p_genesetBrowser_geneInfos",
    column( 8,
      plotOutput("p_genesetBrowser_geneProfile")
    ),
    column( 4,
      plotOutput("p_genesetBrowser_geneMapping")
    )
  )

)



