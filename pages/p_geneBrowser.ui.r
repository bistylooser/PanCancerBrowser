

p_geneBrowser.ui <- fluidPage( id="p_geneBrowser",

  dataTableOutput("p_geneBrowser_geneTable"),

  div( id="p_geneBrowser_checkbox_div", class="side_menu",
    checkboxInput("p_geneBrowser_checkbox", label="class boxplots", value=FALSE )
  ),

  fluidRow( id="p_geneBrowser_geneInfos",
    column( 8,
      plotOutput("p_geneBrowser_geneProfile")
    ),
    column( 4,
      plotOutput("p_geneBrowser_geneMapping")
    )
  )

)



