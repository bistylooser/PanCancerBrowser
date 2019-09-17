

p_geneBrowser.ui <- fluidPage( id="p_geneBrowser",

  dataTableOutput("p_geneBrowser_geneTable"),

  # div( id="p_geneBrowser_checkbox_div", class="side_menu",
  #   checkboxInput("p_geneBrowser_checkbox", label="class boxplots", value=FALSE )
  # ),
  fluidRow(
    column(6, h4("Data set 1:")),
    column(6, h4("Data set 2:"))
  ),
  
  fluidRow(#id="p_geneBrowser_checkbox_div", class="side_menu",
    column(6, 
           checkboxInput("p_geneBrowser_checkboxA", label="class boxplots", value=FALSE )),
    column(6, checkboxInput("p_geneBrowser_checkboxB", label="class boxplots", value=FALSE ))
  ),

  fluidRow( id="p_geneBrowser_geneInfos",
    column( 6,
      plotlyOutput("p_geneBrowser_geneProfileA")
    ),
    column( 6,
      plotlyOutput("p_geneBrowser_geneProfileB")
    )
  )

)



