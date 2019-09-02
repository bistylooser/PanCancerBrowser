

p_phenotypeBrowser.ui <- fluidPage( id="p_phenotypeBrowser", 
  
  fluidRow(
    column( 5,   div( class="side_menu",
                      h4("Select phenotype"),
                      fluidRow(
                        column( 6,
                                selectInput("p_phenotypeBrowser_selectPheno", label = NULL,
                                            choices = c("Age", "Sex", "Histology", "Molecular type"), 
                                            selected = "Age")),
                        column(width = 3, offset = 2, checkboxGroupInput(inputId = "p_phenotypeBrowser_selectdata", 
                                                                    label = NULL, 
                                                                    choices = c("data set 1", "data set 2"), 
                                                                    selected = c("data set 1", "data set 2")))
                        # column(width = 3, offset = 2, checkboxInput(inputId = "p_phenotypeBrowser_selectdataB", 
                        #                                             label = "data set 2", value = TRUE))
                        )
    )
    )),
  fluidRow(
    column(6, offset = 5, h4("Survival curves")),
    column(12, plotlyOutput("p_phenotypeBrowser_survivalCurves")))
)

