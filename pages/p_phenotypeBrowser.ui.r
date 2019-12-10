

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
                                                                    choiceNames = list(
                                                                      tags$span("data set 1", style = "color: cornflowerblue;"),
                                                                      tags$span("data set 2", style = "color: gold;")),
                                                                    choiceValues = c("data set 1", "data set 2"),
                                                                    selected = c("data set 1", "data set 2")))
                        )
    )
    )),
  fluidRow(
    column(6, offset = 5, h4("Survival curves")),
    column(12, plotlyOutput("p_phenotypeBrowser_survivalCurves")))
)

