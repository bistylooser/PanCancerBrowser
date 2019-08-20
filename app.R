
library(shiny)
library(igraph)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)


selectable.data <- c(
  "Lymphoma (873 tumor samples, MMML consortium, LHA id 7WEWFE12CK-4)" = "data/browser_May17_MMML914.RData",
  "Melanoma (80 tumor & nevi samples, Kunz et al., LHA id 7R4PDEM4HG-2)" = "data/browser_Dec16_MelanomaKunz.RData",
  "Glioma, low grade (137 tumor samples, GGN consortium, LHA id 7Q0CFRJKW4-7)" = "data/browser_Sep16_LGGlioma GGN.RData",
  "Pneumonia (180 samples + 10 controls, Burnham et al., LHA id 7RU79AQTJD-9)" = "data/browser_Dec17_BurnhamCAP.RData"
)

info.age <- c(
  "data/browser_May17_MMML914.RData" = "Age",
  "data/browser_Dec16_MelanomaKunz.RData" = "Age",
  "data/browser_Sep16_LGGlioma GGN.RData" = "Age",
  "data/browser_Dec17_BurnhamCAP.RData" = "Age"
)

info.sex <- c(
  "data/browser_May17_MMML914.RData" = "Gender",
  "data/browser_Dec16_MelanomaKunz.RData" = NULL,
  "data/browser_Sep16_LGGlioma GGN.RData" = "Sex",
  "data/browser_Dec17_BurnhamCAP.RData" = "Sex"
)

info.histology <- c(
  "data/browser_May17_MMML914.RData" = "Histology",
  "data/browser_Dec16_MelanomaKunz.RData" = "Melanoma Type",
  "data/browser_Sep16_LGGlioma GGN.RData" = "Histology",
  "data/browser_Dec17_BurnhamCAP.RData" = "Group"
)

info.molecular <- c(
  "data/browser_May17_MMML914.RData" = "PAT type",
  "data/browser_Dec16_MelanomaKunz.RData" = "Type",
  "data/browser_Sep16_LGGlioma GGN.RData" = "Genomic group",
  "data/browser_Dec17_BurnhamCAP.RData" = "Reclassification"
)



source("pages/p_overview.ui.r")
#source("pages/p_geneBrowser.ui.r")
#source("pages/p_genesetBrowser.ui.r")
#source("pages/p_moduleBrowser.ui.r")
#source("pages/p_psfBrowser.ui.r")
source("pages/p_phenotypeBrowser.ui.r")

ui <- shinyUI(fluidPage( theme="style.css", title="PanCancer Browser", id="topFrame",
                         
  fluidRow(
    class ="dataset_select_panel",
    selectInput("dataset_select", label = "Select data set 1", selectable.data )
  ),
  fluidRow(
    class ="dataset_select_panel",
    selectInput("dataset_selectB", label = "Select data set 2", selectable.data, selected = selectable.data[2] )
  ),
  tags$hr(),
  #verbatimTextOutput(outputId = "shiny_variable"),
  
  div( id="content",
    tabsetPanel(  #tabPanel("Overview", p_overview.ui ),
                  #tabPanel("Gene browser", p_geneBrowser.ui ),
                  #tabPanel("Geneset browser", p_genesetBrowser.ui ),
                  #tabPanel("Module browser", p_moduleBrowser.ui ),
                  tabPanel("Survival browser", p_phenotypeBrowser.ui ),
                  #tabPanel("Pathway signal flow", p_psfBrowser.ui ),
                  id="main_menu" #, selected = "Survival browser" 
    )
  ),
  
  tags$script('
    Shiny.addCustomMessageHandler("handler_datasetLoad_start", hide_main_menu );
    function hide_main_menu(message)
    {
      $("#main_menu").css({ visibility: "hidden" });
      $("#content").css({ visibility: "hidden" });
    };    
    Shiny.addCustomMessageHandler("handler_datasetLoad_finish", show_main_menu );
    function show_main_menu(message)
    {
      $("#main_menu").css({ visibility: "visible" });
      $("#content").css({ visibility: "visible" });
    };

    Shiny.addCustomMessageHandler("element_visible", element_visible );
    function element_visible(message)
    {
      $(message.id).css({
        visibility: message.state
      });
    };
  ')

))




server <- function(input, output, session) {
  
  #load("data/kegg.collection.RData")
  
  # browser...Rdata ist als objekt env gespeichert, diese Daten laden und zurückgeben return(env) in variable envA
  envA <- reactive({
    #load(input$dataset_select)
    updateTabsetPanel(session, "main_menu", selected = "Overview" )
    session$sendCustomMessage("handler_datasetLoad_start", message=list(""))
    withProgress(message = 'Loading data', value = 1, style='old', { load(input$dataset_select) } )
    session$sendCustomMessage("handler_datasetLoad_finish", message=list(""))
    
   return(env) 
  })
    
  envB <- reactive({
    #load(input$dataset_selectB)
    updateTabsetPanel(session, "main_menu", selected = "Overview" )
    session$sendCustomMessage("handler_datasetLoad_start", message=list(""))
    withProgress(message = 'Loading data', value = 1, style='old', { load(input$dataset_selectB) } )
    session$sendCustomMessage("handler_datasetLoad_finish", message=list(""))
      
    return(env) 
  })
  
  # output$shiny_variable <- renderPrint(paste( 
  #   envA()$preferences$system.info["user"], 
  #   input$dataset_select,
  #   envB()$preferences$system.info["user"],
  #   input$dataset_selectB))
  #output$shiny_variable <- renderPrint(pheno.info()$classes)
  

  source("pages/p_overview.server.r", local=TRUE)
  #source("pages/p_geneBrowser.server.r", local=TRUE)
  #source("pages/p_genesetBrowser.server.r", local=TRUE)
  #source("pages/p_moduleBrowser.server.r", local=TRUE)
  #source("pages/p_psfBrowser.server.r", local=TRUE)
  source("pages/p_phenotypeBrowser.server.r", local=TRUE)
  
 
  
  # output$info <- renderText({
  #   dummy <- input$dataset_select
  #   env$preferences$system.info["user"]
  # })
  
  
  
  
  
    # observe({
    #   query <- parseQueryString(session$clientData$url_search)
    #   
    #     nameval = names(reactiveValuesToList(input)[i])
    #     valuetoupdate = query[[nameval]]
    #  
    #   
    # })
  }
  

# Run the application 
shinyApp(ui = ui, server = server, options = c("port"=5555) )

