
library(shiny)
library(igraph)
library(DT)



selectable.data <- c(
  "Lymphoma (873 tumor samples, MMML consortium, LHA id 7WEWFE12CK-4)" = "data/browser_May17_MMML914.RData",
  "Melanoma (80 tumor & nevi samples, Kunz et al., LHA id 7R4PDEM4HG-2)" = "data/browser_Dec16_MelanomaKunz.RData",
  "Glioma, low grade (137 tumor samples, GGN consortium, LHA id 7Q0CFRJKW4-7)" = "data/browser_Sep16_LGGlioma GGN.RData",
  "Pneumonia (180 samples + 10 controls, Burnham et al., LHA id 7RU79AQTJD-9)" = "data/browser_Dec17_BurnhamCAP.RData"
)







source("pages/p_overview.ui.r")
source("pages/p_geneBrowser.ui.r")
source("pages/p_genesetBrowser.ui.r")
source("pages/p_moduleBrowser.ui.r")
source("pages/p_psfBrowser.ui.r")
source("pages/p_phenotypeBrowser.ui.r")

ui <- shinyUI(fluidPage( theme="style.css", title="oposSOM Browser", id="topFrame",
                         
  fluidRow(
    id="dataset_select_panel",
    
    selectInput("dataset_select", label = "Selected data set", selectable.data )
  ),
  # verbatimTextOutput("info",placeholder=T),
  
  div( id="content",
    tabsetPanel(  tabPanel("Overview", p_overview.ui ),
                  tabPanel("Gene browser", p_geneBrowser.ui ),
                  tabPanel("Geneset browser", p_genesetBrowser.ui ),
                  tabPanel("Module browser", p_moduleBrowser.ui ),
                  tabPanel("Phenotype browser", p_phenotypeBrowser.ui ),
                  tabPanel("Pathway signal flow", p_psfBrowser.ui ),
                  id="main_menu" #, selected = "Phenotype browser" 
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
  
  load("data/kegg.collection.RData")
  
  env <- reactive({

    updateTabsetPanel(session, "main_menu", selected = "Overview" )
    session$sendCustomMessage("handler_datasetLoad_start", message=list(""))
    withProgress(message = 'Loading data', value = 1, style='old', { load(input$dataset_select) } )
    session$sendCustomMessage("handler_datasetLoad_finish", message=list(""))
    
    return(env) 
  })

  source("pages/p_overview.server.r", local=TRUE)
  source("pages/p_geneBrowser.server.r", local=TRUE)
  source("pages/p_genesetBrowser.server.r", local=TRUE)
  source("pages/p_moduleBrowser.server.r", local=TRUE)
  source("pages/p_psfBrowser.server.r", local=TRUE)
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

