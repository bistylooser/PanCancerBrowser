library(shiny)
library(igraph)
library(DT)
library(plotly)
library(ggplot2)
library(shinyWidgets)



# Infos Data selection
selectable.data <- c(
  "Lymphoma (873 tumor samples, MMML consortium, LHA id 7WEWFE12CK-4)" = "data/browser_May17_MMML914.RData",
  "Melanoma (80 tumor & nevi samples, Kunz et al., LHA id 7R4PDEM4HG-2)" = "data/browser_Dec16_MelanomaKunz.RData",
  "Glioma, low grade (137 tumor samples, GGN consortium, LHA id 7Q0CFRJKW4-7)" = "data/browser_Sep16_LGGlioma GGN.RData",
  "Pneumonia (180 samples + 10 controls, Burnham et al., LHA id 7RU79AQTJD-9)" = "data/browser_Dec17_BurnhamCAP.RData"
)


# Infos Overview
info.links <- data.frame(Name = names(selectable.data), 
                         Publication = c("Loeffler-Wirth et al., 2019", 
                                         "Kunz et al., 2018", 
                                         "Weller et al., 2015", 
                                         "Hopp et al., 2018"),
                         Url = c("https://www.health-atlas.de/publications/243", 
                                 "https://www.health-atlas.de/publications/74", 
                                 "https://www.health-atlas.de/publications/64", 
                                 "https://www.health-atlas.de/publications/223"),
                         row.names = names(selectable.data))


# Infos Survival browser
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
source("pages/p_geneBrowser.ui.r")
source("pages/p_genesetBrowser.ui.r")
source("pages/p_moduleBrowser.ui.r")
source("pages/p_phenotypeBrowser.ui.r")

ui <- shinyUI(
  fluidPage( theme="style.css", title="PanCancer Browser", id="topFrame", 
             useSweetAlert("minimal"),  # appearance infobox "sweetalert2", "minimal", "dark", "bootstrap-4", "borderless"
                         
  fluidRow(class ="dataset_select_panel",
    selectInput("dataset_select", label = "Select data set 1", selectable.data)
  ),
  fluidRow(
    column(11, 
    class ="dataset_select_panel",
    selectInput("dataset_selectB", label = "Select data set 2", selectable.data, selected = selectable.data[2])),
    column(1, circleButton(inputId = "dataset_select_info", icon = "?", status = "#000056af", size = "sm"))
  ),
  tags$hr(),
  
  div( id="content",
    tabsetPanel(  tabPanel("Overview", p_overview.ui ),
                  tabPanel("Gene browser", p_geneBrowser.ui ),
                  tabPanel("Function browser", p_genesetBrowser.ui ),
                  tabPanel("Module browser", p_moduleBrowser.ui ),
                  tabPanel("Survival browser", p_phenotypeBrowser.ui ),
                  id="main_menu" , selected = "Overview" 
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
  
  # Rdata is stored as object env, load this data and return(env) in variable envA
  envA <- reactive({
    #load(input$dataset_select)
    updateTabsetPanel(session, "main_menu", selected = "" )
    session$sendCustomMessage("handler_datasetLoad_start", message=list(""))
    withProgress(message = 'Loading data', value = 1, style='old', { load(input$dataset_select) } )
    session$sendCustomMessage("handler_datasetLoad_finish", message=list(""))
    
   return(env) 
  })
    
  envB <- reactive({
    #load(input$dataset_selectB)
    updateTabsetPanel(session, "main_menu", selected = "" )
    session$sendCustomMessage("handler_datasetLoad_start", message=list(""))
    withProgress(message = 'Loading data', value = 1, style='old', { load(input$dataset_selectB) } )
    session$sendCustomMessage("handler_datasetLoad_finish", message=list(""))
      
    return(env) 
  })
  

 source("pages/p_overview.server.r", local=TRUE)
 source("pages/p_geneBrowser.server.r", local=TRUE)
 source("pages/p_genesetBrowser.server.r", local=TRUE)
 source("pages/p_moduleBrowser.server.r", local=TRUE)
 source("pages/p_phenotypeBrowser.server.r", local=TRUE)
  
  observeEvent(input$dataset_select_info, {
    sendSweetAlert(
      session = session,
      title = "PanCancer Browser",
      text = h6(tags$span(
        tags$a("Interdisciplinary Centre for Bioinformatics, Leipzig University", 
               href= "http://www.izbi.uni-leipzig.de/", 
               target="_blank"),
        tags$br(),
        tags$p("Version 1.0, 09.12.19"), 
        tags$br(),
        tags$br(),
        "The PanCancer Browser is a web application for the interactive comparison of 
        molecular landscapes of different cancers provided by the ", 
        tags$a("oposSOM analysis pipeline.", 
               href= "https://www.health-atlas.de/models/5", 
               target="_blank"),
        "The application is publication-centered and provides an interactive exploration of the corresponding data from clinical trials.",
        tags$br(),
        tags$br(),
        "Recommended form of citation: ",
        tags$br(),
        "Looser V, Loeffler-Wirth H, Binder H (2019). PanCancer Browser. Version 1.0. Interdisciplinary Centre for Bioinformatics, Leipzig University,",
        tags$a("www.health-atlas.de", 
               href= "https://www.health-atlas.de/", 
               target="_blank"),
        tags$br(),
        tags$br(),
        "The web application is licensed under the terms of use defined for the ",
        tags$a("Leipzig Health Atlas.", 
               href= "https://www.health-atlas.de/home/about", 
               target="_blank")
        )),
      html = TRUE,
      btn_labels = "Close",
      btn_colors = "#C0C0C0",
      width = "600px")
  })
}
  

# Run the application 
shinyApp(ui = ui, server = server, options = c("port"=5555) )

