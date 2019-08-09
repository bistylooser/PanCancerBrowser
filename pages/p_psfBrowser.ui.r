 

p_psfBrowser.ui <- fluidPage(

  fluidRow( id="p_psfBrowser",

    uiOutput("p_psfBrowser_menu_div"),
    
    plotOutput("p_psfBrowser_plotframe",
                hover = hoverOpts(id = "p_psfBrowser_plotframe_hover", delay = 100, delayType = "throttle" ),
                click = "p_psfBrowser_plotframe_click"
    ),
    plotOutput("p_psfBrowser_hoverbox", width = 120, height = 120 ),
    div( id="p_psfBrowser_clickbox",
        actionButton("p_psfBrowser_clickbox_close","close"),
        actionButton("p_psfBrowser_clickbox_goto","go to pathway"),
        h4( textOutput("p_psfBrowser_clickbox_info") ),
        plotOutput("p_psfBrowser_clickbox_profile")
    ),

    tags$script('
              $(document).on("shiny:connected", function(e) {
                Shiny.onInputChange("contentWidth", window.innerWidth - 2*30 );
                Shiny.onInputChange("contentHeight", window.innerHeight - 115 - 70 );
              });
              $(window).resize(function(e) {
                Shiny.onInputChange("contentWidth", window.innerWidth - 2*30 );
                Shiny.onInputChange("contentHeight", window.innerHeight - 115 - 70 ) ;
              });

              $(document).ready(function()
              {
                $("#p_psfBrowser_plotframe").mousemove(function(e)
                {
                  $("#p_psfBrowser_hoverbox").css({
                    top: (e.pageY + 5) + "px",
                    left: (e.pageX + 5) + "px"
                  });
                });

              });     ')

  )



)


