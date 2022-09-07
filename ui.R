####    Dashboard ####
temas <- c('tema1', 'tema2', 'tema3', 'tema4')
skin <- 'red'
gm <- c("Guerras & Conflitos", "1ª Guerra Mundial" = 1, "2ª Guerra Mundial" = 2, "Guerra Fria" = 3)

ui <- dashboardPage(skin = skin,
                    title = "GEOPMÍDIA",
                    
     # HEADER     -------------------------------------------------------
                    dashboardHeader(
                      title = "GEOMÍDIA",
                      titleWidth = 300
                    ),
     # SIDEBAR    -------------------------------------------------------               
                    dashboardSidebar(
                      width = 300,
                      tags$div(class = "alinhamento"),
                      
                      sidebarMenu(
                        div(id = "sidebar_button",
                            bsButton(inputId = "inicio", label = "INICIO", icon = icon("play-circle"), style = "danger")
                        )
                      ),
                      br(),
                      # Guerra             -----
                      menuItem("GUERRA", tabName = "antimicrobials", icon = icon("fire"),
                        menuItem("1ª GUERRA MUNDIAL \n (1914-1918)",
                                actionButton(inputId = "gm1", label = "A Grande Guerra", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;"),
                                actionButton(inputId = "imp", label = "Impérios Centrais", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;"),
                                actionButton(inputId = "ali", label = "Aliados", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;")
                        ),
                        menuItem("2ª GUERRA MUNDIAL (1939-1945)",
                                actionButton(inputId = "gm2", label = "A Segunda Guerra Mundial", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;"),
                                actionButton(inputId = "lsg", label = "Lados da Segunda Guerra", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;")
                        ),
                        menuItem("GUERRA FRIA (1945-1991)",
                                actionButton(inputId = "GF", label = "O que foi", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;"),
                                actionButton(inputId = "otan", label = "OTAN", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;"),
                                actionButton(inputId = "pct", label = "Pacto de Varsóvia", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;")
                        )
                      ),
                      br(),
                      # Organização        -----
                      menuItem("ORGANIZÇÕES", tabName = "organizacoes", icon = icon("cog", lib = "glyphicon"),
                               actionButton(inputId = "onu", label = "ONU", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;"),
                               actionButton(inputId = "gst", label = "G7", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;"),
                               actionButton(inputId = "gvt", label = "G20", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;")
                      ),
                      br(),
                      # Sistemas Políticos ----
                      menuItem("SISTEMAS POLÍTICOS", tabName = "aliancas", icon = icon("money"),
                               actionButton(inputId = "sis", label = "Sistemas Políticos", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;")
                      ),
                      br()
                    ),
     # BODY       -------------------------------------------------------
                    dashboardBody(
                      tags$head(
                        tags$link(
                          rel = "stylesheet", 
                          type = "text/css", 
                          href = "style.css")
                      ),
                      
                      useShinyjs(),
                      
                      loadEChartsLibrary(),
                      loadEChartsTheme('vintage'),
                      loadEChartsTheme('roma'),
                      loadEChartsTheme('macarons'),
                      loadEChartsTheme('jazz'),
                      
                      fluidRow(
                        leafletOutput(outputId = "mapa")
                      ),
                      
                      
                      fluidRow(
                        column(4,
                               textOutput(outputId = "GraficoPie"),
                               tags$div(id="graph_pie", style="width:100%;height:200px;"),  # Specify the div for the chart.
                               deliverChart(div_id = "graph_pie"),
                        ),
                        column(4,
                               # Deliver the plotting
                               
                               textOutput(outputId = "GraficoBar"),
                               tags$div(id="graph_bar", style="width:100%;height:200px;"),
                               deliverChart(div_id = "graph_bar")
                        ),
                        column(4,
                               textOutput(outputId = "GraficoLin"),
                               tags$div(id="graph_line", style="width:100%;height:200px;"),
                               deliverChart(div_id = "graph_line")
                        )
                      ),
                      
                      
                      fluidRow(
                        htmlOutput(outputId = "texto")
                      )
                    )         
)