####  bibliotecas ####
library("shiny")
library("ECharts2Shiny")
library("shinyWidgets")
library("shinythemes")
library("shinydashboard")
library("leaflet")
library('rgdal')



####    Mapas     ####
mundo      <- readOGR('C:/Users/Elvins Moraes/Desktop/edu/Projeto - Geopolitica/geodata/mundo/mundoGeopolitico.shp')
continente <- readOGR('C:/Users/Elvins Moraes/Desktop/edu/Projeto - Geopolitica/geodata/continentes/Continents.shp')

mapaPaises <- leaflet(mundo) %>% addTiles()
mapaContin <- leaflet(continente) %>% addTiles()

mapaOnu    <- leaflet(mundo) %>% 
  addTiles() %>%
  addPolygons(color = corOnu,
              label = mundo@data$SOVEREIGN)
mapaOnu


####    ui        ####
temas <- c('tema1', 'tema2', 'tema3', 'tema4')
ui <- dashboardPage(skin = 'purple',
                    dashboardHeader(
                      titleWidth = 1
                      ),
                    dashboardSidebar(
                      tags$p(),
                      selectInput("selectEstado", label = "Temas", choices = temas),
                      uiOutput("ui"),
                      #Documentacao: https://shiny.rstudio.com/articles/selectCidade-ui.html
                      
                      tags$div(class = "alinhamento")
                    ),
                    dashboardBody(
                      
                      loadEChartsLibrary(),
                      loadEChartsTheme('vintage'),
                      loadEChartsTheme('roma'),
                      loadEChartsTheme('macarons'),
                      loadEChartsTheme('jazz'),
                      
                     
                      
                      fluidRow(
                        column(5,
                               leafletOutput(outputId = "mapa")
                        ),
                        column(7,
                               tags$div(id="graph_pie", style="width:100%;height:200px;"),  # Specify the div for the chart.
                               deliverChart(div_id = "graph_pie"),  # Deliver the plotting
                               
                               tags$div(id="graph_bar", style="width:100%;height:300px;"),
                               deliverChart(div_id = "graph_bar")
                        )
                      ),
                      
                      fluidRow(
                               tags$div(id="graph_line", style="width:100%;height:300px;"),
                               deliverChart(div_id = "graph_line")
                      ),
                      
                    ),
                      
)
  
  



####    server    ####
server <- function(input, output,  session){
  
  observeEvent(input$selectEstado, {
    if(input$selectEstado == "tema1"){
      tema <- 'vintage'
    } else
      if(input$selectEstado == "tema2"){
        tema <- 'macarons'
      } else
        if(input$selectEstado == "tema3"){
          tema <- 'roma'
        } else
          if(input$selectEstado == "tema4"){
            tema <- 'jazz'
          }
    
   
  
  renderPieChart(div_id = "graph_pie", data = dat_1, theme = tema, radius = "90%")
  
  renderLineChart(div_id = "graph_line", theme = tema, data = dat_2)
  
  renderBarChart(div_id = "graph_bar", grid_left = '3%', data = dat_2, font.size.legend=15, show.tools=T, show.legend = T)
  
  output$mapa = renderLeaflet(mapaOnu)
  
  
    
  })
}
####    fim       ####
shinyApp(ui, server)