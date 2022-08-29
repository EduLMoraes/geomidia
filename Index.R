####  Bibliotecas ####
library("shiny")
library("ECharts2Shiny")
library("shinyWidgets")
library("shinythemes")
library("shinydashboard")
library("leaflet")
library("rgdal")
library("tidyverse")



####    Mapas     ####
mundo      <- readOGR('C:/Users/Elvins Moraes/Desktop/edu/Projeto - Geopolitica/geodata/mundo/mundoGeopolitico.shp')
continente <- readOGR('C:/Users/Elvins Moraes/Desktop/edu/Projeto - Geopolitica/geodata/continentes/Continents.shp')

## MAPAS GENÉRICOS ##
mapaPaises <- leaflet(mundo) %>% setView(lat=0, lng=0, zoom = 2) %>% addTiles(group = "Satellite", options = tileOptions(minZoom = 2, maxZoom = 10, maxNativeZoom = 5), urlTemplate = "http://mt0.google.com/vt/lyrs=s&hl=en&x={x}&y={y}&z={z}&s=Ga")
mapaContin <- leaflet(continente) %>% setView(lat=0, lng=0, zoom = 2) %>% addTiles(group = "Satellite", options = tileOptions(minZoom = 2, maxZoom = 10, maxNativeZoom = 5), urlTemplate = "http://mt0.google.com/vt/lyrs=s&hl=en&x={x}&y={y}&z={z}&s=Ga")


#       MAPA GM1    #
mapaGM1    <- mapaPaises %>%
  addPolygons(color = mundo@data$corGM1, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA GM2    #
mapaGM2    <- mapaPaises %>%
  addPolygons(color = mundo@data$corGM2, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA G7     #
mapaG7    <- mapaPaises %>%
  addPolygons(color = mundo@data$corG7, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA G20
mapaG20    <- mapaPaises %>%
  addPolygons(color = mundo@data$corG20, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA ONU    #
mapaOnu    <- mapaPaises %>%
  addPolygons(color = mundo@data$corOnu, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA OTAN   #
mapaOtan49    <- mapaPaises %>%
  addPolygons(color = mundo@data$cOTAN_4, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)
mapaOtan22    <- mapaPaises %>%
  addPolygons(color = mundo@data$cOTAN_2, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA PACTO  #
mapaPac    <- mapaPaises %>%
  addPolygons(color = mundo@data$corPact, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA IMPERIO#
mapaImp    <- mapaPaises %>%
  addPolygons(color = mundo@data$corIMP, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA SISTEMA#
mapaSis   <- mapaPaises %>%
  addPolygons(color = mundo@data$corSis, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA LADO   #
mapaLado    <- mapaPaises %>%
  addPolygons(color = mundo@data$corLado, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA ALIANÇA#
mapaAli    <- mapaPaises %>%
  addPolygons(color = mundo@data$corTRI, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)


####    Graficos  ####
tamanho <- data.frame(mundo@data$CNTRY_N, mundo@data$SQKM)
names(tamanho) <- c("Paises", "SQKM")
a <- sort(tamanho$SQKM, decreasing = T)
a <- a[-c(11:251)]
b <- tamanho$Paises[c(167, 35, 229, 40, 28, 11, 95, 105, 8, 190)]
tamanho <- data.frame(b, a)
tamanho1 <- unlist(tamanho) %>%
  matrix(ncol = 2) %>% 
  as_tibble() %>% 
  setNames(c("Paises", "KM²"))
row.names(tamanho1) <- tamanho$b
tamanho1 <- tamanho1[,-1]
tamanho1 <- as.data.frame(tamanho1)
row.names(tamanho1) <- tamanho$b


####    Dashboard ####
temas <- c('tema1', 'tema2', 'tema3', 'tema4')
skin <- 'red'

db <- dashboardPage(skin = skin,
                    
  dashboardHeader(
      title = "GEOPOLÍTICA"
  ),
                    
  dashboardSidebar(
      tags$div(class = "alinhamento"),
      
      actionButton(inputId = "inicio", label = "Início", style="width:88%; background-color:black; color:white;"),
      actionButton(inputId = "1gm", label = "1ª Guerra Mundial", style="width:88%; background-color:black; color:white; margin-left:15px;"),
      actionButton(inputId = "2gm", label = "2ª Guerra Mundial", style="width:88%; background-color:black; color:white; margin-left:15px; margin-top:5px;")
  ),
                    
  dashboardBody(
    loadEChartsLibrary(),
    loadEChartsTheme('vintage'),
    loadEChartsTheme('roma'),
    loadEChartsTheme('macarons'),
    loadEChartsTheme('jazz'),
    
    
    
    fluidRow(
      column(3,
             textOutput(outputId = "GraficoPie"),
             tags$div(id="graph_pie", style="width:100%;height:200px;"),  # Specify the div for the chart.
             deliverChart(div_id = "graph_pie"),
      ),
      column(5,
             # Deliver the plotting
             
             textOutput(outputId = "GraficoBar"),
             tags$div(id="graph_bar", style="width:100%;height:200px;"),
             deliverChart(div_id = "graph_bar")
      ),
      column(4,
             textOutput(outputId = "GraficoLin"),
             tags$div(id="graph_line", style="width:100%;height:200px;"),
             deliverChart(div_id = "graph_line"))
    )
  )
)
  

####    Ui        ####

ui <- fluidPage(
  fluidRow(
    leafletOutput(outputId = "mapa")
  ),
  fluidRow(
    db
  )
)


####    server    ####


server <- function(input, output,  session){
  
  
  output$GraficoPie = renderText("Divisão Global")
  output$GraficoBar = renderText("Ranking Países")
  output$GraficoLin = renderText("Linha Temporal")
  output$mapa = renderLeaflet(mapaGM1)
  
  renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
  renderLineChart(div_id = "graph_line", data = tamanho1, theme = 'roma', show.tools=F, show.legend = F)
  renderBarChart(div_id = "graph_bar", data = tamanho1, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  
  observeEvent(input$inicio,{
    output$GraficoPie = renderText("Divisão Global")
    output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal")
    output$mapa = renderLeaflet(mapaPaises)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = tamanho1, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = tamanho1, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  
}
####    fim       ####
shinyApp(ui, server)