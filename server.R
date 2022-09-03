####    Mapas     ####
mundo      <- readOGR('C:/Users/Elvins Moraes/Desktop/edu/geomidia/geodata/mundo/mundoGeopolitico.shp')
continente <- readOGR('C:/Users/Elvins Moraes/Desktop/edu/geomidia/geodata/continentes/Continents.shp')

## MAPAS GENÉRICOS ##
mapaPaises <- leaflet(mundo) %>% setView(lat=0, lng=0, zoom = 2) %>% addTiles(options = tileOptions(minZoom = 2, maxZoom = 10, maxNativeZoom = 5), urlTemplate = "http://mt0.google.com/vt/lyrs=s&hl=en&x={x}&y={y}&z={z}&s=Ga")
mapaContin <- leaflet(continente) %>% setView(lat=0, lng=0, zoom = 2) %>% addTiles(options = tileOptions(minZoom = 2, maxZoom = 10, maxNativeZoom = 5), urlTemplate = "http://mt0.google.com/vt/lyrs=s&hl=en&x={x}&y={y}&z={z}&s=Ga") %>% addPolygons(color = "black", label = continente@data$PLACENAME, weight = 1, opacity = 0.5)


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
tamanho <- read.csv2("C:/Users/Elvins Moraes/Desktop/edu/geomidia/geodata/mundo/rankingArea.csv")
ranking <- tamanho[,-c(1:2)]
ranking <- as.data.frame(ranking)
row.names(ranking) <- tamanho$Paises


####    Server    ####
server <- function(input, output,  session){
  
  
  output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking dos Países por Território")
  output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaContin)
  output$texto = renderUI(includeHTML("geopol.html"))
  
  renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
  renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
  renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
    
  
  observeEvent(input$inicio,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking dos Países por Território")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaContin)
    output$texto = renderUI(includeHTML("geopol.html"))
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  
  observeEvent(input$gm1,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaGM1)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$gm2,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaGM2)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  

  
  observeEvent(input$pgm,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaGM1)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$sgm,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaGM2)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$otan,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaOtan22)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$onu,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaOnu)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$pct,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaPac)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$imp,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaImp)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$gst,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaG7)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$gvt,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaG20)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$sis,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaSis)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$lsg,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaLado)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  observeEvent(input$ali,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaAli)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$SIS_POL, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
}