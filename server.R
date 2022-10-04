####    Mapas     ####
{
  library("leaflet")
  library("rgdal")
  library("tidyverse")
  library("rsconnect")
  library("geojson")
  library("tidyr")
  library("stringr")
  library("dplyr")
  library("unvotes")
  library("lubridate")
  library("sf")
source("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/DataTratament.R")

##      MAPAS GENÉRICOS ----
mapaPaises <- leaflet(mundo) %>% setView(lat=0, lng=0, zoom = 2) %>% addTiles(options = tileOptions(minZoom = 2, maxZoom = 10, maxNativeZoom = 5), urlTemplate = "http://mt0.google.com/vt/lyrs=s&hl=en&x={x}&y={y}&z={z}&s=Ga")
mapaContin <- leaflet(continente) %>% setView(lat=0, lng=0, zoom = 2) %>% addTiles(options = tileOptions(minZoom = 2, maxZoom = 10, maxNativeZoom = 5), urlTemplate = "http://mt0.google.com/vt/lyrs=s&hl=en&x={x}&y={y}&z={z}&s=Ga") %>% addPolygons(color = "black", label = continente@data$PLACENAME, weight = 1, opacity = 0.5)


#       MAPA GM1        ----
mapaGM1    <- mapaPaises %>%
  addPolygons(color = mundo@data$corGM1, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA GM2        ----
mapaGM2    <- mapaPaises %>%
  addPolygons(color = mundo@data$corGM2, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA G7         ----
mapaG7    <- mapaPaises %>%
  addPolygons(color = mundo@data$corG7, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA G20        ----
mapaG20    <- mapaPaises %>%
  addPolygons(color = mundo@data$corG20, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA ONU        ----
mapaOnu    <- mapaPaises %>%
  addPolygons(color = mundo@data$corOnu, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA OTAN       ----
mapaOtan49    <- mapaPaises %>%
  addPolygons(color = mundo@data$corOTAN_49, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)
mapaOtan22    <- mapaPaises %>%
  addPolygons(color = mundo@data$corOTAN_22, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA PACTO      ----
mapaPac    <- mapaPaises %>%
  addPolygons(color = mundo@data$corPact, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA IMPERIO    ----
mapaImp    <- mapaPaises %>%
  addPolygons(color = mundo@data$corIMP, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA SISTEMA    ----
mapaSis   <- mapaPaises %>%
  addPolygons(color = mundo@data$corSis, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA LADO       ----
mapaLado    <- mapaPaises %>%
  addPolygons(color = mundo@data$corLado, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)

#       MAPA ALIANÇA    ----
mapaAli    <- mapaPaises %>%
  addPolygons(color = mundo@data$corTRI, label = mundo@data$CNTRY_N, weight = 1, opacity = 1)


}
####    Graficos  ####
{
  #     GRAFICO PIZZA     ----
      primeira_guerra_parti <- mundo@data$GM1 %>% as.data.frame()
      segunda_guerra_parti  <- mundo@data$GM2 %>% as.data.frame()
      guerra_fria           <- data.frame(mundo@data$OTAN_195, mundo@data$pacto_1955_1991)
      grupo_sete            <- mundo@data$G7 %>% as.data.frame()
      grupo_vinte           <- mundo@data$G20 %>% as.data.frame()
      sistemas_politicos    <- mundo@data$SIS_POL %>% as.data.frame()
      onu                   <- mundo@data$ONU %>% as.data.frame()
      imperios_centrais     <- mundo@data$IMP_CEN %>% as.data.frame()
      aliados_primeira      <- mundo@data$LADO %>% as.data.frame()
      
      for(i in 1 : nrow(grupo_sete)){
        if(grupo_sete$.[i] == "T"){grupo_sete$.[i] = "Pertencente"}else{grupo_sete$.[i] = "Não pertencente"}
        if(grupo_vinte$.[i] == "T"){grupo_vinte$.[i] = "Pertencente"}else{grupo_vinte$.[i] = "Não pertencente"}
        if(guerra_fria$mundo.data.OTAN_195[i] == "T"){guerra_fria$Alianca[i] = "OTAN"} else{guerra_fria$Alianca[i] = "Outras Alianças"}
          if(guerra_fria$mundo.data.pacto_1955_1991
             [i] == "T"){guerra_fria$Alianca[i] = "Pacto"}
        if(onu$.[i] == "T"){onu$.[i] = "Pertencente"}else{onu$.[i] = "Não Pertencente"}
        if(primeira_guerra_parti$.[i] == "T"){primeira_guerra_parti$.[i] = "Participante"}else{primeira_guerra_parti$.[i] = "Não Participante"}
        if(segunda_guerra_parti$.[i] == "T"){segunda_guerra_parti$.[i] = "Participante"}else{segunda_guerra_parti$.[i] = "Não Participou"}
        if(sistemas_politicos$.[i] == "sociali"){sistemas_politicos$.[i] = "Socialismo"}else{sistemas_politicos$.[i] = "Capitalismo"}
        if(imperios_centrais$.[i] == "T"){imperios_centrais$.[i] = "Imperios Centrais"}else{imperios_centrais$.[i] = "Outras Alianças"}
        if(aliados_primeira$.[i] == "Aliados"){aliados_primeira$.[i] = "Aliados"}else{aliados_primeira$.[i] = "Outras Alianças"}
      }
      
  #     GRAFICO BARRA     ----
      tamanho               <- read.csv2("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/geodata/mundo/rankingArea.csv")
      ranking               <- tamanho[,-c(1:2)] %>% as.data.frame()
      row.names(ranking)    <- tamanho$Paises
      
      Paises                <- c("França", "Alemanha", "Áustria-Hungria", "Grã-Bretanha", "Rússia", "Itália", "EUA", "Colônias Inglesas")
      primeira_guerra_barra <- data.frame("Tropas" = c(8, 13, 9, 9, 18, 6, 4, 2))
      row.names(primeira_guerra_barra) <- Paises
      
      
  #     GRAFICO LINHA     ----  
      ano_primeira_guerra     <- c(1914, 1915, 1916, 1917, 1918)
      entrada_primeira_guerra <- data.frame("Paises" = c(11, 12, 13, 14, 0))
      row.names(entrada_primeira_guerra) <- ano_primeira_guerra
      
      
}
####    Server    ####
server <- function(input, output,  session){
  #     TELA INICIAL              ----
  
  output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking dos Países por Território")
  output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaContin)
  output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/geopol.html"))
  
  renderPieChart(div_id = "graph_pie", data = sistemas_politicos$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
  renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
  renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
    
  
  observeEvent(input$inicio,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking dos Países por Território")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaContin)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/geopol.html"))
    
    renderPieChart(div_id = "graph_pie", data = sistemas_politicos$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA PRIMEIRA GUERRA      ----
  
  observeEvent(input$gm1,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Tropas Movidas (Milhões)")
    output$GraficoLin = renderText("Paises Atuantes Na Primeira Guerra Mundial"); output$mapa = renderLeaflet(mapaGM1)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/prigue.html"))
    
    renderPieChart(div_id = "graph_pie", data = primeira_guerra_parti$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = entrada_primeira_guerra, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = primeira_guerra_barra, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  
  #     TELA IMPERIOS CENTRAIS    ----
  
  observeEvent(input$imp,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Tropas Movidas (Milhões)")
    output$GraficoLin = renderText("Paises Atuantes Na Primeira Guerra Mundial"); output$mapa = renderLeaflet(mapaImp)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/prigue.html"))
    
    renderPieChart(div_id = "graph_pie", data = imperios_centrais$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = entrada_primeira_guerra, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = primeira_guerra_barra, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA ALIADOS              ----
  
  observeEvent(input$ali,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Tropas Movidas (Milhões)")
    output$GraficoLin = renderText("Paises Atuantes Na Primeira Guerra Mundial"); output$mapa = renderLeaflet(mapaAli)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/prigue.html"))
    
    renderPieChart(div_id = "graph_pie", data = aliados_primeira$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = entrada_primeira_guerra, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = primeira_guerra_barra, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA SEGUNDA GUERRA       ----
  
  observeEvent(input$gm2,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaGM2)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/seggue.html"))
    
    renderPieChart(div_id = "graph_pie", data = segunda_guerra_parti$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA LADOS SEGUNDA GUERRA ----
  
  observeEvent(input$lsg,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaLado)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/seggue.html"))
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$LADO, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  
  observeEvent(input$ali,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaAli)
    
    renderPieChart(div_id = "graph_pie", data = mundo@data$LADO, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  #     TELA GUERRA FRIA          ####
  observeEvent(input$gf,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaPaises)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/guefri.html"))
    
    renderPieChart(div_id = "graph_pie", data = guerra_fria$Alianca, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  #     TELA OTAN                 ----
  
  observeEvent(input$otan,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaOtan22)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/otanre.html"))
    
    renderPieChart(div_id = "graph_pie", data = guerra_fria$Alianca, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA PACTO VARSOVIA       ----
  
  observeEvent(input$pct,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaPac)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/pactov.html"))
    
    renderPieChart(div_id = "graph_pie", data = guerra_fria$Alianca, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA ONU                  ----
  
  observeEvent(input$onu,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaOnu)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/onures.html"))
    
    renderPieChart(div_id = "graph_pie", data = onu$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA GRUPO 7              ----
  
  observeEvent(input$gst,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaG7)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/g07res.html"))
    
    renderPieChart(div_id = "graph_pie", data = grupo_sete$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA GRUPO 20             ----
  
  observeEvent(input$gvt,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaG20)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/base.html"))
    
    renderPieChart(div_id = "graph_pie", data = grupo_vinte$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA SISTEMAS POLITICOS   ----
  
  observeEvent(input$sis,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Ranking Países")
    output$GraficoLin = renderText("Linha Temporal"); output$mapa = renderLeaflet(mapaSis)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/base.html"))
    
    renderPieChart(div_id = "graph_pie", data = sistemas_politicos$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  

}