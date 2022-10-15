####    Mapas                       ####
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
#### Graficos Barra                 ####
tamanho               <- read.csv2("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/geodata/mundo/rankingArea.csv")
ranking               <- tamanho[,-c(1:2)] %>% as.data.frame()
row.names(ranking)    <- tamanho$Paises

#======== primeira guerra ==========#
Paises                <- c("França", "Alemanha", "Áustria-Hungria", "Grã-Bretanha", "Rússia", "Itália", "EUA", "Colônias Inglesas")
t_m_p                 <- data.frame("Tropas" = c(8, 13, 9, 9, 18, 6, 4, 2))
row.names(t_m_p)      <- Paises

#======== segunda guerra ===========#
pib_lad               <- c("1938", "1939",  "1940.1", "1940.2", "1941.1", "1941.2", "1942", "1943", "1944.1", "1944.2", "1945") 
pib_tot               <- data.frame("Eixo"      = c(544, 595, 606, 835, 911, 911, 902, 895,748,655, 466),
                                    "Aliados"   = c(470, 486, 398, 316,344,703,1906,2224,2457,2550,2394))
row.names(pib_tot)    <- pib_lad

#======== guerra fria ==============#
p                     <- c("Reino Unido", "França", "Itália", "Trieste", "Alemanha", "Paises Baixos", "Bélgica", "Luxemburgo", "Suiça", "Áustria", "Dinamarca", "Noruega", "Grécia", "Suécia", "Portugal", "Turquia", "Irlanda", "Islândia")
rec_tot               <- data.frame("Total Recebido" = c(3297, 2296, 1204,1204, 1448, 1128, 777, 777, 500, 488, 385, 372, 366, 347, 140, 137, 133, 43))  

row.names(rec_tot)    <- p

#======== onu ======================#
pa                    <- c("Estados Unidos", "Japão", "Alemanha", "França", "Reino Unido", "China", "Itália", "Canadá", "Espanha", "Brasil")
onu_con               <- data.frame("Contribuídos" = c(22.000, 8.564, 6.090, 4.427, 4.567, 12.005, 3.307, 2.734, 2.146, 2.948))

row.names(onu_con)    <- pa  

#======== g7 ======================#
pop                   <- c("Alemanha", "Canadá", "Estados Unidos", "França", "Itália", "Japão", "Reino Unido")
g07                   <- data.frame("População"      = c(83237124,	38929902,	332183000, 67842582, 58983122, 125681593, 67025542),
                                    "PIB_anual"      = c(3601750, 1682943, 19441544, 2500870, 1782050, 4172339, 2695503),
                                    "PIB_per_capta"  = c(43290, 44026, 58527, 36660, 30150, 33198, 40216),
                                    "Dívida_total"   = c(2475776, 1885855, 24610805, 2813087, 2677910, 11454265, 2569385))
row.names(g07)        <- pop

#======== g20 =====================#
plp                   <- c("África do Sul", "Alemanha", "Arábia Saudita", "Argentina", "Austrália", "Brasil", "Canadá", "China", "Coreia do Sul", "Estados Unidos", "França", "Índia", "Indonésia", "Itália", "Japão", "México", "Reino Unido", "Rússia", "Turquia")
g20                   <- data.frame("População"      = c(60143000, 83237124,35340680,45808747,25739256,213993441,38929902,1412360000,51744876,332183000,67842582,1393409033,272249000,58983122,125681593,130262220,67025542,145558000,84680273),
                                    "PIB_anual"      = c(353384,3601750,704656,415568,1380745,1360869,1682943,14758612,1520447,19441544,2500870,2571622,1002673,1782050,4172339,1094618,2695503,1501006,689547),
                                    "PIB_per_capta"  = c(5876,43290,19939,9072,53644,6359,44026,10450,29384,58527,36660,1846,3683,30150,33198,8403,40216,10312,8190),
                                    "Dívida_total"   = c(244276,2475776,211548,332994,688738,1254309,1885855,8876399,621430,24610805,2813087,2108054,429427,2677910,11454265,630793,2569385,255488,284046))
row.names(g20)        <- plp

#### Graficos Linha                 ####

#======== primeira guerra ==========#
ano_primeira_guerra     <- c(1914, 1915, 1916, 1917, 1918)
p_p_g                   <- data.frame("Paises" = c(11, 12, 13, 14, 0))
row.names(p_p_g)          <- ano_primeira_guerra

#======== segunda guerra ===========#
pais                    <- c("Alemanha","Áustria","Estados Unidos","França","Itália","Japão","Reino Unido","União Soviética")
p_i_b_s_g               <- data.frame("1938"   = c(351, 24, 800, 186, 141, 169, 284, 359),
                                      "1939"   = c(384, 27, 869, 199, 151, 184, 287, 366),
                                      "1940.1" = c(387, 27, 943, 82, 147, 192, 316, 417),
                                      "1940.2" = c(387, 27, 943, 82, 147, 192, 316, 417),
                                      "1941.1" = c(412, 29, 1094, 130, 144, 196, 344, 359),
                                      "1941.2" = c(412, 29, 1094, 130, 144, 196, 344, 359),
                                      "1942"   = c(417, 27, 1235, 116, 145, 197, 353, 318),
                                      "1943"   = c(426, 28, 1399, 110, 137, 194, 361, 464),
                                      "1944.1" = c(437, 29, 1499, 93, 117, 189, 346, 495),
                                      "1944.2" = c(437, 29, 1499, 93, 117, 189, 346, 495),
                                      "1945"   = c(310, 12, 1474, 101, 92, 144, 331, 396))
row.names(p_i_b_s_g)    <- pais
p_i_b_s_g               <- as.data.frame(t(p_i_b_s_g))

#======== guerra fria ==============#
anos                  <- c("1949","1959","1969","1979","1989","1999","2009","2019","2022")
n_p_a                 <- data.frame("OTAN"              = c(12,14,13,13,14,18,27,28,30),
                                    "Pacto de Varsóvia" = c(7,7,7,7,7,7,0,0,0))
row.names(n_p_a)      <- anos

#======== onu ======================#
on                    <- data.frame("Países membros"    = c(51, 194))
row.names(on)         <- c("1945","2022")

#======== g7-g20-sis ===============#
nulo                  <- data.frame(c(0))

#### Graficos Pizza                 ####
primeira_guerra_parti <- mundo@data$GM1 %>% as.data.frame()
segunda_guerra_parti  <- mundo@data$GM2 %>% as.data.frame()
guerra_fria           <- data.frame(mundo@data$OTAN_195, mundo@data$pacto_1955_1991)
sistemas_politicos    <- mundo@data$SIS_POL %>% as.data.frame()
onu                   <- mundo@data$ONU %>% as.data.frame()
imperios_centrais     <- mundo@data$IMP_CEN %>% as.data.frame()
aliados_primeira      <- mundo@data$LADO %>% as.data.frame()
ali                   <- sort(mundo@data$LADO) %>% as.data.frame()
ali                   <- ali[-c(8:251),] %>% as.data.frame()

for(i in 1 : nrow(onu)){
  if(guerra_fria$mundo.data.OTAN_195[i] == "T"){guerra_fria$Alianca[i] = "OTAN"} else{guerra_fria$Alianca[i] = "Outros países"}
  if(guerra_fria$mundo.data.pacto_1955_1991[i] == "T"){guerra_fria$Alianca[i] = "Pacto"}
  if(onu$.[i] == "T"){onu$.[i] = "Pertencente"}else{onu$.[i] = "Não Pertencente"}
  if(primeira_guerra_parti$.[i] == "T"){primeira_guerra_parti$.[i] = "Participante"}else{primeira_guerra_parti$.[i] = "Não Participante"}
  if(segunda_guerra_parti$.[i] == "T"){segunda_guerra_parti$.[i] = "Participante"}else{segunda_guerra_parti$.[i] = "Não Participante"}
  if(sistemas_politicos$.[i] == "sociali"){sistemas_politicos$.[i] = "Socialismo"}else{sistemas_politicos$.[i] = "Capitalismo"}
  if(imperios_centrais$.[i] == "T"){imperios_centrais$.[i] = "Imperios Centrais"}else{imperios_centrais$.[i] = "Não participante"}
  if(aliados_primeira$.[i] == "Aliados"){aliados_primeira$.[i] = "Aliados"}else{aliados_primeira$.[i]="Outros países"}
}

g_7 <- c(rep("G7", 774), rep("mundo", 7225)) %>% as.data.frame()
g_0 <- c(rep("G20", 4645), rep("Mundo", 3345)) %>% as.data.frame()


####    Server                      ####
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
    renderLineChart(div_id = "graph_line", data = p_p_g, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = t_m_p, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  
  #     TELA IMPERIOS CENTRAIS    ----
  
  observeEvent(input$imp,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Tropas Movidas (Milhões)")
    output$GraficoLin = renderText("Paises Atuantes Na Primeira Guerra Mundial"); output$mapa = renderLeaflet(mapaImp)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/prigue.html"))
    
    renderPieChart(div_id = "graph_pie", data = primeira_guerra_parti$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = p_p_g, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = t_m_p, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA ALIADOS              ----
  
  observeEvent(input$ali,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Tropas Movidas (Milhões)")
    output$GraficoLin = renderText("Paises Atuantes Na Primeira Guerra Mundial"); output$mapa = renderLeaflet(mapaAli)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/prigue.html"))
    
    renderPieChart(div_id = "graph_pie", data = primeira_guerra_parti$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = p_p_g, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = t_m_p, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA SEGUNDA GUERRA       ----
  
  observeEvent(input$gm2,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("PIB total de cada país")
    output$GraficoLin = renderText("PIB durante a Segunda Guerra Mundial"); output$mapa = renderLeaflet(mapaGM2)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/seggue.html"))
    
    renderPieChart(div_id = "graph_pie", data = segunda_guerra_parti$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = p_i_b_s_g, theme = 'vintage', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = pib_tot, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA LADOS SEGUNDA GUERRA ----
  
  observeEvent(input$lsg,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("PIB total de cada país")
    output$GraficoLin = renderText("PIB durante a Segunda Guerra Mundial"); output$mapa = renderLeaflet(mapaLado)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/seggue.html"))
    
    renderPieChart(div_id = "graph_pie", data = ali$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = p_i_b_s_g, theme = 'vintage', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = pib_tot, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  
  observeEvent(input$ali,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("PIB total de cada país")
    output$GraficoLin = renderText("PIB durante a Segunda Guerra Mundial"); output$mapa = renderLeaflet(mapaAli)
    
    renderPieChart(div_id = "graph_pie", data = ali$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = p_i_b_s_g, theme = 'vintage', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = pib_tot, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  #     TELA GUERRA FRIA          ####
  observeEvent(input$gf,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Total Recebido (em milhões)")
    output$GraficoLin = renderText("Quantidade de países membros ao longo dos anos"); output$mapa = renderLeaflet(mapaPaises)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/guefri.html"))
    
    renderPieChart(div_id = "graph_pie", data = guerra_fria$Alianca, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = n_p_a, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = rec_tot, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  #     TELA OTAN                 ----
  
  observeEvent(input$otan,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Total Recebido (em milhões)")
    output$GraficoLin = renderText("Quantidade de países membros ao longo dos anos"); output$mapa = renderLeaflet(mapaOtan22)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/otanre.html"))
    
    renderPieChart(div_id = "graph_pie", data = guerra_fria$Alianca, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = n_p_a, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = rec_tot, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA PACTO VARSOVIA       ----
  
  observeEvent(input$pct,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Total Recebido (em milhões)")
    output$GraficoLin = renderText("Quantidade de países membros ao longo dos anos"); output$mapa = renderLeaflet(mapaPac)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/pactov.html"))
    
    renderPieChart(div_id = "graph_pie", data = guerra_fria$Alianca, theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = n_p_a, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = rec_tot, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA ONU                  ----
  
  observeEvent(input$onu,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Top 10 doações feitas (em %)")
    output$GraficoLin = renderText("Países membros ao longo dos anos"); output$mapa = renderLeaflet(mapaOnu)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/onures.html"))
    
    renderPieChart(div_id = "graph_pie", data = onu$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = on, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = onu_con, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA GRUPO 7              ----
  
  observeEvent(input$gst,{
    output$GraficoPie = renderText("Divisão Global (em milhões)"); output$GraficoBar = renderText("Ranking Países (Moeda em M.£.)")
    output$mapa = renderLeaflet(mapaG7); output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/g07res.html"))
    output$GraficoLin = renderText("Gráfico de linha (sem dados)");
    
    renderPieChart(div_id = "graph_pie", data = g_7$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = nulo, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = g07, theme = 'roma', show.tools=T, show.legend = T, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA GRUPO 20             ----
  
  observeEvent(input$gvt,{
    output$GraficoPie = renderText("Divisão Global (em milhões)"); output$GraficoBar = renderText("Ranking Países (Moeda em M.£.)")
    output$mapa = renderLeaflet(mapaG20); output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/g20res.html"))
    output$GraficoLin = renderText("Gráfico de linha (sem dados)");
    
    renderPieChart(div_id = "graph_pie", data = g_0$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = nulo, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = g20, theme = 'roma', show.tools=T, show.legend = T, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  
  #     TELA SISTEMAS POLITICOS   ----
  
  observeEvent(input$sis,{
    output$GraficoPie = renderText("Divisão Global"); output$GraficoBar = renderText("Gráfico de Barras (sem dados)")
    output$GraficoLin = renderText("Gráfico de Linhas (sem dados)"); output$mapa = renderLeaflet(mapaSis)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/sisres.html"))
    
    renderPieChart(div_id = "graph_pie", data = sistemas_politicos$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = nulo, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = nulo, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
  

  #     TELA REFERENCIAS          ----
  observeEvent(input$ref,{
    output$GraficoPie = renderText("Gráfico de pizza (sem dados)"); output$GraficoBar = renderText("Gráfico de Barras (sem dados)")
    output$GraficoLin = renderText("Gráfico de Linhas (sem dados)"); output$mapa = renderLeaflet(mapaPaises)
    output$texto = renderUI(includeHTML("https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/www/refere.html"))
    
    renderPieChart(div_id = "graph_pie", data = sistemas_politicos$., theme = 'roma', show.tools=F, show.legend = F,  radius = "80%")
    renderLineChart(div_id = "graph_line", data = ranking, theme = 'roma', show.tools=F, show.legend = F)
    renderBarChart(div_id = "graph_bar", data = ranking, theme = 'roma', show.tools=T, show.legend = F, direction="vertical", grid_left = '3%', font.size.legend=15)
  })
}