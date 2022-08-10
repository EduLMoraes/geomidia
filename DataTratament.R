library('rgdal')

####    dados     ####

# Poligonos dos mapas
mundo <- readOGR('C:/Users/Elvins Moraes/Desktop/edu/Projeto - Geopolitica/geodata/mundo/mundo.shp')
continente <- readOGR('C:/Users/Elvins Moraes/Desktop/edu/Projeto - Geopolitica/geodata/continentes/Continents.shp')

# Cores
cor = c()
nrow(continente@data)

for(i in 1 : nrow(continente@data)){
  
  if(continente@data$JUNK_ID[i] == 1){
    cor[i] = "green"
  }else if(continente@data$JUNK_ID[i] == 2){
    cor[i] = "red"
  }else if(continente@data$JUNK_ID[i] == 3){
    cor[i] = "purple"
  }else if(continente@data$JUNK_ID[i] == 4){
    cor[i] = "yellow"
  }else if(continente@data$JUNK_ID[i] == 5){
    cor[i] = "blue"
  }else if(continente@data$JUNK_ID[i] == 6){
    cor[i] = "grey"
  }else if(continente@data$JUNK_ID[i] == 7){
    cor[i] = "black"
  }else if(continente@data$JUNK_ID[i] == 8){
    cor[i] = "orange"
  }
}

# Organizações 
otan <- c("eua","canada","reino unido", "franca","alemanha","portugal", espanha, italia, paises baixos, belgica, repu tcheca, eslovaquia, ungria, polonia)
pac_varsovia <- c(alemanha orient, urss, lest europeu, cuba)
onu <- c(-republi extrem)
g7 <- c(reino unido, franca, russia, canada, italia, alemanha, japao)
g20 <- c()
unicef <- c(onu)
unesco <- c(onu)
oms <- c(onu)

# Acontecimentos
gf <- c(1945-1991)
guerra1 <- c(14, 18, alemanha, italia, franca, reino unido, eua, europa, russia)
guerra2 <- c(39, 45, )
aliados <- c(eua, urss, franca, reino unido)
eixo <- c(alemanha, italia, japao)

# Sistemas politicos economicos
capital <- c(pais capital)
sociali <- c(urss, china, cuba, coreia do norte, angola)

# Blocos econômicos
omc <- c()
eurOcidental <-c()
an <- c()
lesteasiatico <-c()

# Globalização
cidadesG <- c()

# Tipos de capitais
expec <- c(conteudo)
produ <- c(conteudo)


