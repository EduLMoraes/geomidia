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
otan <- c()
pac_varsovia <- c()
onu <- c()
g7 <- c()
g20 <- c()
unicef <- c()
unesco <- c()
oms <- c()

# Acontecimentos
gf <- c()
guerra1 <- c()
guerra2 <- c()
aliados <- c()
eixo <- c()

# Sistemas politicos economicos
capital <- c()
sociali <- c()
republi <- c()

# Blocos econômicos
omc <- c()
eurOcidental <-c()
an <- c()
lesteasiatico <-c()

# Globalização
paises_g <- c()
porcentual <- c()
rank <- c()

# Tipos de capitais
expec <- c()
produ <- c()

# Ordem econômica
bancos <- c()
