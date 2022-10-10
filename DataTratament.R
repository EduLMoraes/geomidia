#### Dados                          ####
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


mundo <-      geojsonio::geojson_read('https://raw.githubusercontent.com/EduardoMoreaes/Geojson/master/mundo.geojson', what = "sp")
certiMundo <- geojsonio::geojson_read('https://raw.githubusercontent.com/EduardoMoreaes/Geojson/master/mundo.geojson', what = "sp")
continente <- geojsonio::geojson_read("https://raw.githubusercontent.com/EduardoMoreaes/Geojson/master/continentes.geojson", what = "sp")
mundo@data <- mundo@data %>% arrange(CNTRY_NAME)
cont = data.frame(v1 = c(19),
                  v2 = c(17),
                  v3 = c(18),
                  v4 = c(18),
                  v5 = c(17),
                  v6 = c(13),
                  v7 = c(4),
                  v8 = c(3),
                  v9 = c(1))

#### Organizações                   ####

otan <- read.csv2('https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/geodata/mundo/paisesOtan.csv', sep = ",", header = T)
pac_varsovia <- read.csv2('https://raw.githubusercontent.com/EduardoMoreaes/geomidia/master/geodata/mundo/pac_varsovia.csv', sep = ";", header = T)
pac_varsovia <- pac_varsovia %>% arrange(X1955)
pac_varsovia <- pac_varsovia[-3,]

otan <- data.frame(sort(otan$X1949),
                   sort(otan$X1959),
                   sort(otan$X1969),
                   sort(otan$X1979),
                   sort(otan$X1989),
                   sort(otan$X1999),
                   sort(otan$X2009),
                   sort(otan$X2019),
                   sort(otan$X2022))

cont$v9 = 1
# otan
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == otan$sort.otan.X1949.[cont$v1]){
    mundo@data$OTAN_1949[i] = "T"
    cont$v1 = cont$v1 + 1
    if(cont$v1 > 30){cont$v1 = 19}
  } else{ mundo@data$OTAN_1949[i] = "F"}
  
  if(mundo@data$CNTRY_NAME[i] == otan$sort.otan.X1959.[cont$v2]){
    mundo@data$OTAN_1959[i] = "T"
    cont$v2 = cont$v2 + 1
    if(cont$v2 > 30){cont$v2 = 17}
  } else{ mundo@data$OTAN_1959[i] = "F"}
  
  if(mundo@data$CNTRY_NAME[i] == otan$sort.otan.X1969.[cont$v3]){
    mundo@data$OTAN_1969[i] = "T"
    cont$v3 = cont$v3 + 1
    if(cont$v3 > 30){cont$v3 = 18}
  } else{ mundo@data$OTAN_1969[i] = "F"}
  
  if(mundo@data$CNTRY_NAME[i] == otan$sort.otan.X1979.[cont$v4]){
    mundo@data$OTAN_1979[i] = "T"
    cont$v4 = cont$v4 + 1
    if(cont$v4 > 30){cont$v4 = 18}
  } else{ mundo@data$OTAN_1979[i] = "F"}
  
  if(mundo@data$CNTRY_NAME[i] == otan$sort.otan.X1989.[cont$v5]){
    mundo@data$OTAN_1989[i] = "T"
    cont$v5 = cont$v5 + 1
    if(cont$v5 > 30){cont$v5 = 17}
  } else{ mundo@data$OTAN_1989[i] = "F"}
  
  if(mundo@data$CNTRY_NAME[i] == otan$sort.otan.X1999.[cont$v6]){
    mundo@data$OTAN_1999[i] = "T"
    cont$v6 = cont$v6 + 1
    if(cont$v6 > 30){cont$v6 = 13}
  } else{ mundo@data$OTAN_1999[i] = "F"}
  
  if(mundo@data$CNTRY_NAME[i] == otan$sort.otan.X2009.[cont$v7]){
    mundo@data$OTAN_2009[i] = "T"
    cont$v7 = cont$v7 + 1
    if(cont$v7 > 30){cont$v7 = 4}
  } else{ mundo@data$OTAN_2009[i] = "F"}
  
  if(mundo@data$CNTRY_NAME[i] == otan$sort.otan.X2019.[cont$v8]){
    mundo@data$OTAN_2019[i] = "T"
    cont$v8 = cont$v8 + 1
    if(cont$v8 > 30){cont$v8 = 3}
  } else{ mundo@data$OTAN_2019[i] = "F"}
  
  if(mundo@data$CNTRY_NAME[i] == otan$sort.otan.X2022.[cont$v9]){
    mundo@data$OTAN_2022[i] = "T"
    cont$v9 = cont$v9 + 1
    if(cont$v9 > 30){cont$v9 = 1}
  } else{ mundo@data$OTAN_2022[i] = "F"}
}
cont$v9 = 1
# pacto
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == pac_varsovia$X1955[cont$v9]){
    mundo@data$pacto_1955_1991[i] = "T"
    cont$v9 = cont$v9 + 1
    if(cont$v9 > 6){cont$v9 = 1}
  } else{mundo@data$pacto_1955_1991[i] = "F"}  
  if(mundo@data$CNTRY_NAME[i] == "Czech Republic"){
    mundo@data$pacto_1955_1991[i] = "T"
  } else if(mundo@data$CNTRY_NAME[i] == "Slovakia"){
    mundo@data$pacto_1955_1991[i] = "T"
  }
}


rm(otan, pac_varsovia, i)

#https://www.infoescola.com/historia/pacto-de-varsovia/
#https://www.todamateria.com.br/otan-organizacao-do-tratado-do-atlantico-norte/

#### Onu                            ####


abrev_pais  <- mundo@data
onuData     <- read.csv2('https://github.com/EduardoMoreaes/geomidia/raw/master/geodata/mundo/onuData.csv', sep = ",", header = T)
paises      <- onuData %>% 
                distinct(Major.trading.partners)
paises      <- paises[-1,] %>% as.data.frame(paises)
onuPaises   <- abrev_pais %>%
                inner_join(paises, by = c('LONG_NAME'='.'))




onuFundos <- data.frame( nome        = c('Alimentação e Agricultura', 'Fundo Internacional de Desenvolvimento Agrícola',
                                         'Organização da Aviação Civil Internacional', 'Organização Internacional do Trabalho', 
                                         'Organização Marítima Internacional', 'Fundo Monetário Internacional', 
                                         'União Internacional de Telecomunicações', 'Educação, a Ciência e a Cultura',
                                         'Desenvolvimento Industrial', 'União Postal Universal', 'Grupo Banco Mundial',
                                         'Organização Mundial da Saúde', 'Organização Mundial da Propriedade Intelectual',
                                         'Organização Meteorológica Mundial', 'Organização Mundial de Turismo'),
                         abrev       = c('FAO', 'FIDA', 'OACI', 'OIT', 'OMI', 'FMI', 'UIT', 'UNESCO', 'UNIDO',
                                         'UPU', 'GBM', 'OMS', 'OMPI', 'OMM', 'OMT'),
                         sede        = c('Roma', 'Roma', 'Montreal', 'Genebra', 'Londres', 'Washington D.C.', 'Genebra',
                                         'Paris', 'Viena', 'Berna', 'Washington D.C.', 'Genebra', 'Genebra', 
                                         'Genebra', 'Madrid'),
                         ano_fundada = c(1945, 1977, 1947, 1919, 1959, 1945, 1865, 1945, 1985, 1874, 1945, 1948, 1967, 1950, 1974)
                         )


for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == onuPaises$CNTRY_NAME[cont$v9]){
    mundo@data$ONU[i] = "T"
    cont$v9 = cont$v9+1
    if(cont$v9 > 194){cont$v9 = 1}
  } else{ mundo@data$ONU[i] = "F"}
}

rm(paises, abrev_pais, onuData, i, onuPaises)

#http://data.un.org/Default.aspx
#http://data.un.org/Explorer.aspx
#http://data.un.org/UpdateCalendar.aspx

#### Grupos mundiais                ####
g7 <- c("United Kingdom", "France", "United States", "Canada", "Italy", "Germany", "Japan")
g7 <- sort(g7)
g20 <- c("South Africa", "Germany", "Saudi Arabia", "Argentina", "Australia",
         "Brazil", "Canada", "China", "South Korea", "United States", "France", "India",
         "Indonesia", "Italy", "Japan", "Mexico", "United Kingdom", "Russia", "Turkey")
g20 <- sort(g20)

for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == g7[cont$v9]){
    mundo@data$G7[i] = "T"
    cont$v9 = cont$v9+1
    if(cont$v9 > 7){cont$v9 = 1}
  } else{mundo@data$G7[i] = "F"}
}
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == g20[cont$v9]){
    mundo@data$G20[i] = "T"
    cont$v9 = cont$v9+1
    if(cont$v9 > 19){cont$v9 = 1}
  } else{mundo@data$G20[i] = "F"}
}

rm(g7, g20, i)
#https://www.bcb.gov.br/rex/g20/port/mencaog20.asp?frame=1#:~:text=O%20Grupo%20conta%20com%20a,Reino%20Unido%2C%20Rússia%20e%20Turquia.

#### Acontecimentos                 ####
guerra1 = c("1914 - 1918",'France', 'United Kingdom', 'Russia',
             'Canada', 'Italy', 'United States',
             'Belgium', 'Japan', 'Australia',
             'New Zealand', 'Cuba', 'Portugal',
             'Romania', 'Serbia & Montenegro', 'Brazil',
             'Poland', 'South Africa',
             'Germany', 'Hungary', 'Turkey',
            'Bulgaria', 'Luxembourg')
guerra2 = c("1939 - 1945", "United States", "Russia", "France", "United Kingdom",
     "Germany", "Italy", "Japan", 'Canada', 'Brazil', 'Australia', 'China', 'Netherlands')
                              
guerra1 <- sort(guerra1) %>% as.data.frame()
guerra2 <- sort(guerra2) %>% as.data.frame() 

aliados <- c("United States", "Russia", "France", "United Kingdom")
eixo    <- c("Germany", "Italy", "Japan")
triEntente <- c('France', 'Russia', 'United Kingdom')
triAlianca <- c('Germany', 'Hungary', 'Italy')
impCentrais <- c('Germany', 'Hungary', 'Turkey', 'Bulgaria')

aliados <- sort(aliados)
eixo    <- sort(eixo)
triEntente <- sort(triEntente)
triAlianca <- sort(triAlianca)
impCentrais <- sort(impCentrais)



cont$v9 <- cont$v9+1
i=0
# primeira guerra
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == guerra1$.[cont$v9]){
    mundo@data$GM1[i] = "T"
    cont$v9 = cont$v9+1
    if(cont$v9 > 23){cont$v9 = 2}
  }else{mundo@data$GM1[i] = "F"}
}
i=0
# segunda guerra
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == guerra2$.[cont$v9]){
    mundo@data$GM2[i] = "T"
    cont$v9 = cont$v9+1
    if(cont$v9 > 13){cont$v9 = 1}
  } else{mundo@data$GM2[i] = "F"}
}
i=0
# aliado ou eixo
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == aliados[cont$v9]){
    mundo@data$LADO[i] = "Aliados"
    cont$v9 = cont$v9+1
    if(cont$v9 > 4){cont$v9 = 1}
  } else{mundo@data$LADO[i] = NULL}
}
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == eixo[cont$v9]){
    mundo@data$LADO[i] = "Eixo"
    cont$v9 = cont$v9+1
    if(cont$v9 > 3){cont$v9 = 1}
  }
}
# Triplice aliança ou entente
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == triAlianca[cont$v9]){
    mundo@data$TRIPLICE[i] = "Aliança"
    cont$v9 = cont$v9+1
    if(cont$v9 > 3){cont$v9 = 1}
  } else{mundo@data$TRIPLICE[i] = "F"}
}
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == triEntente[cont$v9]){
    mundo@data$TRIPLICE[i] = "Entente"
    cont$v9 = cont$v9+1
    if(cont$v9 > 3){cont$v9 = 1}
  }
}
# Imperio Central
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == impCentrais[cont$v9]){
    mundo@data$IMP_CENTRAIS[i] = "T"
    cont$v9 = cont$v9+1
    if(cont$v9 > 4){cont$v9 = 1}
  } else{mundo@data$IMP_CENTRAIS[i] = "F"}
}

rm(guerra1, guerra2, i, aliados, eixo, triAlianca, triEntente, impCentrais)
guerraFria <- c("Periodo de 1945 até 1991")

#https://brasilescola.uol.com.br/historiag/segunda-guerra-mundial.htm
#https://www.suapesquisa.com/primeiraguerra/paises.htm

#### Sistemas politicos economicos  ####
sociali <- c('Moldova', 'China', 'Cuba', 'North Korea',
             'Laos', 'Vietnam')
sociali <- sort(sociali)
rm(i)
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == sociali[cont$v9]){
    mundo@data$SIS_POL_ECO[i] = "sociali"
    cont$v9 = cont$v9+1
    if(cont$v9 > 6){cont$v9 = 1}
  } else{mundo@data$SIS_POL_ECO[i] = "capital"}
}
rm(sociali, i)

#https://pt.wikipedia.org/wiki/Estado_socialista#:~:text=Atualmente%2C%20os%20Estados%20que%20conservam,a%20República%20Socialista%20do%20Vietnã.

#### Blocos econômicos              ####

nafta <- c('Canada', 'Mexico', 'United States')
mercosul <- c(
  'Argentina', 'Brazil', 'Paraguay',
  'Uruguay', 'Venezuela', 'Bolivia', 'Chile',
  'Colombia', 'Ecuador', 'Guyana', 'Peru', 'Suriname')
uniao_europeia <- c('Germany', 'Austria', 'Belgium',
                    'Bulgaria', 'Czech Republic', 'Cyprus',
                    'Croatia', 'Denmark', 'Slovakia',
                    'Slovenia', 'Spain', 'Estonia',
                    'Finland', 'France', 'Greece',
                    'Hungary', 'Ireland', 'Italy',
                    'Latvia', 'Lithuania', 'Luxembourg',
                    'Malta', 'Netherlands', 'Poland',
                    'Portugal', 'Romania', 'Sweden')
nafta <- sort(nafta)
mercosul <- sort(mercosul)
uniao_europeia <- sort(uniao_europeia)

for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == nafta[cont$v9]){
    mundo@data$BLOCO[i] = "nafta"
    cont$v9 = cont$v9+1
    if(cont$v9 > 3){cont$v9 = 1}
  } else{mundo@data$BLOCO[i] = "NA"}
}
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == mercosul[cont$v9]){
    mundo@data$BLOCO[i] = "mercosul"
    cont$v9 = cont$v9+1
    if(cont$v9 > 12){cont$v9 = 1}
  }
}
for(i in 1 : nrow(mundo@data)){
  if(mundo@data$CNTRY_NAME[i] == uniao_europeia[cont$v9]){
    mundo@data$BLOCO[i] = "UNIeuropeia"
    cont$v9 = cont$v9+1
    if(cont$v9 > 27){cont$v9 = 1}
  }
}

rm(mercosul, nafta, uniao_europeia, i)
           
#https://european-union.europa.eu/easy-read_pt
#https://www.mercosur.int/pt-br/quem-somos/paises-do-mercosul/
#https://www.suno.com.br/artigos/blocos-economicos/

#### Globalização                   ####
cidadesG <- data.frame(cidade = c("London", "New York", "Paris",
                                  "Tokyo", "Los Angeles", "Chicago",
                                  "Frankfurt", "Milao"),
                        long = c(-0.1275, -73.9385, 2.3511, 139.692,
                                 -118.411, -87.6828, 8.682127, 9.18817),
                        lat  = c(51.5072,  40.6643, 48.8032, 35.6894,
                                 34.0194, 41.8379, 50.110924, 45.4637))

#### Tipos de capitais              ####
expec <- c("conteudo")
produ <- c("conteudo")
#### Cores                          ####

for(i in 1 : nrow(mundo@data)){
  if(mundo@data$GM1[i] == "T"){ mundo@data$corGM1[i] = "red" }else{mundo@data$corGM1[i] = "black"}
  if(mundo@data$GM2[i] == "T"){ mundo@data$corGM2[i] = "red" }else{mundo@data$corGM2[i] = "black"}
  if(mundo@data$G7[i] == "T"){ mundo@data$corG7[i] = "yellow" }else{mundo@data$corG7[i] = "black"}
  if(mundo@data$ONU[i] == "T"){ mundo@data$corOnu[i] = "white" }else{mundo@data$corOnu[i] = "black"}
  if(mundo@data$G20[i] == "T"){ mundo@data$corG20[i] = "yellow" }else{ mundo@data$corG20[i] = "black"}
  if(mundo@data$IMP_CENTRAIS[i] == "T"){ mundo@data$corIMP[i] = "red" }else{mundo@data$corIMP[i] = "black"}
  if(mundo@data$SIS_POL[i] == "capital"){ mundo@data$corSis[i] = "blue" }else{mundo@data$corSis[i] = "red"}
  if(mundo@data$OTAN_1949[i] == "T"){ mundo@data$corOTAN_49[i] = "white" }else{mundo@data$corOTAN_49[i] = "black"}
  if(mundo@data$OTAN_2022[i] == "T"){ mundo@data$corOTAN_22[i] = "white" }else{mundo@data$corOTAN_22[i] = "black"}
  if(mundo@data$pacto_1955_1991[i] == "T"){ mundo@data$corPacto[i] = "red" }else{mundo@data$corPacto[i] = "black"}
  if(mundo@data$LADO[i] == "Aliados"){ mundo@data$corLado[i] = "blue" }else if(mundo@data$LADO[i] == "Eixo"){ mundo@data$corLado[i] = "red" }else{mundo@data$corLado[i] = "black"}
  if(mundo@data$TRIPLICE[i] == "Aliança"){ mundo@data$corTRI[i] = "blue" }else if(mundo@data$TRIPLICE[i] == "Entente"){mundo@data$corTRI[i] = "red"}else{mundo@data$corTRI[i] = "black"}
}

#### Final                          ####
mundo@data <- certiMundo@data %>%
  inner_join(mundo@data, by = c('CNTRY_NAME'='CNTRY_NAME'))
mundo@data <- mundo@data[, -c(14:27)]
return(mundo@data)
#writeOGR(mundo, dsn = "./geodata/mundo", layer = "mundoGeopolitico", driver = 'ESRI Shapefile', overwrite_layer = TRUE)
#write.csv2(cidadesG, file = "./geodata/mundo/cidadesGlobalizadas.csv")
#write.csv2(onuFundos, file = "./geodata/mundo/onuFundos.csv")

rm(i, cont, cidadesG, onuFundos, expec, guerraFria, produ, certiMundo)
