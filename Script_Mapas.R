install.packages("ggplot2")
install.packages("tmap")

library(tmap)
library(devtools)

tm_shape(nz)+
  tm_fill()
