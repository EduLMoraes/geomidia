FROM rocker/home/shiny
RUN R -e "install.packages(c('shiny', 'shinyjs','shinyBS','shinythemes','shinyWidgets','ECharts2Shiny','shinydashboard','shinycssloaders','tidyr','stringr','dplyr','unvotes','lubridate','sf','leaflet','rgdal','tidyverse','rsconnect','geojson'), repos='https://cran.rstudio.com/')"
COPY /geomidia /srv/shiny-server/sample-apps/