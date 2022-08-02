library("shiny")
library("ECharts2Shiny")
library("shinyWidgets")
library("leaflet")

temas <- list(
  'tema1',
  'tema2',
  'tema3',
  'tema4'
)

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
                        tags$h1('GEOPOLITICA')
                      ),
                      
                      fluidRow(
                        column(7,
                               leaflet() %>% addTiles()
                        ),
                        column(5,
                               tags$div(id="test_1", style="width:100%;height:200px;"),  # Specify the div for the chart.
                               deliverChart(div_id = "test_1"),  # Deliver the plotting
                               
                               tags$div(id="test_3", style="width:100%;height:300px;"),
                               deliverChart(div_id = "test_3")
                        )
                      ),
                      
                      fluidRow(
                               tags$div(id="test_2", style="width:100%;height:300px;"),
                               deliverChart(div_id = "test_2")
                      ),
                      
                    ),
                      
)
  
  




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
    
   
  
  renderPieChart(div_id = "test_1", data = dat_1, theme = tema, radius = "90%")
  
  renderLineChart(div_id = "test_2", theme = tema, data = dat_2)
  
  renderBarChart(div_id = "test_3", grid_left = '3%', data = dat_2, font.size.legend=15, show.tools=T, show.legend = T)
  
  #grid_right, padrao eh 4% #grid_top padrao eh 16% #grid_bottom 3% #
  
  renderBarChart(div_id = "test_4", theme = tema, direction = "vertical", grid_left = "10%", data = dat_2)
  
  renderGauge(div_id = "test_5", gauge_name = "Meta de vendas", rate = 59.9, theme = tema)
  
  renderWordcloud("test_6", data = palavras, grid_size = 10, sizeRange = c(20, 50))
  
  
  
    
  })
}

shinyApp(ui, server)