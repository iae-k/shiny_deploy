####################################
### Visualizaciones Interactivas ###
####################################

## Clase 5 - Cierre (script básico)

# Cargamos librerías y datos
library(shiny)
library(tidyverse)
library(data.table)
library(plotly)

load("db.RData")

# Limpieza y transformación de datos

cadenas <- c('El Dorado'= '#ffa301',
             'Ta - Ta'= '#89001a',
             'Devoto Express'= '#135fbc',
             'Devoto'= '#135fbc',
             'Tienda Inglesa'= '#2f69c7',
             'Farmashop'= '#014694',
             'FarmaGlobal'= '#087e34',
             'Los Jardines'= '#b2c031',
             'Red Market'= '#c10007',
             'Red Expres'= '#1b619a',
             'Disco'= '#006640',
             'Frog'= '#45bc3b',
             'Macromercado Mayorista'= '#e52037',
             'San Roque'= '#ff7e0a',
             'Planeta'= '#d8e6c8',
             'Frigo'= '#4a71b4',
             'Micro Macro'= '#e52037',
             'Super Tico'= '#21bcee',
             'El Clon'= '#e30613',
             'Super XXI'= '#028e39',
             'Pigalle'= '#525252',
             'La Colonial'= '#ab0f15',
             'Kinko'= '#f69420',
             'Géant'= '#10684d',
             'Supermercados 5 estrellas'= '#005c2c')

# Definimos la UI con una fila para inputs, una para los dos outputs
ui <- fluidPage(
  titlePanel("Comparación de precios de productos básicos"),
  fluidRow(
    column(4,
    selectInput("geo", "Departamento:", choices= setNames(db$id.depto, db$depto), 
                selected= 'Montevideo')),
    column(4,
           selectInput("categoria", "Categoría de producto:", choices=  unique(db$producto),
                selected= 'Arroz blanco')),
    column(4,
           dateInput("fecha", "Fecha:", value= '2024-06-17', min= min(db$Fecha, na.rm= TRUE), max= max(db$Fecha, na.rm= TRUE),
                     format= "yyyy-mm-dd", language= "es"))
    
  ),
  fluidRow(
    column(6, plotOutput("Marcas")),
    #column(6, plotlyOutput("Marcas_Int"))
    column(6, plotOutput("Comercios"))
  )
)  

# Configuración del servidor 

server <- function(input, output, session) {
  db_filtrada <- reactive({
    req(input$geo, input$categoria, input$fecha)
    resultado <- db %>% 
      filter(id.depto== input$geo,
             producto== input$categoria,
             Fecha== input$fecha)
    as.data.frame(resultado)
})
  
  db_marcas <- reactive({
    db_filtrada() %>%
      group_by(Presentacion_Producto, marca) %>%
      summarise(
        Precio= median(Precio, na.rm= TRUE),
        .groups= 'drop'
      )
  })
  
  db_resumen <- reactive({
    db_filtrada() %>%
      count(Precio, cadena, name= 'Frecuencia')
  })
  
#observe({
#    browser()
#    db_filtrada()
#  })
  
  output$Marcas <- renderPlot({
    ggplot(db_marcas(), aes(as.factor(Presentacion_Producto), Precio, fill= marca)) +
      geom_col() +
      ylab('Producto') +
      xlab('Precio (mediana)') +
      coord_flip()
  }, res = 96)
  
  #output$Marcas_Int <- renderPlotly({
  #  g<- ggplot(db_filtrada(), aes(as.factor(Presentacion_Producto), median(Precio), fill= marca,
  #             text= paste("<b>Id Producto:</b>", Presentacion_Producto, "<br>",
  #                         "<b>Precio promedio:</b>", Precio, "<br>",
  #                         "<b>Marca:</b>", marca, "<br>",
  #                         "<b>Producto:</b>", nombre, "<br>"))) +
  #    geom_col() +
  #    coord_flip()
  #  ggplotly(g, tooltip= "text")
  #})
  
  output$Comercios <- renderPlot({
    ggplot(db_resumen(), aes(Precio, Frecuencia, color= cadena, group= Precio)) +
      geom_segment(aes(Precio, xend= Precio, y= 0, yend= Frecuencia), size= 1) +
      geom_point(size= 3) +
      scale_color_manual(values= cadenas) +
      ylab('Cantidad de comercios') 
  }, res = 96)
  
}

shinyApp(ui = ui, server = server)