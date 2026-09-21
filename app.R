library(shiny)
library(tidyverse)
library(data.table)
library(plotly)


load("db.RData")

# Definimos la UI con una fila para inputs, una para los dos outputs
ui <- fluidPage(
  titlePanel("Comparación de precios de productos básicos"),
  fluidRow(
    column(4,
    selectInput("geo", "Departamento:", choices= setNames(db$id.depto, db$depto), 
                selected= 'Montevideo')),
    column(4,
           selectInput("categoria", "Categoría de producto:", choices=  unique(db$producto),
                selected= 'Dulce de leche')),
    column(4,
           dateInput("fecha", "Fecha:", value= max(db$Fecha), min= min(db$Fecha, na.rm= TRUE), max= max(db$Fecha, na.rm= TRUE),
                     format= "yyyy-mm-dd", language= "es"))
    
  ),
  fluidRow(
    column(6, plotOutput("Marcas")),
    column(6, plotlyOutput("Geo"))
  )
)  

server <- function(input, output, session) {
  db_filtrada <- reactive({
    req(input$geo, input$categoria, input$fecha)
    resultado <- db %>% 
      filter(id.depto== input$geo,
             producto== input$categoria,
             Fecha== input$fecha)
    as.data.frame(resultado)
})
  output$Marcas <- renderPlot({
    ggplot(db_filtrada(), aes(as.factor(Presentacion_Producto), median(Precio), fill= marca)) +
      geom_col() +
      coord_flip()
  }, res = 96)
  
  output$Geo <- renderPlotly({
    g<- ggplot(db_filtrada(), aes(as.factor(Presentacion_Producto), median(Precio), fill= marca,
               text= paste("<b>Id Producto:</b>", Presentacion_Producto, "<br>",
                           "<b>Precio promedio:</b>", Precio, "<br>",
                           "<b>Marca:</b>", marca, "<br>",
                           "<b>Producto:</b>", nombre, "<br>"))) +
      geom_col() +
      coord_flip()
    ggplotly(g, tooltip= "text")
  })
}

shinyApp(ui = ui, server = server)