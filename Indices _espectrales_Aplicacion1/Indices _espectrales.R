# Carga las librerías necesarias
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("shinythemes", quietly = TRUE)) {
  install.packages("shinythemes")
}

# Carga las librerías
library(terra)
library(shiny)
library(shinythemes)

# Carga la imagen de Landsat 8
ruta_imagen <- "E:/EA_GIS/BootCamp_IA/Dia1/L8.tif"
landsat8 <- rast(ruta_imagen)

# Definir los índices espectrales con siglas, nombres completos y descripciones mejoradas
indices <- list(
  "NDVI" = list(indice = landsat8[[5]] / landsat8[[4]], nombre = "Índice de Vegetación Proporcionada (NDVI)", descripcion = "Usado para medir la densidad de vegetación verde."),
  "DVI" = list(indice = landsat8[[5]] - landsat8[[4]], nombre = "Índice de Vegetación Diferencial (DVI)", descripcion = "Mide la diferencia entre la vegetación infrarroja y roja."),
  "NDWI" = list(indice = (landsat8[[5]] - landsat8[[4]]) / (landsat8[[5]] + landsat8[[4]]), nombre = "Índice de Vegetación Diferencial Normalizada (NDWI)", descripcion = "Usado para detectar agua en áreas húmedas."),
  "EVI" = list(indice = (landsat8[[5]] - landsat8[[4]]) / (landsat8[[5]] + 6 * landsat8[[4]] - 7.5 * landsat8[[3]] + 1), nombre = "Índice de Vegetación Mejorada (EVI)", descripcion = "Mejora el NDVI al reducir efectos atmosféricos."),
  "TVI" = list(indice = sqrt((landsat8[[5]] - landsat8[[4]]) / (landsat8[[5]] + landsat8[[4]]) + 0.5), nombre = "Índice de Vegetación Transformado (TVI)", descripcion = "Resalta el contraste de la vegetación."),
  "ARVI" = list(indice = (landsat8[[5]] - (landsat8[[4]] - 1) * (landsat8[[4]] - landsat8[[2]])) / (landsat8[[5]] + (landsat8[[4]] - 1) * (landsat8[[4]] - landsat8[[2]])), nombre = "Índice de Vegetación Atmosféricamente Resistente (ARVI)", descripcion = "Resistente a los efectos atmosféricos."),
  "SAVI" = list(indice = ((landsat8[[5]] - landsat8[[4]]) / (landsat8[[5]] + landsat8[[4]] + 0.5)) * (1 + 0.5), nombre = "Índice de Vegetación Ajustado al Suelo (SAVI)", descripcion = "Corrige el efecto del suelo desnudo en áreas con poca vegetación."),
  "LAI" = list(indice = log((0.69 - ((landsat8[[5]] - landsat8[[4]]) / (landsat8[[5]] + landsat8[[4]] + 0.5)) * (1 + 0.5)) / 0.59) / 0.91, nombre = "Índice de Área Foliar (LAI)", descripcion = "Mide la cantidad de área foliar verde."),
  "BSI" = list(indice = ((landsat8[[6]] + landsat8[[4]]) - (landsat8[[5]] + landsat8[[2]])) / ((landsat8[[6]] + landsat8[[4]]) + (landsat8[[5]] + landsat8[[2]])), nombre = "Índice de Suelo Desnudo (BSI)", descripcion = "Identifica áreas de suelo desnudo."),
  "NDWI_Water" = list(indice = (landsat8[[3]] - landsat8[[5]]) / (landsat8[[3]] + landsat8[[5]]), nombre = "Índice de Agua de Diferencia Normalizada (NDWI)", descripcion = "Se utiliza para detectar cuerpos de agua."),
  "NDSI" = list(indice = (landsat8[[3]] - landsat8[[6]]) / (landsat8[[3]] + landsat8[[6]]), nombre = "Índice Diferencial Normalizado de Nieve (NDSI)", descripcion = "Detecta nieve en áreas nevadas."),
  "NBR" = list(indice = (landsat8[[5]] - landsat8[[7]]) / (landsat8[[5]] + landsat8[[7]]), nombre = "Índice Normalizado de Área Quemada (NBR)", descripcion = "Detecta áreas afectadas por incendios.")
)

# Creación del tablero de visualización con Shiny
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Tema más atractivo
  titlePanel(
    tags$div("Visualización de Índices Espectrales - Landsat 8", align = "center")  # Título centrado
  ),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(  
        tags$b(selectInput("indice", "Selecciona un Índice Espectral (SIGLAS):", 
                           choices = names(indices))),  # Solo muestra las siglas en negrita
        tags$span(style = "color: #2C3E50;", tags$b("Índice seleccionado:")),  # Subtítulo en color más oscuro
        textOutput("indice_info"),  # Muestra la información del índice
        br(),  # Espacio
        tags$span(style = "color: #2C3E50;", tags$b("Descripción:")),  # Subtítulo en color más oscuro
        textOutput("indice_desc"),  # Muestra la descripción del índice
        br(),  # Espacio
        tags$b("Aplicar máscara"),  # Mostrar "Aplicar máscara" en negrita, sin checkbox
        sliderInput("min_val", "Valor mínimo:", 
                    min = 0, max = 1, value = 0, step = 0.01),  # Ajuste de step
        sliderInput("max_val", "Valor máximo:", 
                    min = 0, max = 1, value = 1, step = 0.01),  # Ajuste de step
        actionButton("update", "Actualizar")  # Botón de actualización al final
      )
    ),
    mainPanel(
      fluidRow(
        column(12, plotOutput("plot", height = "400px"))  # Ajusta el tamaño de la imagen
      ),
      fluidRow(
        column(12, plotOutput("hist", height = "300px"))  # Histograma más proporcionado
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    # Obtener la información del índice seleccionado
    indice_info <- indices[[input$indice]]
    
    # Mostrar el nombre completo y la descripción del índice
    output$indice_info <- renderText({
      paste0(indice_info$nombre)
    })
    
    output$indice_desc <- renderText({
      paste0(indice_info$descripcion)
    })
    
    # Obtener el índice en sí
    indice <- indice_info$indice
    
    # Calcular el valor mínimo y máximo del índice seleccionado
    min_val <- round(min(values(indice), na.rm = TRUE), 2)  # Redondear a dos decimales
    max_val <- round(max(values(indice), na.rm = TRUE), 2)  # Redondear a dos decimales
    
    # Dividir el rango en más intervalos para mayor precisión
    step <- round((max_val - min_val) / 10, 2)  # Más puntos en el slider
    
    # Actualizar los sliders dinámicamente
    updateSliderInput(session, "min_val", min = min_val, max = max_val, value = min_val, step = step)
    updateSliderInput(session, "max_val", min = min_val, max = max_val, value = max_val, step = step)
  })
  
  output$plot <- renderPlot({
    input$update  # Actualización al presionar el botón
    indice_info <- indices[[input$indice]]
    indice <- indice_info$indice
    
    # Aplicar límites de visualización (los valores fuera del rango serán "NA" para poner en blanco)
    indice_modificado <- indice
    values(indice_modificado)[values(indice_modificado) < input$min_val | values(indice_modificado) > input$max_val] <- NA
    
    # Plotear el índice modificado con el nombre completo en el título
    plot(indice_modificado, col = terrain.colors(50), main = indice_info$nombre)
  })
  
  output$hist <- renderPlot({
    input$update  # Actualización del histograma
    indice_info <- indices[[input$indice]]
    indice <- indice_info$indice
    
    # Filtrar los valores que están dentro del rango de la máscara
    indice_filtrado <- values(indice)[values(indice) >= input$min_val & values(indice) <= input$max_val]
    
    # Plotear el histograma con el nombre completo en el título
    hist(indice_filtrado, main = paste0("Histograma de ", indice_info$nombre), 
         col = "#4682B4", border = "white", xlab = indice_info$nombre, breaks = 50)
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)


