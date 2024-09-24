# Cargar librerías necesarias
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("shinythemes", quietly = TRUE)) {
  install.packages("shinythemes")
}
if (!requireNamespace("leaflet", quietly = TRUE)) {
  install.packages("leaflet")
}

library(terra)
library(shiny)
library(shinythemes)
library(leaflet)

# Aumentar el tamaño máximo de carga de archivos a 100 MB
options(shiny.maxRequestSize = 100*1024^2)

# Definir los índices espectrales
indices <- list(
  "NDVI" = list(indice = function(img) (img[[5]] - img[[4]]) / (img[[5]] + img[[4]]), 
                nombre = "Índice de Vegetación de Diferencia Normalizada (NDVI)", 
                descripcion = "Mide la densidad y la salud de la vegetación.",
                formula = "\\( NDVI = \\frac{\\text{NIR} - \\text{Red}}{\\text{NIR} + \\text{Red}} \\)"),
  "DVI" = list(indice = function(img) img[[5]] - img[[4]], 
               nombre = "Índice de Vegetación Diferencial (DVI)", 
               descripcion = "Mide la diferencia entre la vegetación infrarroja y roja.",
               formula = "\\( DVI = \\text{NIR} - \\text{Red} \\)"),
  "EVI" = list(indice = function(img) 2.5 * (img[[5]] - img[[4]]) / (img[[5]] + 6 * img[[4]] - 7.5 * img[[2]] + 1), 
               nombre = "Índice de Vegetación Mejorada (EVI)", 
               descripcion = "Mejora el NDVI al reducir los efectos atmosféricos.",
               formula = "\\( EVI = 2.5 \\times \\frac{\\text{NIR} - \\text{Red}}{\\text{NIR} + 6 \\times \\text{Red} - 7.5 \\times \\text{Blue} + 1} \\)"),
  "SAVI" = list(indice = function(img) ((img[[5]] - img[[4]]) / (img[[5]] + img[[4]] + 0.5)) * (1 + 0.5), 
                nombre = "Índice de Vegetación Ajustado al Suelo (SAVI)", 
                descripcion = "Corrige el efecto del suelo desnudo en áreas con poca vegetación.",
                formula = "\\( SAVI = \\frac{(\\text{NIR} - \\text{Red})}{(\\text{NIR} + \\text{Red} + 0.5)} \\times (1 + 0.5) \\)"),
  "NDWI" = list(indice = function(img) (img[[3]] - img[[5]]) / (img[[3]] + img[[5]]), 
                nombre = "Índice Diferencial de Agua Normalizada (NDWI)", 
                descripcion = "Detecta cuerpos de agua en áreas húmedas.",
                formula = "\\( NDWI = \\frac{\\text{Green} - \\text{NIR}}{\\text{Green} + \\text{NIR}} \\)"),
  "NDSI" = list(indice = function(img) (img[[3]] - img[[6]]) / (img[[3]] + img[[6]]), 
                nombre = "Índice Diferencial Normalizado de Nieve (NDSI)", 
                descripcion = "Detecta nieve en áreas nevadas.",
                formula = "\\( NDSI = \\frac{\\text{Green} - \\text{SWIR}}{\\text{Green} + \\text{SWIR}} \\)"),
  "NBR" = list(indice = function(img) (img[[5]] - img[[7]]) / (img[[5]] + img[[7]]), 
               nombre = "Índice Normalizado de Área Quemada (NBR)", 
               descripcion = "Detecta áreas afectadas por incendios.",
               formula = "\\( NBR = \\frac{\\text{NIR} - \\text{SWIR}}{\\text{NIR} + \\text{SWIR}} \\)")
)

# Función para ajustar los valores del raster según los rangos seleccionados
ajustar_rango <- function(imagen, min_val, max_val) {
  imagen[imagen < min_val | imagen > max_val] <- NA
  return(imagen)
}

# Creación del tablero de visualización con Shiny
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(tags$script(src = "https://polyfill.io/v3/polyfill.min.js?features=es6")),
  tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")),
  
  titlePanel(tags$div("Visualización de Índices Espectrales y Composiciones RGB - Landsat 8", align = "center")),
  
  sidebarLayout(
    sidebarPanel(
      # Sección 1: Carga de la imagen
      wellPanel(
        tags$h4("1. Carga de la Imagen Satelital"),
        fileInput("image_file", "Carga una imagen satelital (GeoTIFF):", accept = c('.tif', '.tiff')),
        actionButton("update", "Actualizar"),
        br(),
        downloadButton("download_corrected_image", "Descargar Imagen Corregida")
      ),
      
      # Sección 2: Composición RGB
      wellPanel(
        tags$h4("2. Composición RGB"),
        selectInput("composicion", "Selecciona una Composición RGB:", choices = c(
          "Color Natural" = "color_natural",
          "Infrarrojo Rojo" = "infrarrojo_rojo",
          "Tierra Agua" = "tierra_agua",
          "Vegetación" = "vegetacion",
          "Infrarrojo" = "infrarrojo",
          "Área Urbana" = "area_urbana",
          "Geología" = "geologia",
          "Agricultura" = "agricultura",
          "Nieve Nube" = "nieve_nube"
        )),
        tags$span(style = "color: #2C3E50;", tags$b("Descripción:")),
        uiOutput("descripcion_rgb"),
        br(),
        tags$b("Composición RGB:"),
        uiOutput("leyenda_rgb")
      ),
      
      # Sección 3: Índices Espectrales
      wellPanel(
        tags$h4("3. Índices Espectrales"),
        selectInput("indice", "Selecciona un Índice Espectral (SIGLAS):", choices = names(indices)),
        tags$span(style = "color: #2C3E50;", tags$b("Índice seleccionado:")),
        textOutput("indice_info"),
        br(),
        tags$span(style = "color: #2C3E50;", tags$b("Descripción:")),
        textOutput("indice_desc"),
        br(),
        tags$span(style = "color: #2C3E50;", tags$b("Fórmula:")),
        uiOutput("indice_formula"),
        br(),
        sliderInput("min_val", "Valor mínimo:", min = -1, max = 1, value = -1, step = 0.01),
        sliderInput("max_val", "Valor máximo:", min = -1, max = 1, value = 1, step = 0.01),
        br(),
        downloadButton("download_index", "Descargar Índice Ajustado")
      )
    ),
    mainPanel(
      plotOutput("plot_rgb", height = "500px"),  # Mostrar la imagen RGB
      plotOutput("plot_indices", height = "500px"),  # Mostrar la imagen de los índices
      plotOutput("hist", height = "400px")  # Histograma del índice seleccionado
    )
  )
)

server <- function(input, output, session) {
  imagen_cargada <- reactiveVal()
  
  # Cargar la imagen satelital
  observeEvent(input$image_file, {
    req(input$image_file)
    imagen_cargada(rast(input$image_file$datapath))
  })
  
  # Actualizar los valores de los nombres, descripciones y fórmulas de los índices espectrales
  observeEvent(input$indice, {
    indice_info <- indices[[input$indice]]
    output$indice_info <- renderText({ indice_info$nombre })
    output$indice_desc <- renderText({ indice_info$descripcion })
    output$indice_formula <- renderUI({ withMathJax(HTML(indice_info$formula)) })
  })
  
  # Ajustar los rangos dinámicamente según el índice seleccionado
  observe({
    req(imagen_cargada())
    indice_info <- indices[[input$indice]]
    indice <- indice_info$indice(imagen_cargada())
    
    # Calcular los valores mínimos y máximos del índice
    min_val <- round(min(values(indice), na.rm = TRUE), 2)
    max_val <- round(max(values(indice), na.rm = TRUE), 2)
    
    # Actualizar los sliders dinámicamente
    updateSliderInput(session, "min_val", min = min_val, max = max_val, value = min_val)
    updateSliderInput(session, "max_val", min = min_val, max = max_val, value = max_val)
  })
  
  # Descripción de las combinaciones RGB con bandas
  output$descripcion_rgb <- renderUI({
    desc <- switch(input$composicion,
                   "color_natural" = "Composición RGB que representa los colores naturales como los vería el ojo humano.",
                   "infrarrojo_rojo" = "Resalta la vegetación usando la banda infrarroja cercana.",
                   "tierra_agua" = "Diferencia entre tierra y agua, destacando cuerpos de agua.",
                   "vegetacion" = "Realza la vegetación y permite identificar áreas verdes.",
                   "infrarrojo" = "Usa bandas infrarrojas para destacar características del terreno.",
                   "area_urbana" = "Enfatiza áreas urbanas con tonos específicos.",
                   "geologia" = "Útil para análisis geológicos y detección de suelos.",
                   "agricultura" = "Destaca áreas agrícolas para estudios de cultivos.",
                   "nieve_nube" = "Diferencia nieve y nubes, útil en zonas montañosas.")
    tags$p(desc)
  })
  
  # Leyenda de las bandas RGB con rectángulos de colores
  output$leyenda_rgb <- renderUI({
    leyenda <- switch(input$composicion,
                      "color_natural" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band_4"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band_3"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band_2")
                        )),
                      "infrarrojo_rojo" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band_5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band_4"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band_3")
                        )),
                      "tierra_agua" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band_5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band_6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band_4")
                        )),
                      "vegetacion" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band_6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band_5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band_4")
                        )),
                      "infrarrojo" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band_7"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band_5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band_4")
                        )),
                      "area_urbana" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band_7"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band_6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band_4")
                        )),
                      "geologia" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band_7"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band_4"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band_2")
                        )),
                      "agricultura" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " red = Band_6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band_5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band_2")
                        )),
                      "nieve_nube" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " red = Band_2"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band_6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band_7")
                        ))
    )
    leyenda
  })
  
  # Renderizar la imagen RGB
  output$plot_rgb <- renderPlot({
    req(imagen_cargada())
    composicion <- switch(input$composicion,
                          "color_natural" = c(imagen_cargada()[[4]], imagen_cargada()[[3]], imagen_cargada()[[2]]),
                          "infrarrojo_rojo" = c(imagen_cargada()[[5]], imagen_cargada()[[4]], imagen_cargada()[[3]]),
                          "tierra_agua" = c(imagen_cargada()[[5]], imagen_cargada()[[6]], imagen_cargada()[[4]]),
                          "vegetacion" = c(imagen_cargada()[[6]], imagen_cargada()[[5]], imagen_cargada()[[4]]),
                          "infrarrojo" = c(imagen_cargada()[[7]], imagen_cargada()[[5]], imagen_cargada()[[4]]),
                          "area_urbana" = c(imagen_cargada()[[7]], imagen_cargada()[[6]], imagen_cargada()[[4]]),
                          "geologia" = c(imagen_cargada()[[7]], imagen_cargada()[[4]], imagen_cargada()[[2]]),
                          "agricultura" = c(imagen_cargada()[[6]], imagen_cargada()[[5]], imagen_cargada()[[2]]),
                          "nieve_nube" = c(imagen_cargada()[[2]], imagen_cargada()[[6]], imagen_cargada()[[7]]))
    
    # Mostrar la composición RGB seleccionada
    plotRGB(composicion, stretch = "lin", main = paste("Composición RGB:", input$composicion))
  })
  
  # Renderizar la imagen de índices espectrales ajustada a los rangos
  output$plot_indices <- renderPlot({
    req(imagen_cargada())
    input$update
    indice_info <- indices[[input$indice]]
    indice <- ajustar_rango(indice_info$indice(imagen_cargada()), input$min_val, input$max_val)
    
    # Mostrar el índice espectral
    plot(indice, col = terrain.colors(50), main = paste("Índice Espectral:", indice_info$nombre))
  })
  
  # Generar el histograma del índice seleccionado
  output$hist <- renderPlot({
    req(imagen_cargada())
    input$update
    indice_info <- indices[[input$indice]]
    indice <- ajustar_rango(indice_info$indice(imagen_cargada()), input$min_val, input$max_val)
    indice_filtrado <- values(indice)[!is.na(values(indice)) & values(indice) >= input$min_val & values(indice) <= input$max_val]
    
    # Verificación antes de graficar el histograma
    if (length(indice_filtrado) > 0) {
      hist(indice_filtrado, main = paste0("Histograma de ", indice_info$nombre), col = "#4682B4", border = "white", xlab = indice_info$nombre, breaks = 50)
    } else {
      plot.new()
      text(0.5, 0.5, "No hay datos válidos para mostrar", cex = 1.2)
    }
  })
  
  # Descargar la imagen del índice ajustado
  output$download_index <- downloadHandler(
    filename = function() {
      paste0(input$indice, ".tif")
    },
    content = function(file) {
      req(imagen_cargada())
      indice_info <- indices[[input$indice]]
      indice_ajustado <- ajustar_rango(indice_info$indice(imagen_cargada()), input$min_val, input$max_val)
      writeRaster(indice_ajustado, file)
    }
  )
  
  # Descargar la imagen corregida
  output$download_corrected_image <- downloadHandler(
    filename = function() {
      paste("imagen_corregida.tif")
    },
    content = function(file) {
      req(imagen_cargada())
      writeRaster(imagen_cargada(), file)
    }
  )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
