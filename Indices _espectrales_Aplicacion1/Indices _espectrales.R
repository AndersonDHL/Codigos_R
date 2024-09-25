# Cargar librerías necesarias y solo instalar las que no estén presentes
librerias <- c("terra", "shiny", "shinythemes", "leaflet")
paquetes_faltantes <- librerias[!librerias %in% installed.packages()[, "Package"]]
if (length(paquetes_faltantes)) install.packages(paquetes_faltantes)

# Cargar todas las librerías
lapply(librerias, library, character.only = TRUE)

# Aumentar el tamaño máximo de carga de archivos a 2 GB
options(shiny.maxRequestSize = 2 * 1024^3)

# Definir los índices espectrales
indices <- list(
  "NDVI" = list(
    indice = function(img) (img[[5]] - img[[4]]) / (img[[5]] + img[[4]]),
    nombre = "Índice de Vegetación de Diferencia Normalizada (NDVI)",
    descripcion = "Mide la densidad y la salud de la vegetación.",
    formula = "\\( NDVI = \\frac{\\text{NIR} - \\text{Red}}{\\text{NIR} + \\text{Red}} \\)"
  ),
  "DVI" = list(
    indice = function(img) img[[5]] - img[[4]],
    nombre = "Índice de Vegetación Diferencial (DVI)",
    descripcion = "Mide la diferencia entre la vegetación infrarroja y roja.",
    formula = "\\( DVI = \\text{NIR} - \\text{Red} \\)"
  ),
  "NDWI" = list(
    indice = function(img) (img[[3]] - img[[5]]) / (img[[3]] + img[[5]]),
    nombre = "Índice de Agua de Diferencia Normalizada (NDWI)",
    descripcion = "Detecta cuerpos de agua en áreas húmedas.",
    formula = "\\( NDWI = \\frac{\\text{Green} - \\text{NIR}}{\\text{Green} + \\text{NIR}} \\)"
  ),
  "EVI" = list(
    indice = function(img) 2.5 * (img[[5]] - img[[4]]) / (img[[5]] + 6 * img[[4]] - 7.5 * img[[2]] + 1),
    nombre = "Índice de Vegetación Mejorada (EVI)",
    descripcion = "Mejora el NDVI al reducir los efectos atmosféricos.",
    formula = "\\( EVI = 2.5 \\times \\frac{\\text{NIR} - \\text{Red}}{\\text{NIR} + 6 \\times \\text{Red} - 7.5 \\times \\text{Blue} + 1} \\)"
  ),
  "TVI" = list(
    indice = function(img) sqrt((img[[5]] - img[[4]]) / (img[[5]] + img[[4]]) + 0.5),
    nombre = "Índice de Vegetación Transformado (TVI)",
    descripcion = "Resalta el contraste de la vegetación.",
    formula = "\\( TVI = \\sqrt{\\frac{\\text{NIR} - \\text{Red}}{\\text{NIR} + \\text{Red}} + 0.5} \\)"
  ),
  "ARVI" = list(
    indice = function(img) (img[[5]] - (img[[4]] - 1) * (img[[4]] - img[[2]])) / (img[[5]] + (img[[4]] - 1) * (img[[4]] - img[[2]])),
    nombre = "Índice de Vegetación Atmosféricamente Resistente (ARVI)",
    descripcion = "Resistente a los efectos atmosféricos.",
    formula = "\\( ARVI = \\frac{\\text{NIR} - (\\text{Red} - 1) \\times (\\text{Red} - \\text{Blue})}{\\text{NIR} + (\\text{Red} - 1) \\times (\\text{Red} - \\text{Blue})} \\)"
  ),
  "SAVI" = list(
    indice = function(img) ((img[[5]] - img[[4]]) / (img[[5]] + img[[4]] + 0.5)) * (1 + 0.5),
    nombre = "Índice de Vegetación Ajustado al Suelo (SAVI)",
    descripcion = "Corrige el efecto del suelo desnudo en áreas con poca vegetación.",
    formula = "\\( SAVI = \\frac{(\\text{NIR} - \\text{Red})}{(\\text{NIR} + \\text{Red} + 0.5)} \\times (1 + 0.5) \\)"
  ),
  "LAI" = list(
    indice = function(img) log((0.69 - ((img[[5]] - img[[4]]) / (img[[5]] + img[[4]] + 0.5)) * (1 + 0.5)) / 0.59) / 0.91,
    nombre = "Índice de Área Foliar (LAI)",
    descripcion = "Mide la cantidad de área foliar verde.",
    formula = "\\( LAI = \\frac{\\log((0.69 - SAVI) / 0.59)}{0.91} \\)"
  ),
  "BSI" = list(
    indice = function(img) ((img[[6]] + img[[4]]) - (img[[5]] + img[[2]])) / ((img[[6]] + img[[4]]) + (img[[5]] + img[[2]])),
    nombre = "Índice de Suelo Desnudo (BSI)",
    descripcion = "Identifica áreas de suelo desnudo.",
    formula = "\\( BSI = \\frac{(\\text{SWIR} + \\text{Red}) - (\\text{NIR} + \\text{Blue})}{(\\text{SWIR} + \\text{Red}) + (\\text{NIR} + \\text{Blue})} \\)"
  ),
  "NDWI_Water" = list(
    indice = function(img) (img[[3]] - img[[5]]) / (img[[3]] + img[[5]]),
    nombre = "Índice de Agua de Diferencia Normalizada (NDWI)",
    descripcion = "Se utiliza para detectar cuerpos de agua.",
    formula = "\\( NDWI = \\frac{\\text{Green} - \\text{NIR}}{\\text{Green} + \\text{NIR}} \\)"
  ),
  "NDSI" = list(
    indice = function(img) (img[[3]] - img[[6]]) / (img[[3]] + img[[6]]),
    nombre = "Índice Diferencial Normalizado de Nieve (NDSI)",
    descripcion = "Detecta nieve en áreas nevadas.",
    formula = "\\( NDSI = \\frac{\\text{Green} - \\text{SWIR}}{\\text{Green} + \\text{SWIR}} \\)"
  ),
  "NBR" = list(
    indice = function(img) (img[[5]] - img[[7]]) / (img[[5]] + img[[7]]),
    nombre = "Índice Normalizado de Área Quemada (NBR)",
    descripcion = "Detecta áreas afectadas por incendios.",
    formula = "\\( NBR = \\frac{\\text{NIR} - \\text{SWIR}}{\\text{NIR} + \\text{SWIR}} \\)"
  )
)

# Función para ajustar los valores del raster según los rangos seleccionados
ajustar_rango <- function(imagen, min_val, max_val) {
  if (is.na(min_val) || is.na(max_val) || min_val == max_val) return(imagen)
  imagen[imagen < min_val | imagen > max_val] <- NA
  return(imagen)
}

# Función para crear títulos centrados y en negrita
crear_titulo <- function(texto) {
  tags$h3(tags$div(texto, style = "text-align: center; font-weight: bold; color: black;"))
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
        actionButton("update", "Actualizar")
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
        tags$span(style = "color: #2C3E50;", tags$b("Aplicación de Máscaras:")),
        sliderInput("min_val", "Valor mínimo:", min = -1, max = 1, value = -1, step = 0.01),
        sliderInput("max_val", "Valor máximo:", min = -1, max = 1, value = 1, step = 0.01),
        br(),
        downloadButton("download_index", "Descargar Índice Ajustado")
      )
    ),
    mainPanel(
      crear_titulo("Composición RGB"),
      plotOutput("plot_rgb", height = "450px"),
      crear_titulo("Índice Espectral"),
      plotOutput("plot_indices", height = "500px"),
      crear_titulo("Histograma del Índice"),
      plotOutput("hist", height = "380px")
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
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band 4"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band 3"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band 2")
                        )),
                      "infrarrojo_rojo" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band 5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band 4"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band 3")
                        )),
                      "tierra_agua" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band 5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band 6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band 4")
                        )),
                      "vegetacion" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band 6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band 5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band 4")
                        )),
                      "infrarrojo" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band 7"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band 5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band 4")
                        )),
                      "area_urbana" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band 7"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band 6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band 4")
                        )),
                      "geologia" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band 7"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band 4"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band 2")
                        )),
                      "agricultura" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band 6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band 5"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band 2")
                        )),
                      "nieve_nube" = tags$div(
                        tags$ul(
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: red;"), " Red = Band 2"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: green;"), " Green = Band 6"),
                          tags$li(tags$div(style = "display: inline-block; width: 20px; height: 10px; background-color: blue;"), " Blue = Band 7")
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
    
    plotRGB(composicion, stretch = "lin")
  })
  
  # Renderizar la imagen de índices espectrales ajustada a los rangos
  output$plot_indices <- renderPlot({
    req(imagen_cargada())
    input$update
    indice_info <- indices[[input$indice]]
    indice <- ajustar_rango(indice_info$indice(imagen_cargada()), input$min_val, input$max_val)
    
    plot(indice, col = terrain.colors(50))
  })
  
  # Generar el histograma del índice seleccionado
  output$hist <- renderPlot({
    req(imagen_cargada())
    input$update
    indice_info <- indices[[input$indice]]
    indice <- ajustar_rango(indice_info$indice(imagen_cargada()), input$min_val, input$max_val)
    indice_filtrado <- values(indice)[!is.na(values(indice)) & values(indice) >= input$min_val & values(indice) <= input$max_val]
    
    if (length(indice_filtrado) > 0) {
      hist(indice_filtrado, col = "#4682B4", border = "white", xlab = indice_info$nombre, breaks = 50, main = "") 
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
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
