
# Visualización de Índices Espectrales - Aplicación Shiny

Esta aplicación en `R` utilizando `Shiny` permite visualizar diferentes índices espectrales a partir de imágenes multiespectrales de Landsat 8. El código está diseñado para cargar una imagen multiespectral de Landsat 8, calcular varios índices espectrales, y luego mostrar estos índices a través de gráficos y un histograma.

## Ejemplo de la Aplicación

![Ejemplo de visualización](image.png)

La imagen de ejemplo muestra la interfaz de la aplicación en la que se está visualizando el Índice de Vegetación Proporcionada (NDVI).

## Requisitos previos

- Necesitas una imagen multiespectral de Landsat 8 en formato `.tif` que contenga todas las bandas necesarias para calcular los índices.
- La imagen debe estar en el mismo directorio que el código, o puedes especificar su ubicación en el código.

## Cambiar la dirección de la imagen

Por defecto, el código está configurado para cargar una imagen con el nombre `L8.tif` ubicada en el directorio `E:/EA_GIS/BootCamp_IA/Dia1/`. Si tienes la imagen en una ruta diferente o con otro nombre, debes modificar la siguiente línea:

```r
ruta_imagen <- "E:/EA_GIS/BootCamp_IA/Dia1/L8.tif"
```

Cambia `"E:/EA_GIS/BootCamp_IA/Dia1/L8.tif"` por la ruta correcta a tu archivo.

## ¿Qué es una imagen multiespectral de Landsat 8?

Una imagen multiespectral de Landsat 8 contiene múltiples bandas que representan diferentes partes del espectro electromagnético. Estas bandas se utilizan para calcular índices espectrales que ayudan a analizar características de la superficie terrestre como vegetación, agua, áreas quemadas, etc.

### Ejemplo de bandas en Landsat 8:

- Banda 2: Azul
- Banda 3: Verde
- Banda 4: Rojo
- Banda 5: Infrarrojo cercano (NIR)
- Banda 6: Infrarrojo de onda corta (SWIR 1)
- Banda 7: Infrarrojo de onda corta (SWIR 2)

## Descargar imágenes similares

Si deseas utilizar esta aplicación con otras imágenes, asegúrate de descargar imágenes multiespectrales de Landsat 8 o similares desde plataformas como [USGS Earth Explorer](https://earthexplorer.usgs.gov/) o [Google Earth Engine](https://earthengine.google.com/). Estas imágenes deben contener todas las bandas necesarias para calcular los índices.

## Índices calculados

Esta aplicación puede calcular varios índices espectrales, cada uno con un propósito específico:

- **NDVI** (Índice de Vegetación Normalizado): Usado para medir la densidad de vegetación verde.
- **DVI** (Índice de Vegetación Diferencial): Mide la diferencia entre la vegetación infrarroja y roja.
- **NDWI** (Índice de Vegetación Diferencial Normalizada): Usado para detectar agua en áreas húmedas.
- **EVI** (Índice de Vegetación Mejorada): Mejora el NDVI al reducir efectos atmosféricos.
- **TVI** (Índice de Vegetación Transformado): Resalta el contraste de la vegetación.
- **ARVI** (Índice de Vegetación Atmosféricamente Resistente): Resistente a los efectos atmosféricos.
- **SAVI** (Índice de Vegetación Ajustado al Suelo): Corrige el efecto del suelo desnudo en áreas con poca vegetación.
- **LAI** (Índice de Área Foliar): Mide la cantidad de área foliar verde.
- **BSI** (Índice de Suelo Desnudo): Identifica áreas de suelo desnudo.
- **NDWI_Water** (Índice de Agua de Diferencia Normalizada): Detecta cuerpos de agua.
- **NDSI** (Índice Diferencial Normalizado de Nieve): Detecta nieve en áreas nevadas.
- **NBR** (Índice Normalizado de Área Quemada): Detecta áreas afectadas por incendios.

## Cómo usar la aplicación

1. Carga la imagen multiespectral en la ruta correcta.
2. Ejecuta la aplicación en R usando `Shiny`.
3. Selecciona el índice espectral de interés.
4. Ajusta los valores mínimos y máximos utilizando los deslizadores para aplicar una máscara a los valores que te interesen.
5. Visualiza el índice y su histograma en tiempo real.

```r
shinyApp(ui = ui, server = server)
```

Esta línea al final del código ejecutará la aplicación Shiny.

## Créditos

Desarrollado por [Nombre del desarrollador], basado en imágenes de Landsat 8.

