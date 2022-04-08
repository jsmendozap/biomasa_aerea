setwd("D:/Drive/Programación/R/R SIG/Biomasa Aérea con imágenes SAR y R/Resultados")

# Se tienen imágenes SAR descargadas desde GEE las cuales ya se encuentran corregidas
# geométrica y radiométricamente. Con ellas se realiza el siguiente el procesamiento:
# 1. Convertir los valores digitales a decibeles.
# 2. Realizar un filtro para disminuir el ruido.
# 3. Proyectar el sistema de coordenadas a planas.

# Cargando librerias

library(sf)
library(raster)

# Cargando raster de las imágenes SAR

raster <- stack(c("Mosaico.tif", "bosque_noBosque.tif"))
names(raster)[1:2] <- c("HH", "HV")

# Guardando archivos con valores de retrodispersión de las polarizaciones

dir.create("1. Retrodispersión")

ret <- function(x) { # Convierte los valores digitales a coeficientes de retrodispersión
  10 * log10(x^2) - 83
}

for (i in 1:2) {
  calc(
    x = raster[[i]], fun = ret,
    filename = paste("retrodispersión/", names(raster)[i], sep = ""),
    format = "GTiff"
  )
}

HH <- raster("retrodispersión/HH.tif")
HV <- raster("retrodispersión/HH.tif")
plot(HH, col = gray.colors(30, start = 0.3, end = 1), legend = T)
plot(HV, col = gray.colors(30, start = 0.3, end = 1), legend = T)

# Aplicación del filtro de Lee para reducción de ruido

dir.create("2. Lee")

library(whitebox)
install_whitebox() # La primera vez que se instala la libreria se debe correr el comando para descargar el ejecutable

for (i in 1:2) {
  wbt_lee_sigma_filter(
    input = paste(getwd(), "/", dir("retrodispersión", full.names = T)[i], sep = ""),
    output = paste(getwd(), "/Lee/", dir("retrodispersión/")[i], sep = ""),
    filterx = 3, filtery = 3
  )
}

# Proyectando las imágenes a Magna Colombia-Bogotá EPSG: 3116 - Coordenadas planas

imagenes <- list()
dir.create('3. Procesadas')

for (i in 1:2) {
  img <- raster(dir("Lee/", full.names = T)[i])
  imagenes[[i]] <- projectRaster(
    from = img, crs = "EPSG:3116",
    filename = paste('3. Procesadas/', names(img), '.tif', sep = ''), overwrite = T)
}

multibanda <- stack(imagenes)
plotRGB(multibanda, r = 1, g = 2, b = 1, axes = T, stretch = "lin")