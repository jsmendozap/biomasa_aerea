# Cálculo de ínidices y preparación para generar el modelo

setwd("D:/Drive/Programación/R/R SIG/Biomasa Aérea con imágenes SAR y R")

# Librerías necesarias

library(sf)
library(raster)
library(glcm)

# Cargue de información

HH <- raster('Resultados/3. Procesadas/HH.tif')
HV <- raster('Resultados/3. Procesadas/HV.tif')

# Cálculo del NDBI

# Es un índice que ayuda a distinguir áreas con distinta estructura del bosque 
# debido a la contribución diferenciada de la dispersión dada por las dos 
# polarizaciones.

overlay(x = HH, y = HV, fun = function(x,y){return((x-y)/(x+y))},
        filename = 'Resultados/3. Procesadas/NDBI', format = 'GTiff')

# Medidas de textura para NDBI, HH y HV
# http://www3.inpe.br/unidades/cep/atividadescep/jornada/programa/t-9_trab_27.pdf

texturas <- vector(mode = 'list', length = 3)
procesadas <- dir('Resultados/3. Procesadas', full.names = T)

for (i in 1:length(procesadas)){
  texturas[[i]] <- glcm(x = raster(procesadas[i]), n_grey = 64, window = c(3,3),
                        shift = list(c(0,1), c(1,1), c(1,0), c(1,-1)),
                        statistics = c('mean', 'variance', 'homogeneity',
                                       'contrast', 'dissimilarity', 'entropy',
                                       'second_moment', 'correlation'),
                        min_x = NULL, max_x = NULL, na_opt = 'any', na_val = NA,
                        scale_factor = 1, asinteger = F)
}

for (i in 1:length(texturas)){
  plot(texturas[[i]])
}

proc <- c('HH', 'HV', 'NDBI')
dir.create('Resultados/4. Texturas')

for (i in 1:length(procesadas)){
  for (k in 1:nlayers(texturas[[i]])){
    writeRaster(x = texturas[[i]][[k]],
                filename = paste('Resultados/4. Texturas/', proc[i], '_',
                                 names(texturas[[i]])[[k]], sep = ''),
                format = 'GTiff') 
  }
}