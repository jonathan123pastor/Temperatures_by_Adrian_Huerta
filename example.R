# Autor: Ing. Adrian Huerta
rm(list = ls())
`%>%` = magrittr::`%>%`

path = "C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/scripts/"
setwd(path)
## Funciones
source('functions.R')

path2 = "C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/dataset/OBS/TO_send/"
setwd(path2)
## Datos observados de temperatura minima dekadiaria
datos_dek <- read.csv("TN_dekadal_spatiats.csv")

## A dato espacial
datos_obs <- to_spdf(datos = datos_dek, fecha = "X2019.06.1")


path3="C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/results/Dekadal_Normals/"
setwd(path3)
## Seleccion de climatologia de acuerdo a variable y dekada
cov <- cov_clim(direccion = path3,
                variable = "TN",
                dekada = "d_06.1")

## Interpolacion IDW de anomalias y obtencion de temperatura
temp_dk <- temp_dek(Obj_sp = datos_obs,
                    Cov = cov)

## plot
sp::spplot(temp_dk)

## save
# raster::writeRaster(x = temp_dk, 
#                     file = "temp_dk.tif",
#                     format = "GTiff")

### figuras del informe

pG <- raster::extract(temp_dk, datos_obs) %>%
  round(2)

plot(datos_obs@data$X2019.06.1, pG, 
     ylab = "punto de grilla", 
     xlab = "punto de estaciÃ³n", 
     cex = 1.5, pch = 21)

path4 = "C:/Fernando Pastor/Adrian_scripts/PER_adm/"
setwd(path4)

per_shp0 <- raster::shapefile("PER_adm1.shp")
per_shp <- per_shp0 %>% broom::tidy()

lk_shp0 <- raster::shapefile("Lagos_Principales.shp")
lk_shp <- lk_shp0 %>% broom::tidy()

raster::as.data.frame(temp_dk, xy = TRUE) %>%
  ggplot2::ggplot() + 
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = res)) +
  ggplot2::scale_fill_gradientn("T°C", colours = c(topo.colors(10)),
                                na.value = "gray20",
                                limits = c(-20, 20))  + 
  ggplot2::geom_polygon(data = per_shp, ggplot2::aes(x = long, y = lat, group = group),
                        fill = NA, colour = "gray20", size = 0.5) +
  ggplot2::geom_polygon(data = lk_shp, ggplot2::aes(x = long, y = lat, group = group), 
                        fill = "gray20", colour = "gray20", size = .5) + 
  ggplot2::theme_classic(base_size = 17) + 
  ggplot2::coord_quickmap(xlim = c(-72.92, -68),
                          ylim = c(-18.03, -13.31), expand = FALSE) + 
  ggplot2::labs(x = "", y = "")

path5="C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/results/Figures/"

ggplot2::ggsave(paste0(path5,"example.png"), 
                dpi = 100,  scale = 0.75,  height = 9, width = 9)
