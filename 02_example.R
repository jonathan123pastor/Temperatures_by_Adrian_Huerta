# Autor: Ing. Adrian Huerta
path = "C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/scripts/"
setwd(path)
source('help.R')
path2="C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/dataset/OBS/TO_send/"
setwd(path2)
data_example <- read.csv("TX_dekadal_spatiats.csv")     # BASE DE DATOS
data_example <- data_example[c("X", "Y", "X2019.06.1")]                                  # FECHA A INTERPOLAR

# a spatialpointdataframe (punto espacial)

pro4j <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
exp <- sp::SpatialPointsDataFrame(coords = data_example[,c("X", "Y")],
                                  data = data_example["X2019.06.1"],                       
                                  proj4string = pro4j)

# selecciÃ³n de covariable de acuerdo a dekada

file = names(exp) %>% substr(7, 10) %>% paste("d_", ., ".tif", sep = "")
clim <- "02_gridded/results/Dekadal_Normals/TX"                                          # UBICACION DE COVARIABLE
# file <- file.path(clim, file)

## InterpolaciÃ³n

# ordenando informaciÃ³n
path3 = "C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/results/Dekadal_Normals/TX/"
setwd(path3)
lista = list.files(pattern = ".tif")

cov <- raster::raster(lista[16])
raster::projection(cov) = raster::projection(exp)
names(cov) <- "cov"

obs <- exp
names(obs) <- "obs"

obs$cov <- raster::extract(cov, obs)
obs$anom <- obs$obs - obs$cov

# interpolando anomalia mediante IDW

gS <- gstat::gstat(formula = anom ~ 1, 
                   locations = obs,
                   set = list(idp = 2))                                                  # FACTOR DE SUAVIZADO IDW (IDP = 2)

idw <- raster::interpolate(object = cov,
                           model = gS)

# anomalia + climatologÃ­a

idw_clim_anom <- cov + idw$var1.pred                                                     # SUMA DE CLIM + ANOM
sp::spplot(idw_clim_anom)
# raster::writeRaster(idw_clim_anom, "FILE.tif", format="GTiff")                         # GUARDAR INFORMACIÃN

