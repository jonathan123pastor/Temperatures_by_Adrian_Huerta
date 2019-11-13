# Autor: Ing. Adrian Huerta
to_spdf <- function(datos, 
                    fecha)
{
  
  pro4j <- sp::CRS("+proj=longlat +datum=WGS84 
                   +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  sp::SpatialPointsDataFrame(coords = datos[, c("X", "Y")],
                                    data = datos[ fecha ],                       
                                    proj4string = pro4j)

}

cov_clim <- function(direccion,
                     variable,
                     dekada)
{
  pro4j <- sp::CRS("+proj=longlat +datum=WGS84
                   +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  file <- file.path(direccion, variable, 
                    paste(dekada, ".tif", sep = ""))
  
  res <- raster::raster(file)
  raster::projection(res) = pro4j
  res
}

temp_dek <- function(Obj_sp,
                     Cov)
{
  names(Obj_sp) <- "obs"
  names(Cov) <- "cov"
  
  # extracciÃ³n y anomalia
  Obj_sp$cov <- raster::extract(Cov, Obj_sp)
  Obj_sp$anom <- Obj_sp$obs - Obj_sp$cov
  
  # IDW de anoamlia
  gS <- gstat::gstat(formula = anom ~ 1, 
                     locations = Obj_sp,
                     set = list(idp = 2))
  
  idw <- raster::interpolate(object = Cov,
                             model = gS)
  
  # AgregaciÃ³n de climatologia y anomalia
  idw_clim_anom <- Cov + idw$var1.pred
  names(idw_clim_anom) <- "res"
  idw_clim_anom
}
