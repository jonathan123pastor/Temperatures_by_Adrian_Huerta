# Autor: Ing. Adrian Huerta
path = "C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/scripts/"
setwd(path)
source('help.R')

path2 = "C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/dataset/"
setwd(path2)

load("obs_temp_normals.RData")
load("covs.RData")

ls()

tx_xy <- sp::SpatialPointsDataFrame(coords = TTx_xy[, c("X", "Y")],
                                    data = TTx_xy[-c(1:5)],
                                    proj4string = sp::CRS(raster::projection(Elv)))

tn_xy <- sp::SpatialPointsDataFrame(coords = TTn_xy[, c("X", "Y")],
                                    data = TTn_xy[-c(1:5)],
                                    proj4string = sp::CRS(raster::projection(Elv)))

## TN
TNres <- list()
for(i in 1:36)
{
  sp_obs <- tn_xy[,i]
  Dekada <- names(sp_obs)
  MLST <- Dekada %>% substr(3, 4) %>% as.numeric()
  names(sp_obs) <- "Obs"
  
  sp_cov <- raster::brick(LstNight[[ MLST ]], Elv, Lon, Lat)
  names(sp_cov)[1] <- "LstNight"
  
  sp_obs <- cbind(sp_obs, raster::extract(sp_cov, sp_obs))
  lmr <- lm(Obs ~ LstNight + Elv + Lon + Lat, data = sp_obs)
  sp_obs$Rlmr <- lmr$residuals
  rgrid_gs <- as(sp_cov[[1]], 'SpatialGrid')
  
  raster::projection(sp_obs) <- NA
  raster::projection(rgrid_gs) <- NA
  
  RlmrKR <- automap::autoKrige(formula = Rlmr ~ 1, input_data = sp_obs, new_data = rgrid_gs)
  RlmrKR = raster::brick(RlmrKR$krige_output)
  RlmrKR = RlmrKR$var1.pred
  raster::projection(RlmrKR) <- raster::projection(sp_cov)
  
  StModel <- lmr$coefficients[1] +  sum(sp_cov * lmr$coefficients[2:5])
  #print(sp::spplot(StModel + RlmrKR))
  # raster::writeRaster(StModel + RlmrKR,  
  #                     paste("./02_gridded/results/Dekadal_Normals/TN/", Dekada, ".tif", sep = ""),
  #                     format="GTiff")
  TNres[[i]] <- StModel + RlmrKR
}

## TX
TXres <- list()
for(i in 1:36)
{
  sp_obs <- tx_xy[,i]
  Dekada <- names(sp_obs)  
  MLST <- Dekada %>% substr(3, 4) %>% as.numeric()
  names(sp_obs) <- "Obs"
  
  sp_cov <- raster::brick(LstDay[[ MLST ]], Elv, Lon, Lat)
  names(sp_cov)[1] <- "LstDay"
  
  sp_obs <- cbind(sp_obs, raster::extract(sp_cov, sp_obs))
  lmr <- lm(Obs ~ LstDay + Elv + Lon + Lat, data = sp_obs)
  sp_obs$Rlmr <- lmr$residuals
  rgrid_gs <- as(sp_cov[[1]], 'SpatialGrid')
  
  raster::projection(sp_obs) <- NA
  raster::projection(rgrid_gs) <- NA
  
  RlmrKR <- automap::autoKrige(formula = Rlmr ~ 1, input_data = sp_obs, new_data = rgrid_gs)
  RlmrKR = raster::brick(RlmrKR$krige_output)
  RlmrKR = RlmrKR$var1.pred
  raster::projection(RlmrKR) <- raster::projection(sp_cov)
  
  StModel <- lmr$coefficients[1] +  sum(sp_cov * lmr$coefficients[2:5])
  #print(sp::spplot(StModel + RlmrKR))
  
  # raster::writeRaster(StModel + RlmrKR,  
  #                     paste("./02_gridded/results/Dekadal_Normals/TX/", Dekada, ".tif", sep = ""),
  #                     format="GTiff")
  TXres[[i]] <- StModel + RlmrKR
  
}


TNres <- raster::brick(TNres)
names(TNres) <- names(tn_xy)
TXres <- raster::brick(TXres)
names(TXres) <- names(tn_xy)

tx_palette <- c(RColorBrewer::brewer.pal(n = 9, name = "Purples") %>% rev(), 
                RColorBrewer::brewer.pal(n = 9, name = "Reds")%>% rev())

path3="C:/Fernando Pastor/Adrian_scripts/02_entregable/02_gridded/results/Figures/"
setwd(path3)

png(filename=paste0(path3,"normalTN.png"), width = 1220, height = 920, units = "px")
sp::spplot(TNres, col.regions = tx_palette, cut = 18, at = seq(-20, 20, 2.5), layout = c(10, 4))
dev.off()

png(filename=paste0(path3,"normalTX.png"), width = 1220, height = 980, units = "px")
sp::spplot(TXres, col.regions = tx_palette, cut = 13, at = seq(0, 30, 2.5), layout = c(10, 4))
dev.off()

### 
path4 = "C:/Fernando Pastor/Adrian_scripts/PER_adm/"
setwd(path4)

per_shp0 <- raster::shapefile("PER_adm1.shp")
per_shp <- per_shp0 %>% broom::tidy()

lk_shp0 <- raster::shapefile("Lagos_Principales.shp")
lk_shp <- lk_shp0 %>% broom::tidy()


cov_spt <- raster::brick(LstDay[[1]], LstNight[[1]], Elv, Lat, Lon)
names(cov_spt) <- c("LST_day", "LST_night", "ElevaciÃ³n", "Latitud", "Longitud")
mean_Values <-  c(mean(cov_spt[[1]]@data@values, na.rm = T), 
                  mean(cov_spt[[2]]@data@values, na.rm = T),
                  mean(cov_spt[[3]]@data@values, na.rm = T),
                  mean(cov_spt[[4]]@data@values, na.rm = T),
                  mean(cov_spt[[5]]@data@values, na.rm = T))

sd_Values <-  c(sd(cov_spt[[1]]@data@values, na.rm = T), 
                  sd(cov_spt[[2]]@data@values, na.rm = T),
                  sd(cov_spt[[3]]@data@values, na.rm = T),
                  sd(cov_spt[[4]]@data@values, na.rm = T),
                  sd(cov_spt[[5]]@data@values, na.rm = T))

cov_spt <- (cov_spt - mean_Values) / sd_Values
raster::as.data.frame(cov_spt, xy = T) %>%
  reshape2::melt(id = c("x", "y"), 
                 variable.name = "covariable",
                 value.name = "valor") %>%
  ggplot2::ggplot() + 
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = valor)) + 
  ggplot2::scale_fill_gradientn("", colours = rev(topo.colors(5)))  + 
  ggplot2::geom_polygon(data = per_shp, ggplot2::aes(x = long, y = lat, group = group),
                        fill = NA, colour = "gray20", size = 0.5) +
  ggplot2::geom_polygon(data = lk_shp, ggplot2::aes(x = long, y = lat, group = group), 
                        fill = "gray20", colour = "gray20", size = .5) + 
  ggplot2::theme_classic(base_size = 17) + 
  ggplot2::coord_quickmap(xlim = c(-72.92, -68),
                          ylim = c(-18.03, -13.31), expand = FALSE) + 
  ggplot2::labs(x = "", y = "") + 
  ggplot2::facet_wrap(~covariable, ncol = 3) + 
  ggplot2::theme(legend.position = c(0.85, 0.25))


ggplot2::ggsave(paste0(path3,"covs.png"), 
                dpi = 100,  scale = 0.75,  height = 11, width = 11)


