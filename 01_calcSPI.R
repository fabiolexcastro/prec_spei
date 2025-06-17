

# Setup R  ----------------------------------------------------------------
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

require(pacman)
p_load(terra, sf, fs, httr, rvest, crayon, RColorBrewer, rmapshaper, kableExtra, scales, rnaturalearthdata, rnaturalearth, tidyverse, FAOSTAT, crayon, glue, geodata, SPEI)

# Load data ---------------------------------------------------------------

## Raster data 
fles <- dir_ls('./tif', regexp = '.tif$')
fles <- as.character(fles)
prec <- rast(fles)

# To calc SPI -------------------------------------------------------------

## Function
calc_spi <- function(precip_ts) {
  
  if (all(is.na(precip_ts))) return(rep(NA, length(precip_ts)))
  
  # Scale
  scle <- 9
  
  ## To Time Series Object
  precip_ts <- ts(precip_ts, frequency = 12)  # mensual
  
  # To calc the SPI
  spi_vals <- tryCatch({
    spi_obj <- spi(precip_ts, scale = scle)
    as.numeric(spi_obj$fitted)
  }, error = function(e) {
    rep(NA, length(precip_ts))
  })
  
  return(spi_vals)
  
}

## To apply the function
spi_03 <- terra::app(prec, calc_spi)
spi_03
terra::writeRaster(x = spi_layer, filename = './tif/spi/scl_3/spi_3.tif')

## To apply again 

### Raster to table
vls <- terra::as.data.frame(prec, na.rm = T)
crd <- terra::as.data.frame(prec[[1]], na.rm = T, xy = T)[,1:2]

### SPI 03
spi_03 <- apply(vls, 1, calc_spi)
spi_03 <- t(spi_03)
spi_03 <- cbind(crd, spi_03)
spi_03 <- rast(spi_03, type = 'xyz', crs = 'EPSG:4326')

### SPI 06
spi_06 <- apply(vls, 1, calc_spi)
spi_06 <- t(spi_06)
spi_06 <- cbind(crd, spi_06)
spi_06 <- rast(spi_06, type = 'xyz', crs = 'EPSG:4326')

### SPI 09
spi_09 <- apply(vls, 1, calc_spi)
spi_09 <- t(spi_09)
spi_09 <- cbind(crd, spi_09)
spi_09 <- rast(spi_09, type = 'xyz', crs = 'EPSG:4326')

#### To write the rasters
dout <- glue('./tif/spi/')
terra::writeRaster(x = spi_03, filename = glue('{dout}/spi_3.tif'), overwrite = T)
terra::writeRaster(x = spi_06, filename = glue('{dout}/spi_6.tif'), overwrite = T)
terra::writeRaster(x = spi_09, filename = glue('{dout}/spi_9.tif'), overwrite = T)
