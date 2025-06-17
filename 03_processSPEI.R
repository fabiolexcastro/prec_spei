
# Setup R  ----------------------------------------------------------------
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

require(pacman)
p_load(terra, sf, fs, httr, rvest, classInt, crayon, RColorBrewer, rmapshaper, kableExtra, scales, rnaturalearthdata, rnaturalearth, tidyverse, FAOSTAT, crayon, glue, geodata, SPEI)

# Load data ---------------------------------------------------------------

## Raster data 
rstr <- terra::rast('./tif/spei/spei06.nc')
rstr[[1]] * 1
rstr <- rstr[[6:nlyr(rstr)]]


# To calculate the average for all the months -----------------------------
avrg <- map(.x = 1:12, .f = function(m){
  
  cat('Month: ', month.abb[m], '\n')
  mnt <- ifelse(m < 10, paste0('0', m), as.character(mnt))
  rst <- rstr[[grep(paste0('-', mnt, '-'), time(rstr))]]
  avg <- mean(rst)  
  names(avg) <- glue('spei06_{m}')
  rm(rst)
  gc(reset = T)
  return(avg)
  
})

avrg <- reduce(avrg, c)
avrg

## To write the rasters
terra::writeRaster(x = avrg, filename = glue('./tif/spei/spei06_avrg-monthly.nc'), overwrite = TRUE)
avrg <- terra::rast('./tif/spei/spei06_avrg-monthly.nc')

# To make the map  --------------------------------------------------------
avrg.tble <- avrg %>% 
  terra::as.data.frame(xy = T) %>% 
  as_tibble() %>% 
  gather(var, value, -c(x, y)) %>% 
  separate(data = ., col = 'var', into = c('variable', 'month'), sep = '_')
avrg.tble
avrg.tble <- inner_join(avrg.tble, tibble(month = as.character(1:12), month_abb = month.abb), by = 'month')
avrg.tble <- mutate(avrg.tble, month_abb = factor(month_abb, levels = month.abb))

g.spei <- ggplot() + 
  geom_tile(data = avrg.tble, aes(x = x, y = y, fill = value)) + 
  facet_wrap(.~month_abb) + 
  scale_fill_viridis_c() +
  coord_sf() +
  labs(x = 'Lon', y = 'Lat', fill = 'SPEI') +
  theme_minimal() +
  theme(
    legend.position = 'bottom', 
    legend.key.width = unit(3, 'line'), 
    axis.text.x = element_text(size = 5), 
    axis.text.y = element_text(size = 5), 
    axis.title = element_text(size = 6), 
    legend.title.position = 'top', 
    legend.title = element_text(hjuust = 0.5, face = 'bold')
  )

ggsave(plot = g.spei, filename = './png/maps/spei06_world.jpg', units = 'in', width = 12, height = 6, dpi = 300, create.dir = TRUE)
