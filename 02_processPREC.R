
# Setup R  ----------------------------------------------------------------
g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

require(pacman)
p_load(terra, sf, fs, httr, rvest, classInt, crayon, RColorBrewer, rmapshaper, kableExtra, scales, rnaturalearthdata, rnaturalearth, tidyverse, FAOSTAT, crayon, glue, geodata, SPEI)

# Load data ---------------------------------------------------------------

## Raster data 
rstr <- terra::rast('./tif/spei/spei06.nc')
prec <- rast(as.character(dir_ls('./tif/prec')))

## World dataset
wrld <- ne_countries(returnclass = 'sf', scale = 50)
arab <- terra::vect('./gpkg/suit_ara-crn.gpkg')
robu <- terra::vect('./gpkg/suit_rob-crn.gpkg')

# Join both shps into only one --------------------------------------------
arab$type <- 'arabica'
robu$type <- 'robusta'
zone <- rbind(arab[,'type'], robu[,'type'])

# To extract by mask ------------------------------------------------------
rstr <- terra::crop(rstr, zone)
rstr <- terra::mask(rstr, zone)
plot(rstr[[7]])

# Climatology for precipitation -------------------------------------------
prec.avrg <- map(.x = c(paste0('0', 1:9), 10:12), .f = function(m){
  
  ppt <- prec[[grep(paste0('-', m), names(prec))]]
  ppt <- mean(ppt)
  names(ppt) <- glue('prec_{m}')
  return(ppt)
  
})

prec.avrg <- reduce(prec.avrg, c)
prec.avrg
terra::writeRaster(x = prec.avrg, filename = glue('./tif/prec_climatologie/prec_monthly-climatologie.tif'))

# Get Prec driest / wettest -----------------------------------------------

## Semester
prec.avrg
prec.stck <- c(prec.avrg, prec.avrg)

get.sum.max <- function(x){
  
  sma <- sapply(1:12, function(i) sum(x[i:(i+5)], na.rm = TRUE))
  sma <- max(sma)
  return(sma)
  
}

get.sum.min <- function(x){
  
  sma <- sapply(1:12, function(i) sum(x[i:(i+5)], na.rm = TRUE))
  sma <- min(sma)
  return(sma)
  
}

## Wettest semester 
prec.wett <- app(prec.stck, get.sum.max)
prec.wett <- terra::crop(prec.wett, zone)
prec.wett <- terra::mask(prec.wett, zone)

## Driest semester
prec.drys <- app(prec.stck, get.sum.min)
prec.drys <- terra::crop(prec.drys, zone)
prec.drys <- terra::mask(prec.drys, zone)

# Get wet quarter months --------------------------------------------------

# Functions
get_wet_semester_months <- function(x) {
  
  # Calcular sumas para cada ventana de 6 meses
  sums <- sapply(1:12, function(i) sum(x[i:(i+5)], na.rm = TRUE))
  
  # Encontrar la posición del máximo (trimestre más húmedo)
  max_pos <- which.max(sums)
  
  # Obtener los meses del trimestre (ajustar a rango 1-12)
  months <- (max_pos:(max_pos+5)) %% 12
  months[months == 0] <- 12  # Convertir 0 a 12
  
  return(months)
  
}
get_dry_semester_months <- function(x) {
  
  sums <- sapply(1:12, function(i) sum(x[i:(i+5)], na.rm = TRUE))
  min_pos <- which.min(sums)
  
  months <- (min_pos:(min_pos+5)) %% 12
  months[months == 0] <- 12
  
  return(months)
  
}

## To apply the functions (months)
wet_semester <- app(prec.stck, fun = get_wet_semester_months)
dry_semester <- app(prec.stck, fun = get_dry_semester_months)
plot(wet_semester[[1]])

wet_semester <- terra::crop(wet_semester, zone)
wet_semester <- terra::mask(wet_semester, zone)

dry_semester <- terra::crop(dry_semester, zone)
dry_semester <- terra::mask(dry_semester, zone)

# To write the rasters ----------------------------------------------------
terra::writeRaster(x = prec.wett, filename = './tif/prec_climatologie/prec_wettest-semester.tif', overwrite = TRUE)
terra::writeRaster(x = prec.drys, filename = './tif/prec_climatologie/prec_driest-semester.tif', overwrite = TRUE)

terra::writeRaster(x = wet_semester, filename = './tif/prec_climatologie/prec_wettest-semester_months.tif')
terra::writeRaster(x = dry_semester, filename = './tif/prec_climatologie/prec_driest-semester_months.tif')

# To draw the maps --------------------------------------------------------

names(prec.wett) <- glue('prec_wet')
names(prec.drys) <- glue('prec_dry')
names(wet_semester) <- glue('months-wet_{1:6}')
names(dry_semester) <- glue('months-dry_{1:6}')

## 

# -------------------------------------------------------------------------
# Wettest -----------------------------------------------------------------
# -------------------------------------------------------------------------

##
prec.sem <- c(prec.wett, prec.drys)
prec.sem <- terra::as.data.frame(prec.sem, xy = T) %>% as_tibble()
prec.mnt <- c(wet_semester, dry_semester)
prec.mnt <- as.data.frame(prec.mnt, xy = T) %>% as_tibble()

prec.wet <- c(prec.wett, wet_semester[[6]])
prec.wet <- prec.wet %>% terra::as.data.frame(xy = T) %>% as_tibble() %>% gather(var, value, -c(x, y))
prec.wet <- prec.wet %>% mutate(var = ifelse(var == 'prec_wet', 'Prec - Wettest semester', 'Last month of the wettest semester'))

prec.wet.vls <- prec.wet %>% filter(var == 'Prec - Wettest semester')
intr.wet <- classIntervals(var = prec.wet.vls$value, n = 5, style = 'quantile')
intr.wet <- intr.wet$brks
intr.wet <- round(intr.wet, -2)
lbls.wet <- tibble(min = intr.wet[1:5], max = intr.wet[2:6], label = paste0(min, '-', max), class = 1:5)
prec.wet.vls <- mutate(prec.wet.vls, class = findInterval(x = value, vec = intr.wet, all.inside = TRUE))
prec.wet.vls <- inner_join(prec.wet.vls, lbls.wet, by = 'class')
prec.wet.vls <- mutate(prec.wet.vls, label = factor(label, levels = lbls.wet$label))

prec.wet.mnt <- prec.wet %>% filter(var != 'Prec - Wettest semester')
prec.wet.mnt <- prec.wet.mnt %>% mutate(value = as.character(value))
prec.wet.mnt <- prec.wet.mnt %>% mutate(value = factor(value, levels = as.character(1:12)))

## 
g.prec.wet <- ggplot() +
  geom_tile(data = prec.wet.vls, aes(x = x, y = y, fill = label)) +
  scale_fill_viridis_d(direction = -1) +
  geom_sf(data = wrld, fill = NA, col = 'grey80') +
  labs(fill = 'Precipitation (mm)') +
  ggtitle(label = 'Precipitation for the wettest semester') +
  coord_sf(
    crs = 4326,
    xlim = st_bbox(wrld)[c(1, 3)],  
    ylim = c(-30, 30),  
    expand = FALSE        
  ) + 
  labs(x = '', y = '') +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6),
    axis.text.x = element_text(size = 6),
    plot.title = element_text(face = 'bold', hjust = 0.5),
    legend.position = 'bottom', 
    legend.title = element_text(hjust = 0.5, face = 'bold'),
    legend.title.position = 'top',
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

g.prec.wet
ggsave(plot = g.prec.wet, filename = glue('./png/maps/prec_wet-semester.jpg'), units = 'in', width = 9, height = 6, dpi = 300)

##
g.mnth.wet <- ggplot() +
  geom_tile(data = prec.wet.mnt, aes(x = x, y = y, fill = value)) +
  geom_sf(data = wrld, fill = NA, col = 'grey80') +
  labs(fill = 'Month', x = '', y = '') +
  ggtitle(label = 'Last month of the wettest semester') +
  coord_sf(
    crs = 4326,
    xlim = st_bbox(wrld)[c(1, 3)],  
    ylim = c(-30, 30),   
    expand = FALSE        
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6),
    axis.text.x = element_text(size = 6),
    plot.title = element_text(face = 'bold', hjust = 0.5),
    legend.position = 'bottom', 
    legend.title = element_text(hjust = 0.5, face = 'bold'),
    legend.title.position = 'top',
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = T)
  )

g.mnth.wet
ggsave(plot = g.mnth.wet, filename = glue('./png/maps/mnth_wet-semester.jpg'), units = 'in', width = 9, height = 6, dpi = 300)

## Join both maps into only one
gg.wet <- ggarrange(g.prec.wet, g.mnth.wet, nrow = 2, ncol = 1)
ggsave(plot = gg.wet, filename = glue('./png/maps/prec-mnth_wet-semester.jpg'), units = 'in', width = 9, height = 6, dpi = 300)

# -------------------------------------------------------------------------
# Driest ------------------------------------------------------------------
# -------------------------------------------------------------------------

prec.dry.vls <- prec.drys %>% terra::as.data.frame(xy = T) %>% as_tibble()
intr.dry <- classIntervals(var = prec.dry.vls$prec_dry, n = 5, style = 'quantile')
intr.dry <- round(intr.dry$brks, digits = -2)
intr.dry[6] <- 6100
lbls.dry <- tibble(min = intr.dry[1:5], max = intr.dry[2:6], label = paste0(min, '-', max), class = 1:5)

prec.dry.vls <- mutate(prec.dry.vls, class = findInterval(x = prec_dry, vec = intr.dry, all.inside = T))
prec.dry.vls <- inner_join(prec.dry.vls, lbls.dry, by = 'class')
prec.dry.vls <- mutate(prec.dry.vls, label = factor(label, levels = lbls.dry$label))

prec.dry.mnt <- dry_semester[[6]] %>% terra::as.data.frame(xy = T) %>% as_tibble()
prec.dry.mnt <- prec.dry.mnt %>% setNames(c('x', 'y', 'value'))
prec.dry.mnt <- prec.dry.mnt %>% mutate(value = as.character(value))
prec.dry.mnt <- prec.dry.mnt %>% mutate(value = factor(value, levels = as.character(1:12)))

##
g.prec.dry <- ggplot() +
  geom_tile(data = prec.dry.vls, aes(x = x, y = y, fill = label)) +
  scale_fill_viridis_d(direction = -1) +
  geom_sf(data = wrld, fill = NA, col = 'grey80') +
  labs(fill = 'Precipitation (mm)') +
  ggtitle(label = 'Precipitation for the driest semester') +
  coord_sf(
    crs = 4326,
    xlim = st_bbox(wrld)[c(1, 3)],  
    ylim = c(-30, 30),  
    expand = FALSE        
  ) + 
  labs(x = '', y = '') +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6),
    axis.text.x = element_text(size = 6),
    plot.title = element_text(face = 'bold', hjust = 0.5),
    legend.position = 'bottom', 
    legend.title = element_text(hjust = 0.5, face = 'bold'),
    legend.title.position = 'top',
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

g.prec.dry
ggsave(plot = g.prec.dry, filename = glue('./png/maps/prec_dry-semester.jpg'), units = 'in', width = 9, height = 6, dpi = 300)

##
g.mnth.dry <- ggplot() +
  geom_tile(data = prec.dry.mnt, aes(x = x, y = y, fill = value)) +
  geom_sf(data = wrld, fill = NA, col = 'grey80') +
  labs(fill = 'Month', x = '', y = '') +
  ggtitle(label = 'Last month of the driest semester') +
  coord_sf(
    crs = 4326,
    xlim = st_bbox(wrld)[c(1, 3)],  
    ylim = c(-30, 30),   
    expand = FALSE        
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6),
    axis.text.x = element_text(size = 6),
    plot.title = element_text(face = 'bold', hjust = 0.5),
    legend.position = 'bottom', 
    legend.title = element_text(hjust = 0.5, face = 'bold'),
    legend.title.position = 'top',
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = T)
  )

g.mnth.dry
ggsave(plot = g.mnth.dry, filename = glue('./png/maps/mnth_dry-semester.jpg'), units = 'in', width = 9, height = 6, dpi = 300)

## Join both maps into only one
gg.dry <- ggarrange(g.prec.dry, g.mnth.dry, nrow = 2, ncol = 1)
ggsave(plot = gg.dry, filename = glue('./png/maps/prec-mnth_dry-semester.jpg'), units = 'in', width = 9, height = 6, dpi = 300)


# Manually verification  --------------------------------------------------
mtrx <- terra::as.data.frame(prec.stck, xy = T)
mtrx$gid <- 1:nrow(mtrx)

get.wet.dry <- function(fila, df) {
  
  # df <- mtrx
  # fila <- 1
  
  # Extraer solo las columnas de precipitación (24 meses)
  cat('Row: ', fila, '\n')
  prec <- unlist(df[fila, grep('^prec_', colnames(df))])
  df[1,] %>% dplyr::select(x, y)
  
  # Duplicar para cubrir ciclos anuales
  prec_24 <- c(prec, prec[1:12])
  
  # Inicializar resultados
  max_suma <- -Inf
  min_suma <- Inf
  
  # Calcular todas las combinaciones de 6 meses consecutivos
  for (i in 1:12) {
    # Sumar 6 meses consecutivos
    suma <- sum(prec_24[i:(i + 5)], na.rm = TRUE)
    
    # Actualizar máximos y mínimos
    if (suma > max_suma) max_suma <- suma
    if (suma < min_suma) min_suma <- suma
  }
  
  rsl <- tibble(lon = as.numeric(df[fila,c('x')]),
                lat = as.numeric(df[fila,c('y')]),
                wet = max_suma, dry = min(min_suma))
  
  ## Finish
  cat('Done!\n')
  return(rsl)
  
}

terra::extract(prec.wett, df[1,c('x', 'y')])
terra::extract(prec.drys, df[1,c('x', 'y')])

rsl
