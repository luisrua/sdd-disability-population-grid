# DISABILITY POPULATION GRID #
# TONGA CENSUS EXPERIMENT
# Luis de la Rua - June 2025

# DATA INPUT
# 2021 TONGA PHC / Population projections and estimates in PDH.stat 
# https://stats-data-viewer.pacificdata.org/?chartId=f492d0e9-8668-4fa9-bfb4-fdc6170e2533

# SETTINGS =========================
# setup script that loads libraries and resolve conflict between functions 
source("setup.R")

# Country code to name output data and other files
country <- 'TON'
year <- "2025"

# Paths
# Same structure as with the Population Grid update.
# we keep using what we have in 2023 update as it is the latest data available for the input
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/")
output <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/Vulnerability experiment/",country,"/Disab/")



# 1. CENSUS DATA MANIPULATION =================================================
# Import census data.
# Import datasets directly from NADA 
stata_files <- list.files (glue("C:/Users/{user}/OneDrive - SPC/NADA/Tonga/SPC_TON_2021_PHC_v01_M/Data/Original"),
                           pattern = "*.dta", full.names = T)
stata_files

hous <- read_stata(stata_files[3])
pop <- read_stata(stata_files[4])

# Get labels function is defined in setup script
get_labels(hous)
view(get_labels(pop))

# Get coordinates and Admin boundaries codes from the hh module
hous_lite <- hous %>% 
  select(c(interview__key,block_no, census_unit, district, village, buildingGPS__Latitude,buildingGPS__Longitude ))
pop_lite <- pop %>% 
  select(c(interview__key, sex, age, seeing, hearing, walking, remembering, selfcare, communication, 
           some_disab, alot_disab, cannot_disab)) %>% 
  filter(age>4)

# Use directly ILO alot disability cutoff
pop_disab <- pop_lite %>% 
  filter(alot_disab == 1)


# Add geographic info and admin bound codes
pop_disab_geo <- merge(pop_disab, hous_lite, by = "interview__key", all.x = T)
# Run tabulations to double check with census results (ask Elsie)
results_disrtict <- pop_disab_geo %>% 
  group_by(district) %>% 
  summarize(
    total_disab = n(),
    total_dis_pop = nrow(pop_disab_geo),
    disab_rate = total_disab / total_dis_pop * 100
  ) %>%  print()
  
sum(results_disrtict$disab_rate)

# 2. RASTERIZE DATASET =========================================================

# Convert into spatial using coordinates
# This is our resource to distribute spatially the population
pts <- st_as_sf(pop_disab_geo, coords = c("buildingGPS__Longitude", "buildingGPS__Latitude"), crs = 4326)

# 2.2 Extent ----

# Bring Zone layer to avoid points excluded
zone <- st_read(paste0(dd,"layers/zone.gpkg"))

# 2.3 Blank raster ----
# generate blank raster 100m

rast100m <- raster()
extent(rast100m) <- extent(zone)
res(rast100m) <- 0.001
rast100m

# test with 1km resolution 
rast1k <- raster()
extent(rast1k) <- extent(zone)
res(rast1k) <- 0.01
rast1k

# set 4326 projection
crs(rast100m) <- crs(pts)
rast100m

# Produce raster using the census data
rastpop_disab_2021_100m <- rasterize(pts,rast100m, 'alot_disab', fun=sum )

rastpop_disab_2021_1k <- rasterize(pts,rast1k, 'alot_disab', fun=sum )

# Calculate totals for double check
total_disab_100m <- cellStats(rastpop_disab_2021_100m, sum, na.rm = TRUE) %>%  print()
total_disab_1k <- cellStats(rastpop_disab_2021_1k, sum, na.rm = TRUE) %>%  print()

hist(rastpop_disab_2021_100m, na.rm=T)
hist(rastpop_disab_2021_1k, na.rm=T)

hist(rastpop_disab_2021_1k, breaks = 50, main = "Histogram of Raster Values 1km", xlab = "Value")
hist(rastpop_disab_2021_100m, breaks = 20, main = "Histogram of Raster Values 100m", xlab = "Value")

# Export rasters
writeRaster(rastpop_disab_2021_100m,paste0(output,"rastpop_disab_2021_100m.tif"), overwrite = T)
writeRaster(rastpop_disab_2021_1k,paste0(output,"rastpop_disab_2021_1k.tif"), overwrite = T)
# 3. MAP RESULTS SO THEY CAN BE REVIEWED ======================================


raster_100m <- rast(paste0(output,"rastpop_disab_2021_100m.tif"))
raster_1km <- rast(paste0(output,"rastpop_disab_2021_1k.tif"))
# options(viewer = NULL) # force map on browser
library(RColorBrewer)

# Corrected palette for the 100m raster
colorPalette100m <- colorBin(
  palette = "YlOrBr",
  domain = values(raster_100m),
  bins = 5, # Creates 5 equal-interval bins
  na.color = "transparent"
)

# Corrected palette for the 1km raster
colorPalette1km <- colorBin(
  palette = "YlOrBr",
  domain = values(raster_1km),
  bins = 5, # Creates 5 equal-interval bins
  na.color = "transparent"
)

# title
title_text <- "<div style='font-family: Calibri, sans-serif; font-size: 22px; font-weight: bold;'>TONGA - Disab grid experiment</div>"

map_sat <- 
  leaflet((options = leafletOptions(viewer = NULL))) %>%
  addControl(html = title_text, position = "topright") %>% 
  addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
  # 100m
  addRasterImage(raster_100m, 
                 colors = colorPalette100m, 
                 opacity = 0.8,
                 group = "100m Resolution") %>%
  # 1km
  addRasterImage(raster_1km, 
                 colors = colorPalette1km, 
                 opacity = 0.8,
                 group = "1km Resolution") %>%
  # Add layer toggle
  addLayersControl(
    baseGroups = c("1km Resolution", "100m Resolution"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Add legend (shared)
  addLegend(
    pal = colorPalette100m,
    values = values(raster_100m),
    title = "Disability - 100m Res.<br>(pers./ha)",
    position = "bottomright",
    opacity = 0.6
  ) %>% 
  addLegend(
    pal = colorPalette1km,
    values = values(rast1k),
    title = "Disability - 1km Res.<br>(pers./sq.km)",
    position = "bottomleft",
    opacity = 0.6
  )
map_sat

# Produce a map as html that can be shared as it is selfcontained using "htmlwidgets" library
# Save it into github so it can be accessed ????
saveWidget(map_sat, file ="ton_disabgrid.html", selfcontained = TRUE)

