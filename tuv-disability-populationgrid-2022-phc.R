# DISABILITY POPULATION GRID #
# TUVALU CENSUS EXPERIMENT
# Luis de la Rua - November 2025

# DATA INPUT
# 2023 TUVALU PHC / Population projections and estimates in PDH.stat 
# https://stats-data-viewer.pacificdata.org/?chartId=f492d0e9-8668-4fa9-bfb4-fdc6170e2533

# SETTINGS =========================
# setup script that loads libraries and resolve conflict between functions 
source("setup.R")

# Country code to name output data and other files
country <- 'TUV'
year <- "2022"

# Paths
# Same structure as with the Population Grid update.
# we keep using what we have in 2023 update as it is the latest data available for the input
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/")
output <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/Vulnerability experiment/",country,"/Disab/")



# 1. CENSUS DATA MANIPULATION =================================================
# Import census data.
# Import datasets directly from NADA 
stata_files <- list.files (glue("C:/Users/{user}/OneDrive - SPC/NADA/Tuvalu/SPC_TUV_2022_PHC_v01_M/Data/Original/Final"),
                           pattern = "*.dta", full.names = T)
stata_files

hous <- read_stata(stata_files[15])
pop <- read_stata(stata_files[17])

# Get labels function is defined in setup script
get_labels(hous)
view(get_labels(pop))

# Get coordinates and Admin boundaries codes from the hh module
hous_lite <- hous %>% 
  select(c(interview__key, ea, village, island, buildingGPS__Latitude, buildingGPS__Longitude ))
pop_lite <- pop %>% 
  select(c(interview__key, sex, age, seeing, hearing, walking, remembering, selfcare, communicating,ilo_dsb_details )
         )%>% 
  filter(age>4)

# Use directly ILO alot disability cutoff
pop_disab_ilo <- pop_lite %>% 
  filter(ilo_dsb_details %in% c(3,4))

pop_disab_disquest <- pop_lite %>% 
  filter(seeing %in% c(3,4)|
         hearing %in% c(3,4)|
         walking %in% c(3,4)|
         remembering %in% c(3,4)|
         selfcare %in% c(3,4)|
         communicating %in% c(3,4)
  )

# Double check ilo with disability questions to see if filters match
nrow(pop_disab_ilo)
nrow(pop_disab_disquest)

# let's keep ilo one
pop_disab <- pop_disab_ilo %>% 
  select(c(interview__key, sex, age,ilo_dsb_details)) %>% 
  mutate(disab = 1)

# Add geographic info and admin bound codes
pop_disab_geo <- merge(pop_disab, hous_lite, by = "interview__key", all.x = T)

# Run tabulations to double check with census results (ask Elsie)
results_island <- pop_disab_geo %>% 
  group_by(island) %>% 
  summarize(
    total_disab = n(),
    total_dis_pop = nrow(pop_disab_geo),
    disab_rate = total_disab / total_dis_pop * 100
  ) %>%  print()

sum(results_island$disab_rate)

# 2. RASTERIZE DATASET =========================================================

# 2.1 Convert into spatial using coordinates ----
# This is our resource to distribute spatially the population
pts <- st_as_sf(pop_disab_geo, coords = c("buildingGPS__Longitude", "buildingGPS__Latitude"), crs = 4326)

# 2.2 Extent ----

# Bring Zone layer to avoid points excluded
zone <- st_read(paste0(dd,"layers/zone_4326.shp"))
zone <- project(zone, "EPSG:3857")

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

# 2.5  Produce raster using the census data ----
rastpop_disab_100m <- rasterize(pts,rast100m, 'disab', fun=sum )

rastpop_disab_1k <- rasterize(pts,rast1k, 'disab', fun=sum )

# Calculate totals for double check
total_disab_100m <- cellStats(rastpop_disab_100m, sum, na.rm = TRUE) %>%  print()
total_disab_1k <- cellStats(rastpop_disab_1k, sum, na.rm = TRUE) %>%  print()

hist(rastpop_disab_100m, na.rm=T)
hist(rastpop_disab_1k, na.rm=T)

hist(rastpop_disab_1k, breaks = 50, main = "Histogram of Raster Values 1km", xlab = "Value")
hist(rastpop_disab_100m, breaks = 20, main = "Histogram of Raster Values 100m", xlab = "Value")

# Export rasters
writeRaster(rastpop_disab_100m,paste0(output,"rastpop_disab_100m.tif"), overwrite = T)
writeRaster(rastpop_disab_1k,paste0(output,"rastpop_disab_1k.tif"), overwrite = T)


# 3. MAP RESULTS SO THEY CAN BE REVIEWED ======================================

# 3.1 Test to produce rasters in 3857 for mapping ----
pts_3857 <- st_transform(pts, "EPSG:3857")
zone_3857 <- st_transform(zone, "EPSG:3857")

# generate blank raster 100m

rast100m_3857 <- raster()
extent(rast100m_3857) <- extent(pts_3857)
crs(rast100m_3857) <- crs(pts_3857)
res(rast100m_3857) <- 100
rast100m_3857

# test with 1km resolution 
rast1km_3857 <- raster()
crs(rast1km_3857) <- crs(pts_3857)
extent(rast1km_3857) <- extent(pts_3857)
res(rast1km_3857) <- 1000
rast1km_3857

rastpop_disab_100m_3857 <- rasterize(pts_3857,rast100m_3857, 'disab', fun=sum )

rastpop_disab_1k_3857 <- rasterize(pts_3857,rast1km_3857, 'disab', fun=sum )


# options(viewer = NULL) # force map on browser
library(RColorBrewer)

colorPalette1km <- colorBin(
  palette = "YlOrBr",
  domain = values(rastpop_disab_1k_3857),
  bins = 5,
  na.color = "transparent" # Change this to "grey" or a hex code like "#808080"
)

colorPalette100m <- colorBin(
  palette = "YlOrBr",
  domain = values(rastpop_disab_100m_3857),
  bins = 5,
  na.color = "transparent" # Change this to "grey" or a hex code like "#808080"
)

# title
title_text <- "<div style='font-family: Calibri, sans-serif; font-size: 22px; font-weight: bold;'>TUVALU - Disab grid experiment</div>"

map_sat_3857 <- leaflet() %>% 
  addControl(html = title_text, position = "topright") %>% 
  addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
  
  addRasterImage(
    rastpop_disab_100m_3857, colors = colorPalette100m, opacity = 0.8,
    group = "100m Resolution"
  ) %>%

  addRasterImage(
    rastpop_disab_1k_3857, colors = colorPalette1km, opacity = 0.8,
    group = "1km Resolution"
  ) %>%
  
  addLayersControl(
    baseGroups   = c("Esri World Imagery"),
    overlayGroups = c("100m Resolution", "1km Resolution"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  # Add legend (shared)
  addLegend(
    pal = colorPalette100m,
    values = values(rastpop_disab_100m_3857),
    title = "Disability - 100m Res.<br>(pers./ha)",
    position = "bottomright",
    opacity = 0.6
  ) %>% 
  addLegend(
    pal = colorPalette1km,
    values = values(rastpop_disab_1k_3857),
    title = "Disability - 1km Res.<br>(pers./sq.km)",
    position = "bottomleft",
    opacity = 0.6
  )%>%
  setView(
    lng = 179.19,
    lat = -8.52,
    zoom = 13
  )
map_sat_3857



# Produce a map as html that can be shared as it is selfcontained using "htmlwidgets" library
# Save it into github so it can be accessed ????
saveWidget(map_sat_3857, file =paste0(output,"tuv_disabgrid.html"), selfcontained = TRUE)