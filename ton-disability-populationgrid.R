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
  select(c(interview__key, sex, age, seeing, hearing, walking, remembering, selfcare, communication)) %>% 
  filter(age>4)


# Note: filter disability data and keep coordinates and EA codes
# Filter population by disability ILO criteria
# A lot of difficulties (at least one domain scored “a lot of difficulties” or “cannot do at all” 
# in at least one of the six domains).

pop_lite <- pop_lite %>%
  mutate(
    # Detailed disability status
    ilo_dsb_details = case_when(
      communication == 1 & hearing == 1 & remembering == 1 &
        seeing == 1 & walking == 1 & selfcare == 1 ~ 1,
      communication == 2 | hearing == 2 | remembering == 2 |
        seeing == 2 | walking == 2 | selfcare == 2 ~ 2,
      communication == 3 | hearing == 3 | remembering == 3 |
        seeing == 3 | walking == 3 | selfcare == 3 ~ 3,
      communication == 4 | hearing == 4 | remembering == 4 |
        seeing == 4 | walking == 4 | selfcare == 4 ~ 4,
      TRUE ~ 1  # Treat NAs as "No difficulty"
    ),
    
    # Aggregate disability status
    ilo_dsb_aggregate = case_when(
      ilo_dsb_details %in% c(1, 2) ~ 1,  # Persons without disability
      ilo_dsb_details %in% c(3, 4) ~ 2, # Persons with disability
      TRUE ~ NA_real_
    )
  )
table(pop_lite$ilo_dsb_aggregate)

# Keep population with disability
pop_disab <- pop_lite %>% 
  filter(ilo_dsb_aggregate == 2) %>% 
  mutate(with_disab = ifelse(ilo_dsb_aggregate == 2,1,0)) %>% 
  print()
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
rastpop_disab_2021_100m <- rasterize(pts,rast100m, 'with_disab', fun=sum )

rastpop_disab_2021_1k <- rasterize(pts,rast1k, 'with_disab', fun=sum )

hist(rastpop_disab_2021_100m, na.rm=T)
hist(rastpop_disab_2021_1k, na.rm=T)

hist(rastpop_disab_2021_1k, breaks = 50, main = "Histogram of Raster Values 1km", xlab = "Value")
hist(rastpop_disab_2021_100m, breaks = 20, main = "Histogram of Raster Values 100m", xlab = "Value")

# Export rasters
writeRaster(rastpop_disab_2021_100m,paste0(output,"rastpop_disab_2021_100m.tif"))
writeRaster(rastpop_disab_2021_1k,paste0(output,"rastpop_disab_2021_1k.tif"))
# 3. MAP RESULTS SO THEY CAN BE REVIEWED ======================================


raster_data <- rast(paste0(output,"rastpop_disab_2021_100m.tif"))

# options(viewer = NULL) # force map on browser

# Create a color palette with transparent NA values
colorPalette <- colorNumeric(
  palette = "Oranges",  
  domain = values(raster_data),
  na.color = "transparent"  # Specify transparent color for NA values
)
# title
title_text <- "<div style='font-family: Calibri, sans-serif; font-size: 22px; font-weight: bold;'>TONGA - 2025 Population Grid</div>"

# to check tile providers https://leaflet-extras.github.io/leaflet-providers/preview/

map_labels <- leaflet() %>%
  addControl(html = title_text, position = "topright") %>% 
  addProviderTiles("CartoDB.DarkMatter") %>%  # Choose your preferred tile provider
  addRasterImage(raster_data, 
                 colors = colorPalette, 
                 opacity = 0.5) %>%
  addLegend(
    position = "topright",  # Change legend position to "topright"
    pal = colorPalette,  # Use the reversed color palette
    values = values(raster_data),
    title = "Population Density <br> (pers./ha)",
    opacity = 0.5
  )

map_labels

map_sat <- leaflet((options = leafletOptions(viewer = NULL))) %>%
  addControl(html = title_text, position = "topright") %>% 
  addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
  addRasterImage(raster_data, 
                 colors = colorPalette, 
                 opacity = 0.8) %>%
  addLegend(
    position = "topright",  # Change legend position to "topright"
    pal = colorPalette,  # Use the reversed color palette
    values = values(raster_data),
    title = "Population Density <br> (pers./ha)",
    opacity = 0.8
  )
map_sat

# Produce a map as html that can be shared as it is selfcontained using "htmlwidgets" library

saveWidget(map_sat, file = paste0(dmap, "ton_ppg_2025.html"), selfcontained = TRUE)

