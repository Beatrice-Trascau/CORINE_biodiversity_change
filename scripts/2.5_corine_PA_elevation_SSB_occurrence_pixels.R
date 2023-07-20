##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##- 2.5_corine_PA_elevation_SSB_occurrence_pixels ##
##------------------------------------------------##

#This script contains code that combines the CORINE land cover data with PA network, elevation, and SSB
#grid data across Norway

# 0. PACKAGES ----
library(here)
library(wdpar)
library(terra)
library(sf)
library(tidyverse)
library(dplyr)

# 1. LOAD DATA ----

## 1.1. Download data from box (if needed) ----

#Add download links
norway_corine <- ("https://ntnu.box.com/shared/static/ugwtizqcr3a4t4vgxj6qu0rh0h54rtoh.tif")
ruter500m_Norge <- ("https://ntnu.box.com/shared/static/p8896x2epq4bcmfhorsb5qn2m8mxo5ko.zip")
dtm50 <- ("https://ntnu.box.com/shared/static/zqrn2zi85d7nm7aaseuvsetazi4qn5uy.zip")

#Download the file to file
 #land cover
download.file(norway_corine, here("data",
                                  "corine_modified_classes_stack.tif"))

 #SSB grids
download.file(ruter500m_Norge, here("data", "raw_data",
                                     "ruter500m_Norge.zip"))

 #elevation
download.file(dtm50, here("data", "raw_data",
                          "dtm50.zip"))
#Unzip files
 #SSB grids
unzip("ruter500m_Norge.zip")

 #elevation
unzip("dtm50.zip")

## 1.2. Download PA network from WDAP database ----
nor_raw_pa_data <- wdpa_fetch("Norway",
                              wait = TRUE) #downloaded as sf collection


## 1.3 Read in data ----
norway_corine <- rast(here("data",
                                  "corine_modified_classes_stack.tif"))

ssb_grids <- vect(here("data", "raw_data",
                              "ruter500m_Norge.shp"))

elevation <- vect(here("data", "raw_data",
                              "dtm50", "metadata",
                              "dtm50_Tileinndeling.shp"))

#2. PREPARE THE PA, ELEVATION, AND SSB GRID DATA ----
#Prepare = clean, cut, reproject to match CORINE and transform in the correct object

##2.1. PA Network Data ----

#Clean the data
nor_pa_data <- wdpa_clean(nor_raw_pa_data)

#Download Norway boundary from Global Administrative Areas database -  needed to cut the PA data to coastline
norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")

#Convert norway shapefile to sf object
nor_boundary_data <- st_as_sf(norway)

#Repair any geometry issues, dissolve the border, reproject to same
#coordinate system as the protected area data, and repair the geometry again
nor_boundary_data <- 
  nor_boundary_data |>
  st_set_precision(1000) |>
  sf::st_make_valid() |>
  st_set_precision(1000) |>
  st_combine() |>
  st_union() |>
  st_set_precision(1000) |>
  sf::st_make_valid() |>
  st_transform(st_crs(nor_pa_data)) |>
  sf::st_make_valid()

#Subset the PA network to only include terrestrial PAs
nor_pa_terrestrial <- nor_pa_data |>
  filter(MARINE == "terrestrial")


#Clip Norway's protected areas to the coastline
nor_pa_terrestrial <-
  nor_pa_terrestrial |>
  filter(MARINE == "terrestrial") |>
  st_intersection(nor_boundary_data) |>
  rbind(nor_pa_terrestrial |>
          filter(MARINE == "terrestrial") |>
          st_difference(nor_boundary_data)) |>
  rbind(nor_pa_terrestrial |> 
          filter(!MARINE == "terrestrial"))

#Plot to inspect
plot(nor_pa_terrestrial)


#Convert sf object to vector
norway_PA_vect <- vect(nor_pa_terrestrial)

#Project norway_PA to same projection as corine_norway
norway_PA <- terra::project(norway_PA_vect,
                            "epsg:3035")

## 2.2. SSB Grid Data ----
#Reproject to match corine projection and extent
norway_ssb_grids <- terra::project(ssb_grids,
                                   "epsg:3035")

#Check the projection worked as expected
#projection
crs(norway_ssb_grids, proj = TRUE) #+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs
ext(norway_ssb_grids) #SpatExtent : 3999402.08962045, 5135027.9894255, 3851891.81614406, 5439794.32227839 (xmin, xmax, ymin, ymax)
plot(norway_ssb_grids)

## 2.3. Elevation ----
#Reproject to match corine projection and extent
norway_elevation <- terra::project(elevation,
                                   "epsg:3035")

#Check the projection worked as expected
#projection
crs(norway_elevation, proj = TRUE) #+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs
ext(norway_elevation) #SpatExtent : 3973848.11387648, 5161150.18175419, 3815190.99674517, 5483289.51043574 (xmin, xmax, ymin, ymax)
plot(norway_elevation)

#3. COMBINE LAND COVER + PA + ELVATION + SSB GRIDS ----

##3.1. PA Network ----

#Rasterize PA areas to norway_corine
PA_raster <- terra::rasterize(norway_PA,
                              norway_corine[[4]])
##3.2. SSB Grids ----

#Rasterize SSB grids to norway_corine
SSB_raster <- terra::rasterize(norway_ssb_grids,
                               norway_corine[[4]],
                               field = "SSBid")

##3.3. Elevation ----

#rasterize elevation to norway_corine
elevation_raster <- terra::rasterize(norway_elevation,
                                     norway_corine[[4]])


##3.4. Combine all data in a single object ----

#Replace NA values with a specific number (to help with extracting the values)
norway_corine[is.na(norway_corine)] <- -9999
PA_raster[is.na(PA_raster)] <- -9999
#ssb grids
SSB_raster[is.na(SSB_raster)] <- -9999
#elevation
elevation_raster[is.na(elevation_raster)] <- -9999


#Extract values from CORINE and rasterization outputs
#corine
land_cover_values_2000 <- values(norway_corine[[1]])
land_cover_values_2006 <- values(norway_corine[[2]])
land_cover_values_2012 <- values(norway_corine[[3]])
land_cover_values_2018 <- values(norway_corine[[4]])
#PA netwok
norway_pa_values <- values(PA_raster)
#ssb grids
norway_ssb_values <- values(SSB_raster)
#elevation
norway_elevation_values <- values(elevation_raster)

#Combine rasters in dataframe
#only PA for now - add the elevation and SSB data after
all_pixel_data <- data.frame(x = xFromCol(norway_corine[[4]],
                                    1:ncell(norway_corine[[4]])),
                       y = yFromCell(norway_corine[[4]],
                                     1:ncell(norway_corine[[4]])),
                       land_cover_values_2000 = land_cover_values_2000,
                       land_cover_values_2006 = land_cover_values_2006,
                       land_cover_values_2012 = land_cover_values_2012,
                       land_cover_values_2018 = land_cover_values_2018,
                       protected_area = norway_pa_values,
                       norway_ssb_values = norway_ssb_values,
                       norway_elevation_values = norway_elevation_values)


## 3.5. Clean dataframe containing the land cover, PA, elevation and SSB grid data ----
#Check names of df
names(all_pixel_data)

#Replace the -9999 value with NA (which is what it was from the begining)
all_pixel_data <- all_pixel_data |>
  rename("land_cover_2000" = "U2006_CLC2000_V2020_20u1",
         "land_cover_2006" = "U2012_CLC2006_V2020_20u1",
         "land_cover_2012" = "U2018_CLC2012_V2020_20u1",
         "land_cover_2018" = "U2018_CLC2018_V2020_20u1",
         "PA_status" = "layer",
         "elevation_m" = "layer.1") |>
  mutate(land_cover_2000 = na_if(land_cover_2000,
                                 land_cover_2000 == -9999),
         land_cover_2006 = na_if(land_cover_2006,
                                 land_cover_2006 == -9999),
         land_cover_2012 = na_if(land_cover_2012,
                                 land_cover_2012 == -9999),
         land_cover_2018 = na_if(land_cover_2018,
                                 land_cover_2018 == -9999),
         PA_status = case_when(PA_status == NA ~ "N",
                               PA_status != NA ~ "Y"),
         SSBid = na_if(SSBid,
                       SSBid == -9999),
         elevation_m = na_if(elevation_m,
                             elevation_m == -9999))

#Remove rows with NA in the land cover fields
clean_pixel_data <- all_pixel_data |>
  filter(land_cover_2000 == -9999 |
           land_cover_2006 == -9999 |
           land_cover_2012 == -9999 |
           land_cover_2018 == -9999 ) 

#Replace land cover numerical values with the corresponding categories
#have to be done stepwise because of memory issues
#land_cover_2000
clean_pixel_data <- clean_pixel_data |>
  mutate(land_cover_2000 = case_when(land_cover_2000 == 1 ~ "urban_fabric",
                                     land_cover_2000 == 80 ~ "complex_agriculture",
                                     land_cover_2000 == 103 ~ "agriculture_natural_veg",
                                     land_cover_2000 == 250 ~ "forests",
                                     land_cover_2000 == 380 ~ "moors_heath_grass",
                                     land_cover_2000 == 590 ~ "trans_woodland_shrub",
                                     land_cover_2000 == 711 ~ "sparse_vegetation"))
#land_cover_2006
clean_pixel_data <- clean_pixel_data |>
  mutate(land_cover_2006 = case_when(land_cover_2006 == 1 ~ "urban_fabric",
                                     land_cover_2006 == 80 ~ "complex_agriculture",
                                     land_cover_2006 == 103 ~ "agriculture_natural_veg",
                                     land_cover_2006 == 250 ~ "forests",
                                     land_cover_2006 == 380 ~ "moors_heath_grass",
                                     land_cover_2006 == 590 ~ "trans_woodland_shrub",
                                     land_cover_2006 == 711 ~ "sparse_vegetation"))

#land_cover_2012
clean_pixel_data <- clean_pixel_data |>
  mutate(land_cover_2012 = case_when(land_cover_2012 == 1 ~ "urban_fabric",
                                     land_cover_2012 == 80 ~ "complex_agriculture",
                                     land_cover_2012 == 103 ~ "agriculture_natural_veg",
                                     land_cover_2012 == 250 ~ "forests",
                                     land_cover_2012 == 380 ~ "moors_heath_grass",
                                     land_cover_2012 == 590 ~ "trans_woodland_shrub",
                                     land_cover_2012 == 711 ~ "sparse_vegetation"))
 #land_cover_2018
clean_pixel_data <- clean_pixel_data |>
  mutate(land_cover_2018 = case_when(land_cover_2018 == 1 ~ "urban_fabric",
                                     land_cover_2018 == 80 ~ "complex_agriculture",
                                     land_cover_2018 == 103 ~ "agriculture_natural_veg",
                                     land_cover_2018 == 250 ~ "forests",
                                     land_cover_2018 == 380 ~ "moors_heath_grass",
                                     land_cover_2018 == 590 ~ "trans_woodland_shrub",
                                     land_cover_2018 == 711 ~ "sparse_vegetation"))
         

#Save the dataframe
write.csv(all_pixel_data,
          here("data", "combine_pixel_data.csv"),
          overwrite = T)
