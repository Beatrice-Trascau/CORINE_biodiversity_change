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

# 1. LOAD DATA ----

## 1.1. Download data from box (if needed) ----

#Add download links
norway_corine <- ("https://ntnu.box.com/shared/static/ugwtizqcr3a4t4vgxj6qu0rh0h54rtoh.tif")
ruter500m_Norge <- ("https://ntnu.box.com/shared/static/p8896x2epq4bcmfhorsb5qn2m8mxo5ko.zip")

#Download the file to file
download.file(norway_corine, here("data",
                                  "corine_modified_classes_stack.tif"))

download.file(ruter500m_Norge, here("data",
                                     "ruter500m_Norge.zip"))

#Unzip files


## 1.2. Download PA network from WDAP database ----
nor_raw_pa_data <- wdpa_fetch("Norway",
                              wait = TRUE) #downloaded as sf collection


## 1.3 Read in data ----
norway_corine <- rast(here("data",
                                  "corine_modified_classes_stack.tif"))



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
norway_PA_vect <- vect(nor_pa_terrestrial,
                    crs = "+proj=longlat")

#Project norway_PA to same projection as corine_norway
norway_PA <- terra::project(norway_PA_vect,
                            "epsg:3035")

#3. COMBINE LAND COVER + PA + ELVATION + SSB GRIDS ----

##3.1. PA Network ----

#Rasterize PA areas to corine_norway
PA_raster <- terra::rasterize(norway_PA,
                              corine_norway[[4]])

##3.2. Elevation ----

##3.3. SSB Grids ----

##3.4. Combine all data in a single object ----

#Replace NA values with a specific number (to help with extracting the values)
corine_norway[[4]][is.na(corine_norway[[4]])] <- -9999
pa_raster[is.na(pa_raster)] <- -9999
#elevtion
#ssb grids


#Extract values from CORINE and the PA rasterization output
land_cover_values <- values(corine_norway[[4]])
norway_pa_values <- values(pa_raster)
#elevtion
#ssb grids

#Combine rasters in dataframe
#only PA for now - add the elevation and SSB data after
all_pixel_data <- data.frame(x = xFromCol(corine_norway[[4]],
                                    1:ncell(corine_norway[[4]])),
                       y = yFromCell(corine_norway[[4]],
                                     1:ncell(corine_norway[[4]])),
                       land_cover_value = land_cover_values,
                       protected_area = norway_pa_values)


## 3.5. Clean dataframe containing the land cover, PA, elevation and SSB grid data ----
#Replace the -9999 value with NA (which is what it was from the begining)
#maybe I should remove the rows that have NA for land cover completely
all_pixel_data <- all_pixel_data |>
  mutate(land_cover_values = case_when(land_cover_values == -9999 ~ NA,
                                       land_cover_values != -9999 ~ land_cover_values),
         protected_area = case_when(protected_area == NA ~ "N"
                                    protected_area != NA ~ "Y"))



