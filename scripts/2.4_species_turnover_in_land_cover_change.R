##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##--- 2.4_species_turnover_in_land_cover_change --##
##------------------------------------------------##

#This script contains code to calculate the turnover of species in pixels undergoing land cover changes between 1997 and 2018

# 0. LOAD PACKAGES ----
library(here)
library(data.table)
library(terra)
library(sf)
library(epm)
library(tidyverse)
library(dplyr)
library(ggplot2)

# 1. LOAD DATA ----

## 1.1. Download cleaned occurrence records and modified corine stack from box ----

#Add download link
cleaned_occurrences <- ("https://ntnu.box.com/shared/static/cgjbsfs24m31uov6ir4vkkmw6cs7ga36.txt")
norway_corine <- ("https://ntnu.box.com/shared/static/s406n4td0cmtfjsxwllz8klnkvksiyul.tif")

#Download the file
download.file(cleaned_occurrences, "cleaned_occurrences.txt")
download.file(norway_corine, "corine_modified_classes_stack.tif")

## 1.2. Read in the data ----
clean_occurrences <- fread(here::here("data", 
                                "cleaned_occurrences.txt"))

norway_corine <- rast(here::here("data", 
                           "corine_modified_classes_stack.tif"))


# 2. COMBINE OCCUURENCES AND LAND COVER DATA ----
## 2.1. Convert occurrence df to and sf object and transform the CRS ----
#Get CRS from the corine layers
crs_corine <- crs(norway_corine)
#Convert the occurrence df to sf object
occurrences_sf <- st_as_sf(clean_occurrences,
                           coords = c("decimalLongitude", "decimalLatitude"),
                           crs = crs_corine)

## 2.2. Assign pixelnr to each record ----
#Give all records a unique ID for future reference
occurrences_sf$ID <- c(1:nrow(occurrences_sf))

#Convert norway_corine SpatRaster to sf polygons
corine_2000_polygon <- rasterToGrid(norway_corine[[1]],
                                    target = epmGrid,
                                    fun = "mean")

#Assign pixel number to each record for all the point datasets
occurrences_sf$Pixelnr <- as.data.frame(st_drop_geometry(st_join(occurrences_sf,
                                                                 norway_corine[[1]],
                                                                 join = st_intersects)["Pixelnr"]))$Pixelnr
