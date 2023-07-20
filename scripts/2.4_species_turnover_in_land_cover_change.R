##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##--- 2.4_species_turnover_in_land_cover_change --##
##------------------------------------------------##

#This script contains code to calculate the turnover of species in pixels undergoing land cover changes between 1997 and 2018

# 0. LOAD PACKAGES ----
library(terra)
library(data.table)
library(sf)
library(here)

# 1. LOAD DATA ----

## 1.1. Download cleaned occurrence records and modified corine stack from box ----

#Add download link
cleaned_occurrences <- ("https://ntnu.box.com/shared/static/cgjbsfs24m31uov6ir4vkkmw6cs7ga36.txt")
norway_corine <- ("https://ntnu.box.com/shared/static/s406n4td0cmtfjsxwllz8klnkvksiyul.tif")

#Download the file
download.file(cleaned_occurrences, "cleaned_occurrences.txt")
download.file(norway_corine, "corine_modified_classes_stack.tif")


## 1.2. Read in the data ----
norway_corine <- rast(here("data", 
                           "corine_modified_classes_stack.tif"))

#Load species occurrence records
clean_occurrences <- fread(here("data", 
                                "cleaned_occurrences.txt"))

# 2. PREPARE DATA FOR ANALYSIS ----

## 2.1. Raster with unique ID for each cell ----

#Create raster with the same properties as corine
land_cover_id <- norway_corine[[1]]

#Assign each cell a unique ID from 1 to ncell
land_cover_id[] <- 1:ncell(norway_corine[[1]])

## 2.2. Two dataframes for the before (1997-2000) and after (2006-2009) periods ----
#Subset df for the 1997-2000 and 2006-2009 period
#1997-2000
occurrences1997.2000 <- clean_occurrences[clean_occurrences$year %in% c(1997:2000)]
#2006-2009
occurrences2006.2009 <- clean_occurrences[clean_occurrences$year %in% c(2006:2009)]

## 2.3. Convert species occurrence dfs to sf objects ----

#Create sf object for specis data and assign CRS
#1997-2000
occurrences1997.2000 <- st_as_sf(occurrences1997.2000,
                                 coords = c("decimalLongitude", "decimalLatitude"),
                                 crs = 4326)
#2006-2009
occurrences2006.2009 <- st_as_sf(occurrences2006.2009,
                                 coords = c("decimalLongitude", "decimalLatitude"),
                                 crs = 4326)



#Project to match land cover data
occurrences1997.2000 <- st_transform(occurrences1997.2000,
                                     st_crs(norway_corine[[1]]))
occurrences2006.2009 <- st_transform(occurrences2006.2009,
                                     st_crs(norway_corine[[1]]))

# 3. CALCULATE TURNOVER ----

## 3.1. Assign species to land cover cells for each timestep ----
occurrences1997.2000$cell <- extract(land_cover_id,
                                     as.matrix(st_coordinates(occurrences1997.2000)))
occurrences2006.2009$cell <- extract(land_cover_id,
                                     as.matrix(st_coordinates(occurrences2006.2009)))

## 3.2. Group by cell to create list of species for each cell ----
occurrences1997.2000_grouped <- occurrences1997.2000 |>
  group_by(cell) |>
  summarise(species = list(species))

occurrences2006.2009_grouped <- occurrences2006.2009 |>
  group_by(cell) |>
  summarise(species = list(species))

#Join data for the two timesteps
data <- left_join(as.data.frame(occurrences1997.2000_grouped),
                  as.data.frame(occurrences2006.2009_grouped),
                  by = "cell",
                  suffix = c("_1997.2000", "_2006.2009"))

## 3.3. Calculate turnover ----
#Write function to calculate turnover
calculate_turnover <- function(species1, species2) {
  unique1 <- length(setdiff(unlist(species1), unlist(species2)))
  unique2 <- length(setdiff(unlist(species2), unlist(species1)))
  total <- length(union(unlist(species1), unlist(species2)))
  return((unique1 + unique2) / total)
}

#Create df with value of turnover for each cell 
data$turnover <- mapply(calculate_turnover,
                        data$species_1997.2000,
                        data$species_2006.2009)

#Get the land cover values for each cell ID
land_cover <- norway_corine[[1]]
land_cover[is.na(land_cover)] <- -9999


land_cover_values <- land_cover[land_cover_id]
