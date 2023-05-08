##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##------- 2.3_turnover_in_land_cover_change ------##
##------------------------------------------------##

#This script contains code to quantify the number of occurrence records in pixels undergoing land cover changes
 #and measure species turnover in pixels before and after land cover change (years)

# 0. LOAD PACKAGES ----
library(here)
library(data.table)
library(terra)
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)

# 1. LOAD DATA ----

## 1.1. Download cleaned occurrence records and modified corine stack from box ---

#Add download link
cleaned_occurrences <- ("https://ntnu.box.com/shared/static/cgjbsfs24m31uov6ir4vkkmw6cs7ga36.txt")
norway_corine <- ("https://ntnu.box.com/shared/static/s406n4td0cmtfjsxwllz8klnkvksiyul.tif")

#Download the file
download.file(cleaned_occurrences, "cleaned_occurrences.txt")
download.file(norway_corine, "corine_modified_classes_stack.tif")

## 1.2. Read in the data ----
clean_occurrences <- fread(here("data", 
                                "cleaned_occurrences.txt"))

norway_corine <- rast(here("data", 
                           "corine_modified_classes_stack.tif"))

# 2. OCCURRENCE RECORDS AS SPATVECTOR ----

## 2.1. First period of land cover change: 2000 to 2006; BEFORE Change = 1997-2000, AFTER Change = 2006-2009 ----
occurrences2000.2006 <- clean_occurrences[clean_occurrences$year %in% 
                                            c(1997:2000, 2006:2009)] |>
  mutate(year = case_when(year %in% c(1997:2000) ~ "before",
                                 year %in% c(2006:2009) ~ "after"))

### 2.1.1. Subset occurrence data by year into own d.f. ----

#Create empty vector
occurrences <- c()

#For loop to create df for each year of sampling

for(i in c("before", "after")){
  #Extract data frame for each period
  occurrences[[i]] <- occurrences2000.2006[occurrences2000.2006$year == i]
  #Create a name vector to store the names of the vectors created
  new_name <- paste0("occurrences_", i)
  #Store object "occurrneces" under the new name in the global environment
  assign(new_name, occurrences[[i]])
}

### 2.1.2. Convert yearly occurrence records to SpatVector ----
#Define the vectors you want to have in the loop beforehand as empty vectors
long <- c()
lat <- c()
spatial <- list()
spatial_occurrence <- list() 

#Loop to convert yearly occurrence records to SpatVector
for (i in 1:2) {
  #Extract latitude and longitude into vectors
  long[[i]] <- occurrences[[i]]$decimalLongitude
  lat[[i]] <- occurrences[[i]]$decimalLatitude
  #Combine lat and long  vectors into 1 df
  spatial[[i]] <- cbind(long[[i]], lat[[i]])
  #Convert to SpatVector; make sure to define a crs
  spatial_occurrence[[i]] <- terra::vect(spatial[[i]],
                                         crs = "+proj=longlat")
  #Project SpatVectors to EPSG:3035
  spatial_occurrence[[i]] <- terra::project(spatial_occurrence[[i]],
                                            "epsg:3035")
  #Create a name vector to store the names of the spatial points data frames created
  new_name <- paste0("spatial_occurrence", i)
  #Store object "points" under the name "new_name" in the global environment
  assign(new_name, spatial_occurrence[[i]])
}

# 3. COMBINE OCCURRENCE RECORDS AND LAND COVER CHANGE LAYERS ----

## 3.1. Create a stack of land cover change layers ----
#Period1 = 2000-2006 = norway_corine[[1]] - norway_corine[[2]]
#Period2 = 2006-2012 = norway_corine[[2]] - norway_corine[[3]]
#Period3 = 2012-2018 = norway_corine[[3]] - norway_corine[[4]]

norway_land_cover_change <- c(norway_corine[[1]] - norway_corine[[2]],
                              norway_corine[[2]] - norway_corine[[3]],
                              norway_corine[[3]] - norway_corine[[4]])

## 3.2. Calculate number of occurrences in each land-cover pixel/year ----
### 3.2.1. Use rasterize function to extract number of occurrence records in each pixel for 2000 to 2006 period ----
#Rasterize
 #before land cover change: 1997 - 2000
occurrence_land_cover_before <- terra::rasterize(spatial_occurrence1,
                                                    norway_land_cover_change[[1]],
                                                    fun = sum)

 #after land cover change: 2006 - 2009
occurrence_land_cover_after <- terra::rasterize(spatial_occurrence2,
                                                 norway_land_cover_change[[1]],
                                                 fun = sum)
#Convert to dataframe
occurrence_land_cover_before_df <- as.data.frame(occurrence_land_cover_before$sum,
                                                    xy = TRUE)

occurrence_land_cover_after_df <- as.data.frame(occurrence_land_cover_after$sum,
                                                 xy = TRUE)
