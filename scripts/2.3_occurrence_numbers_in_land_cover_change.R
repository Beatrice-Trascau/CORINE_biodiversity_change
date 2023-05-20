##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##-- 2.3_occurrence_numbers_in_land_cover_change -##
##------------------------------------------------##

#This script contains code to quantify the number of occurrence records in pixels undergoing land cover changes


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
                                                    xy = TRUE) |>
  mutate(period = "Before")

occurrence_land_cover_after_df <- as.data.frame(occurrence_land_cover_after$sum,
                                                 xy = TRUE) |>
  mutate(period = "After")

### 3.2.2. Combine the before and after df for histogram visualisation ----
#Combine the dfs
occurrence_before_after <- rbind(occurrence_land_cover_before_df,
                                 occurrence_land_cover_after_df)

#Remove rows with sum < 100
occurrence_before_after_x <- occurrence_before_after |>
  filter(sum > 200)


#Histogram of number of occurrence records/cell before and after the land use change period
species_count_2006 <- ggplot(occurrence_before_after_x, aes(x = sum, fill = period))+
  geom_histogram(color = "black", 
                 position = "dodge",
                 bins = 200,
                 show.legend = FALSE)+
  geom_density(alpha = 5)+
  xlab("Specis Occurrence Records")+
  ylab("Count")+
  facet_grid(factor(period, levels = c("Before", "After")) ~ .)+
  scale_fill_manual(values = c("#faecb7", "#00a000"))+
  scale_x_continuous(n.breaks=100)+
  theme_classic()

#change angle of x axis tick
species_count_2006 + theme(axis.text.x = element_text(angle = 90))

#save as svg
ggsave(here("figures",
            "occurrences_count_2000_2006.svg"))

#Check table with number of occurrence counts 
counts_table_2000_2006 <- table(occurrence_before_after$sum)


## 2.2. Second period of land cover change: 2006 to 2012; BEFORE Change = 2003-2006, AFTER Change = 2012-2015 ----
occurrences2006.2012 <- clean_occurrences[clean_occurrences$year %in% 
                                            c(2003:2006, 2012:2015)] |>
  mutate(year = case_when(year %in% c(2003:2006) ~ "before",
                          year %in% c(2012:2015) ~ "after"))

### 2.2.1. Subset occurrence data by year into own d.f. ----

#Create empty vector
occurrences <- c()

#For loop to create df for each year of sampling

for(i in c("before", "after")){
  #Extract data frame for each period
  occurrences[[i]] <- occurrences2006.2012[occurrences2006.2012$year == i]
  #Create a name vector to store the names of the vectors created
  new_name <- paste0("occurrences_", i)
  #Store object "occurrneces" under the new name in the global environment
  assign(new_name, occurrences[[i]])
}

### 2.2.2. Convert yearly occurrence records to SpatVector ----
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
### 3.2.2. Use rasterize function to extract number of occurrence records in each pixel for 2006 to 2012 period ----
#Rasterize
#before land cover change: 2003 - 2006
occurrence_land_cover_before <- terra::rasterize(spatial_occurrence1,
                                                 norway_land_cover_change[[2]],
                                                 fun = sum)

#after land cover change: 2012 - 2015
occurrence_land_cover_after <- terra::rasterize(spatial_occurrence2,
                                                norway_land_cover_change[[2]],
                                                fun = sum)
#Convert to dataframe
occurrence_land_cover_before_df <- as.data.frame(occurrence_land_cover_before$sum,
                                                 xy = TRUE) |>
  mutate(period = "Before")

occurrence_land_cover_after_df <- as.data.frame(occurrence_land_cover_after$sum,
                                                xy = TRUE) |>
  mutate(period = "After")

### 3.2.3. Combine the before and after df for histogram visualisation ----
#Combine the dfs
occurrence_before_after <- rbind(occurrence_land_cover_before_df,
                                 occurrence_land_cover_after_df)

#Remove rows with sum < 200
occurrence_before_after_x <- occurrence_before_after |>
  filter(sum > 200)


#Histogram of number of occurrence records/cell before and after the land use change period
species_count_2012 <- ggplot(occurrence_before_after_x, aes(x = sum, fill = period))+
  geom_histogram(color = "black", 
                 position = "dodge",
                 bins = 200,
                 show.legend = FALSE)+
  geom_density(alpha = 5)+
  xlab("Specis Occurrence Records")+
  ylab("Count")+
  facet_grid(factor(period, levels = c("Before", "After")) ~ .)+
  scale_fill_manual(values = c("#faecb7", "#00a000"))+
  scale_x_continuous(n.breaks=100)+
  theme_classic()

#change angle of x axis tick
species_count_2012 + theme(axis.text.x = element_text(angle = 90),
                           legend.position = "none")

#save as svg
ggsave(here("figures",
            "occurrences_count_2006_2012.svg"))

#Check table with number of occurrence counts 
counts_table_2006_2012 <- table(occurrence_before_after$sum)


## 2.3. Third period of land cover change: 2012 to 2018; BEFORE Change = 2009-2012, AFTER Change = 2015-2018 ----
occurrences2012.2018 <- clean_occurrences[clean_occurrences$year %in% 
                                            c(2009:2012, 2015:2018)] |>
  mutate(year = case_when(year %in% c(2009:2012) ~ "before",
                          year %in% c(2015:2018) ~ "after"))

### 2.3.1. Subset occurrence data by year into own d.f. ----

#Create empty vector
occurrences <- c()

#For loop to create df for each year of sampling

for(i in c("before", "after")){
  #Extract data frame for each period
  occurrences[[i]] <- occurrences2012.2018[occurrences2012.2018$year == i]
  #Create a name vector to store the names of the vectors created
  new_name <- paste0("occurrences_", i)
  #Store object "occurrneces" under the new name in the global environment
  assign(new_name, occurrences[[i]])
}

### 2.3.2. Convert yearly occurrence records to SpatVector ----
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
### 3.2.2. Use rasterize function to extract number of occurrence records in each pixel for 2006 to 2012 period ----
#Rasterize
#before land cover change: 2003 - 2006
occurrence_land_cover_before <- terra::rasterize(spatial_occurrence1,
                                                 norway_land_cover_change[[3]],
                                                 fun = sum)

#after land cover change: 2012 - 2015
occurrence_land_cover_after <- terra::rasterize(spatial_occurrence2,
                                                norway_land_cover_change[[3]],
                                                fun = sum)
#Convert to dataframe
occurrence_land_cover_before_df <- as.data.frame(occurrence_land_cover_before$sum,
                                                 xy = TRUE) |>
  mutate(period = "Before")

occurrence_land_cover_after_df <- as.data.frame(occurrence_land_cover_after$sum,
                                                xy = TRUE) |>
  mutate(period = "After")

### 3.2.3. Combine the before and after df for histogram visualisation ----
#Combine the dfs
occurrence_before_after <- rbind(occurrence_land_cover_before_df,
                                 occurrence_land_cover_after_df)

#Remove rows with sum < 200
occurrence_before_after_x <- occurrence_before_after |>
  filter(sum > 200)


#Histogram of number of occurrence records/cell before and after the land use change period
species_count_2018 <- ggplot(occurrence_before_after_x, aes(x = sum, fill = period))+
  geom_histogram(color = "black", 
                 position = "dodge",
                 bins = 200,
                 show.legend = FALSE)+
  geom_density(alpha = 5)+
  xlab("Specis Occurrence Records")+
  ylab("Count")+
  facet_grid(factor(period, levels = c("Before", "After")) ~ .)+
  scale_fill_manual(values = c("#faecb7", "#00a000"))+
  scale_x_continuous(n.breaks=100)+
  theme_classic()

#change angle of x axis tick
species_count_2018 + theme(axis.text.x = element_text(angle = 90),
                           legend.position = "none")

#save as svg
ggsave(here("figures",
            "occurrences_count_2012_2018.svg"))

#Check table with number of occurrence counts 
counts_table_2012_2018 <- table(occurrence_before_after$sum)
