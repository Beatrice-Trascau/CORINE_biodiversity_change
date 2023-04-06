##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##---- 2.2_biodiversity_and_land_cover_change ----##
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
clean_occurrences <- fread("cleaned_occurrences.txt")
norway_corine <- rast("corine_modified_classes_stack.tif")

# 2. OCCURRENCE RECORDS AS SPATVECT ----

## 2.1. Subset occurrence data by year into own d.f. ----

#Create empty vector
occurrences <- c()

#For loop to create df for each year of sampling
for(i in c("2000.2005", "2006.2011", "2012.2018")){
  #Extract data frame for each period
  occurrences[[i]] <- clean_occurrences[clean_occurrences$year == i]
  #Create a name vector to store the names of the vectors created
  new_name <- paste0("occurrences_", i)
  #Store object "occurrneces" under the new name in the global environment
  assign(new_name, occurrences[[i]])
}

## 2.2. Convert yearly occurrence records to SpatVector

#Define the vectors you want to have in the loop beforehand as empty vectors
long <- c()
lat <- c()
points <- list()
points_occurrence <- list()

for (i in 1:3) {
  #Extract latitude and longitude into vectors
  long[[i]] <- occurrences[[i]]$decimalLongitude
  lat[[i]] <- occurrences[[i]]$decimalLatitude
  #Combine lat and long  vectors into 1 df
  points[[i]] <- cbind(long[[i]], lat[[i]])
  #Convert to SpatVector
  points_occurrence[[i]] <- terra::vect(points[[i]])
  #Define the crs for the spatial points
  #Create a name vector to store the names of the spatial points data frames created
  new_name <- paste0("points_period", i)
  #Store object "points" under the name "new_name" in the global environment
  assign(new_name, points_occurrence[[i]])
}

# 3. COMBINE OCCURRENCE RECORDS AND LAND COVER CHANGE LAYERS ----

## 3.1. Create a stack of land cover change layers ----
#2000-2006 = norway_corine[[1]] - norway_corine[[2]]
#2006-2012 = norway_corine[[2]] - norway_corine[[3]]
#2012-2018 = norway_corine[[3]] - norway_corine[[4]]

norway_land_cover_change <- c(norway_corine[[1]] - norway_corine[[2]],
                              norway_corine[[2]] - norway_corine[[3]],
                              norway_corine[[3]] - norway_corine[[4]])

## 3.2. Calculate number of occurrences in each land-cover pixel/year ----
#List of SpatialPoints elements for all years
points

#List of all land cover change layers
norway_land_cover_change

#Create an empty list to save the output of the rasterize function
occurrence_land_cover_overlap <- list()

#Create an empty list to save dfs of the rasterize functions
occurrence_land_cover_overlap_dfs <- list()

#Use rasterize function to extract number of occurrence records in each pixels for every land cover survey period (i.e. 2000-2006, 2006-2012, 2012-2018)
for (i in 1:3) {
  #Rasterize overlap between occurrence SpatialPoints and land-cover change layers
  occurrence_land_cover_overlap[[i]] <- terra::rasterize(points_occurrence[[i]],
                                                         norway_land_cover_change[[i]],
                                                         fun = sum)
  #Convert to dataframe
  occurrence_land_cover_overlap_dfs[[i]] <- as.data.frame(occurrence_land_cover_overlap[[i]]$layer,
                                                          xy = TRUE)
  #Create a name vector to store the names of the rasterize output
  new_name <- paste0("occurrence_land_cover_overlap_period", i)
  #Create a name vector to store the names of the dfs from the rasterize output
  new_name_dfs <- paste0("occurrence_land_cover_overlap_df_period", i)
  #Store rasterize output under the new_name in the global environment
  assign(new_name, occurrence_land_cover_overlap[[i]])
  #Store rasterize output under the new_name_dfs in the global environment
  assign(new_name_dfs, occurrence_land_cover_overlap_dfs[[i]])
}

## 3.3. Merge all dfs of occurrence_land_cover_overlap with land_cover change_dfs ----
#List of land_cover_change_dfs
land_cover_change_df_list <- list(as.data.frame(norway_corine[[1]] - norway_corine[[2]]),
                                  as.data.frame(norway_corine[[2]] - norway_corine[[3]]),
                                  as.data.frame(norway_corine[[3]] - norway_corine[[4]]))

#Empty list to save the merged dfs
merged_occurrences_in_land_cover <- list()

#For loop to merge occurrence_land_cover_overlap_dfs and land_cover_change_dfs
for(i in 1: length(occurrence_land_cover_overlap_dfs)){
  #Merge occurrence_land_cover_overlap_dfs and land_cover_changes_dfs
  merged_occurrences_in_land_cover[[i]] <- merge(land_cover_change_df_list[[i]],
                                                 occurrence_land_cover_overlap_dfs[[i]],
                                                 by = c("x", "y"))
  #Create a name vector to store the names of the dfs created
  new_name <- paste0("merged_occurrences_in_land_cover_period", i)
  #Store rasterize() output under the name 'new_name' in the global environment.
  assign(new_name, merged_occurrences_in_land_cover[[i]])
}

## 3.4.  Create df of occurrences in land cover for all time periods----
#Merge all merged_occurrences_in_land_cover into a single df
all_periods_occurrence_in_land_cover <- bind_rows(merged_occurrences_in_land_cover_period1,
                                                  merged_occurrences_in_land_cover_period2,
                                                  merged_occurrences_in_land_cover_period3)

#Write dataframe
write.csv(all_years_occurrence_in_land_cover, "occurrences_in_land_cover_changes.csv")

# 4. VISUALIZE DATA ----

## 4.1. Read in data & clean df (if needed) ----
#Read in data
occurrence_in_land_cover <- all_years_occurrence_in_land_cover

#Change column names 
occurrence_in_land_cover <- occurrence_in_land_cover |>
  colnames(occurrence_in_land_cover) <- c("year", "lat", "long",
                                          "pixel_status", "occurrence_count") |>
  #Replace NA values with 0 in col "occurrence_count"
  mutate(occurrence_count = case_when(occurrence_count == NA ~ 0),
         #Convert values in column pixel_status to factorial constant or changed value
         pixel_status = case_when(pixel_status == 0 ~ "constant",  "changed"))



#Remove "pixels" which did not have any species (i.e. occurrence_count = 0)
new_occurrence_in_land_cover <- occurrence_in_land_cover[
  occurrence_in_land_cover$occurrence_count != 0, ]

#Save the new dataframe
write.csv(new_occurrence_in_land_cover,
          file = "new_occurrence_in_land_cover_for_plotting.csv")


## 4.2. Compare number of occurrences between changed and constant pixels ----
ggplot(occurrence_in_land_cover, 
       aes(x = pixel_status, y = occurrence_count, fill = pixel_status))+
  geom_boxplot()+
  scale_fill_manual(values = c("#faecb7", "#00a000"))+
  scale_x_discrete(breaks = c("changed", "constant"),
                   labels = c("Change", "No Change"))+
  xlab("Land-use Status in Pixel")+
  ylab("Number of Occurrences")+
  theme_classic()+
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14,
                                   color = "#000000"),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14,
                                   color = "#000000"),
        legend.position = "none")

##4.3. Compare number of occurrences between changed and constant pixels and years ----
ggplot(occurrence_in_land_cover)+
  geom_bar(aes(x = year, y = occurrence_count,
               fill = pixel_status),
           stat = "identity", 
           position = "dodge")+
  scale_x_discrete(limits = c(1993:2020))+
  scale_fill_manual(name = "Land-use Status in Pixel",
                    breaks = c("changed", "constant"),
                    labels = c("Change", "No Change"),
                    values = c("#faecb7", "#00a000"))+
  xlab("Year of Sampling")+
  ylab("Number of occurrences")+
  theme_classic()+
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14,
                                   angle = 45,
                                   color = "#000000",
                                   hjust = 1),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14,
                                   color = "#000000"),
        legend.title = element_text(size=14),
        legend.text = element_text(size = 13.5))


