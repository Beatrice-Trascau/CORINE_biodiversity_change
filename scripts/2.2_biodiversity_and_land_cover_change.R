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
clean_occurrences <- fread(here("data", 
                                "cleaned_occurrences.txt"))

norway_corine <- rast(here("data", 
                           "corine_modified_classes_stack.tif"))


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
spatial <- list()
spatial_occurrence <- list()

#Loop to convert yearly occurrence records to SpatVector
for (i in 1:3) {
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
occurrence_land_cover_2000.2006 <- terra::rasterize(spatial_occurrence1,
                                                    norway_land_cover_change[[1]],
                                                    fun = sum)
#Convert to dataframe
occurrence_land_cover_2000.2006_df <- as.data.frame(occurrence_land_cover_2000.2006$U2006_CLC2000_V2020_20u1,
                                                    xy = TRUE)

### 3.2.2. Use rasterize function to extract number of occurrence records in each pixel for 2006 to 2012 period ----
#Rasterize
occurrence_land_cover_2006.2012 <- terra::rasterize(spatial_occurrence2,
                                                    norway_land_cover_change[[2]],
                                                    fun = sum)
#Convert to dataframe
occurrence_land_cover_2006.2012_df <- as.data.frame(occurrence_land_cover_2006.2012$U2012_CLC2006_V2020_20u1,
                                                    xy = TRUE)

### 3.2.3. Use rasterize function to extract number of occurrence records in each pixel for 2012 to 2018 period ----
#Rasterize
occurrence_land_cover_2012.2018 <- terra::rasterize(spatial_occurrence3,
                                                    norway_land_cover_change[[3]],
                                                    fun = sum)
#Convert to dataframe
occurrence_land_cover_2012.2018_df <- as.data.frame(occurrence_land_cover_2012.2018$U2018_CLC2012_V2020_20u1,
                                                    xy = TRUE)

## 3.3. Put all rasterize outputs (SpatRasters and DataFrames) in lists for use below ----
#Number of occurrence records in land cover pixels as SpatRaster
occurrence_land_cover_overlap <- list(occurrence_land_cover_2000.2006,
                                      occurrence_land_cover_2006.2012,
                                      occurrence_land_cover_2012.2018)

#Number of occurrence records in land cover pixels as df
occurrence_land_cover_overlap_dfs <- list(occurrence_land_cover_2000.2006_df,
                                          occurrence_land_cover_2006.2012_df,
                                          occurrence_land_cover_2012.2018_df)

## 3.4. Merge all dfs of occurrence_land_cover_overlap with land_cover change_dfs ----
#List of land_cover_change_dfs
land_cover_change_df_list <- list(as.data.frame(norway_corine[[1]] - norway_corine[[2]],
                                                xy = TRUE),
                                  as.data.frame(norway_corine[[2]] - norway_corine[[3]],
                                                xy = TRUE),
                                  as.data.frame(norway_corine[[3]] - norway_corine[[4]],
                                                xy = TRUE))

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

## 3.5.  Create df of occurrences in land cover for all time periods----
#Change column names of the merged_occurrences_in_land_cover dataframes
colnames(merged_occurrences_in_land_cover_period1) <- c("lat", "long", 
                                                        "pixel_status", 
                                                        "occurrence_count")

colnames(merged_occurrences_in_land_cover_period2) <- c("lat", "long", 
                                                        "pixel_status", 
                                                        "occurrence_count")

colnames(merged_occurrences_in_land_cover_period3) <- c("lat", "long", 
                                                        "pixel_status", 
                                                        "occurrence_count")

#Create new column with period for each dataframe (to differentiate later)
merged_occurrences_in_land_cover_period1 <- merged_occurrences_in_land_cover_period1 |>
  mutate(period = "2000.2006")

merged_occurrences_in_land_cover_period2 <- merged_occurrences_in_land_cover_period2 |>
  mutate(period = "2006.2012")

merged_occurrences_in_land_cover_period3 <- merged_occurrences_in_land_cover_period3 |>
  mutate(period = "2012.2018")


#Merge all merged_occurrences_in_land_cover into a single df
all_periods_occurrence_in_land_cover <- bind_rows(merged_occurrences_in_land_cover_period1,
                                                  merged_occurrences_in_land_cover_period2,
                                                  merged_occurrences_in_land_cover_period3)

#Write dataframe
write.csv(all_periods_occurrence_in_land_cover,
          here("data", "occurrences_in_land_cover_changes.csv"))


# 4. VISUALIZE DATA ----

## 4.1. Compare number of occurrences between changed and constant pixels ----

### 4.1.1. Read in and prepare df for analysis ----
#Read in data
occurrence_in_land_cover <- all_periods_occurrence_in_land_cover

#Convert values in column pixel_status to factorial constant or changed value
occurrence_in_land_cover <- occurrence_in_land_cover |>
  mutate(pixel_status = case_when(pixel_status == 0 ~ "constant",
                                  pixel_status != 0 ~ "changed"))


#Remove "pixels" which did not have any species (i.e. occurrence_count = 0)
new_occurrence_in_land_cover <- occurrence_in_land_cover[
  occurrence_in_land_cover$occurrence_count != 0, ]

#Save the new dataframe
write.csv(new_occurrence_in_land_cover,
          here("data", "new_occurrence_in_land_cover_for_plotting.csv"))


### 4.1.2. Compare number of occurrences between changed and constant pixels ----
ggplot(new_occurrence_in_land_cover, 
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

ggsave(here("figures",
            "occurrences_across_periods_changed_unchanged_pixels.svg"))

### 4.1.3. Compare number of occurrences between changed and constant pixels and years ----
ggplot(occurrence_in_land_cover)+
  geom_bar(aes(x = period, y = occurrence_count,
               fill = pixel_status),
           stat = "identity", 
           position = "dodge")+
  scale_fill_manual(name = "Land-use Status in Pixel",
                    breaks = c("changed", "constant"),
                    labels = c("Change", "No Change"),
                    values = c("#ffd700", "#00a000"))+
  xlab("Sampling Periods")+
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

#Save the plot as Vector Graphics
ggsave(here("figures",
            "occurrences_in_pixels_across_years.svg"))

## 4.2. Compare number of occurrences between changed pixels across periods ----
#Subset df to only contain "changed" pixels
occurrence_in_changed_land_cover <- occurrence_in_land_cover[
  occurrence_in_land_cover$pixel_status != "constant", ]

#Plot number of occurrences between changed pixels across periods 
ggplot(occurrence_in_changed_land_cover)+
  geom_bar(aes(x = period, y = occurrence_count),
           stat = "identity", 
           position = "dodge",
           fill = "#ffd700")+
  xlab("Sampling Periods")+
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
#save plot
ggsave(here("figures",
            "occurrences_in_changed_pixels.svg"))

## 4.3. Compare number of occurrences between different land cover transition categories across periods ----

#Read in data
occurrence_in_land_cover_transitions <- all_periods_occurrence_in_land_cover

#Convert values in column pixel_status to factorial constant or changed value
occurrence_in_land_cover_transitions <- occurrence_in_land_cover_transitions |>
  mutate(pixel_status = case_when(pixel_status == 0 ~ "no change",
                                  pixel_status %in% c(-79, -102, 23, 170, 147,
                                                      300, 277, 510, 487, 631, 608) ~ "Intensification",
                                  pixel_status %in% c(-130, -461, 210) ~ "Deforestation",
                                  pixel_status == -23 ~ "Extensification",
                                  pixel_status == -340 ~ "Forestry",
                                  pixel_status == -710 ~ "Restoration",
                                  pixel_status == 340 ~ "Succession or Forestry",
                                  pixel_status %in% c(-249, -379, -589, -170, -300,
                                                      -510, -147, -277, -487, 130,
                                                      -210, 461, 331, 121) ~ "Succession",
                                  pixel_status %in% c(79, 102, 249, 
                                                      379, 589, 710) ~ "Urbanisation"))


#Remove "pixels" which did not have any species (i.e. occurrence_count = 0)
new_occurrence_in_land_cover_transitions <- occurrence_in_land_cover_transitions[
  occurrence_in_land_cover$occurrence_count != 0, ]

#Remove "pixels" which show "no change"
new_occurrence_in_land_cover_transitions <- new_occurrence_in_land_cover_transitions[
  new_occurrence_in_land_cover_transitions$pixel_status != "no change", ]

#Remove NAs from columns period and pixel_status
new_occurrence_in_land_cover_transitions <- new_occurrence_in_land_cover_transitions |>
  drop_na()

#Plot number of occurrences between different land cover transition categories across periods
ggplot(new_occurrence_in_land_cover_transitions)+
  geom_bar(aes(x = period, y = occurrence_count, fill = pixel_status),
           stat = "identity", 
           position = "dodge")+
  labs(fill = "Land Cover Change",
       x = "Sampling Periods",
       y = "Number of occurrences")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                                            "#6A3D9A", "#FF7F00", 
                                            "gold1", "maroon"))+
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
#save the plot
ggsave(here("figures",
            "occurrences_in_individual_land_cover_changes.svg"))

#Plot number of occurrences between different land cover transition categories across periods
#but without Forestry, Succession OR Forestry, and Urbanisation
smaller_transitions <- new_occurrence_in_land_cover_transitions[
  new_occurrence_in_land_cover_transitions$pixel_status %in% c("Deforestation",
                                                               "Extensification",
                                                               "Intensification",
                                                               "Succession"), ]

ggplot(smaller_transitions)+
  geom_bar(aes(x = period, y = occurrence_count, fill = pixel_status),
           stat = "identity", 
           position = "dodge")+
  labs(fill = "Land Cover Change",
       x = "Sampling Periods",
       y = "Number of occurrences")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C",
                                            "#6A3D9A", "#FF7F00"))+
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

#Remove "pixels" which did not have any species (i.e. occurrence_count = 0)
new_occurrence_in_land_cover <- occurrence_in_land_cover[
  occurrence_in_land_cover$occurrence_count != 0, ]

#Save the new dataframe
write.csv(new_occurrence_in_land_cover,
          here("data", "new_occurrence_in_land_cover_for_plotting.csv"))

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


# END OF SCRIPT ----