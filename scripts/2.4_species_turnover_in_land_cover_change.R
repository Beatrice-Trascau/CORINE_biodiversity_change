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

# 1. LOAD DATA ----

## 1.1. Download cleaned occurrence records and modified corine stack from box ----

#Add download link
cleaned_occurrences <- ("https://ntnu.box.com/shared/static/cgjbsfs24m31uov6ir4vkkmw6cs7ga36.txt")
norway_corine <- ("https://ntnu.box.com/shared/static/knmrgdptal3wx900shryypgprdu3t7sb.tif")

#Download the file
download.file(cleaned_occurrences, "cleaned_occurrences.txt")
download.file(norway_corine, "corine_modified_classes_stack.tif")

## 1.2. Read in the data ----
clean_occurrences <- fread(here::here("data", 
                                      "cleaned_occurrences.txt"))

norway_corine <- rast(here::here("data", 
                                 "corine_modified_classes_stack.tif"))

# 2. PREPARE DATA FOR ANALYSIS ----

## 2.1. Raster with unique ID for each cell ----
#This raster is needed to find the identity of the corine cells in which turnover takes place

#Create a raster with the same properties as corine (no issue that we are only using 1 year)
land_cover_id <- norway_corine[[1]]

#Assign each cell a unique ID from 1 to ncell
land_cover_id[] <- 1:ncell(norway_corine[[1]])

## 2.2. Species occurrence records as sf for each period of change ----

#N.B: Periods pf change are:
#First period of land cover change: 2000 to 2006; BEFORE Change = 1997-2000, AFTER Change = 2006-2009
#Second period of land cover change: 2006 to 2012; BEFORE Change = 2003-2006, AFTER Change = 2012-2015
#Third period of land cover change: 2012 to 2018; BEFORE Change = 2009-2012, AFTER Change = 2015-2018

### 2.2.1. Subset dfs for each before and after period from above (total of 6) ----

#Period 1: 2000 to 2006
#1997-2000
occurrences1997.2000 <- clean_occurrences[clean_occurrences$year %in% c(1997:2000)]
#2006-2009
occurrences2006.2009 <- clean_occurrences[clean_occurrences$year %in% c(2006:2009)]

#Period 2: 2006 to 2012
#2003-2006
occurrences2003.2006 <- clean_occurrences[clean_occurrences$year %in% c(2003:2006)]
#2012-2015
occurrences2012.2015 <- clean_occurrences[clean_occurrences$year %in% c(2012:2015)]

#Period 2: 2006 to 2012
#2009-2012
occurrences2009.2012 <- clean_occurrences[clean_occurrences$year %in% c(2009:2012)]
#2015-2018
occurrences2015.2018 <- clean_occurrences[clean_occurrences$year %in% c(2015:2018)]


### 2.2.2. Create sf object for specis data and assign CRS ----
#1997-2000
occurrences1997.2000_sf <- st_as_sf(occurrences1997.2000,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
#2006-2009
occurrences2006.2009_sf <- st_as_sf(occurrences2006.2009,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
#2003-2006
occurrences2003.2006_sf <- st_as_sf(occurrences2003.2006,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
#2012-2015
occurrences2012.2015_sf <- st_as_sf(occurrences2012.2015,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
#2009-2012
occurrences2009.2012_sf <- st_as_sf(occurrences2009.2012,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
#2015-2018
occurrences2015.2018_sf <- st_as_sf(occurrences2015.2018,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)

### 2.2.3. Project sf objects to match corine ----
#1997-2000
occurrences1997.2000_sf <- st_transform(occurrences1997.2000_sf,
                                        st_crs(norway_corine[[1]]))
#2006-2009
occurrences2006.2009_sf <- st_transform(occurrences2006.2009_sf,
                                        st_crs(norway_corine[[1]]))
#2003-2006
occurrences2003.2006_sf <- st_transform(occurrences2003.2006_sf,
                                        st_crs(norway_corine[[1]]))
#2012-2015
occurrences2012.2015_sf <- st_transform(occurrences2012.2015_sf,
                                        st_crs(norway_corine[[1]]))
#2009-2012
occurrences2009.2012_sf <- st_transform(occurrences2009.2012_sf,
                                        st_crs(norway_corine[[1]]))
#2015-2018
occurrences2015.2018_sf <- st_transform(occurrences2015.2018_sf,
                                        st_crs(norway_corine[[1]]))


# 3. COMBINE LAND COVER AND OCCURRENCE DATA ----

## 3.1. Assign species to land cover cells for each time step ----

#1997-2000
occurrences1997.2000_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(occurrences1997.2000_sf)))

#2006-2009
occurrences2006.2009_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(occurrences2006.2009_sf)))
#2003-2006
occurrences2003.2006_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(occurrences2003.2006_sf)))
#2012-2015
occurrences2012.2015_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(occurrences2012.2015_sf)))
#2009-2012
occurrences2009.2012_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(occurrences2009.2012_sf)))
#2015-2018
occurrences2015.2018_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(occurrences2015.2018_sf)))

## 3.2. Group by cell to create list of species for each cell ----

#1997-2000
occurrences1997.2000_grouped <- occurrences2006.2009_sfs |>
  group_by(cell) |>
  summarise(species = list(species))
#2006-2009
occurrences2006.2009_grouped <- occurrences2006.2009_sf |>
  group_by(cell) |>
  summarise(species = list(species))
#2003-2006
occurrences2003.2006_grouped <- occurrences2003.2006_sf |>
  group_by(cell) |>
  summarise(species = list(species))
#2012-2015
occurrences2012.2015_grouped <- occurrences2012.2015_sf |>
  group_by(cell) |>
  summarise(species = list(species))
#2009-2012
occurrences2009.2012_grouped <- occurrences2009.2012_sf |>
  group_by(cell) |>
  summarise(species = list(species))
#2015-2018
occurrences2015.2018_grouped <- occurrences2015.2018_sf |>
  group_by(cell) |>
  summarise(species = list(species))


## 3.3. Join data for the different timestops ----
occurrences_turnover <- left_join(as.data.frame(occurrences1997.2000_grouped),
                                  as.data.frame(occurrences2006.2009_grouped),
                                  as.data.frame(occurrences2003.2006_grouped),
                                  as.data.frame(occurrences2012.2015_grouped),
                                  as.data.frame(occurrences2009.2012_grouped),
                                  as.data.frame(occurrences2015.2018_grouped),
                                  by = "cell",
                                  suffix = c("_1997.2000", "_2006.2009",
                                             "_2003.2006", "_2012.2015",
                                             "_2009.2012", "_2015.2018"))

# 4. CALCULATE TURNOVER ----

## 4.1. Write function to calculate turnover ----
calculate_turnover <- function(species1, species2) {
  unique1 <- length(setdiff(unlist(species1), unlist(species2)))
  unique2 <- length(setdiff(unlist(species2), unlist(species1)))
  total_occurrences <- length(unlist(species1)) + length(unlist(species2))
  return((unique1 + unique2) / total_occurrences)
}

## 4.2. Calculate turnover for each period ----
#First period = turnover between "_1997.2000" and "_2006.2009"
#Second period = turnover between "_2003.2006" and "_2012.2015"
#Third period = turnover between "_2009.2012" and "_2015.2018"

#First period (2000 to 2006)
occurrences_turnover$turover2000.2006 <- mapply(calculate_turnover,
                                                data$species_1997.2000,
                                                data$species_2006.2009)


#Second period (2006 to 2012)
occurrences_turnover$turover2006.2012 <- mapply(calculate_turnover,
                                                data$species_2003.2006,
                                                data$species_2012.2015)

#Third period (2012 to 2018)
occurrences_turnover$turover2012.2018 <- mapply(calculate_turnover,
                                                data$species_2009.2012,
                                                data$species_2015.2018)



# 5. CREATE DF WITH TURNOVER AND LAND COVER CHANGE VALUES ----

## 5.1. Extract values for land cover change ----

#First period
values_2000.2006 <- terra::values(norway_corine[[1]] - norway_corine[[2]])


#Second period
values_2006.2012 <- terra::values(norway_corine[[2]] - norway_corine[[3]])

#Third period
values_2012.2018 <- terra::values(norway_corine[[3]] - norway_corine[[4]])

## 5.2. Convert to vector ----

#First period
if (is.matrix(values_2000.2006)) {
  values_2000.2006 <- as.vector(values_2000.2006)
}

#Second period
if (is.matrix(values_2006.2012)) {
  values_2006.2012 <- as.vector(values_2006.2012)
}

#Third period
if (is.matrix(values_2012.2018)) {
}

## 5.3. Extract land cover values using the unique cell IDs
occurrences_turnover <- occurrences_turnover |>
  mutate(cover_change_2000.2006 = values_2000.2006[occurrences_turnover$cell[,1]],
         cover_change_2006.2012 = values_2006.2012[occurrences_turnover$cell[,1]],
         cover_change_2012.2018 = values_2012.2018[occurrences_turnover$cell[,1]])

## 5.4. Replace land cover change numerical values with corresponding categories ----
occurrences_turnover <- occurrences_turnover |>
  mutate(cover_change_2000.2006 = case_when(cover_change_2000.2006 == 0 ~ "no change",
                                            cover_change_2000.2006 %in% c(-79, -102, 23, 170, 147,
                                                                          300, 277, 510, 487, 631, 608) ~ "Intensification",
                                            cover_change_2000.2006 %in% c(-130, -461, 210) ~ "Deforestation",
                                            cover_change_2000.2006 == -23 ~ "Extensification",
                                            cover_change_2000.2006 == -340 ~ "Forestry",
                                            cover_change_2000.2006 == -710 ~ "Restoration",
                                            cover_change_2000.2006 == 340 ~ "Succession or Forestry",
                                            cover_change_2000.2006 %in% c(-249, -379, -589, -170, -300,
                                                                          -510, -147, -277, -487, 130,
                                                                          -210, 461, 331, 121) ~ "Succession",
                                            cover_change_2000.2006 %in% c(79, 102, 249, 
                                                                          379, 589, 710) ~ "Urbanisation"),
         cover_change_2006.2012 = case_when(cover_change_2006.2012 == 0 ~ "no change",
                                            cover_change_2006.2012 %in% c(-79, -102, 23, 170, 147,
                                                                          300, 277, 510, 487, 631, 608) ~ "Intensification",
                                            cover_change_2006.2012 %in% c(-130, -461, 210) ~ "Deforestation",
                                            cover_change_2006.2012 == -23 ~ "Extensification",
                                            cover_change_2006.2012 == -340 ~ "Forestry",
                                            cover_change_2006.2012 == -710 ~ "Restoration",
                                            cover_change_2006.2012 == 340 ~ "Succession or Forestry",
                                            cover_change_2006.2012 %in% c(-249, -379, -589, -170, -300,
                                                                          -510, -147, -277, -487, 130,
                                                                          -210, 461, 331, 121) ~ "Succession",
                                            cover_change_2006.2012 %in% c(79, 102, 249, 
                                                                          379, 589, 710) ~ "Urbanisation"),
         cover_change_2012.2018 = case_when(cover_change_2012.2018 == 0 ~ "no change",
                                            cover_change_2012.2018 %in% c(-79, -102, 23, 170, 147,
                                                                          300, 277, 510, 487, 631, 608) ~ "Intensification",
                                            cover_change_2012.2018 %in% c(-130, -461, 210) ~ "Deforestation",
                                            cover_change_2012.2018 == -23 ~ "Extensification",
                                            cover_change_2012.2018 == -340 ~ "Forestry",
                                            cover_change_2012.2018 == -710 ~ "Restoration",
                                            cover_change_2012.2018 == 340 ~ "Succession or Forestry",
                                            cover_change_2012.2018 %in% c(-249, -379, -589, -170, -300,
                                                                          -510, -147, -277, -487, 130,
                                                                          -210, 461, 331, 121) ~ "Succession",
                                            cover_change_2006.2012 %in% c(79, 102, 249, 
                                                                          379, 589, 710) ~ "Urbanisation"))



