##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
###- 4.1_corine_500m_aggreggation_and_turnover -####
##------------------------------------------------##

# 0. PACKAGES ----
library(here)
library(terra)
library(data.table)
library(dplyr)
library(sf)

# 1. LOAD DATA ----

## 1.1. CORINE Land Cover Data ----
norway_corine <- rast(here("data",
                           "corine_modified_classes_stack.tif"))

## 1.2. GBIF Occurrence Records -----
clean_occurrences <- fread(here::here("data", 
                                      "cleaned_occurrences.txt"))


# 2. AGGREGGATE LAND COVER CHANGES TO 500M ----

## 2.1. Calculate land cover changes ----

# Land cover changes between 2000 and 2006
corine_2000_2006 <- norway_corine[[1]] - norway_corine[[2]]

# Land cover changes between 2006 and 2012
corine_2006_2012 <- norway_corine[[2]] - norway_corine[[3]]

# Land cover changes between 2012 and 2018
corine_2012_2018 <- norway_corine[[3]] - norway_corine[[4]]

# Convert NA values in land cover change layers to -9999
corine_2000_2006[is.na(corine_2000_2006)] <- -9999
corine_2006_2012[is.na(corine_2006_2012)] <- -9999
corine_2012_2018[is.na(corine_2012_2018)] <- -9999

## 2.2. Create "intensification" rasters ----

# Intensification = Nature loss = includes the following land cover transitions:
  # Urbanisation (=79, 102, 249, 379, 589, 710), 
  # Intensification (=-79, -102, 23, 170, 147, 300, 277, 510, 487, 631, 608), 
  # Forest Harvesting (= the old "Forestry" category = -340)
  # Deforestation (=-130, -461, 210)
  # All above values will be concerted to 1, everything else will be converted to 0

# Intensification in 2000 - 2006 land cover transitions
intens_00_06 <- terra::ifel(corine_2000_2006 == -9999,  NA, 
                            ifel(corine_2000_2006 %in% c(79, 102, 249, 379, 589, 710,
                                                         -79, -102, 23, 170, 147, 300, 
                                                         277, 510, 487, 631, 608,-340,
                                                         -130, -461, 210), 1, 0))

# Intensification in 2006 - 2012 land cover transitions
intens_06_12 <- terra::ifel(corine_2006_2012 == -9999,  NA, 
                            ifel(corine_2006_2012 %in% c(79, 102, 249, 379, 589, 710,
                                                         -79, -102, 23, 170, 147, 300, 
                                                         277, 510, 487, 631, 608,-340,
                                                         -130, -461, 210), 1, 0))

# Intensification in 2012 - 2018 land cover transitions
intens_12_18 <- terra::ifel(corine_2012_2018 == -9999,  NA, 
                            ifel(corine_2012_2018 %in% c(79, 102, 249, 379, 589, 710,
                                                         -79, -102, 23, 170, 147, 300, 
                                                         277, 510, 487, 631, 608,-340,
                                                         -130, -461, 210), 1, 0))

## 2.3. Create "extenification" rasters ----
# Exrensification = Nature gain = includes the following land cover transitions:
 # Succession (=-249, -379, -589, -170, -300,-510, -147, -277, -487, 130,-210, 461, 331, 121)
 # Forest Succession (=340)
 # Restoration (=-710)
 # Extensification (=-79, -102, 23, 170, 147,300, 277, 510, 487, 631, 608)

# Extensification in 2000 - 2006 land cover transitions
extens_00_06 <- terra::ifel(corine_2000_2006 == -9999, NA,
                            ifel(corine_2000_2006 %in% c(-249, -379, -589, -170, -300,-510, -147, 
                                   -277, -487, 130,-210, 461, 331, 121,
                                   340, -710, -79, -102, 23, 170, 147,
                                   300, 277, 510, 487, 631, 608), 1, 0))


# Extensification in 2006 - 2012 land cover transitions
extens_06_12 <- terra::ifel(corine_2006_2012 == -9999, NA,
                            ifel(corine_2006_2012 %in% c(-249, -379, -589, -170, -300,-510, -147, 
                                                         -277, -487, 130,-210, 461, 331, 121,
                                                         340, -710, -79, -102, 23, 170, 147,
                                                         300, 277, 510, 487, 631, 608), 1, 0))

# Extensification in 2012 - 2018 land cover transitions
extens_12_18 <- terra::ifel(corine_2012_2018 == -9999, NA,
                            ifel(corine_2012_2018 %in% c(-249, -379, -589, -170, -300,-510, -147, 
                                                         -277, -487, 130,-210, 461, 331, 121,
                                                         340, -710, -79, -102, 23, 170, 147,
                                                         300, 277, 510, 487, 631, 608), 1, 0))


## 2.4. Aggregate rasters to 500m x 500m ----

# Intensification in 2000 - 2006 
aggregated_intens_00_06 <- terra::aggregate(intens_00_06, fact = 5,
                                            fun = sum)

# Intensification in 2006 - 2012 
aggregated_intens_06_12 <- terra::aggregate(intens_06_12, fact = 5,
                                            fun = sum)

# Intensification in 2012 - 2018 
aggregated_intens_06_12 <- terra::aggregate(intens_06_12, fact = 5,
                                            fun = sum)

# Extensificatin in 2000 - 2006
aggregated_extens_00_06 <- terra::aggregate(extens_00_06, fact = 5,
                                            fun = sum)

# Extensificatin in 2006 - 2012
aggregated_extens_06_12 <- terra::aggregate(extens_06_12, fact = 5,
                                            fun = sum)

# Extensificatin in 2012 - 2018
aggregated_extens_12_18 <- terra::aggregate(extens_12_18, fact = 5,
                                            fun = sum)

## 2.5. Write rasters to file ----

# Stack layers into one raster stack for intensification and extensificaiton
aggregated_intens <- c(aggregated_intens_00_06,
                       aggregated_intens_06_12,
                       aggregated_intens_12_18)

aggregated_extens <- c(aggregated_extens_00_06,
                       aggregated_extens_06_12,
                       aggregated_extens_12_18)

# Write raster stacks to file
terra::writeRaster(aggregated_intens,
                   here("data", "aggregated_500m_intensification_stack.tif"))

terra::writeRaster(aggregated_extens,
                   here("data", "aggregated_500m_extensification_stack.tif"))


## 2.6. Create a raster with unique ID for each cell ----
#This raster is needed to find the identity of the CORINE cells in which turnover takes place

# Create a raster with the same properties as corine (no issue that we are only using 1 year)
land_cover_id <- aggregated_extens[[1]]

# Assign each cell a unique ID from 1 to ncell
land_cover_id[] <- 1:ncell(aggregated_extens[[1]])

# 3. PREPARE OCCURRENCES FOR TURNOVER ANALYSIS -----

## 3.1. Subset occurrence records df for each period of "before" and "after" change ----

#N.B: Periods pf change are:
#First period of land cover change: 2000 to 2006; BEFORE Change = 1997-2000, AFTER Change = 2006-2009
#Second period of land cover change: 2006 to 2012; BEFORE Change = 2003-2006, AFTER Change = 2012-2015
#Third period of land cover change: 2012 to 2018; BEFORE Change = 2009-2012, AFTER Change = 2015-2018

# Period 1: 2000 to 2006
occurrences1997.2000 <- clean_occurrences |>
  filter(year %in% c(1997:2000)) #Before change = 1997-2000

occurrences2006.2009 <- clean_occurrences |>
  filter(year %in% c(2006:2009)) #After change = 2006-2009

# Period 2: 2006 to 2012
occurrences2003.2006 <- clean_occurrences |>
  filter(year %in% c(2003:2006)) #Before change = 2003-2006

occurrences2012.2015 <- clean_occurrences |>
  filter(year %in% c(2012:2015)) #After change = 2012-2015

# Period 3: 2012 to 2018 
occurrences2009.2012 <- clean_occurrences |>
  filter(year %in% c(2009:2012)) #Before change = 2009-2012

occurrences2015.2018 <- clean_occurrences |>
  filter(year %in% c(2015:2018)) #After change = 2015-2018

## 3.2. Convert occurrences to sf objects ----

# Put all occurrence dfs created above in a list
occurrence_list <- list("1997.2000" = occurrences1997.2000,
                        "2006.2009" = occurrences2006.2009,
                        "2003.2006" = occurrences2003.2006,
                        "2012.2015" = occurrences2012.2015,
                        "2009.2012" = occurrences2009.2012,
                        "2015.2018" = occurrences2015.2018)

# Convert each occurrence df to an sf object
for(name in names(occurrence_list)){
  assign(paste0("occurrences_", name, "_sf"),
         st_as_sf(occurrence_list[[name]],
                  coords = c("decimalLongitude", "decimalLatitude"),
                  crs = 4326))
}


## 3.3. Set CRS for sf objects to match CORINE's CRS ----

# Make a list of the names of the sf objects created above
sf_names <- c("occurrences_1997.2000_sf", "occurrences_2006.2009_sf", 
              "occurrences_2003.2006_sf", "occurrences_2012.2015_sf", 
              "occurrences_2009.2012_sf", "occurrences_2015.2018_sf")

# Extrat target CRS from CORINE
target_crs <- st_crs(norway_corine[[1]])

# Transform each sf object to the correct CRS
for(name in sf_names){
  transformed_sf <- st_transform(get(name),
                                 target_crs)
  assign(name, transformed_sf)
}

# 4. COMBINE INTENSIFICATION RASTERS AND OCCURRENCE SF OBJECTS ----

## 4.1. Assign species to land cover cells for each time step ----

# Create a list of sf object names 
sf_names <- c("occurrences_1997.2000_sf", "occurrences_2006.2009_sf", 
              "occurrences_2003.2006_sf", "occurrences_2012.2015_sf", 
              "occurrences_2009.2012_sf", "occurrences_2015.2018_sf")

# Extract cell values for each sf object
for(name in sf_names){
  sf_object <- get(name)
  sf_object$cell <- terra::extract(land_cover_id,
                                   as.matrix(st_coordinates(sf_object)))
  assign(name, sf_object)
}

## 4.2. Group by cell to create list of species for each cell ----

# Create a list of sf object names
sf_names <- c("occurrences_1997.2000_sf", "occurrences_2006.2009_sf", 
              "occurrences_2003.2006_sf", "occurrences_2012.2015_sf", 
              "occurrences_2009.2012_sf", "occurrences_2015.2018_sf")

# Create list of names for the corresponding grouped sf objects
grouped_names <- c("occurrences_1997.2000_grouped", "occurrences_2006.2009_grouped", 
                   "occurrences_2003.2006_grouped", "occurrences_2012.2015_grouped", 
                   "occurrences_2009.2012_grouped", "occurrences_2015.2018_grouped")

# Group by cell to create list of species for each cell
for (i in seq_along(sf_names)) {
  grouped_obj <- get(sf_names[i]) |>
    group_by(cell) |>
    summarise(species = list(species))
  assign(grouped_names[i], grouped_obj)
}

## 4.3. Join data for different timesteps ----

# Join data 2 by two, based on the corine land cover layer they were created with/from
occurrences_turnover_1 <- left_join(as.data.frame(occurrences_1997.2000_grouped),
                                    as.data.frame(occurrences_2006.2009_grouped),
                                    by = "cell",
                                    suffix = c("_1997.2000", "_2006.2009"))


occurrences_turnover_2 <- left_join(as.data.frame(occurrences_2003.2006_grouped),
                                    as.data.frame(occurrences_2012.2015_grouped),
                                    by = "cell",
                                    suffix = c("_2003.2006", "_2012.2015"))


occurrences_turnover_3 <- left_join(as.data.frame(occurrences_2009.2012_grouped),
                                    as.data.frame(occurrences_2015.2018_grouped),
                                    by = "cell",
                                    suffix = c("_2009.2012", "_2015.2018"))

# Combine all 3 occurrence turnover dataframes in 1
occurrences_turnover_a <- left_join(occurrences_turnover_1,
                                    occurrences_turnover_2,
                                    by = "cell")


occurrences_turnover <- left_join(occurrences_turnover_a,
                                  occurrences_turnover_3,
                                  by = "cell")


# 5. CALCULATE TURNOVER -----

## 5.1. Write function to calculate turnover ----
calculate_turnover <- function(species1, species2) {
  unique1 <- length(setdiff(unlist(species1), unlist(species2)))
  unique2 <- length(setdiff(unlist(species2), unlist(species1)))
  total_occurrences <- length(unlist(species1)) + length(unlist(species2))
  return((unique1 + unique2) / total_occurrences)
}

## 5.2. Calculate turnover in intensification grids for each period of change ----
#First period = turnover between "_1997.2000" and "_2006.2009"
#Second period = turnover between "_2003.2006" and "_2012.2015"
#Third period = turnover between "_2009.2012" and "_2015.2018"

# First period (2000 to 2006)
occurrences_turnover$turover2000.2006 <- mapply(calculate_turnover,
                                                data$species_1997.2000,
                                                data$species_2006.2009)

# Second period (2006 to 2012)
occurrences_turnover$turover2006.2012 <- mapply(calculate_turnover,
                                                data$species_2003.2006,
                                                data$species_2012.2015)

# Third period (2012 to 2018)
occurrences_turnover$turover2012.2018 <- mapply(calculate_turnover,
                                                data$species_2009.2012,
                                                data$species_2015.2018)