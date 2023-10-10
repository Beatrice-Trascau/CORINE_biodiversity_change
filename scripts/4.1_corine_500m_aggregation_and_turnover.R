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
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)

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
# -9999 values will be converted back to NA
# This is done to help the aggregate() function which will calculate an intensification score
# per aggregate cell (0/25, 1/25...25/25) which will tell us how much nature gain or loss
# happened in the aggreggated cell between the periods of change

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
# All above values will be concerted to 1, everything else will be converted to 0
# -9999 values will be converted back to NA
# This is done to help the aggregate() function which will calculate an intensification score
# per aggregate cell (0/25, 1/25...25/25) which will tell us how much nature gain or loss
# happened in the aggreggated cell between the periods of change


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
aggregated_intens_12_18 <- terra::aggregate(intens_12_18, fact = 5,
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

# Create a raster with the same properties as CORINE (no issue that we are only using 1 year)
land_cover_id <- aggregated_extens[[1]]

# Assign each cell a unique ID from 1 to ncell
land_cover_id[] <- 1:ncell(aggregated_extens[[1]])

# 3. PREPARE OCCURRENCES FOR TURNOVER ANALYSIS -----

## 3.1. Subset occurrence records df for each period of "before" and "after" change

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

## 3.2. Convert occurrences to sf objects and set CRS ----

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

# 4. COMBINE OCCURRENCE SF OBJECTS WITH THE LAND COVER ID RASTER ----

## 4.1. Assign species to land cover ID cells for each time step ----

# Create a list of sf object names ----
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

## 5.2. Calculate turnover for each period of change ----
# First period = turnover between "_1997.2000" and "_2006.2009"
# Second period = turnover between "_2003.2006" and "_2012.2015"
# Third period = turnover between "_2009.2012" and "_2015.2018"

# First period (2000 to 2006)
occurrences_turnover$turnover2000.2006 <- mapply(calculate_turnover,
                                                 occurrences_turnover$species_1997.2000,
                                                 occurrences_turnover$species_2006.2009)

# Second period (2006 to 2012)
occurrences_turnover$turnover2006.2012 <- mapply(calculate_turnover,
                                                 occurrences_turnover$species_2003.2006,
                                                 occurrences_turnover$species_2012.2015)

# Third period (2012 to 2018)
occurrences_turnover$turnover2012.2018 <- mapply(calculate_turnover,
                                                 occurrences_turnover$species_2009.2012,
                                                 occurrences_turnover$species_2015.2018)

# 6. CREATE DF WITH TURNOVER AND INTENSIFICATION VALUES ----

## 6.1. Extract intensification values ----

# First period (2000 to 2006)
intens_values_2000.2006 <- values(aggregated_intens_00_06)

# Second period (2006 to 2012)
intens_values_2006.2012 <- values(aggregated_intens_06_12)

# Third period (2012 to 2018)
intens_values_2012.2018 <- values(aggregated_intens_12_18)

## 6.2. Convert values to vector ----

# First period (2000 to 2006)
if (is.matrix(intens_values_2000.2006)) {
  intens_values_2000.2006 <- as.vector(intens_values_2000.2006)
}

# Second period (2006 to 2012)
if (is.matrix(intens_values_2006.2012)) {
  intens_values_2006.2012 <- as.vector(intens_values_2006.2012)
}

# Third period (2012 to 2018)
if (is.matrix(intens_values_2012.2018)) {
  intens_values_2012.2018 <- as.vector(intens_values_2012.2018)
}

## 6.3. Extract intensification values using the unique cell IDs ----
intens_occurrences_turnover <- occurrences_turnover |>
  mutate(intens_2000.2006 = intens_values_2000.2006[occurrences_turnover$cell[,1]],
         intens_2006.2012 = intens_values_2006.2012[occurrences_turnover$cell[,1]],
         intens_2012.2018 = intens_values_2012.2018[occurrences_turnover$cell[,1]])

## 6.4. Write dataframe ----
saveRDS(intens_occurrences_turnover,
        here("data", "intensification_occurrence_turnover.rds"))

# 7. VISUALISE RELATIONSHIP BETWEEN INTENSIFICATION AND TURNOVER ----

## 7.1. Prepare data ----

# Remove rows with NA for intensification and NaN for turnover
intens_occurrences_turnover <- intens_occurrences_turnover |>
  filter_at(vars(intens_2000.2006, intens_2006.2012, intens_2012.2018),
            all_vars(!is.na(.))) |>
  filter_at(vars(turnover2000.2006, turnover2006.2012, turnover2012.2018),
            all_vars(!is.nan(.)))

# Remove unnecessary columns
intens_occurrences_turnover <- intens_occurrences_turnover |>
  select(-c(species_1997.2000, geometry_1997.2000,
            species_2006.2009, geometry_2006.2009, 
            species_2003.2006, geometry_2003.2006,
            species_2012.2015, geometry_2012.2015,
            species_2009.2012, geometry_2009.2012,
            species_2015.2018, geometry_2015.2018))

# Create separate df for turnover
intens_turnover_long <- intens_occurrences_turnover |>
  #select only the turnover columns
  select(c(cell, turnover2000.2006, turnover2006.2012,
           turnover2012.2018)) |>
  #convert to long format
  pivot_longer(
    cols = "turnover2000.2006":"turnover2012.2018",
    names_to = "turnover_year",
    values_to = "turnover") |>
  #change turnover_year value to only contain the year
  mutate(year = case_when(turnover_year == "turnover2000.2006" ~ "2000.2006",
                          turnover_year == "turnover2006.2012" ~ "2006.2012",
                          turnover_year == "turnover2012.2018" ~ "2012.2018"),
         new_cell = cell$U2006_CLC2000_V2020_20u1,
         ID = paste(new_cell, year, sep = "_")) |>
  #remove unnecessary turnover_year column
  select(-c(turnover_year, cell, new_cell))


# Create separate df for intensification (land cover)
intens_long <- intens_occurrences_turnover |>
  #select only the turnover columns
  select(c(cell, intens_2000.2006, intens_2006.2012,
           intens_2012.2018)) |>
  #convert to long format
  pivot_longer(
    cols = "intens_2000.2006":"intens_2012.2018",
    names_to = "intensification_year",
    values_to = "intensification_amount") |>
  #change intensification_year value to only contain the year
  mutate(year = case_when(intensification_year == "intens_2000.2006" ~ "2000.2006",
                          intensification_year == "intens_2006.2012" ~ "2006.2012",
                          intensification_year == "intens_2012.2018" ~ "2012.2018"),
         new_cell = cell$U2006_CLC2000_V2020_20u1,
         ID = paste(new_cell, year, sep = "_")) |>
  #remove unnecessary columns
  select(-c(cell, new_cell))

# Merge the intens_turnover_long and intens_long dfs
intens_turnover <- merge(intens_turnover_long, intens_long,
                         by = "ID")

# Check cols
colnames(intens_turnover)

#Check that year.x & year.y are the same
setequal(intens_turnover$year.x, intens_turnover$year.y) #TRUE

#Remove year.x and year.y columns
intens_turnover <- intens_turnover |>
  mutate(year = year.y) |>
  select(-year.x)

## 7.2. Plot relationship between intensification and turnover ----

# Convert year column to factor
intens_turnover$year <- as.factor(intens_turnover$year)

# Create new facet labels
new_labels <- c("2000.2006" = "2000 - 2006",
                "2006.2012" = "2006 - 2012",
                "2012.2018" = "2012 - 2018")

# Plot scatter plot with facetwrap
ggplot(intens_turnover, aes(x = intensification_amount, y= turnover,
                            color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Intensification")+
  ylab("Turnover")+
  scale_color_manual(values = c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  theme_classic()+
  theme(legend.position = "none")

# Save plot as .svg
ggsave(here("figures",
            "turnover_intensification.svg"))


# 8. CREATE DF WITH TURNOVER AND EXTENSIFICATION VALUES ----

## 8.1. Extract extensification values ----

# First period (2000 to 2006)
extens_values_2000.2006 <- values(aggregated_extens_00_06)

# Second period (2006 to 2012)
extens_values_2006.2012 <- values(aggregated_extens_06_12)

# Third period (2012 to 2018)
extens_values_2012.2018 <- values(aggregated_extens_12_18)

## 8.2. Convert values to vector ----

# First period (2000 to 2006)
if (is.matrix(extens_values_2000.2006)) {
  extens_values_2000.2006 <- as.vector(extens_values_2000.2006)
}

# Second period (2006 to 2012)
if (is.matrix(extens_values_2006.2012)) {
  extens_values_2006.2012 <- as.vector(extens_values_2006.2012)
}

# Third period (2012 to 2018)
if (is.matrix(extens_values_2012.2018)) {
  extens_values_2012.2018 <- as.vector(extens_values_2012.2018)
}

## 8.3. Extract extensification values using the unique cell IDs ----
extens_occurrences_turnover <- occurrences_turnover |>
  mutate(extens_2000.2006 = extens_values_2000.2006[occurrences_turnover$cell[,1]],
         extens_2006.2012 = extens_values_2006.2012[occurrences_turnover$cell[,1]],
         extens_2012.2018 = extens_values_2012.2018[occurrences_turnover$cell[,1]])

## 8.4. Write dataframe ----
saveRDS(extens_occurrences_turnover,
        here("data", "extensification_occurrence_turnover.rds"))

# 9. VISUALISE RELATIONSHIP BETWEEN EXTENSIFICATION AND TURNOVER ----

## 9.1. Prepare data ----

# Remove rows with NA for extensification and NaN for turnover
extens_occurrences_turnover <- extens_occurrences_turnover |>
  filter_at(vars(extens_2000.2006, extens_2006.2012, extens_2012.2018),
            all_vars(!is.na(.))) |>
  filter_at(vars(turnover2000.2006, turnover2006.2012, turnover2012.2018),
            all_vars(!is.nan(.)))

# Remove unneccessary columns
extens_occurrences_turnover <- extens_occurrences_turnover |>
  select(-c(species_1997.2000, geometry_1997.2000,
            species_2006.2009, geometry_2006.2009, 
            species_2003.2006, geometry_2003.2006,
            species_2012.2015, geometry_2012.2015,
            species_2009.2012, geometry_2009.2012,
            species_2015.2018, geometry_2015.2018))

# Create separate df for turnover
extens_turnover_long <- extens_occurrences_turnover |>
  #select only the turnover columns
  select(c(cell, turnover2000.2006, turnover2006.2012,
           turnover2012.2018)) |>
  #convert to long format
  pivot_longer(
    cols = "turnover2000.2006":"turnover2012.2018",
    names_to = "turnover_year",
    values_to = "turnover") |>
  #change turnover_year value to only contain the year
  mutate(year = case_when(turnover_year == "turnover2000.2006" ~ "2000.2006",
                          turnover_year == "turnover2006.2012" ~ "2006.2012",
                          turnover_year == "turnover2012.2018" ~ "2012.2018"),
         new_cell = cell$U2006_CLC2000_V2020_20u1,
         ID = paste(new_cell, year, sep = "_")) |>
  #remove unnecessary turnover_year column
  select(-c(turnover_year, cell, new_cell))

# Create separate df for the extensification values
extens_long <- extens_occurrences_turnover |>
  #select only the turnover columns
  select(c(cell, extens_2000.2006, extens_2006.2012,
           extens_2012.2018)) |>
  #convert to long format
  pivot_longer(
    cols = "extens_2000.2006":"extens_2012.2018",
    names_to = "extensification_year",
    values_to = "extensification_amount") |>
  #change extensification_year value to only contain the year
  mutate(year = case_when(extensification_year == "extens_2000.2006" ~ "2000.2006",
                          extensification_year == "extens_2006.2012" ~ "2006.2012",
                          extensification_year == "extens_2012.2018" ~ "2012.2018"),
         new_cell = cell$U2006_CLC2000_V2020_20u1,
         ID = paste(new_cell, year, sep = "_")) |>
  #remove unnecessary columns
  select(-c(cell, new_cell))

# Merge the extens_turnover_long and extens_long dfs
extens_turnover <- merge(extens_turnover_long, extens_long,
                         by = "ID")

# Check cols
colnames(extens_turnover)

#Check that year.x & year.y are the same
setequal(extens_turnover$year.x, extens_turnover$year.y) #TRUE

#Remove year.x and year.y columns
extens_turnover <- extens_turnover |>
  mutate(year = year.y) |>
  select(-year.x)

## 9.2. Plot relationship between extensification and turnover ----

# Convert year column to factor
extens_turnover$year <- as.factor(extens_turnover$year)

# Create new facet labels
new_labels <- c("2000.2006" = "2000 - 2006",
                "2006.2012" = "2006 - 2012",
                "2012.2018" = "2012 - 2018")

# Plot scatterplot with facetwrap
ggplot(extens_turnover, aes(x = extensification_amount, y= turnover,
                            color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Extensification")+
  ylab("Turnover")+
  scale_color_manual(values = c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  theme_classic()+
  theme(legend.position = "none")

# Save plot as .svg
ggsave(here("figures",
            "turnover_extensification.svg"))

## 9.3. Combine intensification and extensification plots ---

# Intensification figure
intensification_plot <- ggplot(intens_turnover, aes(x = intensification_amount, y= turnover,
                                                    color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Intensification")+
  ylab("Turnover")+
  scale_color_manual(values = c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  theme_classic()+
  theme(legend.position = "none")

# Extenification figure
extensification_plot <- ggplot(extens_turnover, aes(x = extensification_amount, y= turnover,
                                                    color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Extensification")+
  ylab("Turnover")+
  scale_color_manual(values = c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  theme_classic()+
  theme(legend.position = "none")

# Combine intenification and extesification figure
combined_plot <- plot_grid(intensification_plot, extensification_plot, 
                           labels = c('A', 'B'), label_size = 12,
                           ncol = 1)

# Save combined plot
ggsave(filename = here("figures",
                       "intensification_extensification_turnover.svg"),
       plot = combined_plot)

# 10. HISTOGRAMS OF TURNOVER, INTENSIFICATION AND EXTENSIFICATION VALUES ----

## 10.1. Histogram of turnover values ----

# Plot histogram
ggplot(extens_turnover, aes(x = turnover, fill = year))+
  geom_histogram(color = "black", alpha = 0.6, position = "identity")+
  scale_fill_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

# Save histogram to file
ggsave(here("figures",
            "intensification_turnover_histogram.svg"))

## 10.2. Histogram of intensification values ----

# Plot histogram
ggplot(intens_turnover, aes(x = intensification_amount, fill = year))+
  geom_histogram(color = "black", alpha = 0.6, position = "identity")+
  scale_fill_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Intensification Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

# Save histogram to file
ggsave(here("figures",
            "intensification_histogram.svg"))

## 10.3. Histogram of extensification values ----

# Plot histogram
ggplot(extens_turnover, aes(x = extensification_amount, fill = year))+
  geom_histogram(color = "black", alpha = 0.6, position = "identity")+
  scale_fill_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Extensification Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

# Save histogram to file
ggsave(here("figures",
            "extensification_histogram.svg"))

## 10.4. Histograms of intensification without 0 values ----

# Subset df to remove rows with intensification = 0
intens_turnover_non0 <- intens_turnover |>
  filter(intensification_amount != 0)

# Plot histogram
ggplot(intens_turnover_non0, aes(x = intensification_amount, fill = year))+
  geom_histogram(color = "black", alpha = 0.6, position = "identity")+
  scale_fill_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Intensification Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

# Save histogram to file
ggsave(here("figures",
            "intensification_histogram_non0.svg"))

## 10.5. Histograms of extensification without 0 values ----

# Subset df to remove rows with extensification = 0
extens_turnover_non0 <- extens_turnover |>
  filter(extensification_amount != 0)

# Plot histogram
ggplot(extens_turnover_non0, aes(x = extensification_amount, fill = year))+
  geom_histogram(color = "black", alpha = 0.6, position = "identity")+
  scale_fill_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Extensification Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

# Save histogram to file
ggsave(here("figures",
            "extensificatin_histogram_non0.svg"))

# END OF SCRIPT #       