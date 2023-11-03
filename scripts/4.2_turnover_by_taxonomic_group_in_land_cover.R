##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
#- 4.2_turnover_by_taxonomic_group_in_land_cover -##
##------------------------------------------------##

# This script contains code which calculates turnover of species in CORINE 
# land cover layers aggregated to 500m x 500m
# the turnover is calculated separately for each major taxonomic group
# for now, this script only uses the aggregated_500m_intensification_stack.tif
# created in script 4.1. (i.e. only looking at intensification for now)

# 0. PACKAGES ----
library(here)
library(terra)
library(data.table)
library(dplyr)
library(sf)

# 1. LOAD DATA ----

## 1.1. Intensification CORINE Land Cover ----
intens_norway_corine <- rast(here("data",
                                  "aggregated_500m_intensification_stack.tif"))

## 1.2. GBIF Occurrence Records -----
clean_occurrences <- fread(here::here("data", 
                                      "cleaned_occurrences.txt"))

# 2. PREPARE LAND COVER DATA ----

#This raster is needed to find the identity of the corine cells in which turnover takes place

# Create a raster with the same properties as corine (no issue that we are only using 1 year)
land_cover_id <- intens_norway_corine[[1]]

# Assign each cell a unique ID from 1 to ncell
land_cover_id[] <- 1:ncell(intens_norway_corine[[1]])

# 3. PREPARE OCCURRENCE RECORDS FOR TURNOVER CALCULATION ----

## 3.1. Create subset dfs for each taxonomic group conisdered ----

#Taxonomic groups considered: Plant, Fungi, Aves, Mammals, Insects

# Check the families in the GBIF data
levels(as.factor(clean_occurrences$family))

# Plants
plant_occurrences <- clean_occurrences |>
  filter(kingdom == "Plantae")

# Fungi
fungi_occurrences <- clean_occurrences |>
  filter(kingdom == "Fungi")

# Aves
aves_occurrences <- clean_occurrences |>
  filter(class == "Aves")

# Mammalia
mammalia_occurrences <- clean_occurrences |>
  filter(class == "Mammalia")

# Insecta
insecta_occurrencs <- clean_occurrences |>
  filter(class == "Insecta")

#PLANTAE########################################################################
## 3.2. Subset occurrence records df for each period of "before" and "after" change ----
# N.B: Periods of change are:
# First period of land cover change: 2000 to 2006; BEFORE Change = 1997-2000, AFTER Change = 2006-2009
# Second period of land cover change: 2006 to 2012; BEFORE Change = 2003-2006, AFTER Change = 2012-2015
# Third period of land cover change: 2012 to 2018; BEFORE Change = 2009-2012, AFTER Change = 2015-2018

# Period 1: 2000 to 2006
# 1997-2000
plant_occ_1997.2000 <- plant_occurrences[plant_occurrences$year %in% c(1997:2000)]
# 2006-2009
plant_occ_2006.2009 <- plant_occurrences[plant_occurrences$year %in% c(2006:2009)]

# Period 2: 2006 to 2012
# 2003-2006
plant_occ_2003.2006 <- plant_occurrences[plant_occurrences$year %in% c(2003:2006)]
# 2012-2015
plant_occ_2012.2015 <- plant_occurrences[plant_occurrences$year %in% c(2012:2015)]

# Period 3: 2006 to 2012
# 2009-2012
plant_occ_2009.2012 <- plant_occurrences[plant_occurrences$year %in% c(2009:2012)]
# 2015-2018
plant_occ_2015.2018 <- plant_occurrences[plant_occurrences$year %in% c(2015:2018)]

## 3.3. Convert occurrences to sf objects ----
# 1997-2000
plant_occ_1997.2000_sf <- st_as_sf(plant_occ_1997.2000,
                                   coords = c("decimalLongitude", "decimalLatitude"),
                                   crs = 4326)
# 2006-2009
plant_occ_2006.2009_sf <- st_as_sf(plant_occ_2006.2009,
                                   coords = c("decimalLongitude", "decimalLatitude"),
                                   crs = 4326)
#2003-2006
plant_occ_2003.2006_sf <- st_as_sf(plant_occ_2003.2006,
                                   coords = c("decimalLongitude", "decimalLatitude"),
                                   crs = 4326)
#2012-2015
plant_occ_2012.2015_sf <- st_as_sf(plant_occ_2012.2015,
                                   coords = c("decimalLongitude", "decimalLatitude"),
                                   crs = 4326)
#2009-2012
plant_occ_2009.2012_sf <- st_as_sf(plant_occ_2009.2012,
                                   coords = c("decimalLongitude", "decimalLatitude"),
                                   crs = 4326)
#2015-2018
plant_occ_2015.2018_sf <- st_as_sf(plant_occ_2015.2018,
                                   coords = c("decimalLongitude", "decimalLatitude"),
                                   crs = 4326)

## 3.4. Set CRS for sf objects to match CORINE's CRS ----
# 1997-2000
plant_occ_1997.2000_sf <- st_transform(plant_occ_1997.2000_sf,
                                       st_crs(intens_norway_corine[[1]]))
# 2006-2009
plant_occ_2006.2009_sf <- st_transform(plant_occ_2006.2009_sf,
                                       st_crs(intens_norway_corine[[1]]))
# 2003-2006
plant_occ_2003.2006_sf <- st_transform(plant_occ_2003.2006_sf,
                                       st_crs(intens_norway_corine[[1]]))
# 2012-2015
plant_occ_2012.2015_sf <- st_transform(plant_occ_2003.2006_sf,
                                       st_crs(intens_norway_corine[[1]]))
# 2009-2012
plant_occ_2009.2012_sf <- st_transform(plant_occ_2009.2012_sf,
                                       st_crs(intens_norway_corine[[1]]))
# 2015-2018
plant_occ_2015.2018_sf <- st_transform(plant_occ_2015.2018_sf,
                                       st_crs(intens_norway_corine[[1]]))

# 4. COMBINE LAND COVER AND OCCURRENCE DATA ----

## 4.1. Assign species to land cover cells for each time step ----

# 1997-2000
plant_occ_1997.2000_sf$cell <- terra::extract(land_cover_id,
                                              as.matrix(st_coordinates(plant_occ_1997.2000_sf)))

# 2006-2009
plant_occ_2006.2009_sf$cell <- terra::extract(land_cover_id,
                                              as.matrix(st_coordinates(plant_occ_2006.2009_sf)))
# 2003-2006
plant_occ_2003.2006_sf$cell <- terra::extract(land_cover_id,
                                              as.matrix(st_coordinates(plant_occ_2003.2006_sf)))
# 2012-2015
plant_occ_2012.2015_sf$cell <- terra::extract(land_cover_id,
                                              as.matrix(st_coordinates(plant_occ_2012.2015_sf)))
# 2009-2012
plant_occ_2009.2012_sf$cell <- terra::extract(land_cover_id,
                                              as.matrix(st_coordinates(plant_occ_2009.2012_sf)))
# 2015-2018
plant_occ_2015.2018_sf$cell <- terra::extract(land_cover_id,
                                              as.matrix(st_coordinates(plant_occ_2015.2018_sf)))

## 4.2. Group by cell to create list of species for each cell ----

# 1997-2000
plant_occ_1997.2000_grouped <- plant_occ_1997.2000_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2006-2009
plant_occ_2006.2009_grouped <- plant_occ_2006.2009_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2003-2006
plant_occ_2003.2006_grouped <- plant_occ_2003.2006_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2012-2015
plant_occ_2012.2015_grouped <- plant_occ_2012.2015_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2009-2012
plant_occ_2009.2012_grouped <- plant_occ_2009.2012_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2015-2018
plant_occ_2015.2018_grouped <- plant_occ_2015.2018_sf |>
  group_by(cell) |>
  summarise(species = list(species))

## 4.3. Join data for the different timesteps ----

# Join data 2 by two, based on the corine land cover layer they were created with/from
plant_turnover_1 <- left_join(as.data.frame(plant_occ_1997.2000_grouped),
                              as.data.frame(plant_occ_2006.2009_grouped),
                              by = "cell",
                              suffix = c("_1997.2000", "_2006.2009"))


plant_turnover_2 <- left_join(as.data.frame(plant_occ_2003.2006_grouped),
                              as.data.frame(plant_occ_2012.2015_grouped),
                              by = "cell",
                              suffix = c("_2003.2006", "_2012.2015"))


plant_turnover_3 <- left_join(as.data.frame(plant_occ_2009.2012_grouped),
                              as.data.frame(plant_occ_2015.2018_grouped),
                              by = "cell",
                              suffix = c("_2009.2012", "_2015.2018"))


#Combine all 3 occurrence turnover dataframes in 1
plant_turnover_a <- left_join(plant_turnover_1,
                              plant_turnover_2,
                              by = "cell")


plant_turnover <- left_join(plant_turnover_a,
                            plant_turnover_3,
                            by = "cell")

# 5. CALCULATE TURNOVER ----

## 5.1. Write function to calculate turnover ----
calculate_turnover <- function(species1, species2) {
  unique1 <- length(setdiff(unlist(species1), unlist(species2)))
  unique2 <- length(setdiff(unlist(species2), unlist(species1)))
  total_occurrences <- length(unlist(species1)) + length(unlist(species2))
  return((unique1 + unique2) / total_occurrences)
}

list1<-list(c('x','y','z'))
list2<-list(c('x','y','z'))
calculate_turnover(list1,list2)

## 5.2. Calculate turnover for each period ----
# First period = turnover between "_1997.2000" and "_2006.2009"
# Second period = turnover between "_2003.2006" and "_2012.2015"
# Third period = turnover between "_2009.2012" and "_2015.2018"

# First period (2000 to 2006)
plant_turnover$turover2000.2006 <- mapply(calculate_turnover,
                                          plant_turnover$species_1997.2000,
                                          plant_turnover$species_2006.2009)


# Second period (2006 to 2012)
plant_turnover$turover2006.2012 <- mapply(calculate_turnover,
                                          plant_turnover$species_2003.2006,
                                          plant_turnover$species_2012.2015)

# Third period (2012 to 2018)
plant_turnover$turover2012.2018 <- mapply(calculate_turnover,
                                          plant_turnover$species_2009.2012,
                                          plant_turnover$species_2015.2018)


# 6. CREATE DF WITH TURNOVER AND LAND COVER CHANGE VALUES ----

## 6.1. Extract values for land cover change ----

# First period
values_2000.2006 <- terra::values(intens_norway_corine[[1]])


# Second period
values_2006.2012 <- terra::values(intens_norway_corine[[2]])

# Third period
values_2012.2018 <- terra::values(intens_norway_corine[[3]])

## 6.2. Convert to vector ----

# First period
if (is.matrix(values_2000.2006)) {
  values_2000.2006 <- as.vector(values_2000.2006)
}

# Second period
if (is.matrix(values_2006.2012)) {
  values_2006.2012 <- as.vector(values_2006.2012)
}

# Third period
if (is.matrix(values_2012.2018)) {
  values_2012.2018 <- as.vector(values_2012.2018)
}

## 6.3. Extract land cover values using the unique cell IDs ----
plant_turnover <- plant_turnover |>
  mutate(cover_change_2000.2006 = values_2000.2006[plant_turnover$cell[,1]],
         cover_change_2006.2012 = values_2006.2012[plant_turnover$cell[,1]],
         cover_change_2012.2018 = values_2012.2018[plant_turnover$cell[,1]])

## 6.4. Write dataframe ----
saveRDS(plant_turnover,
        here("data", "plant_turnover.rds"))


#AVES###########################################################################
# 7. BIRD TURNOVER ----

## 7.2. Subset occurrence records df for each period of "before" and "after" change ----
# N.B: Periods of change are:
# First period of land cover change: 2000 to 2006; BEFORE Change = 1997-2000, AFTER Change = 2006-2009
# Second period of land cover change: 2006 to 2012; BEFORE Change = 2003-2006, AFTER Change = 2012-2015
# Third period of land cover change: 2012 to 2018; BEFORE Change = 2009-2012, AFTER Change = 2015-2018

# Period 1: 2000 to 2006
# 1997-2000
aves_occ_1997.2000 <- aves_occurrences[aves_occurrences$year %in% c(1997:2000)]
# 2006-2009
aves_occ_2006.2009 <- aves_occurrences[aves_occurrences$year %in% c(2006:2009)]

# Period 2: 2006 to 2012
# 2003-2006
aves_occ_2003.2006 <- aves_occurrences[aves_occurrences$year %in% c(2003:2006)]
# 2012-2015
aves_occ_2012.2015 <- aves_occurrences[aves_occurrences$year %in% c(2012:2015)]

# Period 3: 2006 to 2012
# 2009-2012
aves_occ_2009.2012 <- aves_occurrences[aves_occurrences$year %in% c(2009:2012)]
# 2015-2018
aves_occ_2015.2018 <- aves_occurrences[aves_occurrences$year %in% c(2015:2018)]

## 7.3. Convert occurrences to sf objects ----
# 1997-2000
aves_occ_1997.2000_sf <- st_as_sf(aves_occ_1997.2000,
                                  coords = c("decimalLongitude", "decimalLatitude"),
                                  crs = 4326)
# 2006-2009
aves_occ_2006.2009_sf <- st_as_sf(aves_occ_2006.2009,
                                  coords = c("decimalLongitude", "decimalLatitude"),
                                  crs = 4326)
#2003-2006
aves_occ_2003.2006_sf <- st_as_sf(aves_occ_2003.2006,
                                  coords = c("decimalLongitude", "decimalLatitude"),
                                  crs = 4326)
#2012-2015
aves_occ_2012.2015_sf <- st_as_sf(aves_occ_2012.2015,
                                  coords = c("decimalLongitude", "decimalLatitude"),
                                  crs = 4326)
#2009-2012
aves_occ_2009.2012_sf <- st_as_sf(aves_occ_2009.2012,
                                  coords = c("decimalLongitude", "decimalLatitude"),
                                  crs = 4326)
#2015-2018
aves_occ_2015.2018_sf <- st_as_sf(aves_occ_2015.2018,
                                  coords = c("decimalLongitude", "decimalLatitude"),
                                  crs = 4326)

## 7.4. Set CRS for sf objects to match CORINE's CRS ----
# 1997-2000
aves_occ_1997.2000_sf <- st_transform(aves_occ_1997.2000_sf,
                                      st_crs(intens_norway_corine[[1]]))
# 2006-2009
aves_occ_2006.2009_sf <- st_transform(aves_occ_2006.2009_sf,
                                      st_crs(intens_norway_corine[[1]]))
# 2003-2006
aves_occ_2003.2006_sf <- st_transform(aves_occ_2003.2006_sf,
                                      st_crs(intens_norway_corine[[1]]))
# 2012-2015
aves_occ_2012.2015_sf <- st_transform(aves_occ_2012.2015_sf,
                                      st_crs(intens_norway_corine[[1]]))
# 2009-2012
aves_occ_2009.2012_sf <- st_transform(aves_occ_2009.2012_sf,
                                      st_crs(intens_norway_corine[[1]]))
# 2015-2018
aves_occ_2015.2018_sf <- st_transform(aves_occ_2015.2018_sf,
                                      st_crs(intens_norway_corine[[1]]))

# 8. COMBINE LAND COVER AND OCCURRENCE DATA ----

## 8.1. Assign species to land cover cells for each time step ----

# 1997-2000
aves_occ_1997.2000_sf$cell <- terra::extract(land_cover_id,
                                             as.matrix(st_coordinates(aves_occ_1997.2000_sf)))

# 2006-2009
aves_occ_2006.2009_sf$cell <- terra::extract(land_cover_id,
                                             as.matrix(st_coordinates(aves_occ_2006.2009_sf)))
# 2003-2006
aves_occ_2003.2006_sf$cell <- terra::extract(land_cover_id,
                                             as.matrix(st_coordinates(aves_occ_2003.2006_sf)))
# 2012-2015
aves_occ_2012.2015_sf$cell <- terra::extract(land_cover_id,
                                             as.matrix(st_coordinates(aves_occ_2012.2015_sf)))
# 2009-2012
aves_occ_2009.2012_sf$cell <- terra::extract(land_cover_id,
                                             as.matrix(st_coordinates(aves_occ_2009.2012_sf)))
# 2015-2018
aves_occ_2015.2018_sf$cell <- terra::extract(land_cover_id,
                                             as.matrix(st_coordinates(aves_occ_2015.2018_sf)))

## 8.2. Group by cell to create list of species for each cell ----

# 1997-2000
aves_occ_1997.2000_grouped <- aves_occ_1997.2000_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2006-2009
aves_occ_2006.2009_grouped <- aves_occ_2006.2009_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2003-2006
aves_occ_2003.2006_grouped <- aves_occ_2003.2006_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2012-2015
aves_occ_2012.2015_grouped <- aves_occ_2012.2015_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2009-2012
aves_occ_2009.2012_grouped <- aves_occ_2009.2012_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2015-2018
aves_occ_2015.2018_grouped <- aves_occ_2015.2018_sf |>
  group_by(cell) |>
  summarise(species = list(species))

## 8.3. Join data for the different timesteps ----

# Join data 2 by two, based on the corine land cover layer they were created with/from
aves_turnover_1 <- left_join(as.data.frame(aves_occ_1997.2000_grouped),
                             as.data.frame(aves_occ_2006.2009_grouped),
                             by = "cell",
                             suffix = c("_1997.2000", "_2006.2009"))


aves_turnover_2 <- left_join(as.data.frame(aves_occ_2003.2006_grouped),
                             as.data.frame(aves_occ_2012.2015_grouped),
                             by = "cell",
                             suffix = c("_2003.2006", "_2012.2015"))


aves_turnover_3 <- left_join(as.data.frame(aves_occ_2009.2012_grouped),
                             as.data.frame(aves_occ_2015.2018_grouped),
                             by = "cell",
                             suffix = c("_2009.2012", "_2015.2018"))


#Combine all 3 occurrence turnover dataframes in 1
aves_turnover_a <- left_join(aves_turnover_1,
                             aves_turnover_2,
                             by = "cell")


aves_turnover <- left_join(aves_turnover_a,
                           aves_turnover_3,
                           by = "cell")

# 9. CALCULATE TURNOVER ----

## 9.2. Calculate turnover for each period ----
# First period = turnover between "_1997.2000" and "_2006.2009"
# Second period = turnover between "_2003.2006" and "_2012.2015"
# Third period = turnover between "_2009.2012" and "_2015.2018"

# First period (2000 to 2006)
aves_turnover$turover2000.2006 <- mapply(calculate_turnover,
                                         aves_turnover$species_1997.2000,
                                         aves_turnover$species_2006.2009)


# Second period (2006 to 2012)
aves_turnover$turover2006.2012 <- mapply(calculate_turnover,
                                         aves_turnover$species_2003.2006,
                                         aves_turnover$species_2012.2015)

# Third period (2012 to 2018)
aves_turnover$turover2012.2018 <- mapply(calculate_turnover,
                                         aves_turnover$species_2009.2012,
                                         aves_turnover$species_2015.2018)

# 10. CREATE DF WITH TURNOVER AND LAND COVER CHANGE VALUES ----

## 10.3. Extract land cover values using the unique cell IDs ----
aves_turnover <- aves_turnover |>
  mutate(cover_change_2000.2006 = values_2000.2006[aves_turnover$cell[,1]],
         cover_change_2006.2012 = values_2006.2012[aves_turnover$cell[,1]],
         cover_change_2012.2018 = values_2012.2018[aves_turnover$cell[,1]])

## 10.4. Write dataframe ----
saveRDS(aves_turnover,
        here("data", "aves_turnover.rds"))


#MAMMALS########################################################################
# 11. MAMMAL TURNOVER ----

## 11.1. Subset occurrence records df for each period of "before" and "after" change ----
# N.B: Periods of change are:
# First period of land cover change: 2000 to 2006; BEFORE Change = 1997-2000, AFTER Change = 2006-2009
# Second period of land cover change: 2006 to 2012; BEFORE Change = 2003-2006, AFTER Change = 2012-2015
# Third period of land cover change: 2012 to 2018; BEFORE Change = 2009-2012, AFTER Change = 2015-2018

# Period 1: 2000 to 2006
# 1997-2000
mammal_occ_1997.2000 <- mammalia_occurrences[mammalia_occurrences$year %in% c(1997:2000)]
# 2006-2009
mammal_occ_2006.2009 <- mammalia_occurrences[mammalia_occurrences$year %in% c(2006:2009)]

# Period 2: 2006 to 2012
# 2003-2006
mammal_occ_2003.2006 <- mammalia_occurrences[mammalia_occurrences$year %in% c(2003:2006)]
# 2012-2015
mammal_occ_2012.2015 <- mammalia_occurrences[mammalia_occurrences$year %in% c(2012:2015)]

# Period 3: 2006 to 2012
# 2009-2012
mammal_occ_2009.2012 <- mammalia_occurrences[mammalia_occurrences$year %in% c(2009:2012)]
# 2015-2018
mammal_occ_2015.2018 <- mammalia_occurrences[mammalia_occurrences$year %in% c(2015:2018)]

## 11.2. Convert occurrences to sf objects ----
# 1997-2000
mammal_occ_1997.2000_sf <- st_as_sf(mammal_occ_1997.2000,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
# 2006-2009
mammal_occ_2006.2009_sf <- st_as_sf(mammal_occ_2006.2009,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
#2003-2006
mammal_occ_2003.2006_sf <- st_as_sf(mammal_occ_2003.2006,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
#2012-2015
mammal_occ_2012.2015_sf <- st_as_sf(mammal_occ_2012.2015,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
#2009-2012
mammal_occ_2009.2012_sf <- st_as_sf(mammal_occ_2009.2012,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)
#2015-2018
mammal_occ_2015.2018_sf <- st_as_sf(mammal_occ_2015.2018,
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)

## 11.3. Set CRS for sf objects to match CORINE's CRS ----
# 1997-2000
mammal_occ_1997.2000_sf <- st_transform(mammal_occ_1997.2000_sf,
                                        st_crs(intens_norway_corine[[1]]))
# 2006-2009
mammal_occ_2006.2009_sf <- st_transform(mammal_occ_2006.2009_sf,
                                        st_crs(intens_norway_corine[[1]]))
# 2003-2006
mammal_occ_2003.2006_sf <- st_transform(mammal_occ_2003.2006_sf,
                                        st_crs(intens_norway_corine[[1]]))
# 2012-2015
mammal_occ_2012.2015_sf <- st_transform(mammal_occ_2012.2015_sf,
                                        st_crs(intens_norway_corine[[1]]))
# 2009-2012
mammal_occ_2009.2012_sf <- st_transform(mammal_occ_2009.2012_sf,
                                        st_crs(intens_norway_corine[[1]]))
# 2015-2018
mammal_occ_2015.2018_sf <- st_transform(mammal_occ_2015.2018_sf,
                                        st_crs(intens_norway_corine[[1]]))

# 12. COMBINE LAND COVER AND OCCURRENCE DATA ----

## 12.1. Assign species to land cover cells for each time step ----

# 1997-2000
mammal_occ_1997.2000_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(mammal_occ_1997.2000_sf)))

# 2006-2009
mammal_occ_2006.2009_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(mammal_occ_2006.2009_sf)))
# 2003-2006
mammal_occ_2003.2006_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(mammal_occ_2003.2006_sf)))
# 2012-2015
mammal_occ_2012.2015_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(mammal_occ_2012.2015_sf)))
# 2009-2012
mammal_occ_2009.2012_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(mammal_occ_2009.2012_sf)))
# 2015-2018
mammal_occ_2015.2018_sf$cell <- terra::extract(land_cover_id,
                                               as.matrix(st_coordinates(mammal_occ_2015.2018_sf)))

## 12.2. Group by cell to create list of species for each cell ----

# 1997-2000
mammal_occ_1997.2000_grouped <- mammal_occ_1997.2000_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2006-2009
mammal_occ_2006.2009_grouped <- mammal_occ_2006.2009_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2003-2006
mammal_occ_2003.2006_grouped <- mammal_occ_2003.2006_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2012-2015
mammal_occ_2012.2015_grouped <- mammal_occ_2012.2015_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2009-2012
mammal_occ_2009.2012_grouped <- mammal_occ_2009.2012_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2015-2018
mammal_occ_2015.2018_grouped <- mammal_occ_2015.2018_sf |>
  group_by(cell) |>
  summarise(species = list(species))

## 12.3. Join data for the different timesteps ----

# Join data 2 by two, based on the corine land cover layer they were created with/from
mammal_turnover_1 <- left_join(as.data.frame(aves_occ_1997.2000_grouped),
                               as.data.frame(aves_occ_2006.2009_grouped),
                               by = "cell",
                               suffix = c("_1997.2000", "_2006.2009"))


mammal_turnover_2 <- left_join(as.data.frame(aves_occ_2003.2006_grouped),
                               as.data.frame(aves_occ_2012.2015_grouped),
                               by = "cell",
                               suffix = c("_2003.2006", "_2012.2015"))


mammal_turnover_3 <- left_join(as.data.frame(aves_occ_2009.2012_grouped),
                               as.data.frame(aves_occ_2015.2018_grouped),
                               by = "cell",
                               suffix = c("_2009.2012", "_2015.2018"))


#Combine all 3 occurrence turnover dataframes in 1
mammal_turnover_a <- left_join(mammal_turnover_1,
                               mammal_turnover_2,
                               by = "cell")


mammal_turnover <- left_join(mammal_turnover_a,
                             mammal_turnover_3,
                             by = "cell")

# 13. CALCULATE TURNOVER ----

## 13.1. Calculate turnover for each period ----
# First period = turnover between "_1997.2000" and "_2006.2009"
# Second period = turnover between "_2003.2006" and "_2012.2015"
# Third period = turnover between "_2009.2012" and "_2015.2018"

# First period (2000 to 2006)
mammal_turnover$turover2000.2006 <- mapply(calculate_turnover,
                                           mammal_turnover$species_1997.2000,
                                           mammal_turnover$species_2006.2009)


# Second period (2006 to 2012)
mammal_turnover$turover2006.2012 <- mapply(calculate_turnover,
                                           mammal_turnover$species_2003.2006,
                                           mammal_turnover$species_2012.2015)

# Third period (2012 to 2018)
mammal_turnover$turover2012.2018 <- mapply(calculate_turnover,
                                           mammal_turnover$species_2009.2012,
                                           mammal_turnover$species_2015.2018)

# 14. CREATE DF WITH TURNOVER AND LAND COVER CHANGE VALUES ----

## 14.1. Extract land cover values using the unique cell IDs ----
mammal_turnover <- mammal_turnover |>
  mutate(cover_change_2000.2006 = values_2000.2006[mammal_turnover$cell[,1]],
         cover_change_2006.2012 = values_2006.2012[mammal_turnover$cell[,1]],
         cover_change_2012.2018 = values_2012.2018[mammal_turnover$cell[,1]])

## 14.2. Write dataframe ----
saveRDS(mammal_turnover,
        here("data", "mammalia_turnover.rds"))


#INSECTS########################################################################
# 15. INSECTS TURNOVER ----

## 15.1. Subset occurrence records df for each period of "before" and "after" change ----
# N.B: Periods of change are:
# First period of land cover change: 2000 to 2006; BEFORE Change = 1997-2000, AFTER Change = 2006-2009
# Second period of land cover change: 2006 to 2012; BEFORE Change = 2003-2006, AFTER Change = 2012-2015
# Third period of land cover change: 2012 to 2018; BEFORE Change = 2009-2012, AFTER Change = 2015-2018

# Period 1: 2000 to 2006
# 1997-2000
insecta_occ_1997.2000 <- insecta_occurrencs[insecta_occurrencs$year %in% c(1997:2000)]
# 2006-2009
insecta_occ_2006.2009 <- insecta_occurrencs[insecta_occurrencs$year %in% c(2006:2009)]

# Period 2: 2006 to 2012
# 2003-2006
insecta_occ_2003.2006 <- insecta_occurrencs[insecta_occurrencs$year %in% c(2003:2006)]
# 2012-2015
insecta_occ_2012.2015 <- insecta_occurrencs[insecta_occurrencs$year %in% c(2012:2015)]

# Period 3: 2006 to 2012
# 2009-2012
insecta_occ_2009.2012 <- insecta_occurrencs[insecta_occurrencs$year %in% c(2009:2012)]
# 2015-2018
insecta_occ_2015.2018 <- insecta_occurrencs[insecta_occurrencs$year %in% c(2015:2018)]

## 15.2. Convert occurrences to sf objects ----
# 1997-2000
insecta_occ_1997.2000_sf <- st_as_sf(insecta_occ_1997.2000,
                                     coords = c("decimalLongitude", "decimalLatitude"),
                                     crs = 4326)
# 2006-2009
insecta_occ_2006.2009_sf <- st_as_sf(insecta_occ_2006.2009,
                                     coords = c("decimalLongitude", "decimalLatitude"),
                                     crs = 4326)
#2003-2006
insecta_occ_2003.2006_sf <- st_as_sf(insecta_occ_2003.2006,
                                     coords = c("decimalLongitude", "decimalLatitude"),
                                     crs = 4326)
#2012-2015
insecta_occ_2012.2015_sf <- st_as_sf(insecta_occ_2012.2015,
                                     coords = c("decimalLongitude", "decimalLatitude"),
                                     crs = 4326)
#2009-2012
insecta_occ_2009.2012_sf <- st_as_sf(insecta_occ_2009.2012,
                                     coords = c("decimalLongitude", "decimalLatitude"),
                                     crs = 4326)
#2015-2018
insecta_occ_2015.2018_sf <- st_as_sf(insecta_occ_2015.2018,
                                     coords = c("decimalLongitude", "decimalLatitude"),
                                     crs = 4326)

## 15.3. Set CRS for sf objects to match CORINE's CRS ----
# 1997-2000
insecta_occ_1997.2000_sf <- st_transform(insecta_occ_1997.2000_sf,
                                         st_crs(intens_norway_corine[[1]]))
# 2006-2009
insecta_occ_2006.2009_sf <- st_transform(insecta_occ_2006.2009_sf,
                                         st_crs(intens_norway_corine[[1]]))
# 2003-2006
insecta_occ_2003.2006_sf <- st_transform(insecta_occ_2003.2006_sf,
                                         st_crs(intens_norway_corine[[1]]))
# 2012-2015
insecta_occ_2012.2015_sf <- st_transform(insecta_occ_2012.2015_sf,
                                         st_crs(intens_norway_corine[[1]]))
# 2009-2012
insecta_occ_2009.2012_sf <- st_transform(insecta_occ_2009.2012_sf,
                                         st_crs(intens_norway_corine[[1]]))
# 2015-2018
insecta_occ_2015.2018_sf <- st_transform(insecta_occ_2015.2018_sf,
                                         st_crs(intens_norway_corine[[1]]))

# 16. COMBINE LAND COVER AND OCCURRENCE DATA ----

## 16.1. Assign species to land cover cells for each time step ----

# 1997-2000
insecta_occ_1997.2000_sf$cell <- terra::extract(land_cover_id,
                                                as.matrix(st_coordinates(insecta_occ_1997.2000_sf)))

# 2006-2009
insecta_occ_2006.2009_sf$cell <- terra::extract(land_cover_id,
                                                as.matrix(st_coordinates(insecta_occ_2006.2009_sf)))
# 2003-2006
insecta_occ_2003.2006_sf$cell <- terra::extract(land_cover_id,
                                                as.matrix(st_coordinates(insecta_occ_2003.2006_sf)))
# 2012-2015
insecta_occ_2012.2015_sf$cell <- terra::extract(land_cover_id,
                                                as.matrix(st_coordinates(insecta_occ_2012.2015_sf)))
# 2009-2012
insecta_occ_2009.2012_sf$cell <- terra::extract(land_cover_id,
                                                as.matrix(st_coordinates(insecta_occ_2009.2012_sf)))
# 2015-2018
insecta_occ_2015.2018_sf$cell <- terra::extract(land_cover_id,
                                                as.matrix(st_coordinates(insecta_occ_2015.2018_sf)))

## 16.2. Group by cell to create list of species for each cell ----

# 1997-2000
insecta_occ_1997.2000_grouped <- insecta_occ_1997.2000_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2006-2009
insecta_occ_2006.2009_grouped <- insecta_occ_2006.2009_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2003-2006
insecta_occ_2003.2006_grouped <- insecta_occ_2003.2006_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2012-2015
insecta_occ_2012.2015_grouped <- insecta_occ_2012.2015_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2009-2012
insecta_occ_2009.2012_grouped <- insecta_occ_2009.2012_sf |>
  group_by(cell) |>
  summarise(species = list(species))

# 2015-2018
insecta_occ_2015.2018_grouped <- insecta_occ_2015.2018_sf |>
  group_by(cell) |>
  summarise(species = list(species))

## 16.3. Join data for the different timesteps ----

# Join data 2 by two, based on the corine land cover layer they were created with/from
insecta_turnover_1 <- left_join(as.data.frame(insecta_occ_1997.2000_grouped),
                                as.data.frame(insecta_occ_2006.2009_grouped),
                                by = "cell",
                                suffix = c("_1997.2000", "_2006.2009"))


insecta_turnover_2 <- left_join(as.data.frame(insecta_occ_2003.2006_grouped),
                                as.data.frame(insecta_occ_2012.2015_grouped),
                                by = "cell",
                                suffix = c("_2003.2006", "_2012.2015"))


insecta_turnover_3 <- left_join(as.data.frame(insecta_occ_2009.2012_grouped),
                                as.data.frame(insecta_occ_2015.2018_grouped),
                                by = "cell",
                                suffix = c("_2009.2012", "_2015.2018"))


#Combine all 3 occurrence turnover dataframes in 1
insecta_turnover_a <- left_join(insecta_turnover_1,
                                insecta_turnover_2,
                                by = "cell")


insecta_turnover <- left_join(insecta_turnover_a,
                              insecta_turnover_3,
                              by = "cell")

# 17. CALCULATE TURNOVER ----

## 17.1. Calculate turnover for each period ----
# First period = turnover between "_1997.2000" and "_2006.2009"
# Second period = turnover between "_2003.2006" and "_2012.2015"
# Third period = turnover between "_2009.2012" and "_2015.2018"

# First period (2000 to 2006)
insecta_turnover$turover2000.2006 <- mapply(calculate_turnover,
                                            insecta_turnover$species_1997.2000,
                                            insecta_turnover$species_2006.2009)


# Second period (2006 to 2012)
insecta_turnover$turover2006.2012 <- mapply(calculate_turnover,
                                            insecta_turnover$species_2003.2006,
                                            insecta_turnover$species_2012.2015)

# Third period (2012 to 2018)
insecta_turnover$turover2012.2018 <- mapply(calculate_turnover,
                                            insecta_turnover$species_2009.2012,
                                            insecta_turnover$species_2015.2018)

# 18. CREATE DF WITH TURNOVER AND LAND COVER CHANGE VALUES ----

## 18.1. Extract land cover values using the unique cell IDs ----
insecta_turnover <- insecta_turnover |>
  mutate(cover_change_2000.2006 = values_2000.2006[insecta_turnover$cell[,1]],
         cover_change_2006.2012 = values_2006.2012[insecta_turnover$cell[,1]],
         cover_change_2012.2018 = values_2012.2018[insecta_turnover$cell[,1]])

## 18.2. Write dataframe ----
saveRDS(insecta_turnover,
        here("data", "insecta_turnover.rds"))

# END OF SCRIPT ----