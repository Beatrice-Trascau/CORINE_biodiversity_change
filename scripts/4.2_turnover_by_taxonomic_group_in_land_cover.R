##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
#- 4.2_turnover_by_taxonomic_group_in_land_cover -##
##------------------------------------------------##

# 0. PACKAGES ----
library(here)
library(terra)
library(data.table)
library(dplyr)
library(sf)

# 1. LOAD DATA ----

## 1.1. CORINE LAND COVER DATA ----
norway_corine <- rast(here("data",
                           "corine_modified_classes_stack.tif"))

## 1.2. GBIF Occurrence Records -----
clean_occurrences <- fread(here::here("data", 
                                      "cleaned_occurrences.txt"))

# 2. PREPARE LAND COVER DATA ----

#This raster is needed to find the identity of the corine cells in which turnover takes place

# Create a raster with the same properties as corine (no issue that we are only using 1 year)
land_cover_id <- norway_corine[[1]]

# Assign each cell a unique ID from 1 to ncell
land_cover_id[] <- 1:ncell(norway_corine[[1]])

# 2. PREPARE OCCURRENCE RECORDS FOR TURNOVER CALCULATION ----

## 2.1. Create subset dfs for each taxonomic group conisdered ----

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

## 2.2. Subset occurrence records df for each period of "before" and "after" change ----

#N.B: Periods pf change are:
#First period of land cover change: 2000 to 2006; BEFORE Change = 1997-2000, AFTER Change = 2006-2009
#Second period of land cover change: 2006 to 2012; BEFORE Change = 2003-2006, AFTER Change = 2012-2015
#Third period of land cover change: 2012 to 2018; BEFORE Change = 2009-2012, AFTER Change = 2015-2018

# Create function to subser data based on the year ranges
subset_occurrences <- function(df, prefix) {
  periods <- list(
    "1997.2000" = c(1997:2000),
    "2006.2009" = c(2006:2009),
    "2003.2006" = c(2003:2006),
    "2012.2015" = c(2012:2015),
    "2009.2012" = c(2009:2012),
    "2015.2018" = c(2015:2018)
  )
  
  for (period_name in names(periods)) {
    subset_name <- paste(prefix, period_name, sep = "_")
    assign(subset_name, df %>% filter(year %in% periods[[period_name]]), 
           envir = .GlobalEnv)
  }
}

# Apply function to each of the taxonomic group dataframes
subset_occurrences(plant_occurrences, "plant_occurrences")
subset_occurrences(fungi_occurrences, "fungi_occurrences")
subset_occurrences(aves_occurrences, "aves_occurrences")
subset_occurrences(mammalia_occurrences, "mammalia_occurrences")
subset_occurrences(insecta_occurrencs, "insecta_occurrences")


## 2.3. Convert occurrences to sf objects and set CRS ---

# Create a list of prefixes for each group
prefixes <- c("plant_occurrences", "fungi_occurrences", "aves_occurrences", 
              "mammalia_occurrences", "insecta_occurrences")

# Create a list of period names
periods <- c("1997.2000", "2006.2009", "2003.2006", 
             "2012.2015", "2009.2012", "2015.2018")

# Create list of dfs by looping through each prefix and period
occurrence_list <- list()

for (prefix in prefixes) {
  for (period in periods) {
    df_name <- paste0(prefix, "_", period)
    occurrence_list[[df_name]] <- get(df_name)
  }
}

# Convert each df in the list to an sf object
for(name in names(occurrence_list)){
  assign(paste0(name, "_sf"),
         st_as_sf(occurrence_list[[name]],
                  coords = c("decimalLongitude", "decimalLatitude"),
                  crs = 4326))
}

## 2.4. Set CRS for sf objects to match CORINE's CRS ----

# Create a list of prefixes for each group
prefixes <- c("plant_occurrences", "fungi_occurrences", "aves_occurrences", 
              "mammalia_occurrences", "insecta_occurrences")

# Create a list of period names
periods <- c("1997.2000", "2006.2009", "2003.2006", 
             "2012.2015", "2009.2012", "2015.2018")


# Create list of sf objects by looping through each prefix and period
sf_names <- c()

for (prefix in prefixes) {
  for (period in periods) {
    sf_name <- paste0(prefix, "_", period, "_sf")
    sf_names <- c(sf_names, sf_name)
  }
}

# Extract target CRS from CORINE
target_crs <- st_crs(norway_corine[[1]])

# Transform each sf object to the correct CRS
for(name in sf_names){
  transformed_sf <- st_transform(get(name), target_crs)
  assign(name, transformed_sf)
}

# 3. COMBINE OCCURRENCE SF OBJECTS WITH THE LAND COVER ID RASTER ----

## 3.1. Assign species to land cover ID cells for each time step ----

# Create a list of prefixes for each group
prefixes <- c("plant_occurrences", "fungi_occurrences", "aves_occurrences", 
              "mammalia_occurrences", "insecta_occurrences")

# Create a list of period names
periods <- c("1997.2000", "2006.2009", "2003.2006", 
             "2012.2015", "2009.2012", "2015.2018")

# Create list of sf objects by looping through each prefix and period
sf_names <- c()

for (prefix in prefixes) {
  for (period in periods) {
    sf_name <- paste0(prefix, "_", period, "_sf")
    sf_names <- c(sf_names, sf_name)
  }
}

# Extract cell values for each sf object
for(name in sf_names){
  sf_object <- get(name)
  sf_object$cell <- terra::extract(land_cover_id,
                                   as.matrix(st_coordinates(sf_object)))
  assign(name, sf_object)
}

## 3.2. Group by cell to create list of species for each cell ----

# Create a list of prefixes for each group
prefixes <- c("plant_occurrences", "fungi_occurrences", "aves_occurrences", 
              "mammalia_occurrences", "insecta_occurrences")

# Create a list of period names
periods <- c("1997.2000", "2006.2009", "2003.2006", 
             "2012.2015", "2009.2012", "2015.2018")

# Create list of sf object names and corresponding grouped names
sf_names <- c()
grouped_names <- c()

for (prefix in prefixes) {
  for (period in periods) {
    sf_name <- paste0(prefix, "_", period, "_sf")
    grouped_name <- paste0(prefix, "_", period, "_grouped")
    sf_names <- c(sf_names, sf_name)
    grouped_names <- c(grouped_names, grouped_name)
  }
}

# Group by cell to create a list of species for each cell
for (i in seq_along(sf_names)) {
  grouped_obj <- get(sf_names[i]) |>
    group_by(cell) |>
    summarise(species = list(species), .groups = "drop")
  assign(grouped_names[i], grouped_obj)
}

## 3.3. Join data for different timesteps ----

### 3.3.1. Plants ----
# Join data 2 by two, based on the corine land cover layer they were created with/from
plant_turnover_1 <- left_join(as.data.frame(plant_occurrences_1997.2000_grouped),
                              as.data.frame(plant_occurrences_2006.2009_grouped),
                              by = "cell",
                              suffix = c("_1997.2000", "_2006.2009"))

plant_turnover_2 <- left_join(as.data.frame(plant_occurrences_2003.2006_grouped),
                              as.data.frame(plant_occurrences_2012.2015_grouped),
                              by = "cell",
                              suffix = c("_2003.2006", "_2012.2015"))

plant_turnover_3 <- left_join(as.data.frame(plant_occurrences_2009.2012_grouped),
                              as.data.frame(plant_occurrences_2015.2018_grouped),
                              by = "cell",
                              suffix = c("_2009.2012", "_2015.2018"))

# Combine all 3 occurrence turnover dataframes in 1
plant_turnover_4 <-  left_join(plant_turnover_1,
                               plant_turnover_2,
                               by = "cell")

plant_turnover <- left_join(plant_turnover_4,
                            plant_turnover_3,
                            by = "cell")

### 3.3.2. Fungi ----
# Join data 2 by two, based on the corine land cover layer they were created with/from
fungi_turnover_1 <- left_join(as.data.frame(fungi_occurrences_1997.2000_grouped),
                              as.data.frame(fungi_occurrences_2006.2009_grouped),
                              by = "cell",
                              suffix = c("_1997.2000", "_2006.2009"))

fungi_turnover_2 <- left_join(as.data.frame(fungi_occurrences_2003.2006_grouped),
                              as.data.frame(fungi_occurrences_2012.2015_grouped),
                              by = "cell",
                              suffix = c("_2003.2006", "_2012.2015"))

fungi_turnover_3 <- left_join(as.data.frame(fungi_occurrences_2009.2012_grouped),
                              as.data.frame(fungi_occurrences_2015.2018_grouped),
                              by = "cell",
                              suffix = c("_2009.2012", "_2015.2018"))

# Combine all 3 occurrence turnover dataframes in 1
fungi_turnover_4 <-  left_join(fungi_turnover_1,
                               fungi_turnover_2,
                               by = "cell")

fungi_turnover <- left_join(fungi_turnover_4,
                            fungi_turnover_3,
                            by = "cell")

### 3.3.3. Aves ----
# Join data 2 by two, based on the corine land cover layer they were created with/from
aves_turnover_1 <- left_join(as.data.frame(aves_occurrences_1997.2000_grouped),
                             as.data.frame(aves_occurrences_2006.2009_grouped),
                             by = "cell",
                             suffix = c("_1997.2000", "_2006.2009"))

aves_turnover_2 <- left_join(as.data.frame(aves_occurrences_2003.2006_grouped),
                             as.data.frame(aves_occurrences_2012.2015_grouped),
                             by = "cell",
                             suffix = c("_2003.2006", "_2012.2015"))

aves_turnover_3 <- left_join(as.data.frame(aves_occurrences_2009.2012_grouped),
                             as.data.frame(aves_occurrences_2015.2018_grouped),
                             by = "cell",
                             suffix = c("_2009.2012", "_2015.2018"))

# Combine all 3 occurrence turnover dataframes in 1
aves_turnover_4 <-  left_join(aves_turnover_1,
                              aves_turnover_2,
                              by = "cell")

aves_turnover <- left_join(aves_turnover_4,
                           aves_turnover_3,
                           by = "cell")

### 3.3.4. Mammalia ----
# Join data 2 by two, based on the corine land cover layer they were created with/from
mammalia_turnover_1 <- left_join(as.data.frame(mammalia_occurrences_1997.2000_grouped),
                                 as.data.frame(mammalia_occurrences_2006.2009_grouped),
                                 by = "cell",
                                 suffix = c("_1997.2000", "_2006.2009"))

mammalia_turnover_2 <- left_join(as.data.frame(mammalia_occurrences_2003.2006_grouped),
                                 as.data.frame(mammalia_occurrences_2012.2015_grouped),
                                 by = "cell",
                                 suffix = c("_2003.2006", "_2012.2015"))

mammalia_turnover_3 <- left_join(as.data.frame(mammalia_occurrences_2009.2012_grouped),
                                 as.data.frame(mammalia_occurrences_2015.2018_grouped),
                                 by = "cell",
                                 suffix = c("_2009.2012", "_2015.2018"))

# Combine all 3 occurrence turnover dataframes in 1
mammalia_turnover_4 <-  left_join(mammalia_turnover_1,
                                  mammalia_turnover_2,
                                  by = "cell")

mammalia_turnover <- left_join(mammalia_turnover_4,
                               mammalia_turnover_3,
                               by = "cell")

### 3.3.5. Insecta ----
# Join data 2 by two, based on the corine land cover layer they were created with/from
insecta_turnover_1 <- left_join(as.data.frame(insecta_occurrences_1997.2000_grouped),
                                as.data.frame(insecta_occurrences_2006.2009_grouped),
                                by = "cell",
                                suffix = c("_1997.2000", "_2006.2009"))

insecta_turnover_2 <- left_join(as.data.frame(insecta_occurrences_2003.2006_grouped),
                                as.data.frame(insecta_occurrences_2012.2015_grouped),
                                by = "cell",
                                suffix = c("_2003.2006", "_2012.2015"))

insecta_turnover_3 <- left_join(as.data.frame(insecta_occurrences_2009.2012_grouped),
                                as.data.frame(insecta_occurrences_2015.2018_grouped),
                                by = "cell",
                                suffix = c("_2009.2012", "_2015.2018"))

# Combine all 3 occurrence turnover dataframes in 1
insecta_turnover_4 <-  left_join(insecta_turnover_1,
                                 insecta_turnover_2,
                                 by = "cell")

insecta_turnover <- left_join(insecta_turnover_4,
                              insecta_turnover_3,
                              by = "cell")



# 4. CALCULATE TURNOVER -----

## 4.1. Write function to calculate turnover ----
calculate_turnover <- function(species1, species2) {
  unique1 <- length(setdiff(unlist(species1), unlist(species2)))
  unique2 <- length(setdiff(unlist(species2), unlist(species1)))
  total_occurrences <- length(unlist(species1)) + length(unlist(species2))
  return((unique1 + unique2) / total_occurrences)
}


## 4.2. Plant turnover for each period of change ----
# First period = turnover between "_1997.2000" and "_2006.2009"
# Second period = turnover between "_2003.2006" and "_2012.2015"
# Third period = turnover between "_2009.2012" and "_2015.2018"

# First period (2000 to 2006)
plant_turnover$turnover2000.2006 <- mapply(calculate_turnover,
                                           plant_turnover$species_1997.2000,
                                           plant_turnover$species_2006.2009)

# Second period (2006 to 2012)
plant_turnover$turnover2006.2012 <- mapply(calculate_turnover,
                                           plant_turnover$species_2003.2006,
                                           plant_turnover$species_2012.2015)

# Third period (2012 to 2018)
plant_turnover$turnover2012.2018 <- mapply(calculate_turnover,
                                           plant_turnover$species_2009.2012,
                                           plant_turnover$species_2015.2018)


## 4.3. Fungi turnover for each period of change ----
# First period (2000 to 2006)
fungi_turnover$turnover2000.2006 <- mapply(calculate_turnover,
                                           fungi_turnover$species_1997.2000,
                                           fungi_turnover$species_2006.2009)

# Second period (2006 to 2012)
fungi_turnover$turnover2006.2012 <- mapply(calculate_turnover,
                                           fungi_turnover$species_2003.2006,
                                           fungi_turnover$species_2012.2015)

# Third period (2012 to 2018)
fungi_turnover$turnover2012.2018 <- mapply(calculate_turnover,
                                           fungi_turnover$species_2009.2012,
                                           fungi_turnover$species_2015.2018)

## 4.4. Aves turnover for each period of change ----
# First period (2000 to 2006)
aves_turnover$turnover2000.2006 <- mapply(calculate_turnover,
                                          aves_turnover$species_1997.2000,
                                          aves_turnover$species_2006.2009)

# Second period (2006 to 2012)
aves_turnover$turnover2006.2012 <- mapply(calculate_turnover,
                                          aves_turnover$species_2003.2006,
                                          aves_turnover$species_2012.2015)

# Third period (2012 to 2018)
aves_turnover$turnover2012.2018 <- mapply(calculate_turnover,
                                          aves_turnover$species_2009.2012,
                                          aves_turnover$species_2015.2018)

## 4.5. Mammalia turnover for each period of change ----
# First period (2000 to 2006)
mammalia_turnover$turnover2000.2006 <- mapply(calculate_turnover,
                                              mammalia_turnover$species_1997.2000,
                                              mammalia_turnover$species_2006.2009)

# Second period (2006 to 2012)
mammalia_turnover$turnover2006.2012 <- mapply(calculate_turnover,
                                              mammalia_turnover$species_2003.2006,
                                              mammalia_turnover$species_2012.2015)

# Third period (2012 to 2018)
mammalia_turnover$turnover2012.2018 <- mapply(calculate_turnover,
                                              mammalia_turnover$species_2009.2012,
                                              mammalia_turnover$species_2015.2018)

## 4.6. Insecta turnover for each period of change ----
# First period (2000 to 2006)
insecta_turnover$turnover2000.2006 <- mapply(calculate_turnover,
                                             insecta_turnover$species_1997.2000,
                                             insecta_turnover$species_2006.2009)

# Second period (2006 to 2012)
insecta_turnover$turnover2006.2012 <- mapply(calculate_turnover,
                                             insecta_turnover$species_2003.2006,
                                             insecta_turnover$species_2012.2015)

# Third period (2012 to 2018)
insecta_turnover$turnover2012.2018 <- mapply(calculate_turnover,
                                             insecta_turnover$species_2009.2012,
                                             insecta_turnover$species_2015.2018)






