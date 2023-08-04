#Packages
library(terra)
library(data.table)
library(sf)
library(here)

#Load land cover data
norway_corine <- rast(here("data", 
                           "corine_modified_classes_stack.tif"))

#Create a raster with a unique ID for each cell
#raster with the same properties as corine
land_cover_id <- norway_corine[[1]]
#assign each cell a unique ID from 1 to ncell
land_cover_id[] <- 1:ncell(norway_corine[[1]])


#Load species occurrence records
clean_occurrences <- fread(here("data", 
                                "cleaned_occurrences.txt"))

#Subset df for the 1997-2000 and 2006-2009 period
#1997-2000
occurrences1997.2000 <- clean_occurrences[clean_occurrences$year %in% c(1997:2000)]
#2006-2009
occurrences2006.2009 <- clean_occurrences[clean_occurrences$year %in% c(2006:2009)]

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

#Assign species to land cover cells for each timestep
occurrences1997.2000$cell <- terra::extract(land_cover_id,
                                            as.matrix(st_coordinates(occurrences1997.2000)))

occurrences2006.2009$cell <- terra::extract(land_cover_id,
                                            as.matrix(st_coordinates(occurrences2006.2009)))

#Group by cell to create list of species for each cell
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

#Calculate turnover
#write function to calculate turnover
calculate_turnover <- function(species1, species2) {
  unique1 <- length(setdiff(unlist(species1), unlist(species2)))
  unique2 <- length(setdiff(unlist(species2), unlist(species1)))
  total <- length(union(unlist(species1), unlist(species2)))
  return((unique1 + unique2) / total)
}


data$turnover <- mapply(calculate_turnover,
                        data$species_1997.2000,
                        data$species_2006.2009)

land_cover_corine <- norway_corine[[1]]

# Get all values of the raster
all_values <- terra::values(land_cover_corine)

# Convert to vector if necessary
if (is.matrix(all_values)) {
  all_values <- as.vector(all_values)
}

# Extract land cover values using the unique cell IDs
data$land_cover <- all_values[data$cell[,1]] # Accessing the first column of data$cell
