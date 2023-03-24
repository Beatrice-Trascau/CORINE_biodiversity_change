##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##---- 1.1_corine_land_cover_class_exploration ---##
##------------------------------------------------##

# This script contains code which loads, explored, and modify the land cover classes described by CORINE

# 0. LOAD PACKAGES ----
library(here)
library(terra)

# 1. LOAD CORINE STACKS ----

## 1.1. Download Corine stacks for Norway & Trondelag from Box ----

#Add download link
norway_corine_stack <- ("https://ntnu.box.com/shared/static/bjtenswrhno2z1dbsh9aym3jsux5l81s.tif")
trondelag_corine_stack <- ("https://ntnu.box.com/shared/static/k0ku0ssgdcl5lsb1yyzf4ijzlmq9qnv3.tif")

#Download the files
download.file(norway_corine_stack, "norway_corine_stack.tif")
download.file(trondelag_corine_stack, "trondelag_corine_stack.tif")

## 1.2. Read the layers
norway_corine <- rast("norway_corine_stack.tif")
trondelag_corine <- rast("trondelag_corine_stack.tif")

# 2. PLOT CORINE CLASSES INDIVIDUALLY ----
#This is done in order to evaluate the distribution of different CORINE land cover classes
 #and assess if distributions are as expected

## 2.1. Artificial Surfaces ----

### 2.1.1. Maps for class 1.1.1. Continuous urban fabric ----
#Change the layers so that everything other than continuous urban fabric is 0
cont_urban_norway <- app(norway_corine,
                         fun = function(x){x[x != 1] <- 0; return(x)})
cont_urban_trondelag <- app(trondelag_corine,
                            fun = function(x){x[x != 1] <- 0; return(x)})

#Plot the maps
plot(cont_urban_norway)
plot(cont_urban_trondelag)

### 2.1.2. Maps for class 1.1.2. Discontinuous urban Fabric ----
#Change the layers so that everything other than discontinuous urban fabric is 0
disc_urban_norway <- app(norway_corine,
                         fun = function(x){x[x != 2] <- 0; return(x)})
disc_urban_trond <- app(trondelag_corine,
                        fun = function(x){x[x != 2] <- 0; return(x)})
#Plot the maps
plot(disc_urban_norway, axes = FALSE)
plot(disc_urban_trond, axes = FALSE)

### 2.1.3. Maps for class 1.2.1. Industrial or commercial units ----
#Change the layers so that everything other than Industrial or commercial units is 0
industrial_norway <- app(norway_corine,
                         fun = function(x){x[x != 3] <- 0; return(x)})
industrial_trondelag <- app(trondelag_corine,
                            fun = function(x){x[x != 3] <- 0; return(x)})

#Plot the maps
plot(industrial_norway, axes = FALSE)
plot(industrial_trondelag, axes = FALSE)

### 2.1.4. Maps for class 1.2.2. Road and rail networks and associated land ----
#Change the layers so that everything other than Road and rail networks and associated land is 0
road_norway <- app(norway_corine,
                   fun = function(x){x[x != 4] <- 0; return(x)})
road_trondelag <- app(trondelag_corine,
                      fun = function(x){x[x != 4] <- 0; return(x)})

#Plot the maps
plot(road_norway, axes = FALSE)
plot(road_trondelag, axes = FALSE)

### 2.1.5. Maps for class 1.2.3. Port areas ----
#Change the layers so that everything other than Port area is 0
port_norway <- app(norway_corine,
                   fun = function(x){x[x != 5] <- 0; return(x)})
port_trondelag <- app(trondelag_corine,
                      fun = function(x){x[x != 5] <- 0; return(x)})

#Plot the maps
plot(road_norway, axes = FALSE)
plot(port_trondelag, axes = FALSE)

### 2.1.6. Maps for class 1.2.4. Airports ----
#Change the layers so that everything other than Airport is 0
airport_norway <- app(norway_corine,
                      fun = function(x){x[x != 6] <- 0; return(x)})
airport_trondelag <- app(trondelag_corine,
                         fun = function(x){x[x != 6] <- 0; return(x)})

#Plot the maps
plot(airport_norway, axes = FALSE)
plot(airport_trondelag, axes = FALSE)

### 2.1.7. Maps for class 1.3.1. Mineral extraction sites ----
#Change the layers so that everything other than Mineral extraction sites is 0
mineral_norway <- app(norway_corine,
                      fun = function(x){x[x != 7] <- 0; return(x)})
mineral_trondelag <- app(trondelag_corine,
                         fun = function(x){x[x != 7] <- 0; return(x)})

#Plot the maps
plot(mineral_norway, axes = FALSE)
plot(mineral_trondelag, axes = FALSE)

### 2.1.8. Maps for class 1.3.2. Dump sites ----
#Change the layers so that everything other than Dump sites is 0
dump_norway <- app(norway_corine,
                   fun = function(x){x[x != 8] <- 0; return(x)})
dump_trondelag <- app(trondelag_corine,
                      fun = function(x){x[x != 8] <- 0; return(x)})

#Plot the maps
plot(dump_norway, axes = FALSE)
plot(dump_trondelag, axes = FALSE)

### 2.1.9. Maps for class 1.3.3. Construction sites ----
#Change the layers so that everything other than Construction sites is 0
constructions_norway <- app(norway_corine,
                            fun = function(x){x[x != 9] <- 0; return(x)})
constructions_trondelag <- app(trondelag_corine,
                               fun = function(x){x[x != 9] <- 0; return(x)})

#Plot the maps
plot(constructions_norway, axes = FALSE)
plot(dump_trondelag, axes = FALSE)

### 2.1.10. Maps for class 1.4.1. Green urban areas ----
#Change the layers so that everything other than Green urban area is 0
green_norway <- app(norway_corine,
                    fun = function(x){x[x != 10] <- 0; return(x)})
green_trondelag <- app(trondelag_corine,
                       fun = function(x){x[x != 10] <- 0; return(x)})

#Plot the maps
plot(green_norway, axes = FALSE)
plot(green_trondelag, axes = FALSE)

### 2.1.11. Maps fpr class 1.4.2. Sport and leisure facilities ----
#Change the layers so that everything other than Sport and leisure facilities is 0
sport_norway <- app(norway_corine,
                    fun = function(x){x[x != 11] <- 0; return(x)})
sport_trondelag <- app(trondelag_corine,
                       fun = function(x){x[x != 11] <- 0; return(x)})

#Plot the maps
plot(sport_norway, axes = FALSE)
plot(sport_trondelag, axes = FALSE)

## 2.2. Agricultural Areas ----
### 2.2.1. Maps for class 2.1.1. Non-irrigated arable land ----
#Change the layers so that everything other than Non-irrigated arable land is 0
non_irrigated_norway <- app(norway_corine,
                            fun = function(x){x[x != 12] <- 0; return(x)})
non_irrigated_trondelag <- app(trondelag_corine,
                               fun = function(x){x[x != 12] <- 0; return(x)})

#Plot the maps
plot(non_irrigated_norway, axes = FALSE)
plot(non_irrigated_trondelag, axes = FALSE)

### 2.2.2. Maps for class 2.3.1. Pastures ----
#Change the layers so that everything other than Pastures is 0
pastures_norway <- app(norway_corine,
                       fun = function(x){x[x != 18] <- 0; return(x)})
pastures_trondelag <- app(trondelag_corine,
                          fun = function(x){x[x != 18] <- 0; return(x)})

#Plot the maps
plot(pastures_norway, axes = FALSE)
plot(pastures_trondelag, axes = FALSE)

### 2.2.3. Maps for class 2.4.2. Complex cultivation patterns ----
#Change the layers so that everything other than Complex cultivation patterns is 0
complex_cultivation_norway <- app(norway_corine,
                                  fun = function(x){x[x != 20] <- 0; return(x)})
complex_cultivation_trondelag <- app(trondelag_corine,
                                     fun = function(x){x[x != 20] <- 0; return(x)})

#Plot the maps
plot(complex_cultivation_norway, axes = FALSE)
plot(complex_cultivation_trondelag, axes = FALSE)

### 2.2.4. Maps for class 2.4.3. Land principally occupied by agriculture with significant areas of natural vegetation ---- 
#Change the layers so that everything other than Land principally occupied by agriculture is 0
principal_agri_norway <- app(norway_corine,
                             fun = function(x){x[x != 21] <- 0; return(x)})
principal_agri_trondelag <- app(trondelag_corine,
                                fun = function(x){x[x != 21] <- 0; return(x)})

#Plot the maps
plot(principal_agri_norway, axes = FALSE)
plot(principal_agri_trondelag, axes = FALSE)

## 2.3. Forest and Semi-natural Areas ----

### 2.3.1. Maps for class 3.1.1. Broad-leaved forest ----
#Change the layers so that everything other than Broad-leaved forest is 0
broadleaved_norway <- app(norway_corine,
                          fun = function(x){x[x != 23] <- 0; return(x)})
broadleaved_trondelag <- app(trondelag_corine,
                             fun = function(x){x[x != 23] <- 0; return(x)})

#Plot the maps
plot(broadleaved_norway, axes = FALSE)
plot(broadleaved_trondelag, axes = FALSE)

### 2.3.2. Maps for class 3.1.2. Coniferous forest ----
#Change the layers so that everything other than Coniferous forest  is 0
coniferous_norway <- app(norway_corine,
                         fun = function(x){x[x != 24] <- 0; return(x)})
coniferous_trondelag <- app(trondelag_corine,
                            fun = function(x){x[x != 24] <- 0; return(x)})

#Plot the maps
plot(coniferous_norway, axes = FALSE)
plot(coniferous_trondelag, axes = FALSE)

### 2.3.3. Maps for class 3.1.3. Mixed forest ----
#Change the layers so that everything other than Mixed forest  is 0
mixed_norway <- app(norway_corine,
                    fun = function(x){x[x != 25] <- 0; return(x)})
mixed_trondelag <- app(trondelag_corine,
                       fun = function(x){x[x != 25] <- 0; return(x)})

#Plot the maps
plot(mixed_norway, axes = FALSE)
plot(mixed_trondelag, axes = FALSE)

### 2.3.4. Maps for class 3.2.1. Natural grassland ----
#Change the layers so that everything other than Natural grassland  is 0
grassland_norway <- app(norway_corine,
                        fun = function(x){x[x != 26] <- 0; return(x)})
grassland_trondelag <- app(trondelag_corine,
                           fun = function(x){x[x != 26] <- 0; return(x)})

#Plot the maps
plot(grassland_norway, axes = FALSE)
plot(grassland_trondelag, axes = FALSE)

### 2.3.5. Maps for class 3.2.2. Moors and heathland ----
#Change the layers so that everything other than Moors and heathland   is 0
moors_norway <- app(norway_corine,
                    fun = function(x){x[x != 27] <- 0; return(x)})
moors_trondelag <- app(trondelag_corine,
                       fun = function(x){x[x != 27] <- 0; return(x)})
#Plot the maps
plot(moors_norway, axes = FALSE)
plot(moors_trondelag, axes = FALSE)

### 2.3.6. Maps for class 3.2.4. Transitional woodland shrub ----
#Change the layers so that everything other than Transitional woodland shrub is 0
transitional_norway <- app(norway_corine,
                           fun = function(x){x[x != 29] <- 0; return(x)})
transitional_trondelag <- app(trondelag_corine,
                              fun = function(x){x[x != 29] <- 0; return(x)})
#Plot the maps
plot(transitional_norway, axes = FALSE)
plot(transitional_trondelag, axes = FALSE)

### 2.3.7. Maps for class 3.3.2. Bare rock ----
#Change the layers so that everything other than Bare rock is 0
rock_norway <- app(norway_corine,
                   fun = function(x){x[x != 31] <- 0; return(x)})
rock_trondelag <- app(trondelag_corine,
                      fun = function(x){x[x != 31] <- 0; return(x)})
#Plot the maps
plot(rock_norway, axes = FALSE)
plot(rock_trondelag, axes = FALSE)

### 2.3.8. Maps for class 3.3.3. Sparsely vegetated areas ----
#Change the layers so that everything other than Sparsely vegetated areas is 0
sparse_vegetation_norway <- app(norway_corine,
                                fun = function(x){x[x != 32] <- 0; return(x)})
sparse_vegetation_trondelag <- app(trondelag_corine,
                                   fun = function(x){x[x != 32] <- 0; return(x)})
#Plot the maps
plot(sparse_vegetation_norway, axes = FALSE)
plot(sparse_vegetation_trondelag, axes = FALSE)

## 2.4. Inland wetlands ----

### 2.4.1. Inland marshes ----
#Change the layers so that everything other than Inland marshes is 0
marshes_norway <- app(norway_corine,
                      fun = function(x){x[x != 35] <- 0; return(x)})
marshes_trondelag <- app(trondelag_corine,
                         fun = function(x){x[x != 35] <- 0; return(x)})
#Plot the maps
plot(marshes_norway, axes = FALSE)
plot(marshes_trondelag, axes = FALSE)

### 2.4.2. Peatbogs ----
#Change the layers so that everything other than Peatbogs is 0
peatbogs_norway <- app(norway_corine,
                       fun = function(x){x[x != 36] <- 0; return(x)})
marshes_trondelag <- app(trondelag_corine,
                         fun = function(x){x[x != 35] <- 0; return(x)})
#Plot the maps
plot(peatbogs_norway, axes = FALSE)
plot(peatbogs_trondelag, axes = FALSE)

# 3. CHANGE COVER LAYERS TO HELP IDENTIFY CHANGE ----
#The class codes/values are changed to unique numbers which will help identigfy the land cover transitions between years
 #this will only be done for the Norway stack, as this is the one that will be used for analysis

## 3.1. Change land cover class values ----
#Discontinuous urban fabric (changed from 2 to 20)
 #all the urban classes are pooled together, due to their sparse distribution across Norway
norway_corine_modified <- app(norway_corine,
                              fun = function(x){x[x %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)] <- 1; return(x)})

#Non-irrigated arable land
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 12] <- 250; return(x)})

#Pastures
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 18] <- 380; return(x)})

#Complex cultivation patterns
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 20] <- 590; return(x)})

#Agriculture and significant natural vegetation
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 21] <- 711; return(x)})

#Broad-leaved forest
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 23] <- 859; return(x)})

#Coniferous forest
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 24] <- 1033; return(x)})

#Mixed forest
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 25] <- 1377; return(x)})

#Natural grasslands
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 26] <- 2020; return(x)})

#Moors and heathland
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 27] <- 3000; return(x)})

#Transitional woodland shrub
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 29] <- 4500; return(x)})

#Bare rock
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 31] <- 7111; return(x)})

#Sparsely vegetated areas
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x == 32] <- 9999; return(x)})

#Inland marshes & Peat bogs
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x %in% c(35, 36)] <- 108700; return(x)})

#Other classes
norway_corine_modified <- app(norway_corine_modified,
                              fun = function(x){x[x %in% c(3, 5, 6, 7, 8, 9, 11, 30, 33, 34, 39, 40, 41, 43, 44, 128)] <- NA; return(x)})
## 3.2. Save the modified corine stack ----
#New name for the raster stack
rater_name <- paste0("corine_modified_classes_stack.tif")

#Save the new raster stack
terra::writeRaster(norway_corine_modified, 
                   filename = rater_name,
                   overwrite = TRUE)

# END OF SCRIPT ----
