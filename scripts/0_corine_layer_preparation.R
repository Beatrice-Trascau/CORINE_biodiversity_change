##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##---------- 0_corine_layer_preparation ----------##
##------------------------------------------------##

#This script contains code which loads and prepares CORINE land cover layers for analysis.

# 0. LOAD PACKAGES ----
library(here)
library(terra)
library(sf)
library(geodata)

# 1. DOWNLOAD CORINE LAYERS (2000, 2006, 2012, 2018) FROM BOX ----

#Add Download link from box
U2006_CLC2000_V2020_20u1 <- ("https://ntnu.box.com/shared/static/ffmbbb89aikwg64tg9ei30c8fnf7chl2.tif")
U2012_CLC2006_V2020_20u1 <- ("https://ntnu.box.com/shared/static/2x6g9jaov5rex3u0xt3hq9mmy91d63ew.tif")
U2018_CLC2012_V2020_20u1 <- ("https://ntnu.box.com/shared/static/ut1pcbnj7xgfwv3ptahu5c3krdy24l7d.tif")
U2018_CLC2018_V2020_20u1 <- ("https://ntnu.box.com/shared/static/iub514rfjnkopg3nu4nc18j4axq5jfon.tif")

#Download the files
download.file(U2006_CLC2000_V2020_20u1, "U2006_CLC2000_V2020_20u1.tif")
download.file(U2012_CLC2006_V2020_20u1, "U2012_CLC2006_V2020_20u1.tif")
download.file(U2018_CLC2012_V2020_20u1, "U2018_CLC2012_V2020_20u1.tif")
download.file(U2018_CLC2018_V2020_20u1, "U2018_CLC2018_V2020_20u1.tif")

# 2. READ IN CORINE LAYERS ----

#Read in CORINE layers downloaded above
corine_2000 <- rast(here("data", "raw_data", "U2006_CLC2000_V2020_20u1.tif"))
corine_2006 <- rast(here("data", "raw_data", "U2012_CLC2006_V2020_20u1.tif"))
corine_2012 <- rast(here("data", "raw_data", "U2018_CLC2012_V2020_20u1.tif"))
corine_2018 <- rast(here("data", "raw_data", "U2018_CLC2018_V2020_20u1.tif"))


#Stack rasters
corine_stack <- c(corine_2000, corine_2006,
                  corine_2012, corine_2018)

# 3. CUT & MASK LAYERS TO NORWAY ----

## 3.1. Download country shapefile ----
norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")
#Check shapefile
plot(norway)

## 3.2. Reproject shapefile to match projection of CORINE layers ----

#Check projections of Norway and CORINE layers
crs(norway, proj = TRUE) #"+proj=longlat +datum=WGS84 +no_defs"
crs(corine_2018, proj = TRUE) #"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

#Reproject Norway shapefile to the CORINE layers
norway_corine_projection <- project(norway, crs(corine_stack))
#check projection
crs(norway_corine_projection, proj = TRUE) #projection correct now

## 3.3. Crop and mask CORINE stack to Norway ----

norway_corine_stack <- crop(corine_stack, norway_corine_projection,
                            mask = TRUE)

## 3.4. Save the cropped layers ----
terra::writeRaster(norway_corine_stack,
                   here("data", "norway_corine_stack.tif"))


# 4. CUT & MASK LAYERS TO TRONDELAG ----

## 4.1. Create Trondelag Shapefile ----

#Download country shapefile to level 1
norway_counties <- geodata::gadm(country = "NOR", level = 1, 
                                 path = tempdir(),
                                 version = "latest")

#Create Trondelag from Nord-Trondelag an Sor-Trondelag
 #extract Nord-TrC8ndelag and SC8r-Trondelag shapes
nord_trondelag <- norway_counties[norway_counties$NAME_1 == "Nord-TrC8ndelag", ]
sor_trondelag <- norway_counties[norway_counties$NAME_1 == "SC8r-TrC8ndelag", ]

 #combine into Trondelag
trondelag <- nord_trondelag + sor_trondelag

## 4.2. Reproject shapefile to match projection of CORINE layers ----

#Check projections of norway and CORINE layers
crs(trondelag, proj = TRUE) #"+proj=longlat +datum=WGS84 +no_defs"
crs(corine_2018, proj = TRUE) #"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

#Reproject Norway shapefile to the CORINE layers
trondelag_corine_projection <- project(trondelag, crs(corine_stack))
 #check projection
crs(trondelag_corine_projection, proj = TRUE) #projection correct now

## 4.3. Crop and mask CORINE stack to Trondelag ----

trondelag_corine_stack <- crop(corine_stack, trondelag_corine_projection,
                               mask = TRUE)

## 4.3. Save the cropped layers ----

terra::writeRaster(trondelag_corine_stack,
                   here("data", "rondelag_corine_stack.tif"))

#END OF SCRIPT ----