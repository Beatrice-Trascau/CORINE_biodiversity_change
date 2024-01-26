##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##------ 0_corine_change_layer_preparation -------##
##------------------------------------------------##

# This sctript contains code which loads and prepares the CORINE land cover CHANGE layers for analysis

# 0. PACKAGES ----
library(here)
library(terra)
library(sf)
library(geodata)

# 1. READ IN CORINE CHANGE LAYERS ----

## 1.1. Download layers (if needed) ----

# Add download link from box
# U2006_CHA0006_00_V2020_20u1 <- ("https://ntnu.box.com/shared/static/vduuevecunldbrc7jb60jarjts99c4db.tif")
# U2006_CHA0006_06_V2020_20u1 <- ("https://ntnu.box.com/shared/static/nmn2kguk9ipx0u4a2a8yfvcozdf6g9ij.tif")
# U2012_CHA0612_06_V2020_20u1 <- ("https://ntnu.box.com/shared/static/pah7ig013inqeepg3gwvfan9w00anitp.tif")
# U2012_CHA0612_12_V2020_20u1 <- ("https://ntnu.box.com/shared/static/g9grkxsvv20sz48rkbig8f9tb8gennfy.tif")
# U2018_CHA1218_12_V2020_20u1 <- ("https://ntnu.box.com/shared/static/v51lua6b9fph0k7bmsbbc1g20tkdjqh9.tif")
# U2018_CHA1218_18_V2020_20u1 <- ("https://ntnu.box.com/shared/static/x7ck0jnagfoxvjxvxf99l9lhknky5xlt.tif")

# Download the files
# download.file(U2006_CHA0006_00_V2020_20u1, "U2006_CHA0006_00_V2020_20u1.tif")
# download.file(U2006_CHA0006_06_V2020_20u1, "U2006_CHA0006_06_V2020_20u1.tif")
# download.file(U2012_CHA0612_06_V2020_20u1, "U2012_CHA0612_06_V2020_20u1.tif")
# download.file(U2012_CHA0612_12_V2020_20u1, "U2012_CHA0612_12_V2020_20u1.tif")
# download.file(U2018_CHA1218_12_V2020_20u1, "U2018_CHA1218_12_V2020_20u1.tif")
# download.file(U2018_CHA1218_18_V2020_20u1, "U2018_CHA1218_18_V2020_20u1.tif")

## 1.2. Read in layers -----

# Read in the CORINE CHANGE layers
corine_change0006_00 <- rast(here("data", "raw_data", "U2006_CHA0006_00_V2020_20u1.tif"))
corine_change0006_06 <- rast(here("data", "raw_data", "U2006_CHA0006_06_V2020_20u1.tif"))
corine_change0612_06 <- rast(here("data", "raw_data", "U2012_CHA0612_06_V2020_20u1.tif"))
corine_change0612_12 <- rast(here("data", "raw_data", "U2012_CHA0612_12_V2020_20u1.tif"))
corine_change1218_12 <- rast(here("data", "raw_data", "U2018_CHA1218_12_V2020_20u1.tif"))
corine_change1218_18 <- rast(here("data", "raw_data", "U2018_CHA1218_18_V2020_20u1.tif"))

# Stack layers into 1 object
corine_change_stack <- c(corine_change0006_00, corine_change0006_06,
                         corine_change0612_06, corine_change0612_12,
                         corine_change1218_12, corine_change1218_18)

# 2. CUT AND MASK LAYERS TO NORWAY ----

## 2.1. Download country shapefile ----
norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")
#Check shapefile
plot(norway)

## 2.2. Re-project Norway shapefile to match projection of CORINE layers ----

# Check projections
crs(norway, proj = TRUE)
crs(corine_change_stack[[1]], proj = TRUE)

# Reproject Norway shapefile to the CORINE layers
norway_corine_projection <- project(norway, crs(corine_change_stack))

# Check projection
crs(norway_corine_projection, proj = TRUE) #projection correct now

## 2.2. Crop and mask CORINE stack to Norway ----

# Crop and mask
norway_corine_change_stack <- crop(corine_change_stack, norway_corine_projection,
                                   mask = TRUE)

# Save the cropped layers 
terra::writeRaster(norway_corine_change_stack,
                   here("data", "norway_corine_change_stack.tif"))
