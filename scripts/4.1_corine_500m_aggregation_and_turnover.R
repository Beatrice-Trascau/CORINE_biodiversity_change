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
library(dplyr)
library(sf)

# 1. LOAD DATA ----

## 1.1. CORINE Land Cover Data ----
norway_corine <- rast(here("data",
                           "corine_modified_classes_stack.tif"))

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