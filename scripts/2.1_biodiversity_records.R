##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##----------- 2.1_biodiversity_records -----------##
##------------------------------------------------##

#This script contains code to clean the biodiversity records downloaded from GBIF

# 0. LOAD PACKAGES ----
library(here)
library(data.table)
library(CoordinateCleaner)

# 1. LOAD OCCURRENCE RECORDS ----

## 1.1. Download the occurrence records from box ----

#Add download link
occurrences <- ("https://ntnu.box.com/shared/static/ua75pdgoggzxo6zoxakg4les6nph2wsy.txt")

#Download the file
download.file(occurrences, "occurrence.txt")

## 1.2. Read in occurrence records ----
occurrences_norway <- fread(here("data", "raw_data",
                                 "occurrence.txt"))

# 2. CLEAN AND PREPARE DATA FOR ANALYSIS ----

## 2.1. Remove unnecessary columns & records ----
#Check names of columns
names(occurrences_norway)

#Check records per year
plot(2000:2018, summary(as.factor(occurrences_norway$year)),
     type = "b")

#Remove not needed columns
clean_occurrences_norway <- occurrences_norway[,c(1,16,27,64,84,103,107,125:129,
                                                  138:140,197:201,203,207,240, 241)]

#Remove records which are not Animalia, Fungi & Plantae
clean_occurrences_norway <- subset(clean_occurrences_norway, kingdom!="Bacteria" & kingdom!="Chromista" & 
                                     kingdom!="Protozoa" & kingdom!="Viruses" &
                                     kingdom!="incertae sedis")

#Remove records with no registered species-level information
clean_occurrences_norway <- clean_occurrences_norway[!clean_occurrences_norway$specificEpithet=="",]

#Remove duplicate records
clean_occurrences_norway_try <- clean_occurrences_norway |>
  distinct()


## 2.2 Clean coordinates ----
#Remove records without coorinates
clean_occurrences_norway <- clean_occurrences_norway |>
  filter(!is.na(decimalLongitude)) |>
  filter(!is.na(decimalLatitude))

#Remove flagged records
#identify flagged records
coordinate_flags <- clean_coordinates(x = clean_occurrences_norway,
                                      lon  = "decimalLongitude", lat = "decimalLatitude",
                                      countries = "NO",
                                      species = "species",
                                      test = c("centroids", "equal",
                                               "gbif", "zeros"))

summary(coordinate_flags)
#exclude flagged records
clean_occurrences_norway <- clean_occurrences_norway[coordinate_flags$.summary == TRUE]

#Remove records with coordinate uncertainty >= 142 (1/2 length of diagonal of 100x100m grid cell)
summary(clean_occurrences_norway$coordinateUncertaintyInMeters, na.rm = TRUE)

uncertainty_occurrences_norway <- clean_occurrences_norway  |>
  dplyr::filter(coordinateUncertaintyInMeters <= 142 |
                  is.na(coordinateUncertaintyInMeters))


## 2.3. Change df for analysis ----
#Change years of records to factorial values which refer to the periods of land cover sampling - this will be used to 
 #year = 2000 - 2005 => year = 2000.2005
 #year = 2006 - 2011 => year = 2006.2011
 #year = 2012 - 2018 => year = 2012.2018
clean_occurrences_norway <- clean_occurrences_norway |>
  mutate(year = case_when(year %in% c(2000:2005) ~ "2000.2005",
                          year %in% c(2006:2011) ~ "2006.2011",
                          year %in% c(2012:2018) ~ "2012.2018"))

## 2.4. Save cleaned occurrence records df ----
write.csv(clean_occurrences_norway,
          here("data", "cleaned_occurrences.txt"))

# END OF SCRIPT ----