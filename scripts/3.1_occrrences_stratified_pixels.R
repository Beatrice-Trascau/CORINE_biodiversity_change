##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
########- 3.1_occurrences_stratified_pixels -#######
##------------------------------------------------##

#This script contains code to compare the number of occurrence records in pixels affected by different land-uses
#based on a stratified selection of the pixels

# 0. PACKAGES ----
library(here)
library(tidyverse)
library(dplyr)
library(splitstackshape)
# 1. LOAD DATA ----

## 1.1. Download data from box (if needed) ----

#Add download links
pixel_data <- ("https://ntnu.box.com/shared/static/nlsh5z5an7wauixylskh2mg13rwgt7uk.rds")

#Download file to local
download.file(pixel_data, here("data",
                               "combine_pixel_data.rds"))

## 1.2. Read in the data ----
all_pixel_data <-readRDS(here("data",
                              "combine_pixel_data.rds"))


# 2. PREPARE DATA FOR ANALYSIS ----

## 2.1. Remove NAs ----

#Check available colmns
colnames(raw_pixel_data)

#Remove NAs from the land cover columns
pixel_data <- raw_pixel_data |>
  filter_at(vars(land_cover_2000, land_cover_2006, land_cover_2012, land_cover_2018),
            all_vars(!is.na(.)))


# 3. URBAN FABRIC ----

#Load data
urban_fabric <- readRDS(here("data",
                             "urban_fabric.rds"))

## 3.1. Stratify selection ----

#Create "control" df
urban_control <- urban_fabric |>
  filter(land_cover_change_2000_2006 == "no change" )


#Create "response" df
urban_response <- urban_fabric |>
  filter(land_cover_change_2000_2006 != "no change" )

#Set seed
set.seed(7562424556)

#Calculate the number of response rows needed
n_response <- nrow(urban_control) * 5

#Sample the response dataframe
urban_response_sampled <- stratified(urban_response,
                                     c("PA_status", "SSBid"),
                                     n_response)


#Bind contrl rows and sampled response rows into one df
urban_sampled <- bind_rows(urban_control, urban_response_sampled)


## 3.2. Plot ----

# 4. COMPLEX AGRICULTURAL COVER ----

#Load data
complex_agriculture <- readRDS(here("data",
                                    "complex_agriculture.rds"))

## 4.1. Stratify selection ----

#Create "control" df
complex_agriculture_control <- complex_agriculture |>
  filter(land_cover_change_2000_2006 == "no change" )


#Create "response" df
complex_agriculture_response <- complex_agriculture |>
  filter(land_cover_change_2000_2006 != "no change" )

#Set seed
set.seed(7562424556)

#Calculate the number of response rows needed
n_response <- nrow(df_control) * 5

#Sample the response dataframe
complex_agriculture_response_sampled <- stratified(complex_agriculture_response,
                                                   c("PA_status", "SSBid"),
                                                   n_response)


#Bind contrl rows and sampled response rows into one df
complex_agriculture_sampled <- bind_rows(complex_agriculture_control, 
                                         complex_agriculture_response_sampled)


## 4.2. Plot ----


# 5. AGRICULTURE & SIGNIFICANT NATURAL VEGETATION ----

#Load data
agriculture_natural_veg <- readRDS(here("data",
                                        "agriculture_natural_veg.rds"))

## 5.1. Stratify selection ----

#Create "control" df
agriculture_natural_veg_control <- agriculture_natural_veg |>
  filter(land_cover_change_2000_2006 == "no change" )


#Create "response" df
agriculture_natural_veg_response <- agriculture_natural_veg |>
  filter(land_cover_change_2000_2006 != "no change" )

#Set seed
set.seed(7562424556)

#Calculate the number of response rows needed
n_response <- nrow(agriculture_natural_veg_control) * 5

#Sample the response dataframe
agriculture_natural_veg_response_sampled <- stratified(agriculture_natural_veg_response,
                                                       c("PA_status", "SSBid"),
                                                       n_response)


#Bind contrl rows and sampled response rows into one df
agriculture_natural_veg_sampled <- bind_rows(agriculture_natural_veg_control, 
                                             agriculture_natural_veg_response_sampled)


## 5.2. Plot ----


# 6. FORESTS ----

#Load data
forests <- readRDS(here("data",
                        "forests.rds"))

## 6.1. Stratify selection ----

#Create "control" df
forests_control <- forests |>
  filter(land_cover_change_2000_2006 == "no change" )


#Create "response" df
forests_response <- forests |>
  filter(land_cover_change_2000_2006 != "no change" )

#Set seed
set.seed(7562424556)

#Calculate the number of response rows needed
n_response <- nrow(forests_control) * 5

#Sample the response dataframe
forests_response_sampled <- stratified(forests_response,
                                       c("PA_status", "SSBid"),
                                       n_response)


#Bind contrl rows and sampled response rows into one df
forests_sampled <- bind_rows(forests_control, 
                             forests_response_sampled)


## 6.2. Plot ----

# 7. MOORS, HEATHLAND & NATURAL GRASSLAND ----

#Load data
moors_heath_grass <- readRDS(here("data",
                                  "moors_heath_grass.rds"))

## 7.1. Stratify selection ----

#Create "control" df
moors_control <- moors_heath_grass |>
  filter(land_cover_change_2000_2006 == "no change" )


#Create "response" df
moors_response <- moors_heath_grass |>
  filter(land_cover_change_2000_2006 != "no change" )

#Set seed
set.seed(7562424556)

#Calculate the number of response rows needed
n_response <- nrow(moors_control) * 5

#Sample the response dataframe
moors_response_sampled <- stratified(moors_response,
                                     c("PA_status", "SSBid"),
                                     n_response)


#Bind contrl rows and sampled response rows into one df
moors_sampled <- bind_rows(moors_control, 
                           moors_response_sampled)


## 7.2. Plot ----

# 8. TRANSITIONAL WOODLAND SHRUB ----

#Load data
trans <- readRDS(here("data",
                      "trans_woodland_shrub.rds"))

## 8.1. Stratify selection ----

#Create "control" df
trans_control <- trans |>
  filter(land_cover_change_2000_2006 == "no change" )


#Create "response" df
trans_response <- trans |>
  filter(land_cover_change_2000_2006 != "no change" )

#Set seed
set.seed(7562424556)

#Calculate the number of response rows needed
n_response <- nrow(trans_control) * 5

#Sample the response dataframe
trans_response_sampled <- stratified(trans_response,
                                     c("PA_status", "SSBid"),
                                     n_response)


#Bind contrl rows and sampled response rows into one df
trans_sampled <- bind_rows(trans_control, 
                           trans_response_sampled)


## 8.2. Plot ----

# 9. SPARSELY VEGETATED AREAS ----

#Load data
sparse <- readRDS(here("data",
                       "sparse_vegetation.rds"))

## 9.1. Stratify selection ----

#Create "control" df
sparse_control <- sparse |>
  filter(land_cover_change_2000_2006 == "no change" )


#Create "response" df
sparse_response <- sparse |>
  filter(land_cover_change_2000_2006 != "no change" )

#Set seed
set.seed(7562424556)

#Calculate the number of response rows needed
n_response <- nrow(sparse_control) * 5

#Sample the response dataframe
sparse_response_sampled_response_sampled <- stratified(sparse_response,
                                                       c("PA_status", "SSBid"),
                                                       n_response)


#Bind contrl rows and sampled response rows into one df
sparse_response_sampled_sampled <- bind_rows(sparse_control, 
                                             sparse_response_sampled)


## 9.2. Plot ----