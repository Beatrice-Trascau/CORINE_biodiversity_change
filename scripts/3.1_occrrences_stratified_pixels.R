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

# 1. LOAD DATA ----

## 1.1. Download data from box (if needed) ----

#Add download links
pixel_data <- ("https://ntnu.box.com/shared/static/t2ylwpe6fk5koxb9wlv3nt1m8oz1y4l2.csv")

#Download file to local
download.file(pixel_data, here("data",
                               "combine_pixel_data.csv"))

## 1.2. Read in the data ----
pixel_data <- read.csv(here("data",
                            "combine_pixel_data.csv"),
                       header = TRUE)





