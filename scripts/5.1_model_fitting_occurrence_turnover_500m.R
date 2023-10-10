##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
###- 5.1_model_fitting_occurrence_turnover_500m -###
##------------------------------------------------##

#This script contains code fitting models to the turnover of occurrence records in the 
 #intensification and extnsification 500m grids derived from CORINE

# 0.PACKAGES ----
library(here)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lme4)

# 1. READ IN AND PREPARE DATA ----

## 1.1. Intensification ----
# Read in data
intens_turnover <- readRDS(here("data", 
                                "intensification_occurrence_turnover.rds"))

# Remove unnecessary columns
intens_turnover <- intens_turnover |>
  select(-c(species_1997.2000, geometry_1997.2000,
            species_2006.2009, geometry_2006.2009, 
            species_2003.2006, geometry_2003.2006,
            species_2012.2015, geometry_2012.2015,
            species_2009.2012, geometry_2009.2012,
            species_2015.2018, geometry_2015.2018))

# Remove rows with NA for intensification and NaN for turnover
intens_turnover <- intens_turnover |>
  filter_at(vars(intens_2000.2006, intens_2006.2012, intens_2012.2018),
            all_vars(!is.na(.))) |>
  filter_at(vars(turnover2000.2006, turnover2006.2012, turnover2012.2018),
            all_vars(!is.nan(.)))

# Create separate df for turnover
intens_turnover_long <- intens_turnover |>
  #select only the turnover columns
  select(c(cell, turnover2000.2006, turnover2006.2012,
           turnover2012.2018)) |>
  #convert to long format
  pivot_longer(
    cols = "turnover2000.2006":"turnover2012.2018",
    names_to = "turnover_year",
    values_to = "turnover") |>
  #change turnover_year value to only contain the year
  mutate(year = case_when(turnover_year == "turnover2000.2006" ~ "2000.2006",
                          turnover_year == "turnover2006.2012" ~ "2006.2012",
                          turnover_year == "turnover2012.2018" ~ "2012.2018"),
         new_cell = cell$U2006_CLC2000_V2020_20u1,
         ID = paste(new_cell, year, sep = "_")) |>
  #remove unnecessary turnover_year column
  select(-c(turnover_year, cell, new_cell))


# Create separate df for intensification (land cover)
intens_long <- intens_turnover |>
  #select only the turnover columns
  select(c(cell, intens_2000.2006, intens_2006.2012,
           intens_2012.2018)) |>
  #convert to long format
  pivot_longer(
    cols = "intens_2000.2006":"intens_2012.2018",
    names_to = "intensification_year",
    values_to = "intensification_amount") |>
  #change intensification_year value to only contain the year
  mutate(year = case_when(intensification_year == "intens_2000.2006" ~ "2000.2006",
                          intensification_year == "intens_2006.2012" ~ "2006.2012",
                          intensification_year == "intens_2012.2018" ~ "2012.2018"),
         new_cell = cell$U2006_CLC2000_V2020_20u1,
         ID = paste(new_cell, year, sep = "_")) |>
  #remove unnecessary columns
  select(-c(cell, new_cell))

# Merge the intens_turnover_long and intens_long dfs
intens_turnover_for_model <- merge(intens_turnover_long, intens_long,
                         by = "ID")

# Check cols
colnames(intens_turnover_for_model)

#Check that year.x & year.y are the same
setequal(intens_turnover_for_model$year.x, intens_turnover_for_model$year.y) #TRUE

#Remove year.x and year.y columns
intens_turnover_for_model <- intens_turnover_for_model |>
  mutate(year = as.factor(year.y),
         intensification_amount = as.factor(intensification_amount)) |>
  select(-year.x)



# 2. GLM ----

## 2.1. GLM for Intensification ----
# Run GLM
intens_glm <- glm(turnover ~ intensification_amount + year.y,
                  intens_turnover_for_model,
                  family = "poisson")

# Check output
summary(intens_glm)
anova(intens_glm, test = "Chisq")
