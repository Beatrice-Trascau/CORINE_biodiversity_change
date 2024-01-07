##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
###- 5.2_models_turnover_taxonomic_groups_500m -####
##------------------------------------------------##

# This script contains code fitting models to the turnover of occurrence records broken down by taxonomic groups in the intensification 500m grids derived from CORINE

# 0. PACKAGES ----
library(here)
library(dplyr)
library(tidyverse)
library(sf)
library(purrr)
library(ggplot2)
library(lattice)
library(cowplot)
library(lme4)
library(betareg)
library(mgcv)
library(psych)
library(DHARMa)
source("HighstatLibV14.R") # useful functions from GAM course, see below
# Remember to cite this library as:
# Mixed effects models and extensions in ecology with R. (2009).
# Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer

# 1. READ IN AND PREPARE DATA ---

## 1.1. Read in turnover broken down by taxonomic group ----

# Plantae
plantae_turnover <- readRDS(here("data",
                               "plant_turnover.rds"))

# Aves
aves_turnover <- readRDS(here("data",
                              "aves_turnover.rds"))

# Mammalia
mammalia_turnover <- readRDS(here("data",
                                "mammalia_turnover.rds"))

# Insecta
insecta_turnover <- readRDS(here("data",
                                 "insecta_turnover.rds"))

## 1.2. Combine all separate dfs into a single df ----

# Add a column to each dataframe with the group name

 # Plantae
plantae_turnover <- plantae_turnover |>
  mutate(taxonomic_group = "Plantae")

 # Aves
aves_turnover <- aves_turnover |>
  mutate(taxonomic_group = "Aves")

 # Mammalia
mammalia_turnover <- mammalia_turnover |>
  mutate(taxonomic_group = "Mammalia")

 # Insecta
insecta_turnover <- insecta_turnover |>
  mutate(taxonomic_group = "Insecta")

# Combine dataframes into a single dataframe
groups_turnover <-  rbind(plantae_turnover, aves_turnover,
                          mammalia_turnover, insecta_turnover)

# 2. CLEAN COMBINED DATAFRAME ----

## 2.1. Extract coordinates from geometry columns

# Make a copy of tha dataframe that you can edit
groups_turnover_coords <- groups_turnover

# Make a list of the geometry columns
geometry_columns <- c("geometry_1997.2000",
                      "geometry_2006.2009",
                      "geometry_2003.2006",
                      "geometry_2012.2015",
                      "geometry_2009.2012",
                      "geometry_2015.2018")


# Loop through each geometry column
for(col_name in geometry_columns){
  # print column name to check that they will all be processed
  cat("Processing column:", col_name, "\n")
  
  # extract geometry column
  geometry_col <- groups_turnover[[col_name]]
  
  # cast to specific geometry (POINT)
  points <- st_cast(geometry_col, "POINT")
  
  # extract coordinates
  coords <- st_coordinates(points)
  coords_df <- as.data.frame(coords)
  
  # create new column names for the coordinates
  new_col_names <- paste0(col_name, "_coords_",
                          names(coords_df))
  
  # rename the columns of the coords_df to avoid namne clashes
  names(coords_df) <- new_col_names
  
  # bind the new columns to the original dataframe
  groups_turnover_coords <- cbind(groups_turnover_coords, coords_df)
}


## 2.2. Clean dataframe ----
# Remove unnecessary columns
clean_groups_turnover_coords <- groups_turnover_coords |>
  select(-c(species_1997.2000, species_2006.2009,
            species_2003.2006, species_2012.2015,
            species_2009.2012, species_2015.2018,
            geometry_1997.2000, geometry_2006.2009,
            geometry_2003.2006, geometry_2012.2015,
            geometry_2009.2012, geometry_2015.2018)) |>
  # Remove rows with NA for intensification and NaN for turnover
  filter_at(vars(cover_change_2000.2006, cover_change_2006.2012, cover_change_2012.2018),
            all_vars(!is.na(.))) |>
  filter_at(vars(turover2000.2006, turover2006.2012, turover2012.2018),
            all_vars(!is.nan(.))) |>
  # Change cover_change column names to intens_
  rename(intens_2000.2006 = cover_change_2000.2006,
         intens_2006.2012 = cover_change_2006.2012,
         intens_2012.2018 = cover_change_2012.2018)

# Create separate df for turnover   
groups_turnover_long <- clean_groups_turnover_coords |>
  #select only the turnover columns
  select(c(cell, turover2000.2006, turover2006.2012,
           turover2012.2018, taxonomic_group,
           geometry_1997.2000_coords_X, geometry_1997.2000_coords_Y,
           geometry_2006.2009_coords_X,  geometry_2006.2009_coords_Y,
           geometry_2003.2006_coords_X, geometry_2003.2006_coords_Y,
           geometry_2012.2015_coords_X, geometry_2012.2015_coords_Y,
           geometry_2009.2012_coords_X,  geometry_2009.2012_coords_Y,
           geometry_2015.2018_coords_X, geometry_2015.2018_coords_Y)) |>
  #convert to long format
  pivot_longer(
    cols = "turover2000.2006":"turover2012.2018",
    names_to = "turnover_year",
    values_to = "turnover") |>
  #change turnover_year value to only contain the year
  mutate(year = case_when(turnover_year == "turover2000.2006" ~ "2000.2006",
                          turnover_year == "turover2006.2012" ~ "2006.2012",
                          turnover_year == "turover2012.2018" ~ "2012.2018"),
         new_cell = cell$U2006_CLC2000_V2020_20u1,
         ID = paste(new_cell, year, sep = "_")) |>
  #remove unnecessary turnover_year column
  select(-c(turnover_year, cell))


# Create separate df for intensification (land cover)
intens_long <- clean_groups_turnover_coords |>
  #select only the turnover columns
  select(c(cell, intens_2000.2006, intens_2006.2012,
           intens_2012.2018, taxonomic_group)) |>
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
intens_turnover_by_group_for_model <- merge(groups_turnover_long, intens_long,
                                   by = "ID")

# Check cols
colnames(intens_turnover_by_group_for_model)

# Check that year.x & year.y are the same
setequal(intens_turnover_by_group_for_model$year.x, 
         intens_turnover_by_group_for_model$year.y) #TRUE

# Remove year.x and year.y columns and rows without coordinates
intens_turnover_by_group_for_model <- intens_turnover_by_group_for_model |>
  filter_at(vars(geometry_1997.2000_coords_X, geometry_1997.2000_coords_Y,
                 geometry_2006.2009_coords_X, geometry_2006.2009_coords_Y,
                 geometry_2003.2006_coords_X, geometry_2003.2006_coords_Y,
                 geometry_2012.2015_coords_X, geometry_2012.2015_coords_Y,
                 geometry_2009.2012_coords_X, geometry_2009.2012_coords_Y,
                 geometry_2015.2018_coords_X, geometry_2015.2018_coords_Y),
            all_vars(!is.na(.))) |>
  mutate(year = as.factor(year.y),
         taxonomic_group = as.factor(taxonomic_group.y)) |>
  select(-c(year.x, taxonomic_group.x))

## 2.3. Calculate Xkm and Ykm ----

# The below code is adapted from the Highland Statistics GAM Course
# Doing this stepwise for each time period included in model

### 2.3.1. Xkm and Ykm 1997-2000 ----

# Transform coordinates to UTM
XY.utm <- UTM_Transform(x = intens_turnover_by_group_for_model$geometry_1997.2000_coords_X, 
                        y = intens_turnover_by_group_for_model$geometry_1997.2000_coords_Y, 
                        zone = 20, 
                        Hemisphere = "north") 

# Convert UTM  coordinates to kilometers
intens_turnover_by_group_for_model$Xkm_1997.2000 <- XY.utm[,"X"] / 1000
intens_turnover_by_group_for_model$Ykm_1997.2000<- XY.utm[,"Y"] / 1000

### 2.3.2. Xkm and Ykm 2006-2009 ----

# Transform coordinates to UTM
XY.utm <- UTM_Transform(x = intens_turnover_by_group_for_model$geometry_2006.2009_coords_X, 
                        y = intens_turnover_by_group_for_model$geometry_2006.2009_coords_Y, 
                        zone = 20, 
                        Hemisphere = "north") 

# Convert UTM  coordinates to kilometers
intens_turnover_by_group_for_model$Xkm_2006.2009 <- XY.utm[,"X"] / 1000
intens_turnover_by_group_for_model$Ykm_2006.2009<- XY.utm[,"Y"] / 1000

## 2.3.3. Xkm and Ykm 2003-2006 ----

# Transform coordinates to UTM
XY.utm <- UTM_Transform(x = intens_turnover_by_group_for_model$geometry_2003.2006_coords_X, 
                        y = intens_turnover_by_group_for_model$geometry_2003.2006_coords_Y, 
                        zone = 20, 
                        Hemisphere = "north") 

# Convert UTM  coordinates to kilometers
intens_turnover_by_group_for_model$Xkm_2003.2006 <- XY.utm[,"X"] / 1000
intens_turnover_by_group_for_model$Ykm_2003.2006<- XY.utm[,"Y"] / 1000

## 2.3.4. Xkm and Ykm 2012-2015 ----

# Transform coordinates to UTM
XY.utm <- UTM_Transform(x = intens_turnover_by_group_for_model$geometry_2012.2015_coords_X, 
                        y = intens_turnover_by_group_for_model$geometry_2012.2015_coords_Y, 
                        zone = 20, 
                        Hemisphere = "north") 

# Convert UTM  coordinates to kilometers
intens_turnover_by_group_for_model$Xkm_2012.2015 <- XY.utm[,"X"] / 1000
intens_turnover_by_group_for_model$Ykm_2012.2015<- XY.utm[,"Y"] / 1000

## 2.3.5. Xkm and Ykm 2009-2012 ----

# Transform coordinates to UTM
XY.utm <- UTM_Transform(x = intens_turnover_by_group_for_model$geometry_2009.2012_coords_X, 
                        y = intens_turnover_by_group_for_model$geometry_2009.2012_coords_Y, 
                        zone = 20, 
                        Hemisphere = "north") 

# Convert UTM  coordinates to kilometers
intens_turnover_by_group_for_model$Xkm_2009.2012 <- XY.utm[,"X"] / 1000
intens_turnover_by_group_for_model$Ykm_2009.2012<- XY.utm[,"Y"] / 1000

## 2.3.6. Xkm and Ykm 2009-2012 ----

# Transform coordinates to UTM
XY.utm <- UTM_Transform(x = intens_turnover_by_group_for_model$geometry_2015.2018_coords_X, 
                        y = intens_turnover_by_group_for_model$geometry_2015.2018_coords_Y, 
                        zone = 20, 
                        Hemisphere = "north") 

# Convert UTM  coordinates to kilometers
intens_turnover_by_group_for_model$Xkm_2015.2018 <- XY.utm[,"X"] / 1000
intens_turnover_by_group_for_model$Ykm_2015.2018<- XY.utm[,"Y"] / 1000

# 3. DATA EXPLORATION ----

## 3.1. Zeros ----
# Check how many observations in the response variable are = 0
zeros_response <- 100 * sum(intens_turnover_by_group_for_model$turnover 
                            == 0) / nrow(intens_turnover_by_group_for_model)
zeros_response # = 0.715 - not too bad

#Check how many observations in one of the explanatory variables are = 0
zeros_explan <- 100 * sum(intens_turnover_by_group_for_model$intensification_amount 
                          == 0) / nrow(intens_turnover_by_group_for_model)
zeros_explan # = 98.26

## 3.2. Outliers ----
# Cleaveland dotplot for the response variable and covariates
# Here I am using the cleaveland dotplot code provided in the "Generalised Additve Models for 
# the analysis of spatial and spatial-temporal data" (04.09 - 07.09.2023)

# List of variables to plot
ToPlot <- c("turnover", "intensification_amount")

# Plot Cleaveland dotplot
Mydotplot(intens_turnover_by_group_for_model[,ToPlot])
 # 2 outliers in intensification amount (they are equal to 25)

# Save Cleaveland dotplot to file
svg(here("figures", "cleaveland_dotplot_turnover_intensification.svg"))

## 3.3. Normality ----
# Plot histogram of all turnover values 
all_turnover <- ggplot(intens_turnover_by_group_for_model, 
                       aes(x = turnover))+
  geom_histogram(color = "black", alpha = 0.6, position = "identity")+
  xlab("Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

# Plot histogram of turnover values broken down by year
new_labels <- c("2000.2006" = "2000 - 2006",
                "2006.2012" = "2006 - 2012",
                "2012.2018" = "2012 - 2018")

turnover_by_year <- ggplot(intens_turnover_by_group_for_model, 
                           aes(x = turnover, fill = year))+
  geom_histogram(color = "black", alpha = 0.6, position = "identity")+
  scale_fill_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

# Combine the 2 into one single figure
combined_hist <- plot_grid(all_turnover, turnover_by_year,
                           labels = c('A', 'B'), label_size = 12,
                           ncol = 1)

# Plot histogram of turnover values broken down by taxonomic group'
new_labels_groups <- c("Aves" = "Aves",
                       "Insecta" = "Insecta",
                       "Mammalia" = "Mammalia",
                       "Plantae" = "Plantae")

ggplot(intens_turnover_by_group_for_model, 
       aes(x = turnover, fill = taxonomic_group))+
  geom_histogram(color = "black", alpha = 0.6, position = "identity")+
  scale_fill_manual(values=c("#6DD3CE", "#C8E9A0", 
                             "#F7A278", "#E8DAB2"))+
  facet_wrap(~taxonomic_group, labeller = labeller(taxonomic_group = new_labels_groups))+
  xlab("Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

# Save histogram of turnover values broken down by taxonomic group plot
ggsave(filename = here("figures",
                       "turnover_intensification_hist_taxonomic_group.svg"),
       width = 10, height = 5.37)

## 3.4. Collinearity ----

# Check for collinearity between covariates
# Pairplot of covariates
ToPlot <- c("intensification_amount", "year.y")
Mypairs(intens_turnover_by_group_for_model[,ToPlot])
# No collinearity detected

svg(here("figures",
         "collinearity_check_covariates.svg"))
dev.off()

## 3.5. Relationships ----

# Convert year and taxonomic group columns to factor
intens_turnover_by_group_for_model$taxonomic_group <- as.factor(intens_turnover_by_group_for_model$taxonomic_group)

# Plot scatter plot with facetwrap
ggplot(intens_turnover_by_group_for_model, aes(x = intensification_amount, y= turnover,
                                      color = taxonomic_group))+
  geom_point(size = 2)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5))+
  facet_wrap(~taxonomic_group)+
  xlab("Intensification")+
  ylab("Turnover")+
  scale_color_manual(values = c("#6DD3CE", "#C8E9A0", 
                                "#F7A278", "#E8DAB2"))+
  theme_classic()+
  theme(legend.position = "none")

# Save plot as .svg
ggsave(here("figures", 
            "turnover_intensification_relationshio_by_taxonomic_group.svg"),
       width = 10, height = 5.37)

### 3.5.1. Relationship by year for each taxonomic group: Aves ----

# Create new facet labels
new_labels <- c("2000.2006" = "2000 - 2006",
                "2006.2012" = "2006 - 2012",
                "2012.2018" = "2012 - 2018")

# Filter df to only have bird records 
aves_turnover_for_model <- intens_turnover_by_group_for_model |>
  filter(taxonomic_group == "Aves") |>
  mutate(year = as.factor(year))

# Plot scatter plot with facetwrap
ggplot(aves_turnover_for_model, 
                           aes(x = intensification_amount,
                               y= turnover, color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  scale_color_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Aves Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")
  

# Save histogram of turnover values broken down by taxonomic group plot
ggsave(here("figures", 
            "aves_turnover_intensification_by_year.svg"),
       width = 10, height = 5.37)

### 3.5.2. Relationship by year for each taxonomic group: Insecta ----

# Filter df to only have bird records 
insecta_turnover_for_model <- intens_turnover_by_group_for_model |>
  filter(taxonomic_group == "Insecta") |>
  mutate(year = as.factor(year))

# Plot scatter plot with facetwrap
ggplot(insecta_turnover_for_model, 
       aes(x = intensification_amount,
           y= turnover, color = year))+
  geom_point(size = 2)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5))+
  scale_color_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Insecta Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")


# Save histogram of turnover values broken down by taxonomic group plot
ggsave(here("figures", 
            "insecta_turnover_intensification_by_year.svg"),
       width = 10, height = 5.37)

### 3.5.3. Relationship by year for each taxonomic group: Mammalia ----

# Filter df to only have bird records 
mammalia_turnover_for_model <- intens_turnover_by_group_for_model |>
  filter(taxonomic_group == "Mammalia") |>
  mutate(year = as.factor(year))

# Plot scatter plot with facetwrap
ggplot(mammalia_turnover_for_model, 
       aes(x = intensification_amount,
           y= turnover, color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  scale_color_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Mammalia Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")


# Save histogram of turnover values broken down by taxonomic group plot
ggsave(here("figures", 
            "mammalia_turnover_intensification_by_year.svg"),
       width = 10, height = 5.37)

### 3.5.4. Relationship by year for each taxonomic group: Plantae ----

# Filter df to only have bird records 
plantae_turnover_for_model <- intens_turnover_by_group_for_model |>
  filter(taxonomic_group == "Plantae") |>
  mutate(year = as.factor(year))

# Plot scatter plot with facetwrap
ggplot(plantae_turnover_for_model, 
       aes(x = intensification_amount,
           y= turnover, color = year))+
  geom_point(size = 2)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5))+
  scale_color_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Plantae Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")


# Save histogram of turnover values broken down by taxonomic group plot
ggsave(here("figures", 
            "plantae_turnover_intensification_by_year.svg"),
       width = 10, height = 5.37)

## 3.6. Relationships without 0s ----

# Subset dataframe to contain turnover values when all years have a non-zero value for intensification
all_intens_corr <- intens_turnover_by_group_for_model |>
  filter(intensification_amount != 0)

# Plot scatter plot with facetwrap
ggplot(all_intens_corr, aes(x = intensification_amount, y= turnover,
                                               color = taxonomic_group))+
  geom_point(size = 2)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5))+
    scale_x_continuous(name = "Intensification Amount",
                     breaks = c(1, 5, 10, 15, 20, 25),
                     labels = c("4%", "20%", "40%",
                                "60%", "80%", "100%"))+
  facet_wrap(~taxonomic_group)+
  xlab("Intensification")+
  ylab("Turnover")+
  scale_color_manual(values = c("#6DD3CE", "#C8E9A0", 
                                "#F7A278", "#E8DAB2"))+
  theme_classic()+
  theme(legend.position = "none")

# Save plot as .svg
ggsave(here("figures", 
            "turnover_non0_intensification_relationship_by_taxonomic_group.svg"),
       width = 10, height = 5.37)

### 3.6.1. Relationship by year for each taxonomic group: Aves ----

# Filter df to only have bird records 
aves_non0_turnover_for_model <- all_intens_corr |>
  filter(taxonomic_group == "Aves") |>
  mutate(year = as.factor(year))

# Plot scatter plot with facetwrap
ggplot(aves_non0_turnover_for_model, 
       aes(x = intensification_amount,
           y= turnover, color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  scale_color_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Aves Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")


# Save histogram of turnover values broken down by taxonomic group plot
ggsave(here("figures", 
            "aves_turnover_non0_intensification_by_year.svg"),
       width = 10, height = 5.37)

### 3.6.2. Relationship by year for each taxonomic group: Insecta ----

# Filter df to only have bird records 
insecta_non0_turnover_for_model <- all_intens_corr |>
  filter(taxonomic_group == "Insecta") |>
  mutate(year = as.factor(year))

# Plot scatter plot with facetwrap
ggplot(insecta_non0_turnover_for_model, 
       aes(x = intensification_amount,
           y= turnover, color = year))+
  geom_point(size = 2)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 1))+
  scale_color_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Insecta Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

# Save histogram of turnover values broken down by taxonomic group plot
ggsave(here("figures", 
            "insecta_turnover_non0_intensification_by_year.svg"),
       width = 10, height = 5.37)

### 3.6.3. Relationship by year for each taxonomic group: Mammalia ----

# Filter df to only have bird records 
mammalia_non0_turnover_for_model <- all_intens_corr |>
  filter(taxonomic_group == "Mammalia") |>
  mutate(year = as.factor(year))

# Plot scatter plot with facetwrap
ggplot(mammalia_non0_turnover_for_model, 
       aes(x = intensification_amount,
           y= turnover, color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  scale_color_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Mammalia Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")


# Save histogram of turnover values broken down by taxonomic group plot
ggsave(here("figures", 
            "mammalia_turnover_non0_intensification_by_year.svg"),
       width = 10, height = 5.37)

### 3.6.4. Relationship by year for each taxonomic group: Plantae ----

# Filter df to only have bird records 
plantae_non0_turnover_for_model <- all_intens_corr |>
  filter(taxonomic_group == "Plantae") |>
  mutate(year = as.factor(year))

# Plot scatter plot with facetwrap
ggplot(plantae_non0_turnover_for_model, 
       aes(x = intensification_amount,
           y= turnover, color = year))+
  geom_point(size = 2)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5))+
  scale_color_manual(values=c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Plantae Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")


# Save histogram of turnover values broken down by taxonomic group plot
ggsave(here("figures", 
            "plantae_turnover_non0_intensification_by_year.svg"),
       width = 10, height = 5.37)

# 4. GAM with non-zero intensification ----

## 4.1. Run model ----

# Convert year to numeric
all_intens_corr$year <- as.numeric(as.character(all_intens_corr$year))

# Run beta GAM
m1 <- gam(turnover ~ s(year, k = 3) + s(new_cell) + intensification_amount + taxonomic_group,
          method = "REML",
          family = betar,
          data = all_intens_corr)

summary(m1)
draw(m1)
gam.check(m1)

## 4.2 Model validation ----

# Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = m1)

# Plot the simulate residuals
plot(simulationOutput) # Horrible violations of all the assumptions

# 5. GAMM ----

## 5.1. Model formulation ----
m2 <- gamm(turnover ~ s(year, k = 3) + intensification_amount + taxonomic_group)


year_gam <- gamm(nottem ~ s(nottem_year) + s(nottem_month, bs = "cc"),
                 data = df)

year_gam_AR1 <- gamm(nottem ~ s(nottem_year) + s(nottem_month,
                                                 bs = "cc"), correlation = corARMA(form = ~1 | nottem_year,
                                                                                   p = 1), data = df)

year_gam_AR2 <- gamm(nottem ~ s(nottem_year) + s(nottem_month,
                                                 bs = "cc"), correlation = corARMA(form = ~1 | nottem_year,
                                                                                   p = 2), data = df)
