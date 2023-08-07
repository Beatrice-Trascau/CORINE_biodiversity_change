##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
########- 3.2_compare_turnover_in_land-use -########
##------------------------------------------------##

#This script contains code to compare calculated turnover between different land use changes

# 0. PACKAGES ----
library(here)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(svglite)
library(car)

# 1. LOAD DATA ----

## 1.1. Download data from box (if needed) ----

#Add download link
occurrences_turnover <- ("https://ntnu.box.com/shared/static/h4r6naswo0frbxkfnae5tyx1d7ld4v3z.rds")

#Download the file to local
download.file(occurrences_turnover, here("data",
                                         "occurrences_turnover.rds"))


## 1.2. Read in the data ----
occurrences_turnover <- readRDS(here("data",
                                     "occurrences_turnover.rds"))

# 2. PREPARE DATA FOR ANALYSIS ----

## 2.1. Remove rows with NA for land cover change columns and NaN for turnover columns----
occurrences_turnover <- occurrences_turnover |>
  filter_at(vars(cover_change_2000.2006, cover_change_2006.2012, cover_change_2012.2018),
            all_vars(!is.na(.))) |>
  filter_at(vars(turover2000.2006, turover2006.2012, turover2012.2018),
            all_vars(!is.nan(.)))

## 2.2. Convert to long format ----

#Remove columns 2 to 13 (not needed anymore)
compressed_occurrence_turnover <- occurrences_turnover |>
  select(-c(species_1997.2000, geometry_1997.2000,
            species_2006.2009, geometry_2006.2009, 
            species_2003.2006, geometry_2003.2006,
            species_2012.2015, geometry_2012.2015,
            species_2009.2012, geometry_2009.2012,
            species_2015.2018, geometry_2015.2018)) |>
  mutate(year2000.2006 = "2000.2006",
         year2006.2012 = "2006.2012",
         year2012.2018 = "2012.2018")

#Create separate df for turnover
turnover_long <- compressed_occurrence_turnover |>
  #Select only the turnover columns
  select(c(cell, turover2000.2006, turover2006.2012,
           turover2012.2018)) |>
  #Convert to long format
  pivot_longer(
    cols = "turover2000.2006":"turover2012.2018",
    names_to = "turnover_year",
    values_to = "turnover") |>
  #Change turnover_year value to only contain the year
  mutate(year = case_when(turnover_year == "turover2000.2006" ~ "2000.2006",
                          turnover_year == "turover2006.2012" ~ "2006.2012",
                          turnover_year == "turover2012.2018" ~ "2012.2018"),
         new_cell = cell$U2006_CLC2000_V2020_20u1,
         ID = paste(new_cell, year, sep = "_")) |>
  #Remove unnecessary turnover_year column
  select(-c(turnover_year, cell, new_cell))

#Create separate df for the land cover changes
land_change_long <- compressed_occurrence_turnover |>
  #Select only the turnover columns
  select(c(cell, cover_change_2000.2006, cover_change_2006.2012,
           cover_change_2012.2018)) |>
  #Convert to long format
  pivot_longer(
    cols = "cover_change_2000.2006":"cover_change_2012.2018",
    names_to = "cover_change_year",
    values_to = "cover_change") |>
  #Change cover_change_year value to only contain the year
  mutate(year = case_when(cover_change_year == "cover_change_2000.2006" ~ "2000.2006",
                          cover_change_year == "cover_change_2006.2012" ~ "2006.2012",
                          cover_change_year == "cover_change_2012.2018" ~ "2012.2018"),
         new_cell = cell$U2006_CLC2000_V2020_20u1,
         ID = paste(new_cell, year, sep = "_")) |>
  #Remove unnecessary cover_change_year column
  select(-c(cover_change_year, cell, new_cell))

#Merge the turnover_long and land_cover_change dfs
land_turnover <- merge(turnover_long, land_change_long,
                       by = "ID")

#Check cols
colnames(land_turnover)

#Check that year.x & year.y are the same
setequal(land_turnover$year.x, land_turnover$year.y) #TRUE

#Remove year.x and year.y columns
land_turnover <- land_turnover |>
  mutate(year = year.y) |>
  select(-year.x)

# 3. COMPARE TURNOVER BETWEEN LAND COVER CHANGES AND YEARS ----

## 3.1. Visualise data ----

#Boxplot with land cover and year
ggboxplot(land_turnover, x = "cover_change", y = "turnover", 
          color = "year", fill = "year",
          palette = c("#6DD3CE", "#C8E9A0", "#F7A278"))


#Another boxplot with land cover and year
ggplot(land_turnover, aes(x = cover_change, y = turnover,
                          fill = year, color = year),
       stat = "identity",
       position = "dodge")+
  geom_boxplot()+
  scale_color_manual(labels = c("2000-2006", "2006-2012", "2012-2018"),
                     values = c("#0A2544", "#2C6F2C", "#D10BD1"),
                     name = "Sampling Years")+
  scale_fill_manual(name = "Sampling Years",
                    labels = c("2000-2006", "2006-2012", "2012-2018"),
                    values = c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  
  scale_x_discrete(labels = c("Deforestation", "Extensification", "Forestry",
                              "Intensification", "No Change", "Succession",
                              "Succession/Forestry", "Urbanisation"))+
  xlab("Land Cover Transitions")+
  ylab("Turnover")+
  coord_flip()+
  theme_classic()+
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14,
                                   color = "#000000"),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14,
                                   color = "#000000"),
        legend.title = element_text(size=14),
        legend.text = element_text(size = 13.5))
#save the plot
ggsave(here("figures",
            "turnover_land_cover_changes.svg"))


#Boxplot with jitter
#Another boxplot with land cover and year
ggplot(land_turnover, aes(x = cover_change, y = turnover,
                          fill = year, color = year),
       stat = "identity",
       position = "dodge")+
  geom_boxplot()+
  scale_color_manual(labels = c("2000-2006", "2006-2012", "2012-2018"),
                     values = c("#0A2544", "#2C6F2C", "#D10BD1"),
                     name = "Sampling Years")+
  scale_fill_manual(name = "Sampling Years",
                    labels = c("2000-2006", "2006-2012", "2012-2018"),
                    values = c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  
  scale_x_discrete(labels = c("Deforestation", "Extensification", "Forestry",
                              "Intensification", "No Change", "Succession",
                              "Succession/Forestry", "Urbanisation"))+
  geom_jitter(size = 0.7, alpha = 0.5)+
  xlab("Land Cover Transitions")+
  ylab("Turnover")+
  coord_flip()+
  theme_classic()+
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14,
                                   color = "#000000"),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14,
                                   color = "#000000"),
        legend.title = element_text(size=14),
        legend.text = element_text(size = 13.5))

#save the plot
ggsave(here("figures",
            "turnover_land_cover_changes_with_jitter.svg"))

## 3.2. Compute two-way ANOVA test ----

#Run two-way ANOVA test
turnover_aov <- aov(turnover ~ year * cover_change,
                    data = land_turnover)
#Get summary
summary(turnover_aov)

#Tukey post hoc test
TukeyHSD(turnover_aov, which = "year")

#Compute summary statistics
group_by(land_turnover, cover_change, year) %>%
  summarise(
    count = n(),
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )

## 3.3. Check ANOVA assumptions ----

#Homogenous variance
plot(turnover_aov, 1)
#Levene's test to check for homogeneity of variance
leveneTest(turnover ~ year.y * cover_change, data = land_turnover)

#Normality
plot(turnover_aov, 2)
#Shapiro-Wilk test
aov_residuals <- residuals(object = turnover_aov) #extract residuals
shapiro.test(x = aov_residuals)
