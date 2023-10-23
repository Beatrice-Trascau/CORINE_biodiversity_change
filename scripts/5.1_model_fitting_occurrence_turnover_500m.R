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
library(lattice)
library(cowplot)
library(lme4)
library(betareg)
library(mgcv)
library(psych)
source("HighstatLibV14.R") #useful functions from GAM course, see below
# Remember to cite this library as:
# Mixed effects models and extensions in ecology with R. (2009).
#Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer

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
  mutate(year = as.factor(year.y)) |>
  select(-year.x)

## 1.2. Extensification -----
# Read in data
extens_turnover <- readRDS(here("data", 
                                "extensification_occurrence_turnover.rds"))

# Remove unnecessary columns
extens_turnover <- extens_turnover |>
  select(-c(species_1997.2000, geometry_1997.2000,
            species_2006.2009, geometry_2006.2009, 
            species_2003.2006, geometry_2003.2006,
            species_2012.2015, geometry_2012.2015,
            species_2009.2012, geometry_2009.2012,
            species_2015.2018, geometry_2015.2018))

# Remove rows with NA for intensification and NaN for turnover
extens_turnover <- extens_turnover |>
  filter_at(vars(extens_2000.2006, extens_2006.2012, extens_2012.2018),
            all_vars(!is.na(.))) |>
  filter_at(vars(extens_2000.2006, extens_2006.2012, extens_2012.2018),
            all_vars(!is.nan(.)))


# 2. DATA EXPLORATION ----

## 2.1. Zeros ----
# Check how many observations in the response variable are = 0
zeros_response <- 100 * sum(intens_turnover_for_model$turnover 
                            == 0) / nrow(intens_turnover_for_model)
zeros_response # = 0.81 - not too bad

#Check how many observations in one of the explanatory variables are = 0
zeros_explan <- 100 * sum(intens_turnover_for_model$intensification_amount 
                          == 0) / nrow(intens_turnover_for_model)
zeros_explan # = 97.62

## 2.2. Outliers ----
# Cleaveland dotplot for the response variable and covariates
# Here I am using the cleaveland dotplot code provided in the "Generalised Additve Models for 
# the analysis of spatial and spatial-temporal data" (04.09 - 07.09.2023)

# List of variables to plot
ToPlot <- c("turnover", "intensification_amount")

# Plot Cleaveland dotplot
Mydotplot(intens_turnover_for_model[,ToPlot])

# Save Cleaveland dotplot to file
svg(here("figures", "cleaveland_dotplot_turnover_intensification.svg"))

dev.off()

## 2.3. Normality ----
#Plot histogram of all turnover values 
all_turnover <- ggplot(intens_turnover_for_model, aes(x = turnover))+
  geom_histogram(color = "black", alpha = 0.6, position = "identity")+
  xlab("Turnover Value")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position = "none")

#Plot histogram of turnover values broken down by year
new_labels <- c("2000.2006" = "2000 - 2006",
                "2006.2012" = "2006 - 2012",
                "2012.2018" = "2012 - 2018")

turnover_by_year <- ggplot(intens_turnover_for_model, aes(x = turnover, fill = year))+
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


# Save combined plot
ggsave(filename = here("figures",
                       "turnover_intensification_hist.svg"),
       plot = combined_hist)


## 2.4. Collinearity ----
# Check for collinearity between covariates
# Pairplot of covariates
ToPlot <- c("intensification_amount", "year.y")
Mypairs(intens_turnover_for_model[,ToPlot])
# No collinearity detected

svg(here("figures",
         "collinearity_check_covariates.svg"))

## 2.5. Relationships ----

# Convert year column to factor
intens_turnover_for_model$year <- as.factor(intens_turnover_for_model$year)

# Create new facet labels
new_labels <- c("2000.2006" = "2000 - 2006",
                "2006.2012" = "2006 - 2012",
                "2012.2018" = "2012 - 2018")

# Plot scatter plot with facetwrap
ggplot(intens_turnover_for_model, aes(x = intensification_amount, y= turnover,
                            color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Intensification")+
  ylab("Turnover")+
  scale_color_manual(values = c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  theme_classic()+
  theme(legend.position = "none")

# Save plot as .svg
#ggsave(here("figures",
            #"turnover_intensification.svg"))

## 2.6. Interactions ----
# Plot coplot to visualise potential presence of interactions

# Define year and intensification amount as categorial values
intens_turnover_for_model$year.y <- as.factor(intens_turnover_for_model$year.y)
intens_turnover_for_model$intens_fact <- as.factor(intens_turnover_for_model$intensification_amount)

# Simple lm
intens_M1 <- lm(intens_turnover_for_model$turnover ~ 
                  intens_turnover_for_model$year.y*
                  intens_turnover_for_model$intens_fact)
summary(intens_M1)
anova(intens_M1)

#Make the coplot
coplot(turnover ~ intensification_amount| year.y * intens_fact,
       data = intens_turnover_for_model)

coplot(year.y ~ intens_fact | turnover,
       data = intens_turnover_for_model)


# 3. GLM ----

## 3.1. GLM for Intensification ----
# Run GLM
intens_glm <- glm(turnover ~ intensification_amount + year.y,
                  intens_turnover_for_model,
                  family = "poisson")

# Check output
summary(intens_glm)
anova(intens_glm, test = "Chisq")

# Poisson is a wildly inappropriate family for the data that we have
# Turnover data is proportion in [0,1] => need beta distribution
# Beta Regression alternative to GLM for beta distributed data

## 3.2. Beta Regression ----
# Beta distribution assumes that data are in (0,1)
# Beta alternative to GAM can automatically deal with these
# But for Beta regression, data transformation is needed

# Add and subtract very small value to 0s and 1s respectively
intens_turnover_for_model <- intens_turnover_for_model |>
  mutate(adjusted_turnover = case_when(turnover == 0 ~ turnover + 1e-10,
                                       turnover == 1 ~ turnover - 1e-10, 
                                       TRUE ~ turnover))
# Run Beta regression
betareg_intens <- betareg(adjusted_turnover ~ year * intensification_amount,
                          data = intens_turnover_for_model)

summary(betareg_intens)

## 3.3. Beta regression model validation ----

# 4. GAM ----

## 4.1. Intensification amount as smoother ----
# Run model
betagam_intens <- gam(turnover ~ year + s(intensification_amount),
                      method = "REML",
                      family = betar,
                      data = intens_turnover_for_model)

# Check model output
summary(betagam_intens)
gam.check(betagam_intens)

## 4.2. Intensification amount and year as smoothers ----
# Convert year to numeric and not factor
intens_turnover_for_model <- intens_turnover_for_model |>
  mutate(year_numeric = as.numeric(year))

# Run model
betagam_intens_year <- gam(turnover ~ s(year_numeric, k = 3) 
                           + s(intensification_amount),
                      method = "REML",
                      family = betar,
                      data = intens_turnover_for_model)

# Check model output
summary(betagam_intens)
gam.check(betagam_intens)


m3_fixed_intercept <- gam(positive_centered_turnover ~ 0 + s(year, k = 3) 
                          + s(intensification_amount),
                          method = "REML",
                          family = betar,
                          data = natural_intens_model)

# 5. No Zero intensification values ----

## 5.1. Prepare the data ----

# Remove 0s
natural_intens_model <- intens_turnover_for_model |>
  filter(intensification_amount != 0)


## 5.2. Data Exploration ----

### 5.2.1. Outliers ----

# Cleaveland dotplot for the response variable and covariates
# Here I am using the cleaveland dotplot code provided in the "Generalised Additve Models for 
# the analysis of spatial and spatial-temporal data" (04.09 - 07.09.2023)

# List of variables to plot
ToPlot <- c("turnover", "intensification_amount")

# Plot Cleaveland dotplot
Mydotplot(natural_intens_model[,ToPlot])

# Save Cleaveland dotplot to file
#svg(here("figures", "cleaveland_dotplot_turnover_non0_intensification.svg"))

#dev.off()


### 5.2.2. Relationships ----

# Want to see if the relationship between turnover and intensification change when the 0s are removed

# Convert year column to factor
natural_intens_model$year <- as.factor(natural_intens_model$year)

# Create new facet labels
new_labels <- c("2000.2006" = "2000 - 2006",
                "2006.2012" = "2006 - 2012",
                "2012.2018" = "2012 - 2018")

# Plot scatter plot with facetwrap
ggplot(natural_intens_model, aes(x = intensification_amount, y= turnover,
                                      color = year))+
  geom_point(size = 2)+
  geom_smooth()+
  scale_x_continuous(name = "Intensification Amount",
                     breaks = c(1, 5, 10, 15, 20, 25),
                     labels = c("4%", "20%", "40%",
                                "60%", "80%", "100%"))+
  facet_wrap(~year, labeller = labeller(year = new_labels))+
  xlab("Intensification")+
  ylab("Turnover")+
  scale_color_manual(values = c("#6DD3CE", "#C8E9A0", "#F7A278"))+
  theme_classic()+
  theme(legend.position = "none")

# The relationships change significantly - the 0 inflation in the intensification

# Save plot as .svg
ggsave(here("figures",
            "turnover_non0_intensification.svg"))

# 5.3. GAM with non-zero intesification ----
# Convert year to numeric
natural_intens_model$year <- as.numeric(as.character(natural_intens_model$year))

# Run beta GAM
m1 <- gam(turnover ~ s(year, k = 3) + s(intensification_amount),
          method = "REML",
          family = betar,
          data = natural_intens_model)

summary(m1)
draw(m1)
gam.check(m1)

# 6. MODELS WITH FIXED INTERCEPT ----

## 6.1. Calculate mean turnover at intensification = 0 ----

# Extract rows with intensification_amount = 0
no_change <- intens_turnover_for_model |>
  filter(intensification_amount == 0)

# Calculate mean turnover 
avg_turnover <- mean(no_change$turnover) #0.61

## 6.2. GLM ----
m2_fxed_intercept <- lm(I(turnover - avg_turnover) ~ 0 + intensification_amount + year.y,
                        no_change)

summary(m2_fxed_intercept)

## 6.3. Beta GAM ----

# Fixed intercepts cannot be set in GAM -> use workaround
#  Centre turnover around the "fixed intercept" calcualted above (0.61)
#  Do this with the forula: Y - mean(Y) + 0.61, Y = response variable
natural_intens_model <- natural_intens_model |>
  mutate(centered_turnover = turnover - mean(turnover) + avg_turnover)

# Restrict values to be within (0,1)
#  Do this to deal with any negative values and any values larger than 1
natural_intens_model <- natural_intens_model |>
  mutate(positive_centered_turnover = pmin(pmax(centered_turnover, 1e-6),
                                           1-(1e-6)))

# Fit GAM
m3_fixed_intercept <- gam(positive_centered_turnover ~ 0 + s(year, k = 3) 
                          + s(intensification_amount),
                          method = "REML",
                          family = betar,
                          data = natural_intens_model)

summary(m3_fixed_intercept)
gam.check(m3_fixed_intercept)

# 7. INTENSIFICATION CORRELATIONS ----
# Look at correlations between turnover values in different time periods

## 7.1. Turover correlations when there is no chage ----

# Subset dataframe to only contain turnover values when there is no change
no_change_corr <- intens_turnover |>
  filter(intens_2000.2006 == 0 & intens_2006.2012 == 0 & intens_2012.2018 == 0) |>
  select(-c(cell, intens_2000.2006, intens_2006.2012, intens_2012.2018))

# Open SVG device
svg(filename = here("figures", 
                    "turnover_correlations_no_change.svg"))

# Plot pairwise correlations 
pairs.panels(no_change_corr)

# Close the SVG device
dev.off()

## 7.2. Turnover correlations when there is some intensification ----
# Subset dataframe to contain turnover values when there is at least one change throughout the years of sampling
some_intens_corr <- intens_turnover |>
  filter(intens_2000.2006 != 0 | intens_2006.2012 != 0 | intens_2012.2018 != 0) |>
  select(-c(cell, intens_2000.2006, intens_2006.2012, intens_2012.2018))

# Open SVG device
svg(filename = here("figures",
                    "turnover_correlations_some_intensification.svg"))

# Plot pairwise correlations 
pairs.panels(some_intens_corr)

# Close SVG device
dev.off()

## 7.3. Turnover correlations when there is only intensification ----
# Subset dataframe to contain turnover values when all years have a non-zero value for intensification
all_intens_corr <- intens_turnover |>
  filter(intens_2000.2006 != 0 & intens_2006.2012 != 0 & intens_2012.2018 != 0) |>
  select(-c(cell, intens_2000.2006, intens_2006.2012, intens_2012.2018))

# Open SVG device
svg(filename = here("figures",
                    "turnover_correlations_all_intensification.svg"))

# Plot pairwise correlations
pairs.panels(all_intens_corr)

# Close SVG device
dev.off()

# 8. EXTENSIFICATION CORRELATIONS ----

## 8.2. Turnover correlations when there is some extensification ----
# Subset dataframe to contain turnover values when there is at least one change throughout the years of sampling
some_extens_corr <- extens_turnover |>
  filter(extens_2000.2006 != 0 | extens_2006.2012 != 0 | extens_2012.2018 != 0) |>
  select(-c(cell, extens_2000.2006, extens_2006.2012, extens_2012.2018))

# Open SVG device
svg(filename = here("figures",
                    "turnover_correlations_some_extensification.svg"))

# Plot pairwise correlations 
pairs.panels(some_extens_corr)

# Close SVG device
dev.off()

## 7.3. Turnover correlations when there is only extensification ----
# Subset dataframe to contain turnover values when all years have a non-zero value for intensification
all_extens_corr <- extens_turnover |>
  filter(extens_2000.2006 != 0 | extens_2006.2012 != 0 | extens_2012.2018 != 0) |>
  select(-c(cell, extens_2000.2006, extens_2006.2012, extens_2012.2018))

# Open SVG device
svg(filename = here("figures",
                    "turnover_correlations_all_extensification.svg"))

# Plot pairwise correlations
pairs.panels(all_extens_corr)

# Close SVG device
dev.off()
