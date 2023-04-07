##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##------- 1.2_corine_land_cover_transitions ------##
##------------------------------------------------##

#This script contains code to explore the land cover transitions between each year of survey

# 0. LOAD PACKAGES ----
library(here)
library(terra)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gt)
library(networkD3)

# 1. LOAD MODIFIED CORINE STACKS ----

## 1.1. Download the modified CORINE stack and df with transition score meaning from Box ----

#Add download link for CORINE stack with modified layers
norway_corine <- ("https://ntnu.box.com/shared/static/ugwtizqcr3a4t4vgxj6qu0rh0h54rtoh.tif")

#Download the file
download.file(norway_corine, "corine_modified_classes_stack.tif")


## 1.2. Read the layers stack ----
norway_corine <- rast("corine_modified_classes_stack.tif")

# 2. LAND COVER TRANSITIONS BETWEEN 2000 AND 2006 ----

## 2.1. Calculate change between 2000 and 2006 ----
#Extract values for the difference between 2000 and 2006 as df
corine_2000_2006_df <- as.data.frame(freq(norway_corine[[1]] -
                                            norway_corine[[2]])) |>
  mutate(source_year = "2000",
         target_year = "2006",
         difference = value) |>
  select(-layer)

## 2.2. Create score meaning data frame ----
#Create vectors for each column
source_number <- c(rep(1,7), rep(80,7), rep(103,7),
                   rep(250,7), rep(380,7), rep(590,7),
                   rep(711,7))

source_name <- c(rep("urban.fabric",7), rep("complex.agriculture",7), 
                 rep("agriculture.and.vegetation",7), rep("forests",7), 
                 rep("moors.heath.grass",7), rep("transitional.woodland",7),
                 rep("sparse.vegetation",7))

target_number <- c(rep(c(1,80,103,250,380,590,711), 7))

target_name <- c(rep(c("urban.fabric", "complex.agriculture", "agriculture.and.vegetation",
                       "forests", "moors.heath.grass", "transitional.woodland",
                       "sparse.vegetation"), 7))

#Combine vectors in df
corine_class_meaning <- data.frame(source_number, source_name,
                                   target_number, target_name)

corine_class_meaning <- corine_class_meaning |>
  mutate(difference = source_number - target_number)

## 2.3. Get values for source and target land cover in the change layer from the score meaning data frame ----
#Subset score meaning df to only contain the "differences" which are found across Norway
norway_corine_class_meaning <- corine_class_meaning |>
  filter(difference %in% corine_2000_2006_df$value)

#Change column names
colnames(norway_corine_class_meaning) <- c("source_number", "source_name",
                                           "target_number", "target_name",
                                           "value")


#Merge norway_corine_class_meaning df and corine_2000_2006_df into one
corine_2000_2006_change_meaning <- merge(corine_2000_2006_df,
                                         norway_corine_class_meaning,
                                         by = "value")

## 2.4. Prepare df for sankey plot ----
#Merge columns "source_year" with "source_name" and "target_year" with "target_name" so that we can differentiate the transitions
corine_2000_2006_sankey <- corine_2000_2006_change_meaning |>
  unite(source, c(source_year, source_name), sep = ".",
        remove = FALSE) |>
  unite(target, c(target_year, target_name), sep = ".",
        remove = FALSE)

#Remove the rows that show "no change" (i.e. value = 0)
corine_2000_2006_sankey <- corine_2000_2006_sankey |>
  filter(value != 0)

#Remove columns:value, layer, source_year, target_year, source_number, source_name, target_number, target_name
corine_2000_2006_sankey <- corine_2000_2006_sankey |>
  select(count, source, target)

#Re-arrange columns in the order: source, target, count
corine_2000_2006_sankey <- corine_2000_2006_sankey |>
  relocate(source, target, count)

#Change values in target so that they are different from the source one (add a space at the end)
corine_2000_2006_sankey <- corine_2000_2006_sankey |>
  mutate(target = paste(target, " ", sep = ""))

## 2.5. Sankey Plot for transitions between 2000 and 2006 ----
### 2.5.1. Sankey Plot for transitions between 2000 and 2006 (all classes included) ----
#Create node dataframe
nodes2000_2006 <- data.frame(name = c(as.character(corine_2000_2006_sankey$source),
                                      as.character(corine_2000_2006_sankey$target)) |>
                               unique())

#Create ID to provide connection for networkD3
corine_2000_2006_sankey$IDsource=match(corine_2000_2006_sankey$source, nodes2000_2006$name)-1 

corine_2000_2006_sankey$IDtarget=match(corine_2000_2006_sankey$target, nodes2000_2006$name)-1

#Prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

#Make the Network
sankeyNetwork(Links = corine_2000_2006_sankey, Nodes = nodes2000_2006,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=40, fontSize=11, nodePadding=20)

### 2.5.2. Sankey Plot for transitions between 2000 and 2006 (excluding coniferous forest to transitional woodland shrub) ----
#The transitions conigerous forest -> transitional woodland shrub (and vice versa) were removed to allow better visualisation of the other transitions (which are not dominant)
#Remove rows 3 and 26
forestless_2000_2006_sankey <- corine_2000_2006_sankey |>
  filter(!row_number() %in% c(3, 26))

#Colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

#Create node dataframe
nodes_forestless <- data.frame(name = c(as.character(forestless_2000_2006_sankey$source),
                                        as.character(forestless_2000_2006_sankey$target)) |>
                                 unique())

#Reformat for ID
forestless_2000_2006_sankey$IDsource = match(forestless_2000_2006_sankey$source,
                                             nodes_forestless$name) - 1

forestless_2000_2006_sankey$IDtarget = match(forestless_2000_2006_sankey$target,
                                             nodes_forestless$name) - 1

#Make Network
sankeyNetwork(Links = forestless_2000_2006_sankey, Nodes = nodes_forestless,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=20, fontSize=11, nodePadding=25)

## 2.6. Barplots of land cover transitions between 2000 and 2006 ----
### 2.6.1. Barplots of land cover transitions between 2000 and 2006 (all classes) ----
#Create dataframe of "transition to" values
#this dataframe will have the "focus" cover class in the "source" column and the cover class it transitions to in the "target" column
#the values will be negative because for every source class, this is the area "lost" that is converted to the "target" cover class
loss_2000_2006 <- corine_2000_2006_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select (focus, transition, count)

#Create dataframe of "transitions from" values
#this dataframe will have "focus" cover class in the "target" column and the cover class it transitions from in the "target" column
#the values will be positive because for every source class, this is the area "gained" that is converted from the "source" cover clas
gain_2000_2006 <- corine_2000_2006_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count)


#Merge the gain and loss dataframes into a single df
gain_loss_2000_2006 <- rbind(loss_2000_2006, gain_2000_2006)

#Plot gain_loss_2000_2006
gain_loss_plot <- gain_loss_2000_2006 |>
  ggplot(aes(fill = transition,
             y = count,
             x = focus))+
  geom_bar(position = "stack",
           stat = "identity")+
  labs(fill = "Land Cover Classes",
       x = "Land cover classes",
       y = bquote("Area changes"~("km"^2)),
       title = "Land Cover Transitions 2000 - 2006")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                                            "#6A3D9A", "#FF7F00",
                                            "gold1","maroon"))+
                                              theme_classic()
#Change x axis ticks
gain_loss_plot + theme(axis.text.x = element_text(angle = 30,
                                                  hjust = 1))

### 2.6.2. Barplots of land cover transitions between 2000 and 2006 (without coniferous and transitional woodland shrub) ----
#Creat df that does not have the coniferous -> transitional woodlans shrub (and vice versa) columns
gain_loss_2000_2006_forestless <- gain_loss_2000_2006 |>
  filter(!row_number() %in% c(3, 26, 36, 59))

#Plot gain_loss_2000_2006
gain_loss_plot_forestless <- gain_loss_2000_2006_forestless |>
  ggplot(aes(fill = transition,
             y = count,
             x = focus))+
  geom_bar(position = "stack",
           stat = "identity")+
  labs(fill = "Land Cover Classes",
       x = "Land cover classes",
       y = bquote("Area changes"~("km"^2)),
       title = "Land Cover Transitions 2000 - 2006")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                                            "#6A3D9A", "#FF7F00",
                                            "gold1","maroon"))+
                                              theme_classic()
#Change x axis ticks
gain_loss_plot_forestless + theme(axis.text.x = element_text(angle = 30,
                                                             hjust = 1))


# 3. LAND COVER TRANSITIONS BETWEEN 2006 AND 2012 ----
## 3.1. Calculate change between 2006 and 2012 ----
#Extract values for the difference between 2000 and 2006 as df
corine_2006_2012_df <- as.data.frame(freq(norway_corine[[2]] -
                                            norway_corine[[3]])) |>
  mutate(source_year = "2006",
         target_year = "2012",
         difference = value) |>
  select(-layer)

## 3.2. Get values for source and target land cover in the change layer from the score meaning data frame ----
#Subset score meaning df to only contain the "differences" which are found across Norway
norway_corine_class_meaning_2006_2012 <- corine_class_meaning |>
  filter(difference %in% corine_2006_2012_df$value)

colnames(norway_corine_class_meaning_2006_2012) <- c("source_number", "source_name",
                                                     "target_number", "target_name",
                                                     "value")


#Merge norway_corine_class_meaning_2006_2012 df and corine_2006_2012_df into one
corine_2006_2012_change_meaning <- merge(corine_2006_2012_df,
                                         norway_corine_class_meaning_2006_2012,
                                         by = "value")

## 3.3. Prepare df for sankey plot ----
#Merge columns "source_year" with "source_name" and "target_year" with "target_name" so that we can differentiate the transitions
corine_2006_2012_sankey <- corine_2006_2012_change_meaning |>
  unite(source, c(source_year, source_name), sep = ".",
        remove = FALSE) |>
  unite(target, c(target_year, target_name), sep = ".",
        remove = FALSE)

#Remove the rows that show "no change" (i.e. value = 0)
corine_2006_2012_sankey <- corine_2006_2012_sankey |>
  filter(value != 0)

#Remove columns:value, layer, source_year, target_year, source_number, source_name, target_number, target_name
corine_2006_2012_sankey <- corine_2006_2012_sankey |>
  select(count, source, target)

#Re-arrange columns in the order: source, target, count
corine_2006_2012_sankey <- corine_2006_2012_sankey |>
  relocate(source, target, count)

#Change values in target so that they are different from the source one (add a space at the end)
corine_2006_2012_sankey <- corine_2006_2012_sankey |>
  mutate(target = paste(target, " ", sep = ""))

## 3.4. Sankey Plot for transitions between 2006 and 2012 ----
### 3.4.1. Sankey Plot for transitions between 2006 and 2012 (all classes included) ----
#Create node dataframe
nodes2006_2012 <- data.frame(name = c(as.character(corine_2006_2012_sankey$source),
                                      as.character(corine_2006_2012_sankey$target)) |>
                               unique())

#Create ID to provide connection for networkD3
corine_2006_2012_sankey$IDsource=match(corine_2006_2012_sankey$source, nodes2006_2012$name)-1 

corine_2006_2012_sankey$IDtarget=match(corine_2006_2012_sankey$target, nodes2006_2012$name)-1

#Prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

#Make the Network
sankeyNetwork(Links = corine_2006_2012_sankey, Nodes = nodes2006_2012,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=40, fontSize=11, nodePadding=20)

### 3.4.2. Sankey Plot for transitions between 2006 and 2012 (excluding coniferous forest to transitional woodland shrub) ----
#Remove rows 25 and 90
forestless_2006_2012_sankey <- corine_2006_2012_sankey |>
  filter(!row_number() %in% c(6, 29))

#Colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

#Create node dataframe
nodes_forestless2006_2012 <- data.frame(name = c(as.character(forestless_2006_2012_sankey$source),
                                                 as.character(forestless_2006_2012_sankey$target)) |>
                                          unique())

#Reformat for ID
forestless_2006_2012_sankey$IDsource = match(forestless_2006_2012_sankey$source,
                                             nodes_forestless2006_2012$name) - 1

forestless_2006_2012_sankey$IDtarget = match(forestless_2006_2012_sankey$target,
                                             nodes_forestless2006_2012$name) - 1

#Make Network
sankeyNetwork(Links = forestless_2006_2012_sankey, Nodes = nodes_forestless2006_2012,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=20, fontSize=11, nodePadding=25)

## 3.5. Barplots of land cover transitions between 2006 and 2012 ----
### 3.5.1. Barplots of land cover transitions between 2006 and 2012 (all classes) ----
#Create dataframe of "transition to" values
 #this dataframe will have the "focus" cover class in the "source" column and the cover class it transitions to in the "target" column
 #the values will be negative because for every source class, this is the area "lost" that is converted to the "target" cover class
loss_2006_2012 <- corine_2006_2012_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select (focus, transition, count)

#Create dataframe of "transitions from" values
 #this dataframe will have "focus" cover class in the "target" column and the cover class it transitions from in the "target" column
 #the values will be positive because for every source class, this is the area "gained" that is converted from the "source" cover clas
gain_2006_2012 <- corine_2006_2012_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count)


#Merge the gain and loss dataframes into a single df
gain_loss_2006_2012 <- rbind(loss_2006_2012, gain_2006_2012)

#Plot gain_loss_2000_2006
gain_loss_plot_2006_2012 <- gain_loss_2006_2012 |>
  ggplot(aes(fill = transition,
             y = count,
             x = focus))+
  geom_bar(position = "stack",
           stat = "identity")+
  labs(fill = "Land Cover Classes",
       x = "Land cover classes",
       y = bquote("Area changes"~("km"^2)),
       title = "Land Cover Transitions 2006 - 2012")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                                            "#6A3D9A", "#FF7F00", 
                                            "gold1", "maroon"))+
                                              theme_classic()
#Change x axis tick marks
gain_loss_plot_2006_2012 + theme(axis.text.x = element_text(angle = 30,
                                                            hjust = 1))

### 3.5.2. Barplots of land cover transitions between 2006 and 2012 (without coniferous and transitional woodland shrub) ----
#Creat df that does not have the coniferous -> transitional woodlans shrub (and vice versa) columns
gain_loss_2006_2012_forestless <- gain_loss_2006_2012 |>
  filter(!row_number() %in% c(6, 29, 43, 66))

#Plot gain_loss_2006_2012
gain_loss_plot_forestless_2006_2012 <- gain_loss_2006_2012_forestless |>
  ggplot(aes(fill = transition,
             y = count,
             x = focus))+
  geom_bar(position = "stack",
           stat = "identity")+
  labs(fill = "Land Cover Classes",
       x = "Land cover classes",
       y = bquote("Area changes"~("km"^2)),
       title = "Land Cover Transitions 2006 - 2012")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                                            "#6A3D9A", "#FF7F00", 
                                            "gold1", "maroon"))+
                                              theme_classic()
#Change x axis tick marks
gain_loss_plot_forestless_2006_2012 + theme(axis.text.x = element_text(angle = 30,
                                                                       hjust = 1))

# 4. LAND COVER TRANSITIONS BETWEEN 2012 AND 2018 ----
## 4.1. Calculate change between 2012 and 2018 ----
#Extract values for the difference between 2000 and 2006 as df
corine_2012_2018_df <- as.data.frame(freq(norway_corine[[3]] -
                                            norway_corine[[4]])) |>
  mutate(source_year = "2012",
         target_year = "2018",
         difference = value) |>
  select(-layer)

## 4.2. Get values for source and target land cover in the change layer from the score meaning data frame ----
#Subset score meaning df to only contain the "differences" which are found across Norway
norway_corine_class_meaning_2012_2018 <- corine_class_meaning |>
  filter(difference %in% corine_2012_2018_df$value)

colnames(norway_corine_class_meaning_2012_2018) <- c("source_number", "source_name",
                                                     "target_number", "target_name",
                                                     "value")


#Merge norway_corine_class_meaning_2012_2018 df and corine_2006_2012_df into one
corine_2012_2018_change_meaning <- merge(corine_2012_2018_df,
                                         norway_corine_class_meaning_2012_2018,
                                         by = "value")

## 4.3. Prepare df for sankey plot ----
#Merge columns "source_year" with "source_name" and "target_year" with "target_name" so that we can differentiate the transitions
corine_2012_2018_sankey <- corine_2012_2018_change_meaning |>
  unite(source, c(source_year, source_name), sep = ".",
        remove = FALSE) |>
  unite(target, c(target_year, target_name), sep = ".",
        remove = FALSE)

#Remove the rows that show "no change" (i.e. value = 0)
corine_2012_2018_sankey <- corine_2012_2018_sankey |>
  filter(value != 0)

#Remove columns:value, layer, source_year, target_year, source_number, source_name, target_number, target_name
corine_2012_2018_sankey <- corine_2012_2018_sankey |>
  select(count, source, target)

#Re-arrange columns in the order: source, target, count
corine_2012_2018_sankey <- corine_2012_2018_sankey |>
  relocate(source, target, count)

#Change values in target so that they are different from the source one (add a space at the end)
corine_2012_2018_sankey <- corine_2012_2018_sankey |>
  mutate(target = paste(target, " ", sep = ""))

## 4.4. Sankey Plot for transitions between 2012 and 2018 ----
### 4.4.1. Sankey Plot for transitions between 2012 and 2018 (all classes included) ----
#Create node dataframe
nodes2012_2018 <- data.frame(name = c(as.character(corine_2012_2018_sankey$source),
                                      as.character(corine_2012_2018_sankey$target)) |>
                               unique())

#Create ID to provide connection for networkD3
corine_2012_2018_sankey$IDsource=match(corine_2012_2018_sankey$source, 
                                       nodes2012_2018$name)-1 

corine_2012_2018_sankey$IDtarget=match(corine_2012_2018_sankey$target, 
                                       nodes2012_2018$name)-1

#Prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

#Make the Network
sankeyNetwork(Links = corine_2012_2018_sankey, Nodes = nodes2012_2018,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=40, fontSize=11, nodePadding=20)

### 4.4.2. Sankey Plot for transitions between 2012 and 2018 (excluding coniferous forest to transitional woodland shrub) ----
#Remove rows 16 and 76
forestless_2012_2018_sankey <- corine_2012_2018_sankey |>
  filter(!row_number() %in% c(4, 26))

#Colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

#Create node dataframe
nodes_forestless2012_2018 <- data.frame(name = c(as.character(forestless_2012_2018_sankey$source),
                                                 as.character(forestless_2012_2018_sankey$target)) |>
                                          unique())

#Reformat for ID
forestless_2012_2018_sankey$IDsource = match(forestless_2012_2018_sankey$source,
                                             nodes_forestless2012_2018$name) - 1

forestless_2012_2018_sankey$IDtarget = match(forestless_2012_2018_sankey$target,
                                             nodes_forestless2012_2018$name) - 1

#Make Network
sankeyNetwork(Links = forestless_2012_2018_sankey, Nodes = nodes_forestless2012_2018,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=20, fontSize=11, nodePadding=25)

## 4.5. Barplots of land cover transitions between 2012 and 2018 ----
### 4.5.1. Barplots of land cover transitions between 2012 and 2018 (all classes) ----
#Create dataframe of "transition to" values
 #this dataframe will have the "focus" cover class in the "source" column and the cover class it transitions to in the "target" column
 #the values will be negative because for every source class, this is the area "lost" that is converted to the "target" cover class
loss_2012_2018 <- corine_2012_2018_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select (focus, transition, count)

#Create dataframe of "transitions from" values
 #this dataframe will have "focus" cover class in the "target" column and the cover class it transitions from in the "target" column
 #the values will be positive because for every source class, this is the area "gained" that is converted from the "source" cover clas
gain_2012_2018 <- corine_2012_2018_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count)


#Merge the gain and loss dataframes into a single df
gain_loss_2012_2018 <- rbind(loss_2012_2018, gain_2012_2018)

#Plot gain_loss_2000_2006
gain_loss_plot_2012_2018 <- gain_loss_2012_2018 |>
  ggplot(aes(fill = transition,
             y = count,
             x = focus))+
  geom_bar(position = "stack",
           stat = "identity")+
  labs(fill = "Land Chttp://127.0.0.1:18341/graphics/plot_zoom_png?width=1536&height=814over Classes",
       x = "Land cover classes",
       y = bquote("Area changes"~("km"^2)),
       title = "Land Cover Transitions 2012 - 2018")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                                            "#6A3D9A", "#FF7F00", 
                                            "gold1", "maroon"))+
                                              theme_classic()
#Change x axis tick marks
gain_loss_plot_2012_2018 + theme(axis.text.x = element_text(angle = 30,
                                                            hjust = 1))

### 4.5.2. Barplots of land cover transitions between 2012 and 2018 (without coniferous and transitional woodland shrub) ----
#Creat df that does not have the coniferous -> transitional woodlans shrub (and vice versa) columns
gain_loss_2012_2018_forestless <- gain_loss_2012_2018 |>
  filter(!row_number() %in% c(4, 26, 38, 60))

#Plot gain_loss_2012_2018
gain_loss_plot_forestless_2012_2018 <- gain_loss_2012_2018_forestless |>
  ggplot(aes(fill = transition,
             y = count,
             x = focus))+
  geom_bar(position = "stack",
           stat = "identity")+
  labs(fill = "Land Cover Classes",
       x = "Land cover classes",
       y = bquote("Area changes"~("km"^2)),
       title = "Land Cover Transitions 2012 - 2018")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                                            "#6A3D9A", "#FF7F00", 
                                            "gold1", "maroon"))+
                                              theme_classic()

gain_loss_plot_forestless_2012_2018 + theme(axis.text.x = element_text(angle = 30,
                                                                       hjust = 1))


# 5. LAND COVER TRANSITIONS BETWEEN 2000 AND 2018 ----
## 5.1. Calculate change between 2000 and 2018 ----
corine_2000_2018_df <- as.data.frame(freq(norway_corine[[1]] - 
                                            norway_corine[[4]])) |>
  mutate(source_year = "2000",
         target_year = "2018",
         difference = value) |>
  select(-layer)

## 5.2. Get values for source and target land cover in the change layer from the score meaning data frame ----
#Subset score meaning df to only contain the "differences" which are found across Norway
norway_corine_class_meaning_2000_2018 <- corine_class_meaning |>
  filter(difference %in% corine_2000_2018_df$value)

colnames(norway_corine_class_meaning_2000_2018) <- c("source_number", "source_name",
                                                     "target_number", "target_name",
                                                     "value")


#Merge norway_corine_class_meaning_2012_2018 df and corine_2006_2012_df into one
corine_2000_2018_change_meaning <- merge(corine_2000_2018_df,
                                         norway_corine_class_meaning_2000_2018,
                                         by = "value")

## 5.3. Prepare df for sankey plot ----
#Merge columns "source_year" with "source_name" and "target_year" with "target_name" so that we can differentiate the transitions
corine_2000_2018_sankey <- corine_2000_2018_change_meaning |>
  unite(source, c(source_year, source_name), sep = ".",
        remove = FALSE) |>
  unite(target, c(target_year, target_name), sep = ".",
        remove = FALSE)

#Remove the rows that show "no change" (i.e. value = 0)
corine_2000_2018_sankey <- corine_2000_2018_sankey |>
  filter(value != 0)

#Remove columns:value, layer, source_year, target_year, source_number, source_name, target_number, target_name
corine_2000_2018_sankey <- corine_2000_2018_sankey |>
  select(count, source, target)

#Re-arrange columns in the order: source, target, count
corine_2000_2018_sankey <- corine_2000_2018_sankey |>
  relocate(source, target, count)

#Change values in target so that they are different from the source one (add a space at the end)
corine_2000_2018_sankey <- corine_2000_2018_sankey |>
  mutate(target = paste(target, " ", sep = ""))

## 5.4. Sankey Plot for transitions between 2000 and 2018 ----
### 5.4.1. Sankey Plot for transitions between 2006 and 2012 (all classes included) ----
#Create node dataframe
nodes2000_2018 <- data.frame(name = c(as.character(corine_2000_2018_sankey$source),
                                      as.character(corine_2000_2018_sankey$target)) |>
                               unique())

#Create ID to provide connection for networkD3
corine_2000_2018_sankey$IDsource=match(corine_2000_2018_sankey$source, nodes2000_2018$name)-1 

corine_2000_2018_sankey$IDtarget=match(corine_2000_2018_sankey$target, nodes2000_2018$name)-1

#Prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

#Make the Network
sankeyNetwork(Links = corine_2000_2018_sankey, Nodes = nodes2000_2018,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=40, fontSize=11, nodePadding=20)

### 5.4.2. Sankey Plot for transitions between 2006 and 2012 (excluding coniferous forest to transitional woodland shrub) ----
#Remove rows 27 and 99
forestless_2000_2018_sankey <- corine_2000_2018_sankey |>
  filter(!row_number() %in% c(6, 29))

#Colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

#Create node dataframe
nodes_forestless2000_2018 <- data.frame(name = c(as.character(forestless_2000_2018_sankey$source),
                                                 as.character(forestless_2000_2018_sankey$target)) |>
                                          unique())

#Reformat for ID
forestless_2000_2018_sankey$IDsource = match(forestless_2000_2018_sankey$source,
                                             nodes_forestless2000_2018$name) - 1

forestless_2000_2018_sankey$IDtarget = match(forestless_2000_2018_sankey$target,
                                             nodes_forestless2000_2018$name) - 1

#Make Network
sankeyNetwork(Links = forestless_2000_2018_sankey, Nodes = nodes_forestless2000_2018,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=20, fontSize=11, nodePadding=25)

## 5.5. Barplots of land cover transitions between 2000 and 2018 ----
### 5.5.1. Barplots of land cover transitions between 2012 and 2018 (all classes) ----
#Create dataframe of "transition to" values
#this dataframe will have the "focus" cover class in the "source" column and the cover class it transitions to in the "target" column
#the values will be negative because for every source class, this is the area "lost" that is converted to the "target" cover class
loss_2000_2018 <- corine_2000_2018_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select (focus, transition, count)

#Create dataframe of "transitions from" values
#this dataframe will have "focus" cover class in the "target" column and the cover class it transitions from in the "target" column
#the values will be positive because for every source class, this is the area "gained" that is converted from the "source" cover clas
gain_2000_2018 <- corine_2000_2018_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count)


#Merge the gain and loss dataframes into a single df
gain_loss_2000_2018 <- rbind(loss_2000_2018, gain_2000_2018)

#Plot gain_loss_2000_2006
gain_loss_plot_2000_2018 <- gain_loss_2000_2018 |>
  ggplot(aes(fill = transition,
             y = count,
             x = focus))+
  geom_bar(position = "stack",
           stat = "identity")+
  labs(fill = "Land Cover Classes",
       x = "Land cover classes",
       y = bquote("Area changes"~("km"^2)),
       title = "Land Cover Transitions 2000 - 2018")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                                            "#6A3D9A", "#FF7F00", 
                                            "gold1", "maroon"))+
                                              theme_classic()
#Change x axis tick marks
gain_loss_plot_2000_2018 + theme(axis.text.x = element_text(angle = 30,
                                                            hjust = 1))

### 5.5.2. Barplots of land cover transitions between 2000 and 2018 (without coniferous and transitional woodland shrub) ----
#Remove the rows with transition coniferous forest - transitional woodland shrub and vice versa
gain_loss_2000_2018_forestless <- gain_loss_2000_2018 |>
  filter(!row_number() %in% c(6, 29, 43, 66))

#Plot gain_loss_2000_2018
gain_loss_plot_forestless_2000_2018 <- gain_loss_2000_2018_forestless |>
  ggplot(aes(fill = transition,
             y = count,
             x = focus))+
  geom_bar(position = "stack",
           stat = "identity")+
  labs(fill = "Land Cover Classes",
       x = "Land cover classes",
       y = bquote("Area changes"~("km"^2)),
       title = "Land Cover Transitions 2000 - 2018")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                                            "#6A3D9A", "#FF7F00", 
                                            "gold1", "maroon"))+
                                              theme_classic()
#Change x axis tick marks
gain_loss_plot_forestless_2000_2018 + theme(axis.text.x = element_text(angle = 30,
                                                                       hjust = 1))

# END OF SCRIPT ----