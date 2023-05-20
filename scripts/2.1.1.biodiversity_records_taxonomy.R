##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Chapter 1: Land-use change induced     ---##
##- shifts in Norwegian biodiversity assemblages -##
##-------        between 2000 and 2018     -------##
##----- 2.1.1_biodiversity_records_taxonomy ------##
##------------------------------------------------##

#This script contains code to explore the taxonomic distribution of species occurrence records in the GBIF download

# 0. LOAD LIBRARIES ----
library(here)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

# 1. LOAD SPECIES OCCURRENCE RECORDS ----
occurrences_corine <- fread(here::here("data", "cleaned_occurrences.txt"))

# 2. SUMMARY STATISTICS ----
## 2.1 Most and least sampled family throughout the years ----

#Which family has been sampled the most throughout the years?
names(which.max(table(occurrences_corine$family))) #Anatidae

#Which family has been sampled the least throughout the years?
names(which.min(table(occurrences_corine$family))) #Aegisthidae (family of copepods)
#The results are clearly influenced by the presence of marine and aquatic species

#Top 10 most sampled families for broader look
sort(table(occurrences_corine$family), decreasing = TRUE)[1:11]
#Most sampled families are: Anatidae  , Fringillidae, Laridae, Corvidae, Paridae, Scolopacidae,
#Turdidae, Motacillidae, Muscicapidae, Accipitridae, Passeridae 

#Top 10 least sampled families: 
sort(table(occurrences_corine$family), decreasing = FALSE)[1603:1613]
#Pycnoraceae, Ptychomitriaceae, Tetratomidae, Palaemonidae, Anthicidae, Strongylocentrotidae,
#Vertiginidae, Edwardsiidae, Eulophidae, Pharidae, Priapulidae 

## 2.2. Most and least sampled class throughout the years ----
#Which class has been sampled the most throughout the years?
names(which.max(table(occurrences_corine$class))) #Aves
#what are the top 10 most sampled classes?
sort(table(occurrences_corine$class), decreasing = TRUE)[1:11]

#Which class has been sampled the least throughout the years?
names(which.min(table(occurrences_corine$class))) #Craniata 
#what are the bottom 10 least sampled classes
sort(table(occurrences_corine$class), decreasing = FALSE)[1:200]


# 3. OCCURRENCE RECORDS (class) THROUGH TIME ----
## 3.1. Convert data to dataframe which can be used for plot
records_time <- table(occurrences_corine$year, occurrences_corine$kingdom)

#Check if records_time is a matrix
is.matrix(records_time) #output is true => need to use function as.data.frame.matrix

#Convert to dataframe
records_time <- as.data.frame.matrix(records_time) |>
  mutate(records_time = row.names(records_time))

#Convert df to long format
records_time_long <- records_time |>
  pivot_longer(cols = c("Animalia", "Plantae", "Fungi"),
               names_to = "kingdom",
               values_to = "records")
  
## 3.2. Spaghetti plot for all 3 kingdoms
ggplot(records_time_long, aes(x=records_time, y=records, group=kingdom, color=kingdom))+
  geom_line()+
  xlab("Year")+
  ylab("Number of Samples")+
  #scale_x_discrete(breaks = seq(1997, 2018, by = 2)) +
  scale_color_manual(values = c("#063970", "#1e81b0", "#eab676"))+
  theme_classic()

#save the plot
ggsave(here("figures",
            "records_across_kingdoms_1997_2018.svg"))


# 4. OCCURRENCE RECORDS (CLASS) THROUGH TIME ----
## 4.1. Create df for plotting ----

#Create df of records grouped by class and kingdom
occurrences_class <- occurrences_corine |>
  group_by(class, kingdom) |>
  count(year) 

#Remove cells with no value for class
occurrences_class <- occurrences_class[!occurrences_class$class=="",]

#Check types of data in df
str(occurrences_class) #class & kingdom = chr; year & n = int

#Change data type of class, kingdom and year to factor
occurrences_class <- occurrences_class |>
  mutate(class = as.factor(class),
         kingdom = as.factor(kingdom),
         year = as.factor(year))

#Remove aquatic classes
terrestrial_occurrences <- occurrences_class |>
  filter(!class %in% c("Actinopterygii", "Anthozoa", "Ascidiacea", "Asteroidea",
                      "Bivalvia","Branchiopoda", "Caudofoveata", "Cephalaspidomorphi", "Cephalopoda",
                      "Craniata", "Crinoidea", "Demospongiae", "Echinoidea", "Elasmobranchii", "Enoplea",
                      "Eurotatoria", "Gordioida", "Gymnolaemata", "Hexanauplia", "Holocephali", 
                      "Holothuroidea", "Hydrozoa", "Malacostraca", "Maxillopoda", "Monogenea", 
                      "Myxini", "Ophiuroidea", "Ostracoda", "Palaeacanthocephala", "Phylactolaemata", 
                      "Polychaeta", "Polyplacophora", "Pterobranchia", "Rhynchonellata", "Scaphopoda", 
                      "Scyphozoa", "Sipunculidea", "Solenogastres", "Tentaculata"))



## 4.2. Plot classes by kingdom ----
y <- ggplot(terrestrial_occurrences, aes(x = kingdom, y = n, fill = class))+
  geom_bar(position = "stack", stat = "identity")+
  xlab("Kingdom")+
  ylab("Occurrences")+
  theme_classic()

y1 <- y + scale_y_continuous(labels = scales::comma)  
 
y1 + theme(legend.position = "none")

## 4.3. Fungi classes through time ----
#Subset df to only contain fungal classes
fungi_occurreces <- occurrences_class |>
  filter(kingdom == "Fungi")

#Plot fungal classes through time
f <- ggplot(fungi_occurreces, aes(x = year, y = n, fill = class))+
  geom_bar(position = "stack", stat = "identity")+
  xlab("Year")+
  ylab("Occurrences")+
  theme_classic()

f + theme(legend.position = "none")

## 4.4. Plant classes through time ----
#Subset df to only contain plant classes
plant_occurreces <- occurrences_class |>
  filter(kingdom == "Plantae")

#Plot fungal classes through time
p <- ggplot(plant_occurreces, aes(x = year, y = n, fill = class))+
  geom_bar(position = "stack", stat = "identity")+
  xlab("Year")+
  ylab("Occurrences")+
  scale_y_continuous(labels = scales::comma)+
  theme_classic()

p + theme(legend.position = "none")


## 4.5. Animal classes through time ----
#Subset df to only contain animal classes
animal_occurreces <- occurrences_class |>
  filter(kingdom == "Animalia")

#Plot fungal classes through time
a <- ggplot(animal_occurreces, aes(x = year, y = n, fill = class))+
  geom_bar(position = "stack", stat = "identity")+
  xlab("Year")+
  ylab("Occurrences")+
  scale_y_continuous(labels = scales::comma)+
  theme_classic()

a + theme(legend.position = "none")

### 4.5.1. Remove the major classes (Aves, Insecta, Mammalia) to see smaller patterns ----
#Subset df to exclude major classes
other_animal_occurreces <- animal_occurreces |>
  filter(!class %in% c("Aves", "Insecta", "Mammalia"))

#Plot fungal classes through time
a2 <- ggplot(other_animal_occurreces, aes(x = year, y = n, fill = class))+
  geom_bar(position = "stack", stat = "identity")+
  xlab("Year")+
  ylab("Occurrences")+
  scale_y_continuous(labels = scales::comma)+
  theme_classic()

a2 + theme(legend.position = "none")
