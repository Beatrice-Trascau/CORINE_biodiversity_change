# 1. WDPAR Interface to he World Database on Protected Areas Tutorial ----
#https://cran.r-hub.io/web/packages/wdpar/vignettes/wdpar.html
#Adapted to Norway

#Load libraries
library(wdpar)
library(dplyr)
library(ggmap)

nor_raw_pa_data <- wdpa_fetch("Norway", wait = TRUE)

#Clean Norway data
nor_pa_data <- wdpa_clean(nor_raw_pa_data)

#Download Norway boundary From Global Administrative Areas dataset
norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")

nor_boundary_data <- st_as_sf(norway)


#Repair any geometry issues, dissolve the border, reproject to same
#coordinate system as the protected area data, and repair the geometry again
nor_boundary_data <- 
  nor_boundary_data %>%
  st_set_precision(1000) %>%
  sf::st_make_valid() %>%
  st_set_precision(1000) %>%
  st_combine() %>%
  st_union() %>%
  st_set_precision(1000) %>%
  sf::st_make_valid() %>%
  st_transform(st_crs(nor_pa_data)) %>%
  sf::st_make_valid()

#Subset only terrestrial PAs
nor_pa_terrestrial <- nor_pa_data %>%
  filter(MARINE == "terrestrial")


#Clip Norway's protected areas to the coastline
nor_pa_terrestrial <-
  nor_pa_terrestrial %>%
  filter(MARINE == "terrestrial") %>%
  st_intersection(nor_boundary_data) %>%
  rbind(nor_pa_terrestrial %>%
          filter(MARINE == "terrestrial") %>%
          st_difference(nor_boundary_data)) %>%
  rbind(nor_pa_terrestrial %>% filter(!MARINE == "terrestrial"))

nor_pa_data <-
  nor_pa_data %>%
  filter(MARINE == "terrestrial") %>%
  st_intersection(nor_boundary_data) %>%
  rbind(nor_pa_data %>%
          filter(MARINE == "terrestrial") %>%
          st_difference(nor_boundary_data)) %>%
  rbind(nor_pa_data %>% filter(!MARINE %in% c("terrestrial", "marine")))

#Recalculate the area of each protected area
nor_pa_terrestrial <-
  nor_pa_terrestrial %>%
  mutate(AREA_KM2 == as.numeric(st_area(.)) * 1e-6)


nor_pa_data <-
  nor_pa_data %>%
  mutate(AREA_KM2 == as.numeric(st_area(.)) * 1e-6)

#print first six rows of the data
head(nor_pa_data)

#Reproject data to longitude/latitude coordinates for visualization purposes
nor_pa_data <- st_transform(nor_pa_data, 4326)

nor_pa_terrestrial <- st_transform(nor_pa_terrestrial, 4326)
#Plot a map shoring the boundaries of Norway's protected area system
#Download basemap for making the map
bg <- get_stamenmap(
  unname(st_bbox(nor_pa_data)), zoom = 8,
  maptype = "watercolor", force = TRUE
)

tg <- get_stamenmap(
  unname(st_bbox(nor_pa_terrestrial)), zoom = 8,
  maptype = "watercolor", force = TRUE
)

#Print map
ggmap(bg)+
  geom_sf(data = nor_pa_data, fill = "#31A35480", inherit.aes = FALSE)+
  theme(axis.title = element_blank())

ggmap(tg)+
  geom_sf(data = nor_pa_terrestrial, fill = "#31A35480", inherit.aes = FALSE)+
  theme(axis.title = element_blank())

#Create a histogram showing the year when each PA was established
hist(nor_pa_data$STATUS_YR,
     main = "Norway's protected areas",
     xlab = "Year established")

#Calculate total amount of area inside protected areas (km^2)
statistic <- 
  nor_pa_data %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(MARINE) %>%
  summarize(area_km = sum(AREA_KM2)) %>%
  ungroup() %>%
  arrange(desc(area_km))
#print statistic
print(statistic)


# calculate percentage of land inside protected areas (km^2)
statistic <-
  nor_pa_data %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(IUCN_CAT) %>%
  summarize(area_km = sum(AREA_KM2)) %>%
  ungroup() %>%
  mutate(percentage = (area_km / sum(area_km)) * 100) %>%
  arrange(desc(area_km))

# print statistic
print(statistic)

#plot a map showing Norway's protected areas and color each area according to it???s management category
ggmap(bg) +
  geom_sf(aes(fill = IUCN_CAT), data = nor_pa_data, inherit.aes = FALSE) +
  theme(axis.title = element_blank(), legend.position = "bottom")



#Download Norway shapefile from Natural Earth database
norway_natearth <- ne_countries(
  scale = "medium",
  country = "Norway",
  returnclass = "sf") %>% 
  dplyr::select(sovereignt)

ggplot() + 
  geom_sf(data = norway_natearth)



#2 Try 1: Use extract ----
#Read in Pa data as vect
nor_pa_vect <- vect(nor_pa_terrestrial)

#Check extent and projection
crs(nor_pa_vect, proj = TRUE)
ext(nor_pa_vect)


#Download Norway shapefile
norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")

#Cut and mask PA to Norway
norway_PA_cropped <- crop(nor_pa_vect, norway)
plot(norway_PA_cropped)

#Add buffer - I have no idea why I did this
nor_pa_vect <- gBuffer(nor_pa_vect, byid=TRUE, width=0)

#Read in land cover data
corine_norway <- rast(here::here("data", "corine_modified_classes_stack.tif"))

#Reproject norway_PA to have the same projection as the corine layers
norway_PA_for_corine <- terra::project(norway_PA,
                                       "epsg:3035") 
#Resample 
norway_PA_resampled2 <- resample(PA_unedited,
                                 corine_norway[[1]],
                                 method = "near") #abandoning this

#Reproject PA network to the same projection as the corine layers. which is 3035
nor_pa_terrestrial <- st_transform(nor_pa_terrestrial, 3035)

#Read in corine layers
corine_norway <- rast("corine_modified_classes_stack.tif")

#Match the extent of PA network data 
ext(nor_pa_terrestrial) <- ext(corine_norway[[1]])

#Use Extract
try1 <- extract(corine_norway[[4]],
                nor_pa_terrestrial,
                cells = FALSE,
                method = "simple")
#The output gives the points where the corine raster overlaps with a PA polygon
#but this is not helpful because it does not tell me the value of the corine cell or the values of the cells
#that do not overlap with polygons

#3.Try with st_intersection ----
#Rasterize PA network vector
try2 <- terra::rasterize(nor_pa_vect,
                         corine_norway[[4]],
                         "WDPAID")

#Convert PA network vector to raster -  why???
nor_pa_raster <- rast(nor_pa_terrestrial)

#Resample PA network vector to corine
nor_pa_raster_resampled <- resample(nor_pa_raster,
                                    corine_norway[[4]])   

#Add the PA network raster to the corine raster
add(corine_norway) <- nor_pa_raster_resampled 

#Drop geometry - ????
nor_pa_terrestrial_df <- st_drop_geometry(nor_pa_terrestrial)

#Convert dataframe from above to vector - ????
nor_pa_terrestrial_vect <- vect(nor_pa_terrestrial_df,
                                geom = c("Longitude", "Latitude"),
                                crs = crs(corine_norway[[4]]))

#Try to use extract again??  
del <- terra::extract(corine_norway[[4]],
                      nor_pa_terrestrial_vect,
                      xy = T,
                      method = "simple",
                      bind = T)  


#4. Try to use intersect
#Libraries
library(terra)
library(sf)

#Read in the PA network data with terra
PA <- vect("WDPA_May2023_Public_shp-polygons.shp",
           crs = "+proj=longlat")

#Check extent and projection
crs(norway_PA, proj = TRUE)
extent(norway_PA)


#Download Norway shapefile
norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")

#Cut and mask PA to Norway
norway_PA <- crop(PA, norway)

plot(norway_PA)

#Read in land cover data
corine_norway <- rast("norway_corine_stack.tif")

#Reproject norway_PA to have the same projection as the corine layers
norway_PA_for_corine <- terra::project(norway_PA,
                                       "epsg:3035")

#Transform norway_PA_for_corine to SpatRaster
norway_Pa_for_corine_raster <- rast(norway_PA_for_corine)



#Combine land cover and PA network data
land_cover_PA <- c(corine_norway[[1]],
                   norway_Pa_for_corine_raster) #this didnt work because they dont have the same extents

#Change extent of PA network to match the extent of the corine layers
ext(norway_Pa_for_corine_raster) <- ext(corine_norway[[1]]) #this ran but now I get this error: Error: [rast] number of rows and/or columns do not match

#Online resources say that the cell sizes need to match, which they probably don't tbh
#Re-sample one raster with the other to make sure that they have the same cell size and are matched
resampled_norway_PA <- resample(norway_Pa_for_corine_raster,
                                corine_norway[[1]])

#Try again to combine the layers
land_cover_PA <- c(corine_norway[[1]],
                   resampled_norway_PA) #now it ran, but it didn't do much other than just stacking the layers

#Maybe the way forward is to intersect them and then see where they overlap? 
land_cover_PA <- intersect(resampled_norway_PA, 
                           corine_norway[[1]]) #this ran but found no intersection, which there should be


#Check resolution of PA data before and after resampling to CORINE layers 
res(resampled_norway_PA) #100 x 100
res(corine_norway[[1]]) #100 x 100
res(norway_Pa_for_corine_raster) #108430.6 154042.9

#Check size of PA data before and after resampling to CORINE layers 
size(resampled_norway_PA) #nlyr(x) x ncell(x), 177007364
size(corine_norway[[1]]) #177007364
size(norway_Pa_for_corine_raster) #100

#Check nrow and ncols
ncol(resampled_norway_PA) #11491
nrow(resampled_norway_PA) #15404

ncol(norway_Pa_for_corine_raster) #10
nrow(norway_Pa_for_corine_raster) #10

#Try to resample PA network again, but specify which method
##read in a raster of the PA that does not have changed extent and CRS
PA_unedited <- rast(norway_PA)
##resample
norway_PA_resampled2 <- resample(PA_unedited,
                                 corine_norway[[1]],
                                 method = "near")
##check details
size(norway_PA_resampled2) #177007364
res(norway_PA_resampled2) #100 x 100
crs(norway_PA_resampled2, proj = TRUE) #+proj=longlat +datum=WGS84 +no_defs - not the same as corine
ext(norway_PA_resampled2) #SpatExtent : 3982300, 5131400, 3875500, 5415900 (xmin, xmax, ymin, ymax)

##reproject PA data to have the same CRS as the corine data
crs(norway_PA_resampled2) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

#Try intersecting them again
land_cover_PA_try2 <- intersect(norway_PA_resampled2, 
                                corine_norway[[1]])
plot(land_cover_PA_try2) #this does not show anything

#Check values 
values(land_cover_PA_try2)
levels(as.factor(values(land_cover_PA_try2))) #character(0), Warning message: [readValues] raster has no values

#Try to use the "wdpar" package to download the PA data
library(wdpar)

cod_raw <- wdpa_fetch("Norway",
                      wait = TRUE) #THIS WORKED!!!

#Try using terra's extract() to extract values from the spatraster for the locations in the SpatVect
wdpa_overlap <- terra::extract(
  x = corine_norway[[1]],
  y = norway_PA_for_corine)

#try to extract coordinates from the sf collection nor_pa_terrestrial
a <- st_coordinates(nor_pa_terrestrial)

corine_norway[[5]]  

try3 <- geom(nor_pa_terrestrial_vect)



#Reproject nor_pa_terrestrial to corine_norway
nor_pa_terrestrial_vect <- vect(nor_pa_terrestrial,
                                crs = "+proj=longlat")

nor_pa_terrestrial_vect <- project(nor_pa_terrestrial_vect,
                                   "epsg:3035")


#5. Try to rasterize the PA network, extract values and combine in df
#This is the one approach that actually yielded results
#Libraries
library(terra)
library(sf)

#Land cover data
corine_norway <- rast(here::here("data", "corine_modified_classes_stack.tif"))

#Protected area shapefile
#read
PA <- vect(here::here("data", "raw_data",
                      "WDPA_May2023_Public_shp-polygons.shp"),
           crs = "+proj=longlat")
#norway shapefile
norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")
#cut to norway
norway_PA <- crop(PA, norway)


#project norway_PA to same projection as corine_norway
norway_PA <- terra::project(norway_PA,
                            "epsg:3035")

#Raster for theprotected areas
pa_raster <- terra::rasterize(norway_PA,
                              corine_norway[[4]])

#Replace NA with a specific number
corine_norway[[4]][is.na(corine_norway[[4]])] <- -9999
pa_raster[is.na(pa_raster)] <- -9999


#Extract values from both  rasters
land_cover_values <- values(corine_norway[[4]])

norway_pa_values <- values(pa_raster)

#Combine the two rasters in a dataframe
data <- data.frame(x = xFromCol(corine_norway[[4]],
                                1:ncell(corine_norway[[4]])),
                   y = yFromCell(corine_norway[[4]],
                                 1:ncell(corine_norway[[4]])),
                   land_cover_value = land_cover_values,
                   protected_area = norway_pa_values)

# Replace -9999 with NA
data[data == -9999] <- NA

# Add a column to the data frame to indicate whetherr the cell overlaps with a protected area

data <- data |>
  mutate(in_protected_area = ifelse(is.na(layer),
                                    "N", "Y"))
