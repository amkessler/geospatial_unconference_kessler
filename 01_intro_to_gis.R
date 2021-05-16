library(tidyverse)
library(tigris)
library(tmap)
library(tmaptools)
library(albersusa)
library(rmapshaper)
library(htmltools)
library(janitor)
options(tigris_class = "sf")


# ggplot(va_counties_cb) +
#   geom_sf()


#what are cartographic boundaries (CB)? ####

#look at the difference between this...
va_counties_noncb <- counties(state = "VA")

tm_shape(va_counties_noncb) + 
  tm_polygons()


# ...and this:
va_counties_cb <- counties(state = "VA", cb = TRUE)

tm_shape(va_counties_cb) + 
  tm_polygons()







### National Maps of the US ####

#several options for resolution - when using whole nation, 20m is usually better
#for individual states 5m may be preferable
#we'll use the tigris package to directly pull down the geodata into R
usstates_geo <- tigris::states(resolution = "20m", cb = TRUE)


ggplot(usstates_geo) +
  geom_sf()


tm_shape(usstates_geo) + 
  tm_polygons()




## Dealing with Alaska, Hawaii and Puerto Rico ####

#new feature in latest version of tigris package
#to help with repositioning AK, HI and PR

#can apply to existing sf geospatial objects
test <- shift_geometry(usstates_geo, preserve_area = FALSE, position = "below")

tm_shape(test) + 
  tm_polygons()


#or you can apply this from the get-go when pulling down geospatial data using tigris
#can pipe it in on the fly like this:
usstates_geo_shifted <- states(resolution = "20m", cb = TRUE) %>% 
                        shift_geometry(preserve_area = FALSE, position = "below")


tm_shape(usstates_geo_shifted) + 
  tm_polygons()


#the albersusa also provides functions for dealing with albers projections
#as well as splitting off AK and HI

## MORE HERE






### DEMO CASE - CONGRESSIONAL DISTRICT DEMOGRAHPICS AND VOTING ####

#load dataset of district characteristics
alldistricts <- readRDS("data/alldistricts.rds")

#congressional districts - we'll use a shapefile here this time to mix things up
#after using the tigris package earlier
cd_geo <- st_read("data/cb_2018_us_cd116_20m/cb_2018_us_cd116_20m.shp")

#join our dataset to geography 
#note that we can use dplyr's inner join here
districtmap <- inner_join(cd_geo, alldistricts, by = c("GEOID" = "geoid"))

glimpse(districtmap)

#remove AK and HI for expediency here
districtmap <- districtmap %>% 
  filter(state_name != "Alaska",
         state_name != "Hawaii")


#Use TMAP to map it out

tm_shape(districtmap) +
  tm_polygons()

tm_shape(districtmap) +
  tm_polygons(id = "house_dist")


tm_shape(districtmap) +
  tm_polygons("prez_winner_2016", id = "house_dist")

tmap_mode(mode = "view")
tmap_mode(mode = "plot")


#you can also filter our geospatial dataset to create subsets
rheld_allkey <- districtmap %>% 
  filter(incumbent_2018 == "R",
         race_rating_2018 %in% c("likely democratic", "lean democratic", "tossup",
                               "likely republican", "lean republican"))

rheld_demtossorbetter <- districtmap %>% 
  filter(incumbent_2018 == "R",
         race_rating_2018 %in% c("likely democratic", "lean democratic", "tossup"))

rheld_favorgop <- districtmap %>% 
  filter(incumbent_2018 == "R",
         race_rating_2018 %in% c("likely republican", "lean republican"))


#and then once again use tmap to display that
tm_shape(rheld_demtossorbetter) +
  tm_polygons(id = "house_dist")

#Of course what happens here - we don't have the rest of the CD map shown.
#So we can simply layer the base CD map underneath the filtered districts
tm_shape(districtmap) +
  tm_polygons(id = "house_dist") +
  #now we are simply add another shape/polygon combo on top of it
  tm_shape(rheld_demtossorbetter) + 
  tm_polygons(col = "red", id = "house_dist") 

#we can also symbolize the filtered districts by a variable
tm_shape(districtmap) +
  tm_polygons(id = "house_dist") +
  tm_shape(rheld_demtossorbetter) +
  tm_polygons("pct_ed_college_all_abovebelow_natl", id = "house_dist")

#once we have what we want can save it as its own object
map_rheld_demtossorbetter_byeducation <- tm_shape(districtmap) +
  tm_polygons() +
  tm_shape(rheld_demtossorbetter) +
  tm_polygons("pct_ed_college_all_abovebelow_natl", id = "house_dist")



map_rheld_demtossorbetter_byeducation

#now we can export it
tmap_save(map_rheld_demtossorbetter_byeducation, "map_rheld_demtossorbetter_byeducation.pdf")

#and we can also save it as an RDS file - the entire map becomes the saved object
saveRDS(map_rheld_demtossorbetter_byeducation, "data/map_rheld_demtossorbetter_byeducation.rds")

#why might we want to do this?
#one use case: if displaying in a rmarkdown document / website
map_rheld_demtossorbetter_byeducation
