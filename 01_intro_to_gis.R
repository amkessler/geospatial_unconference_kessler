library(tidyverse)
library(tigris)
library(tmap)
library(tmaptools)
library(albersusa)
library(rmapshaper)
library(maps)
library(geosphere)
library(htmltools)
library(janitor)
options(tigris_class = "sf")


# ggplot(va_counties_cb) +
#   geom_sf()


# what are cartographic boundaries (CB)? ####

# look at the difference between this...
va_counties_noncb <- counties(state = "VA")

tm_shape(va_counties_noncb) + 
  tm_polygons()


# ...and this:
va_counties_cb <- counties(state = "VA", cb = TRUE)

tm_shape(va_counties_cb) + 
  tm_polygons()







### National Maps of the US ####

# several options for resolution - when using whole nation, 20m is usually better
# for individual states 5m may be preferable
# we'll use the tigris package to directly pull down the geodata into R
usstates_geo <- tigris::states(resolution = "20m", cb = TRUE)

ggplot(usstates_geo) +
  geom_sf()

tm_shape(usstates_geo) + 
  tm_polygons()









### DEMO CASE - CONGRESSIONAL DISTRICT DEMOGRAHPICS AND VOTING ####

# load dataset of district characteristics
alldistricts <- readRDS("data/alldistricts.rds")

# congressional districts - we'll use a shapefile here this time to mix things up
# after using the tigris package earlier
cd_geo <- st_read("data/cb_2018_us_cd116_20m/cb_2018_us_cd116_20m.shp")

# join our dataset to geography 
# note that we can use dplyr's inner join here
districtmap <- inner_join(cd_geo, alldistricts, by = c("GEOID" = "geoid"))

glimpse(districtmap)

# remove AK and HI for expediency here
districtmap <- districtmap %>% 
  filter(state_name != "Alaska",
         state_name != "Hawaii")


# Use TMAP to map it out

tm_shape(districtmap) +
  tm_polygons()

tm_shape(districtmap) +
  tm_polygons(id = "house_dist")


tm_shape(districtmap) +
  tm_polygons("prez_winner_2016", id = "house_dist")

# tmap_mode(mode = "view")
# tmap_mode(mode = "plot")


# you can also filter our geospatial dataset to create subsets
# let's look at just GOP-held seats where the race was favoring the Dems
rheld_demadvantage <- districtmap %>% 
  filter(incumbent_2018 == "R",
         race_rating_2018 %in% c("likely democratic", "lean democratic", "tossup"))

# and then once again use tmap to display that
tm_shape(rheld_demadvantage) +
  tm_polygons(id = "house_dist")

# Of course what happens here - we don't have the rest of the CD map shown.
# So we can simply layer the base CD map underneath the filtered districts
tm_shape(districtmap) +
  tm_polygons(id = "house_dist") +
  #now we are simply add another shape/polygon combo on top of it
  tm_shape(rheld_demadvantage) + 
  tm_polygons(col = "red", id = "house_dist") 

# we can also symbolize the filtered districts by a variable
tm_shape(districtmap) +
  tm_polygons(id = "house_dist") +
  tm_shape(rheld_demadvantage) +
  tm_polygons("pct_ed_college_all_abovebelow_natl", id = "house_dist")


# once we have something we want can save it as its own object
# here we'll add a title for the map, as well as a title for the legend 
map_rheld_demadvantage_byeducation <- tm_shape(districtmap) +
  tm_polygons() +
  tm_shape(rheld_demadvantage) +
  tm_polygons(col = "red", id = "house_dist") +
  tm_layout(main.title = "GOP-Held Seats in 2018 Where Democrats Were Most Competitive",
            main.title.position = "center",
            main.title.color = "darkred",
            main.title.size = 1.2) 

map_rheld_demadvantage_byeducation

# now we can export it
tmap_save(map_rheld_demadvantage_byeducation, "map_rheld_demadvantage_byeducation.pdf")


# and we can also save it as an RDS file - the entire map becomes the saved object
saveRDS(map_rheld_demadvantage_byeducation, "data/map_rheld_demadvantage_byeducation.rds")

# why might we want to do this?
# one use case: if displaying in a rmarkdown document / website
map1 <- readRDS("data/map_rheld_demadvantage_byeducation.rds")

map1




### ROUTE TRACING ##### -----------------------------------------------------

# Will show how to achieve paths-to-a-destination similar to this project:
# https://www.bloomberg.com/graphics/2021-citylab-how-americans-moved/

# Here's we'll use the maps and geosphere packages to demonstrate


# First, let's load a dataset with outbound moves from New York City 
zips_nyc_out <- readRDS("data/zips_nyc_out_beyondcity.rds")

zips_nyc_out
# View(zips_nyc_out)

## split into permanent and temporary tables ####
zips_nyc_out_TEMP <- zips_nyc_out %>% 
  filter(temp_moves > 0)

zips_nyc_out_PERM <- zips_nyc_out %>% 
  filter(perm_moves > 0)


# first we have to set point for center of the paths
# for now we'll hard code this in as NYC's lat/lon
centerpoint_coord <- tibble(location = c("center point nyc"),
                            latitude = c(40.7128),
                            longitude = c(-74.006)
                            )


# create basemap using the maps package
# note the maps package operates a little differently in how we add elements sequentially
maps::map("state", 
          fill = TRUE, 
          col = "grey8", 
          bg = "grey15")

# overlay location points - TEMPORARY moves
points(zips_nyc_out_TEMP$new_longitude, zips_nyc_out_TEMP$new_latitude, 
       pch = 3, cex = 0.1, col = "green")

# overlay location points - PERMANENT moves (some of these may overlap with temp as well)
points(zips_nyc_out_PERM$new_longitude, zips_nyc_out_PERM$new_latitude, 
       pch = 3, cex = 0.1, col = "chocolate1")

# Now we'll calculate the paths using the geosphere package

# The gcIntermediate() function takes two points and creates a connection between them 
# that accounts for the curvature and therefore creates a circular path.
# The lines() function then adds the generated path to the map. 
# We'll use a for loop here to cycle through all the points at once.

for (i in (1:dim(zips_nyc_out)[1])) { 
  inter <- gcIntermediate(c(centerpoint_coord$longitude[1], 
                            centerpoint_coord$latitude[1]),
                          c(zips_nyc_out$new_longitude[i], 
                            zips_nyc_out$new_latitude[i]), 
                          n=200)
  lines(inter, lwd = 0.1, col = "purple")    
}  


# We can do something similar focusing on only certain states
# create basemap of just states around New York using regions parameter
maps::map("state", 
          regions = c("new york", "new jersey", "connecticut", "rhode island",
                      "pennsylvania", "massachusetts", "new hampshire", "vermont"),
          fill = TRUE, 
          col = "grey8", 
          bg = "grey15")

# overlay location points - TEMPORARY
points(zips_nyc_out_TEMP$new_longitude, zips_nyc_out_TEMP$new_latitude, 
       pch = 3, cex = 0.1, col = "green")

# overlay location points - PERMANENT (some of these may overlap with temp as well)
points(zips_nyc_out_PERM$new_longitude, zips_nyc_out_PERM$new_latitude, 
       pch = 3, cex = 0.1, col = "chocolate1")

# now we'll use geosphere again as above
for (i in (1:dim(zips_nyc_out)[1])) { 
  inter <- gcIntermediate(c(centerpoint_coord$longitude[1], 
                            centerpoint_coord$latitude[1]),
                          c(zips_nyc_out$new_longitude[i], 
                            zips_nyc_out$new_latitude[i]), 
                          n=200)
  lines(inter, lwd = 0.1, col = "purple")    
}  

# Note that in this case the paths extend beyond our chosen states
# since the dataset itself still includes them. 
# So you'd want to similarly filter out those states from the table
# if doing this for real.







## STRATEGIES FOR DEALING WITH ALASKA, HAWAII AND PUERTO RICO #### -----------------

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
