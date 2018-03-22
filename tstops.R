# loading all the packages I can think of
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(rgdal)
library(quantmod)
library(highcharter)
library(RColorBrewer)
library(forcats)
library(htmlwidgets)
library(ggiraph)
library(tidyverse)
library(data.table)
library(censusapi)
library(tidycensus)
library(tigris)
library(tidyr)
library(kwgeo)
library(googledrive)
library(utils)
library(googlesheets)
library(zipcode)
library(plotKML)
library(maptools)
library(ggmap)

options(tigris_use_cache = TRUE)

# set census api key
Sys.setenv(CENSUS_KEY="f6811bb29b8f3f4de930ececf654c6d0ebe6c7be")
readRenviron("~/.Renviron")

# loading data
tstops <- read_csv("trafficStops_fpd_16.csv")
locations <- read_csv("trafficStops_locations_16.csv")

# joining the locations data onto tstops ... notice there are some duplicates but I think this is OK as each line counts a separate car.

tstops_locations <- inner_join(tstops,locations, by=c("EVENT_NUM"="EVENT_NO"))

# stops by race
stops_race <- tstops %>%
  group_by(APPARENT_RACE) %>%
  summarise(ct=n()) %>% 
  arrange(desc(ct)) %>%
  mutate(freq = ct/sum (ct)*100)


# dropping t stops onto a map

stops_map <- leaflet() %>%
  setView(lng = -119.7726, lat = 36.7468, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(lng=tstops_locations$LONGT,lat=tstops_locations$LAT, weight = 3, radius=40, 
             color="#ffa500", stroke = TRUE, fillOpacity = 0.8 )

print(stops_map)


# making map of car searches 

search_location <- tstops_locations %>%
  filter(VEHICLE_SEARCHED=="Y")

search_map <- leaflet () %>%
  setView(lng = -119.7726, lat = 36.7468, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(lng=search_location$LONGT,lat=search_location$LAT, weight = 3, radius=40, 
             color="#ffa500", stroke = TRUE, fillOpacity = 0.8 )
print(search_map)

# making map of arrests

arrest_location <- tstops_locations %>%
  filter(ACTION_TAKEN=="A")

arrest_map <- leaflet () %>%
  setView(lng = -119.7726, lat = 36.7468, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(lng=arrest_location$LONGT,lat=arrest_location$LAT, weight = 3, radius=40, 
             color="#ffa500", stroke = TRUE, fillOpacity = 0.8 )
print(search_map)


# grabbing census data for race, poverty, unemployment, etc. and creating map

# grabbing census data at census tract level

# generating list of variables for 2015 acs
vars_acs15 <- load_variables(2015, "acs5", cache = TRUE)

ct <- getCensus(name = "acs5",
                vintage = 2015,
                vars = c("GEOID",
                         "B01003_001E",
                         "B03002_003E",
                         "B03002_004E",
                         "B03002_012E",
                         "B03002_006E",
                         "B17017_001E",
                         "B17017_002E",
                         "B25077_001E",
                         "B23025_003E",
                         "B23025_005E",
                         "B06009_001E",
                         "B06009_002E"),
                region = "tract:*",
                regionin = "state:06+county:019")

names(ct) <- c("GEOID", "state", "county", "tract", "population","white_population", "black_population", "hispanic_latino_population", "asian_population", "households", "households_in_poverty","median_home_value","labor_force","unemployed","total_educated","less_than_hs")

ct <- ct %>%
  mutate(pct_black_population = round(black_population/population*100,2),
         pct_white_population = round(white_population/population*100,2),
         pct_hispanic_latino_population = round(hispanic_latino_population/population*100,2),
         pct_asian = round(asian_population/population*100,2),
         pc_inPoverty = round(households_in_poverty/households*100,2),
         pc_unemployed = round(unemployed/labor_force*100,2),
         pc_no_hs = round(less_than_hs/total_educated*100,2))



# fixing NaNs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
ct[is.nan(ct)] <- 0

# load TIGER/LINE shapefiles for census tracts in Fresno
tracts_map <- tracts("06", county = "019")
plot(tracts_map)
View(tracts_map@data)


tracts_merged <- geo_join(tracts_map,ct, "TRACTCE", "tract")

# there are some tracts with no land that we should exclude
tracts_merged <- tracts_merged[tracts_merged$ALAND>0,]

View(tracts_merged@data)


# creating color pal and pop up for map
popup <- paste0("GEOID: ", tracts_merged$GEOID, "<br>", "Percent white: ", tracts_merged$pct_white_population)
pal <- colorNumeric(
  palette = "Blues",
  domain = tracts_merged$pct_white_population
)

# mapping white population and stops
stops_white_map<-leaflet() %>%
  setView(-119.780642,36.750352, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = tracts_merged, 
              fillColor = ~pal(pct_white_population), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 0, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addCircles(lng=tstops_locations$LONGT,lat=tstops_locations$LAT, weight = 3, radius=40, 
             color="#fb6a4a", stroke = TRUE, fillOpacity = 0.8)

print(stops_white_map)

# saving map as html
saveWidget(stops_white_map, "stops_white_map.html", selfcontained = TRUE, libdir = NULL, background = "white")


# mapping white population and searches

# creating color pal and pop up for map
popup <- paste0("GEOID: ", tracts_merged$GEOID, "<br>", "Percent white: ", tracts_merged$pct_white_population)
pal <- colorNumeric(
  palette = "Blues",
  domain = tracts_merged$pct_white_population
)


searches_white_map<-leaflet() %>%
  setView(-119.780642,36.750352, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = tracts_merged, 
              fillColor = ~pal(pct_white_population), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 0, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addCircles(lng=search_location$LONGT,lat=search_location$LAT, weight = 3, radius=40, 
             color="#cb181d", stroke = TRUE, fillOpacity = 0.8 )

print(searches_white_map)

# saving map as html
saveWidget(searches_white_map, "searches_white_map.html", selfcontained = TRUE, libdir = NULL, background = "white")

# mapping white population and arrests

# creating color pal and pop up for map
popup <- paste0("GEOID: ", tracts_merged$GEOID, "<br>", "Percent white: ", tracts_merged$pct_white_population)
pal <- colorNumeric(
  palette = "Blues",
  domain = tracts_merged$pct_white_population
)


arrests_white_map<-leaflet() %>%
  setView(-119.780642,36.750352, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = tracts_merged, 
              fillColor = ~pal(pct_white_population), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 0, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addCircles(lng=arrest_location$LONGT,lat=arrest_location$LAT, weight = 3, radius=40, 
             color="#67000d", stroke = TRUE, fillOpacity = 0.8 )

print(arrests_white_map)

# saving map as html
saveWidget(arrests_white_map, "arrests_white_map.html", selfcontained = TRUE, libdir = NULL, background = "white")


