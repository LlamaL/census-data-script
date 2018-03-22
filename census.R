# load required libraries
library(tidycensus)
library(rgdal)
library(readr)
library(dplyr)
library(tidyr)
library(tigris)
library(kwgeo)
library(googledrive)
library(utils)
library(censusapi)
library(googlesheets)
library(zipcode)

options(tigris_use_cache = TRUE)

# set census api key
Sys.setenv(CENSUS_KEY="f6811bb29b8f3f4de930ececf654c6d0ebe6c7be")
readRenviron("~/.Renviron")

# vars <- makeVarlist(name = "acs5", vintage = 2015, "household", varsearch = "concept", output = dataframe)

# generating list of variables for 2015 acs
vars_acs15 <- load_variables(2015, "acs5", cache = TRUE)

############# ZIP-LEVEL CODE

# get ACS 2015 5-year data, ztca nationwide
zctas <- getCensus(name = "acs5",
                          vintage = 2015,
                          vars = c("GEOID",
                                   "B01003_001E",
                                   "B17017_001E",
                                   "B19013_001E",
                                   "B17017_002E",
                                   "B03002_003E",
                                   "B03002_004E",
                                   "B03002_012E",
                                   "B03002_006E",
                                   "B06009_001E",
                                   "B06009_002E",
                                   "B06009_003E",
                                   "B06009_005E",
                                   "B06009_006E", 
                                   "B25077_001E",
                                   "B25003_001E",
                                   "B25003_002E",
                                   "B25003_003E"),
                          region = "zip code tabulation area:*")
                          
names(zctas) <- c("GEOID","zip","population","households","median_household_income","households_in_poverty","white_population", "black_population", "hispanic_latino_population", "asian_population", "total_educated", "less_than_hs_grad", "hs_grad","bachelors_degree","advanced_degree", "median_home_value", "total_units", "owner_occupied_units", "renter_occupied_units")

# load TIGER/LINE shapefiles for zcta nationwide
# zctas_map <- zctas(cb = TRUE)

# clipping the zctas map to CA
# zctas_map_ca <- raster::intersect(zctas_map,ca_counties_map)
# zctas_map_ca@data <- zctas_map_ca@data %>%
#   select(1:5)
# zctas_ca <- as.data.frame(zctas_map_ca) %>%
#   select(1:5)

# joining zcta data to the above ... hang on, this join gives me a bunch of duplicates ... update: the problem seems to be with the code above
# zctas_ca_join <- inner_join(zctas, zctas_ca, by=c("zip"="ZCTA5CE10"))

# using zipcode package to pull in zipcodes from ca only this works, so I am commenting out the above, where we tried to clip ca counties onto the nation's zips to come up with ca zips only
data(zipcode)
zip_ca <- subset(zipcode, state=='CA')

# joining zip_ca with zcta data
ca_zip <- inner_join(zip_ca, zctas, by=c("zip"="zip"))

# cleaning up for google drive
ca_zip <- ca_zip %>%
  select(1:3,7:23)

# finding percentages for race, education, and housing variables for ca_zip
ca_zip <- ca_zip %>%
  mutate(pct_households_in_poverty = (households_in_poverty/households),
         pct_black_population = (black_population/population),
         pct_white_population = (white_population/population),
         pct_hispanic_latino_population = (hispanic_latino_population/population),
         pct_asian = (asian_population/population),
         pct_less_than_hs = (less_than_hs_grad/total_educated),
         pct_hs_grad = (hs_grad/total_educated),
         pct_bachelors = (bachelors_degree/total_educated),
         pct_advanced = (advanced_degree/total_educated),
         pct_owner = (owner_occupied_units/total_units),
         pct_renter = (renter_occupied_units/total_units))

# fixing NaNs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
ca_zip[is.nan(ca_zip)] <- 0



############# CITY/PLACE-LEVEL CODE

# get ACS 2015 5-year data, cities/places in CA
ca_cities <- getCensus(name = "acs5",
                   vintage = 2015,
                   vars = c("GEOID",
                            "NAME",
                            "B01003_001E",
                            "B17017_001E",
                            "B19013_001E",
                            "B17017_002E",
                            "B03002_003E",
                            "B03002_004E",
                            "B03002_012E",
                            "B03002_006E",
                            "B06009_001E",
                            "B06009_002E",
                            "B06009_003E",
                            "B06009_005E",
                            "B06009_006E", 
                            "B25077_001E",
                            "B25003_001E",
                            "B25003_002E",
                            "B25003_003E"),
                   region = "place:*", regionin = "state:06")

names(ca_cities) <- c("GEOID", "NAME", "state", "place","population","households","median_household_income","households_in_poverty","white_population", "black_population", "hispanic_latino_population", "asian_population", "total_educated", "less_than_hs_grad", "hs_grad","bachelors_degree","advanced_degree", "median_home_value","total_units", "owner_occupied_units", "renter_occupied_units")

# cleaning up for google drive
ca_cities <- ca_cities %>%
  select(1:2,4:21)

# finding percentages for race and education variables for ca_cities
ca_cities <- ca_cities %>%
  mutate(pct_households_in_poverty = (households_in_poverty/households),
         pct_black_population = (black_population/population),
         pct_white_population = (white_population/population),
         pct_hispanic_latino_population = (hispanic_latino_population/population),
         pct_asian = (asian_population/population),
         pct_less_than_hs = (less_than_hs_grad/total_educated),
         pct_hs_grad = (hs_grad/total_educated),
         pct_bachelors = (bachelors_degree/total_educated),
         pct_advanced = (advanced_degree/total_educated),
         pct_owner = (owner_occupied_units/total_units),
         pct_renter = (renter_occupied_units/total_units))

# fixing NaNs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
ca_cities[is.nan(ca_cities)] <- 0




############# COUNTY-LEVEL CODE

# get ACS 2015 5-year data, counties nationwide
counties <- getCensus(name = "acs5",
                   vintage = 2015,
                   vars = c("GEOID",
                            "NAME",
                            "B01003_001E",
                            "B17017_001E",
                            "B19013_001E",
                            "B17017_002E",
                            "B03002_003E",
                            "B03002_004E",
                            "B03002_012E",
                            "B03002_006E",
                            "B06009_001E",
                            "B06009_002E",
                            "B06009_003E",
                            "B06009_005E",
                            "B06009_006E", 
                            "B25077_001E",
                            "B25003_001E",
                            "B25003_002E",
                            "B25003_003E"),
                   region = "county:*")

# renaming columns
names(counties) <- c("GEOID","name", "state", "county", "population","households","median_household_income","households_in_poverty","white_population", "black_population", "hispanic_latino_population", "asian_population", "total_educated", "less_than_hs_grad", "hs_grad","bachelors_degree","advanced_degree", "median_home_value", "total_units", "owner_occupied_units", "renter_occupied_units")

counties <- counties %>%
  mutate(pct_households_in_poverty = (households_in_poverty/households),
         pct_black_population = (black_population/population),
         pct_white_population = (white_population/population),
         pct_hispanic_latino_population = (hispanic_latino_population/population),
         pct_asian = (asian_population/population),
         pct_less_than_hs = (less_than_hs_grad/total_educated),
         pct_hs_grad = (hs_grad/total_educated),
         pct_bachelors = (bachelors_degree/total_educated),
         pct_advanced = (advanced_degree/total_educated),
         pct_owner = (owner_occupied_units/total_units),
         pct_renter = (renter_occupied_units/total_units))


# load TIGER/LINE shapefiles for counties in CA
ca_counties_map <- counties(state = "06", cb = TRUE)

# checking it looks OK (just shown here for counties)
plot(ca_counties_map)
View(ca_counties_map@data)

# join to map
counties <- counties %>%
  mutate(GEOID2 = substr(GEOID,8,12))
ca_counties_map@data <- inner_join(ca_counties_map@data,counties, by=c("GEOID"="GEOID2")) 

# extract data frame for CA only
ca_counties <- as.data.frame(ca_counties_map)

# cleaning up ca_counties for google drive
ca_counties <- ca_counties %>%
  select(5:6,14:41)

# finding percentages for race and education variables for ca_counties ... UPDATE: moved this code above so that my below calculations would be attached to the shapefiles for mapping
# ca_counties <- ca_counties %>%
#   mutate(pct_households_in_poverty = (households_in_poverty/households),
#          pct_black_population = (black_population/population),
#          pct_white_population = (white_population/population),
#          pct_hispanic_latino_population = (hispanic_latino_population/population),
#          pct_asian = (asian_population/population),
#          pct_less_than_hs = (less_than_hs_grad/total_educated),
#          pct_hs_grad = (hs_grad/total_educated),
#          pct_bachelors = (bachelors_degree/total_educated),
#          pct_advanced = (advanced_degree/total_educated),
#          pct_owner = (owner_occupied_units/total_units),
#          pct_renter = (renter_occupied_units/total_units))

# fixing NaNs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
ca_counties_map@data[is.nan(ca_counties_map@data)] <- 0

# fixing NaNs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
ca_counties[is.nan(ca_counties)] <- 0



############# STATE-LEVEL CODE

# get ACS 2015 5-year data, state nationwide
states <- getCensus(name = "acs5",
                      vintage = 2015,
                      vars = c("GEOID",
                               "NAME",
                               "B01003_001E",
                               "B17017_001E",
                               "B19013_001E",
                               "B17017_002E",
                               "B03002_003E",
                               "B03002_004E",
                               "B03002_012E",
                               "B03002_006E",
                               "B06009_001E",
                               "B06009_002E",
                               "B06009_003E",
                               "B06009_005E",
                               "B06009_006E",
                               "B25077_001E",
                               "B25003_001E",
                               "B25003_002E",
                               "B25003_003E"),
                    region = "state:*")

# renaming columns
names(states) <- c("GEOID","name", "state", "population","households","median_household_income","households_in_poverty","white_population", "black_population", "hispanic_latino_population", "asian_population", "total_educated", "less_than_hs_grad", "hs_grad","bachelors_degree","advanced_degree", "median_home_value", "total_units", "owner_occupied_units", "renter_occupied_units")


# finding percentages for race and education variables for ca_cities
states <- states %>%
  mutate(pct_households_in_poverty = (households_in_poverty/households),
         pct_black_population = (black_population/population),
         pct_white_population = (white_population/population),
         pct_hispanic_latino_population = (hispanic_latino_population/population),
         pct_asian = (asian_population/population),
         pct_less_than_hs = (less_than_hs_grad/total_educated),
         pct_hs_grad = (hs_grad/total_educated),
         pct_bachelors = (bachelors_degree/total_educated),
         pct_advanced = (advanced_degree/total_educated),
         pct_owner = (owner_occupied_units/total_units),
         pct_renter = (renter_occupied_units/total_units))

# fixing NaNs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
states[is.nan(states)] <- 0



########## PUSHING TO GOOGLE SHEETS AND CARTO

# set carto account details
cartodb_id <- "reis-thebault"
cartodb_api <- "yours"

# upload to carto  
r2cartodb(ca_counties_map, "ca_counties_map", cartodb_id, cartodb_api)

# save shapefile
writeOGR(ca_counties_map,"ca_counties_map",layer="ca_counties_map", driver = "ESRI Shapefile")

writeOGR(zctas_map, "zctas_map", layer="zctas_map", driver = "ESRI Shapefile")

# zip shapefile
zip(zipfile = "ca_counties_map", files = "ca_counties_map")


# uploading data into google drive 'population' folder

drive_api_key()
drive_auth(oauth_token = NULL, service_token = NULL, reset = FALSE,
           cache = getOption("httr_oauth_cache"),
           use_oob = getOption("httr_oob_default"), verbose = TRUE)

write_csv(ca_counties, "populationCounty_ca_15.csv", na="")
drive_upload("populationCounty_ca_15.csv", type = "spreadsheet", name = "populationCounty_ca_15", as_dribble("population/"))

write_csv(ca_zip, "populationZip_ca_15.csv", na="")
drive_upload("populationZip_ca_15.csv", type = "spreadsheet", name = "populationZip_ca_15", as_dribble("population/"))

write_csv(ca_cities, "populationCities_ca_15.csv", na="")
drive_upload("populationCities_ca_15.csv", type = "spreadsheet", name = "populationCities_ca_15", as_dribble("population/"))

write_csv(states, "populationStates_us_15.csv", na="")
drive_upload("populationStates_us_15.csv", type = "spreadsheet", name = "populationStates_ca_15", as_dribble("population/"))
