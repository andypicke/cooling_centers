#------------------------------------------
#
# process_data.R
#
# This Script does processing for Denver cooling centers analysis:
# - get census tracts data w/ tidycensus
# - load rec center locations and compute isochrones
# - load library locations and compute isochrones
# - save above to RDS format to laod for further analysis
#
# Andy Pickering
# andypicke@gmail.com
# 2024-09-04
#
#
#------------------------------------------

#------------------------
# libraries
#------------------------

library(janitor)
library(glue)
library(mapboxapi) # compute isocrhone/isodistance 
library(leaflet) # mapping
library(sf) # working with shapefiles
library(dplyr)
library(here)
library(tidycensus) # census tracts
library(tigris) # county shapefile
options(tigris_use_cache = TRUE)


#------------------------
# load data
#------------------------

# load shapefile with rec center locations
# rec shape file crs = 2877 for Colorado
shp_file <- here('data', 'raw',"ODC_PARK_RECCENTER_P_-1008034318397091855/PARK_RECCENTER_P.shp")

rec_sf <- sf::read_sf(shp_file) |>
  sf::st_transform(4326) |>
  janitor::clean_names()


# load file with Denver library locations (see scrape_library_locations.R)
libraries <- readRDS(here('data', 'processed', 'denver_library_locations.rds'))
# convert to a sf object
lib_sf <- libraries |> tidyr::drop_na() |> sf::st_as_sf(coords = c("lib_lng","lib_lat"))
lib_sf <- `st_crs<-`(lib_sf, 4326) # specify crs

# load county shapefiles
counties <- tigris::counties(state = "CO",)
denver <- counties |> filter(NAME == "Denver")

# load Denver census tracts
den_tracts <- get_decennial(
  geography = "tract", 
  variables = "P1_001N",
  year = 2020,
  state = "CO",
  county = "Denver", 
  geometry = TRUE
) |> st_transform(4326)

glue('There are {nrow(den_tracts)} census tracts with a total population of {sum(den_tracts$value)}')

save_file <- paste0("Denver_census_tracts.rds")
saveRDS(den_tracts, file = here('data', 'processed', save_file))


#------------------------
# calculate isochrones
#------------------------
time_minutes <- 20

# rec centers
isos_rec <- mapboxapi::mb_isochrone(
  location = rec_sf,
  profile = "walking",
  time = time_minutes,
  id_column = "rec_name"
)

save_file <- paste0("isos_rec_", time_minutes, "mins.rds")
saveRDS(isos_rec, file = here('data', 'processed', save_file))

# libraries
isos_lib <- mapboxapi::mb_isochrone(
  location = lib_sf,
  profile = "walking",
  time = time_minutes,
  id_column = "lib_name"
)

save_file <- paste0("isos_lib_", time_minutes, "mins.rds")
saveRDS(isos_lib, file = here('data', 'processed', save_file))



