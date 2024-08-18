#------------------------------------------
#
# script to run analysis for cooling centers 
# (originally in qmd file for blog post)
#
# andypicke@gmail.com
# 2024-08-17
#
#
#------------------------------------------

#------------------------
# libraries
#------------------------

library(mapboxapi) # compute isocrhone/isodistance 
library(leaflet) # mapping
library(sf) # working with shapefiles
library(dplyr)
library(janitor)
library(glue)
library(tidycensus) # census tracts
library(tigris) # county shapefile
options(tigris_use_cache = TRUE)


#------------------------
# load data
#------------------------

# load shapefile with rec center locations
# rec shape file crs = 2877 for Colorado; use for calculations later?
shp_file <- "./data/ODC_PARK_RECCENTER_P_-1008034318397091855/PARK_RECCENTER_P.shp"

rec_shp <- sf::read_sf(shp_file) |>
  sf::st_transform(4326) |>
  janitor::clean_names()

#head(rec_shp)

# load file with Denver library locations
libraries <-readRDS(here('data','denver_library_locations.rds'))
# convert to a sf object
libs <- libraries |> tidyr::drop_na() |> sf::st_as_sf(coords = c("lib_lng","lib_lat"))
st_crs(libs) <- 4326

# load county shapefiles
counties <- tigris::counties(state = "CO",)
denver <- counties |> filter(NAME == "Denver")

# load Denver census tracts
den_tracts <- get_decennial(
  geography = "tract", 
  variables = "P001001",
  year = 2010,
  state = "CO",
  county = "Denver", 
  geometry = TRUE
) |> st_transform(4326)


#------------------------
# calculate isochrones
#------------------------
time_minutes <- 20

isos <- mapboxapi::mb_isochrone(
  location = rec_shp,
  profile = "walking",
  time = time_minutes,
  id_column = "rec_name"
)

isos_libs <-  mapboxapi::mb_isochrone(
  location = libs,
  profile = "walking",
  time = time_minutes,
  id_column = "lib_name"
)

leaflet() |>
  addTiles() |>
  #addCircleMarkers(data = libs) |>
  addPolygons(data = den_tracts, fillColor = "grey", color = "black", weight = 1) |>
  addPolygons(data = isos_libs, label = "libraries") |>
  addPolygons(data = isos, fillColor = "red", color = "red", label = "rec center")



#------------------------
# calculate the intersection between isochrones and census tracts
#------------------------

# function to compute percent overlap and weighted population for 1 tract
calc_pop_out <- function(tract, isos) {
  
  tract_intersect <- sf::st_intersection(tract, isos)
  
  if ( nrow(tract_intersect) == 0) {return(0)}
  
  area_tot <- sf::st_area(tract)
  area_intersect <- sf::st_area(tract_intersect)
  perc_intersect <- as.numeric(area_intersect/area_tot*100)
  perc_area_not_in <- 100 - perc_intersect
  
  total_pop <- tract$value
  pop_out <- perc_area_not_in/100*total_pop
  
}

# apply to each tract (could use *map* function instead of loop? )
pops_out <- rep(NA, times = nrow(den_tracts))
for (i in 1:nrow(den_tracts)) {
  pops_out[i] <- calc_pop_out(den_tracts[i,], isos_union)
}

# total population that does not intersect isochrones
pop_out <- round(sum(pops_out))



#------------------------
# to calc total area that intersects/not, easier to use union/intersect/differenc
#------------------------

# union of all Denver census tracts (combines into 1 multi-polygon)
tracts_union <- sf::st_union(den_tracts)
# union of all isochrones
isos_union <- sf::st_union(isos, by_feature = FALSE) |> sf::st_as_sf()

# calculate intersection and difference
tracts_isos_intersect <- sf::st_intersection(tracts_union, isos_union)
tracts_isos_diff <- sf::st_difference(tracts_union, isos_union)

# calculate area in/out
area_tot <- sf::st_area(tracts_union)
area_in <- sf::st_area(tracts_isos_intersect)
area_out <- sf::st_area(tracts_isos_diff)
percent_out <- round(area_out/area_tot*100)



