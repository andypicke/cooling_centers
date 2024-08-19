#------------------------------------------
#
# Analyze access to cooling centers in Denver, CO
#
# Script to run analysis for cooling centers 
# (originally in qmd file for blog post)
#
# Andy Pickering
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
library(here)
library(janitor)
library(glue)
library(tidycensus) # census tracts
library(tigris) # county shapefile
options(tigris_use_cache = TRUE)


#------------------------
# load data
#------------------------

# load shapefile with rec center locations
# rec shape file crs = 2877 for Colorado
shp_file <- "./data/ODC_PARK_RECCENTER_P_-1008034318397091855/PARK_RECCENTER_P.shp"

rec_shp <- sf::read_sf(shp_file) |>
  sf::st_transform(4326) |>
  janitor::clean_names()


# load file with Denver library locations
libraries <- readRDS(here('data','denver_library_locations.rds'))
# convert to a sf object
lib_sf <- libraries |> tidyr::drop_na() |> sf::st_as_sf(coords = c("lib_lng","lib_lat"))
lib_sf <- `st_crs<-`(lib_sf, 4326) # specify crs

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

glue('There are {nrow(den_tracts)} census tracts with a total population of {sum(den_tracts$value)}')



#------------------------
# calculate isochrones
#------------------------
time_minutes <- 20

# rec centers
isos_rec <- mapboxapi::mb_isochrone(
  location = rec_shp,
  profile = "walking",
  time = time_minutes,
  id_column = "rec_name"
)

# libraries
isos_lib <- mapboxapi::mb_isochrone(
  location = lib_sf,
  profile = "walking",
  time = time_minutes,
  id_column = "lib_name"
)



#------------------------
# Plot map of census tracts and cooling center isochrones
#------------------------

leaflet() |>
  addTiles() |>
  addPolygons(data = den_tracts, fillColor = "grey", color = "black", weight = 1, label = ~NAME) |>
  addPolygons(data = isos_rec, fillColor = "blue", color = "blue", label = ~id) |>
  addPolygons(data = isos_lib, fillColor = "magenta", color = "magenta", label = ~id)


#------------------------
# calculate the intersection between isochrones and census tracts
#------------------------

# union of all rec center isochrones
isos_rec_union <- sf::st_union(isos_rec, by_feature = FALSE) |> sf::st_as_sf()

# union of all library isochrones
isos_lib_union <- sf::st_union(isos_lib, by_feature = FALSE) |> sf::st_as_sf()


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

# apply to each tract for rec centers
pops_out_rec <- rep(NA, times = nrow(den_tracts))
for (i in 1:nrow(den_tracts)) {
  pops_out_rec[i] <- calc_pop_out(den_tracts[i,], isos_rec_union)
}

# total population that does not intersect rec center isochrones
pop_out_rec <- round(sum(pops_out_rec))


# apply to each tract for libraries
pops_out_lib <- rep(NA, times = nrow(den_tracts))
for (i in 1:nrow(den_tracts)) {
  pops_out_lib[i] <- calc_pop_out(den_tracts[i,], isos_lib_union)
}

# total population that does not intersect rec center isochrones
pop_out_lib <- round(sum(pops_out_lib))


#------------------------
# calc total area that intersects/not
#------------------------

# union of all Denver census tracts (combines into 1 multi-polygon)
tracts_union <- sf::st_union(den_tracts)

# calculate intersection and difference for rec centers
tracts_isos_rec_intersect <- sf::st_intersection(tracts_union, isos_rec_union)
tracts_isos_rec_diff <- sf::st_difference(tracts_union, isos_rec_union)

# calculate intersection and difference for libraries
tracts_isos_lib_intersect <- sf::st_intersection(tracts_union, isos_lib_union)
tracts_isos_lib_diff <- sf::st_difference(tracts_union, isos_lib_union)


# calculate area in/out
area_tot <- sf::st_area(tracts_union)
area_in_rec <- sf::st_area(tracts_isos_rec_intersect)
area_out_rec <- sf::st_area(tracts_isos_rec_diff)
percent_out_rec <- round(area_out_rec/area_tot*100)

glue('Approximately {percent_out_rec} % of the area of Denver county is not within a {time_minutes} minute walk of a cooling center (rec center).')
glue('Approximately {scales::comma(pop_out_rec)} people in Denver county are not within a {time_minutes} minute walk of a cooling center (rec center)')

area_out_lib <- sf::st_area(tracts_isos_lib_diff)
percent_out_lib <- round(area_out_lib/area_tot*100)
glue('Approximately {percent_out_lib} % of the area of Denver county is not within a {time_minutes} minute walk of a library.')
glue('Approximately {scales::comma(pop_out_lib)} people in Denver county are not within a {time_minutes} minute walk of a library')


# do calcs for rec centers and libraries *combined*
isos_rec_lib_union <- sf::st_union(isos_rec_union, isos_lib_union)

leaflet() |>
  addTiles() |>
  addPolygons(data = isos_rec_lib_union)

tracts_isos_rec_lib_intersect <- sf::st_intersection(tracts_union, isos_rec_lib_union)
tracts_isos_rec_lib_diff <- sf::st_difference(tracts_union, isos_rec_lib_union)

area_in_rec_lib <- sf::st_area(tracts_isos_rec_lib_intersect)
area_out_rec <- sf::st_area(tracts_isos_rec_lib_diff)
percent_out_rec_lib <- round(area_out_rec/area_tot*100)

leaflet() |>
  addTiles() |>
  addPolygons(data = tracts_isos_rec_lib_intersect) |>
  addPolygons(data = tracts_isos_rec_lib_diff, color = "red")


# apply to each tract for rec centers AND libraries
pops_out_rec_lib <- rep(NA, times = nrow(den_tracts))
for (i in 1:nrow(den_tracts)) {
  pops_out_rec_lib[i] <- calc_pop_out(den_tracts[i,], isos_rec_lib_union)
}

# total population that does not intersect rec center isochrones
pop_out_rec_lib <- round(sum(pops_out_rec_lib))

glue('Approximately {percent_out_rec_lib} % of the area of Denver county is not within a {time_minutes} minute walk of a cooling center (rec center) OR library.')
glue('Approximately {scales::comma(pop_out_rec_lib)} people in Denver county are not within a {time_minutes} minute walk of a cooling center (rec center) OR library')
