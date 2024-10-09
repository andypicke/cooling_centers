#---------------------------------------------------
#
# Goal: Scrape website to get a list of Denver library locations, to add to cooling
# center analysis (since Libraries are open/available to public as cooling centers).
#
# I used https://rvest.tidyverse.org/articles/selectorgadget.html to figure out a css selector
# that worked to get the locations
#
# andypicke@gmail.com
# 2024-08-15
#
#---------------------------------------------------


library(here)
library(rvest)
library(stringr)
library(leaflet)


# website with list of Denver libraries/locations
url <- "https://www.denverlibrary.org/locations"

# read the html
html <- rvest::read_html(url)

# This gets a list of street addresses (could geocode them)
#lib_addresses <- html |> html_elements(".views-field-field-address a") |> html_text()

# this gets a list of the links to google maps of each location (which include the lat/lng)
# used selectorgadget tool to get this
gmap_links <- html |> html_elements(".views-field-field-address a") |> html_attr("href")


#------------------------------
# function to extract the name, lat, and lng from the googlemaps link
extract_info <- function(x){
  
  # extract the name (between "place/" and "/@")
  ind1 <- stringr::str_locate(x,"place/")
  ind2 <- stringr::str_locate(x,"/@")
  lib_name <- str_sub(x,ind1[2] + 1, ind2[1] - 1)
  lib_name <- str_replace_all(lib_name, "\\+", " ") # note need to escape plus sign
  
  # extract lat,lng location (starts after "@")
  lib_loc <- str_sub(x, ind2[1] + 2, nchar(x))
  
  # find first comma
  ind_comma1 <- str_locate(lib_loc, ",")
  
  # get latitude
  lib_lat <- as.numeric(str_sub(lib_loc, 1, ind_comma1[1] - 1))
  
  # find next comma
  y <- str_sub(lib_loc, ind_comma1[1] + 1, nchar(lib_loc) - 2)
  ind_comma2 <- str_locate(y, ",")
  
  # get longitude
  lib_lng <- as.numeric(str_sub(y, 1, ind_comma2[1] - 1))
  
  df_out <- data.frame(lib_name, lib_lat, lib_lng)
}
#------------------------------

#df <- extract_info(gmap_links[9])

# apply to all links to get dataframe of locations
df <- purrr::map(gmap_links, extract_info) |> purrr::list_rbind() |> tidyr::drop_na()

# plot locations on map
leaflet() |>
  addTiles() |>
  addMarkers(data = df, lng = ~lib_lng, lat = ~lib_lat, label = ~lib_name)

# save data
saveRDS(df, here('data/processed/','denver_library_locations.rds'))

# to read data
#libraries <-readRDS(here('data','denver_library_locations.rds'))