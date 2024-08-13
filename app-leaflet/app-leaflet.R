#
# 
# To-Do: When user clicks on map, show isochrone from that location, and highlight cooling centers within isochrone
#
#
#
#
#

library(shiny)
library(bslib)
#library(mapgl)
library(leaflet)
library(mapboxapi)
library(sf)
library(DT)

# read in the data with cooling centers (ie rec centers) locations and info
cool <- sf::st_read('../data/ODC_PARK_RECCENTER_P_-1008034318397091855/PARK_RECCENTER_P.shp') |> sf::st_transform(4326)


#----------------------------------------------------
# Define UI 
#----------------------------------------------------

ui <- bslib::page_sidebar(
  theme = bs_theme(bootswatch = "simplex"),
  title = "Denver Cooling Centers",
  sidebar = bslib::sidebar(
    #selectInput(inputId = "mode", label = "Travel Mode", choices = c("driving", "walking","cycling")),
    #verbatimTextOutput("click_info")
  sliderInput(inputId = "time", label = "Time (minutes)", min = 5, max = 30, step = 5, value = 20)
  ),
  value_box(
    title = "# Cooling Centers Within 30min Walk",
    value = textOutput("n_cool"),
    theme = "bg-blue"
  ),
  #  height = 100
  leaflet::leafletOutput("map"),
  dataTableOutput("table")
)



#----------------------------------------------------
# Define server 
#----------------------------------------------------


server <- function(input, output) {
  
  n_cool <- reactiveVal(NULL)
  
  # base map with cooling center locations
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addMarkers(data = cool, label = ~REC_NAME)
  })
  

  output$click_info <- renderPrint({
    input$map_click$lng
  })
  
  
  observe({
    
    click <- input$map_click
    #time = input$time
    #   
    isochrone <- mb_isochrone(
      location = c(click$lng, click$lat),
      time = input$time,#30,
      profile = "walking"
    )
    
    # update the map
    leafletProxy("map") |> 
      clearShapes() |>
      removeMarker(layerId = "clicked") |>
      addMarkers(lng = click$lng, lat = click$lat, label = "You clicked here", layerId = "clicked") |>
      addPolygons(data = isochrone)
    
    # calculate how many cooling centers are within isochrone
    cool_in <- sf::st_filter(cool, isochrone)
    n <- cool_in |> nrow()
    
    
    output$table <- DT::renderDataTable({
      datatable(cool_in)
    })
    
    n_cool(n)
    
  }) |> bindEvent(c(input$map_click, input$time), ignoreInit = TRUE)
  
  # output number for value box
  output$n_cool <- renderText(n_cool())
  
  # make data dable output
  
  
  
  
} # SERVER

#----------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
