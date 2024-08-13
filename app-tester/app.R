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
library(mapgl)
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
    selectInput(inputId = "mode", label = "Travel Mode", choices = c("driving", "walking","cycling"), selected = "walking"),
    sliderInput(inputId = "time", label = "Time (minutes)", min = 5, max = 30, step = 5, value = 20)
    #verbatimTextOutput("feature_info")
  ),
  value_box(
    title = "# Cooling Centers Within Range",
    value = textOutput("n_cool"),
    theme = "bg-blue"
  ),
  #  height = 100
  mapboxglOutput("map"),
  dataTableOutput("table",height = 20)
)



#----------------------------------------------------
# Define server 
#----------------------------------------------------


server <- function(input, output) {
  
  n_cool <- reactiveVal(NULL)
  
  
  # base map with cooling center locations
  output$map <- renderMapboxgl({
    mapboxgl(center = c(-104.9, 39.7), zoom = 9) |>
      add_circle_layer(id = "centers", 
                       source = cool, 
                       circle_color = "#8ab8d6",
                       circle_stroke_color = "#4c2f1a",
                       circle_stroke_width = 2,
                       circle_radius = 5,
                       hover_options = list(circle_color = "magenta",
                                            circle_radius = 8)
      )
  })
  
  
  # When user clicks compute isochrone and update map
  observe({
    click <- input$map_click
    
    isochrone <- mb_isochrone(
      location = c(click$lng, click$lat),
      time = input$time,  
      profile = input$mode
    )
    
    # click location as sf point feature
    clicked_loc <- sf::st_point(c(click$lng, click$lat))
    
    # calculate how many cooling centers are within isochrone
    cool_in <- sf::st_filter(cool, isochrone)
    n <- cool_in |> nrow()
    n_cool(n)
    
    # data table of cooling centers within isochrone
    output$table <- renderDataTable({
      DT::datatable(cool_in)
    })
    
    
    # Update the map with the new marker and isochrone
    mapboxgl_proxy("map") |>
      clear_layer("isochrone") |>
      clear_markers() |>
      add_fill_layer(
        source = isochrone,
        id = "isochrone",
        fill_color = "grey",
        fill_outline_color = "black",
        fill_opacity = 0.5
      ) |>
      add_markers(clicked_loc) |>
      fit_bounds(isochrone, animate = TRUE)
    #}
    
  }) |> 
    bindEvent(c(input$map_feature_click, input$mode, input$time), ignoreInit = TRUE)
  
  output$n_cool <- renderText(n_cool())
  
  output$feature_info <- renderPrint({
    input$map_click
  })
}

#----------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
