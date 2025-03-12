library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(magrittr)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)




# load data (for Ben)
# dt <- fread("~/Documents/nonattainment1/nonatt_status_72_78.csv")
# usgeo <- st_read("~/Documents/non_attainment_counties_interactive/input/cb_2014_us_county_5m/cb_2014_us_county_5m.shp")
# pop <- fread("~/Documents/nonattainment1/data/data_pop_employment.csv")

# load data (for Yongjoon)
data_path <- "data"
dt <- fread(sprintf("%s/nonatt_status_72_78.csv", data_path))
usgeo <- st_read(sprintf("%s/cb_2014_us_county_5m/cb_2014_us_county_5m.shp", data_path))
aqcr <- st_read(sprintf("%s/aqcr_border/aqcr_border.shp", data_path))
pop <- fread(sprintf("%s/data_pop_employment.csv", data_path))

usgeo$GEOID <- as.numeric(usgeo$GEOID)

mypal <- colorFactor("RdYlBu", levels = c("Nonattainment", "Attainment"))

world <- ne_countries(scale = "medium", returnclass = "sf")
us_boundary <- world %>% filter(admin == "United States of America")
ocean <- st_as_sf(st_make_grid(world, n = 1))  # Large polygon
masked_ocean <- st_difference(ocean, st_union(us_boundary))

ui <- bootstrapPage(
  tags$style(type = "text/css", "
    html, body {width: 100%; height: 100%;}
    .card-container {margin: 10px; padding: 15px; border-radius: 10px; background-color: #f8f9fa; box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1); font-size:30px; text-align: center;}
    .map-container {margin-bottom: 20px;}
    .title-container {text-align: center; font-size: 28px; font-weight: bold; color: #ffffff; background-color: #881c1c; padding: 8 px 15px; border-radius: 10px; border: 2px solid #000000; margin-top: 10px}
    .legend {
      font-size: 16px;
      padding: 10px;
      line-height: 16px !important;
      background-color: light grey;
      border-radius: 8px;
      border: 2px solid black;
    }
    .radio-inline { /* checkbox is a div class*/
        line-height: 20px;        
        padding-left: 150px;
        -webkit-transform: scale(1.5);
      }
     input[type='radio-inline']{ /* style for checkboxes */
        width: 70px; /*Desired width*/
        height: 70px; /*Desired height*/
        line-height: 70px; 
      }
  "),
  
  fluidRow(
    column(12, div("Nonattainment Status Designated by EPA (1972 & 1978)", class = "title-container"))
  ),
  
  fluidRow(    
    column(12, div(radioButtons("pollutant", "Select Pollutant", 
      choices = c("TSP", "CO", "OX", "SO2"), 
      selected = "TSP", inline = TRUE), class = "card-container"))
  ),
  
  fluidRow(
    column(6, div(h2(textOutput("title_1972")), leafletOutput("map_1972", height = "530px"), class = "card-container map-container")),
    column(6, div(h2(textOutput("title_1978")), leafletOutput("map_1978", height = "530px"), class = "card-container map-container"))
  ),
  
  fluidRow(
    column(12, div(h2("Economic Condition"), tableOutput("DisplayEconVars"), class = "card-container", style = "display: flex; flex-direction: column; align-items: center; text-align: center;"))
  )
)


server <- function(input, output, session) {
  
  filtered_data_1972 <- reactive({
    which_col <- sprintf("na72_%s", tolower(input$pollutant))
    final_out <- dt[, .(GEOID, county, state, nonatt_status = get(which_col))]
    final_out[, nonatt_status := ifelse(nonatt_status == 0, "Attainment", "Nonattainment")]
        
    final_out <- left_join(final_out, usgeo, by = c("GEOID")) %>% st_as_sf()
    final_out$geometry <- st_zm(final_out$geometry, drop = T, what = "ZM")
    final_out <- sf::st_transform(final_out, "+proj=longlat +datum=WGS84")

    final_out
  })

  filtered_data_1978 <- reactive({
    
    which_col <- sprintf("na78_%s", tolower(input$pollutant))
    final_out <- dt[, .(GEOID, county, state, nonatt_status = get(which_col))]
    final_out[, nonatt_status := ifelse(nonatt_status == 0, "Attainment", "Nonattainment")]
    #                  by = GEOID]
    
    final_out <- left_join(final_out, usgeo, by = c("GEOID")) %>% st_as_sf()
    final_out$geometry <- st_zm(final_out$geometry, drop = T, what = "ZM")
    final_out <- sf::st_transform(final_out, "+proj=longlat +datum=WGS84")
    
    final_out
  })
  
  # basic settings for map 1972
  output$map_1972 <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl = FALSE,
      zoomControl = FALSE,
      scrollWheelZoom = FALSE,
      dragging = FALSE,
      touchZoom = FALSE,
      doubleClickZoom = FALSE,
      zoomSnap = 0.25, zoomDelta=0.25 # YP: the arguments below help us control how small we can define the zoom level.
    ))
  })

  # basic settings for map 1978
  output$map_1978 <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl = FALSE,
      zoomControl = FALSE,
      scrollWheelZoom = FALSE,
      dragging = FALSE,
      touchZoom = FALSE,
      doubleClickZoom = FALSE,    
      zoomSnap = 0.25, zoomDelta=0.25 # YP: the arguments below help us control how small we can define the zoom level.
    )) 
  })

  selected_fips <- reactiveVal(NULL)
  
  observeEvent(input$map_1972_shape_click, {
    selected_fips(as.numeric(input$map_1972_shape_click$id))
  })
  
  observeEvent(input$map_1978_shape_click, {
    selected_fips(as.numeric(input$map_1978_shape_click$id))
  })

  observeEvent(input$pollutant, {
    output$title_1972 <- renderText({paste(input$pollutant, "Nonattainment Status (1972)")})
    output$title_1978 <- renderText({paste(input$pollutant, "Nonattainment Status (1978)")})
    }
  )
  
  observe({
    req(selected_fips())

    county_data <- unique(pop[fips == selected_fips(), .(state, county, year = designation_year, population, total_emp_CBP = as.integer(total_emp_CBP), dmfg_emp = as.integer(dmfg_emp))][order(year)])
    

    # nonatt_str <- sprintf("%s Nonattainment Status", input$pollutant)
        
    colnames(county_data) <- c("State", "County", "Year", "Population", "Total Employment", "Dirty Manufacturing Employment")
    
    output$DisplayEconVars <- renderTable({
      county_data
    }, rownames = FALSE)
  })
  
  
  observe({
    # send the filtered_data to map1972 (we need the similar code chunk for map_1978 )
    ..obs <- filtered_data_1972()
    leafletProxy("map_1972", data = ..obs) %>%
      clearShapes() %>%
      clearControls() %>%
      setView(-96, 37.8, 4.5) %>%
      addPolygons(
        data = masked_ocean,
        fillColor = "#f8f9fa",
        fillOpacity = 1,
        color = NA
      ) %>%
      addPolygons(stroke = TRUE, color = "black", weight = 0.3, 
        smoothFactor = 0.2, fillOpacity = 0.7,
        fillColor = ~mypal(..obs$nonatt_status),
        layerId = ~GEOID,
        label = paste(
          "County: ", ..obs$county
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "white",
          bringToFront = TRUE
          )) %>%
      addPolygons(data = aqcr, 
                  stroke = TRUE, 
                  color = "black", 
                  weight = 1.5,
                  fill = FALSE,
                  highlightOptions = highlightOptions(
                    bringToFront = TRUE
                  )) %>%
      addLegend(position = "bottomright",
                pal = mypal, 
                values = ~..obs$nonatt_status,
                title = "Non Attainment Status",
                className = "legend")
    
  })

  observe({
    ..obs <- filtered_data_1978()
    leafletProxy("map_1978", data = ..obs) %>%
      clearShapes() %>%
      clearControls() %>%
      setView(-96, 37.8, 4.5) %>%
      addPolygons(
        data = masked_ocean,
        fillColor = "#f8f9fa",
        fillOpacity = 1,
        color = NA
      ) %>%
      addPolygons(stroke = TRUE, color = "black", weight = 0.3, 
                  smoothFactor = 0.2, fillOpacity = 0.7,
                  fillColor = ~mypal(..obs$nonatt_status),
                  layerId = ~GEOID,
                  label = paste(
                    "County: ", ..obs$county
                  ),
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "white",
                    bringToFront = TRUE
                  )) %>%
      addPolygons(data = aqcr, 
                  stroke = TRUE, 
                  color = "black", 
                  weight = 1.5,
                  fill = FALSE,
                  highlightOptions = highlightOptions(
                    bringToFront = TRUE
                  )) %>%
      addLegend(position = "bottomright",
                pal = mypal, 
                values = ~..obs$nonatt_status,
                title = "Non Attainment Status",
                className = "legend")
  })

}


shinyApp(ui, server)