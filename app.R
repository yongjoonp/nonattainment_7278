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
# library(leaflet.extras2)



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
nonatt_tab <- fread(sprintf("%s/nonatt_status_tabulation.csv", data_path))

dt[, county_state := paste0(county, ", ", state)]
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
    .fontsize-adjuster {font-size:20px;}
    .map-container {margin-bottom: 20px;}
    .title-container {text-align: center; font-size: 28px; font-weight: bold; color: #ffffff; background-color: #881c1c; padding: 8 px 15px; border-radius: 10px; border: 2px solid #000000; margin-top: 10px}
    .footnote-container{
      text-align: center; font-size: 1em; font-weight: bold; color: #ddd; background-color: #881c1c; padding: 4 px 15px; margin-top: 10px
    }
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
        padding-left: 50px;
        padding-right: 50px;
        -webkit-transform: scale(1.3);
      }
     input[type='radio-inline']{ /* style for checkboxes */
        width: 100px; /*Desired width*/
        height: 100px; /*Desired height*/
        line-height: 100px; 
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
    column(6, div(h2("Economic Condition"), tableOutput("DisplayEconVars"), "Please click on a county on the map to view its economic conditions.", class = "card-container fontsize-adjuster", style = "display: flex; flex-direction: column; align-items: center; text-align: center;")),

    column(6, div(h2(textOutput("tab_title")), tableOutput("DisplayNACounts"), class = "card-container fontsize-adjuster", style = "display: flex; flex-direction: column; align-items: center; text-align: center;"))
  ),
  

  fluidRow(
    column(12, div("This Shiny application is developed by Yongjoon Park (yongjoonpark@umass.edu) and Ben Heep (benheep@gmail.com) based on the working paper, 'Geographic Resolution in Environmental Policy: EPA's Shift from Regions to Counties Under the Clean Air Act' by Cropper et al 2025.", class = "footnote-container"))
  )

)

filter_out_data_as_fcn <- function(yy, pollutant){
  which_col <- sprintf("na%d_%s", yy - 1900, tolower(pollutant))
  final_out <- dt[, .(GEOID, county, state, county_state, nonatt_status = get(which_col))]
  final_out[, nonatt_status := ifelse(nonatt_status == 0, "Attainment", "Nonattainment")]
      
  final_out <- left_join(final_out, usgeo, by = c("GEOID")) %>% st_as_sf()
  final_out$geometry <- st_zm(final_out$geometry, drop = T, what = "ZM")
  final_out <- sf::st_transform(final_out, "+proj=longlat +datum=WGS84")

  final_out

}

drawing_leaplet_map <- function(..obs, yy){

  
  if(yy == 1972){
    out_port <- "map_1972"
  } else{
    out_port <- "map_1978"
  }

  leafletProxy(out_port, data = ..obs) %>%
    clearShapes() %>%
    clearControls() %>%
    setView(-96, 37.8, 4.5) %>%
    # addSpinner() %>%
    # startSpinner(options = list("lines" = 7, "length" = 20)) %>%
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
      label = ..obs$county_state,
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
    #   %>%
    # stopSpinner()

}

server <- function(input, output, session) {
  
  # data filtering based on year and pollutant
  filtered_data_1972 <- reactive({
    filter_out_data_as_fcn(1972, input$pollutant)
  })

  filtered_data_1978 <- reactive({
    filter_out_data_as_fcn(1978, input$pollutant)
  })

  filtered_na_count <- reactive({
    nonatt_tab[poll == tolower(input$pollutant)]
  })
  
  # basic settings for map 1972
  output$map_1972 <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl = FALSE,
      zoomControl = TRUE,
      scrollWheelZoom = TRUE,
      dragging = FALSE,
      touchZoom = TRUE,
      doubleClickZoom = FALSE,
      zoomSnap = 0.25, zoomDelta=0.25, 
      minZoom = 4,
      maxZoom = 8
    )) 
  }) 

  # basic settings for map 1978
  output$map_1978 <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl = FALSE,
      zoomControl = TRUE,
      scrollWheelZoom = TRUE,
      dragging = FALSE,
      touchZoom = TRUE,
      doubleClickZoom = FALSE,
      zoomSnap = 0.25, zoomDelta=0.25,
      minZoom = 4,
      maxZoom = 8
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
    output$tab_title <- renderText({sprintf("Number of Counties by %s Nonattainment Status", input$pollutant)})
    }
  )
  
  
  observe({
    
    output$DisplayNACounts <- renderTable({
      dcast(filtered_na_count(), (`Nonattainment Status` = nonatt_status) ~ year, value.var = "N")
    }, rownames = FALSE)

    # plot the 1972 map
    obs_1972 <- filtered_data_1972()
    drawing_leaplet_map(obs_1972, 1972)

    # plot the 1978 map
    obs_1978 <- filtered_data_1978()
    drawing_leaplet_map(obs_1978, 1978)
  })

  observe({
    req(selected_fips())

    county_data <- unique(pop[fips == selected_fips(), .(state, county, year = designation_year, population, total_emp_CBP = as.integer(total_emp_CBP), dmfg_emp = as.integer(dmfg_emp))][order(year)])
    colnames(county_data) <- c("State", "County", "Year", "Population", "Total Employment", "Dirty Manufacturing Employment")
    
    output$DisplayEconVars <- renderTable({
      county_data
    }, rownames = FALSE)
  })

}


shinyApp(ui, server)