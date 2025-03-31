library(dplyr)
library(sf)
library(bcmaps)
library(readxl)
library(shiny)
library(leaflet)
library(tidyr)
library(viridisLite)
library(shinycssloaders)
library(memoise)
library(bslib)
library(plotly)
library(RColorBrewer)
library(shinyjs)
library(leaflet.extras)

# Pre-processing####
#data_2019_20 <- read.csv("cchs-2019-2020.csv", numerals = "warn.loss")
#data_2017_18 <- read.csv("cchs-2017-2018.csv", numerals = "warn.loss")
#data_2015_16 <- read.csv("cchs-2015-2016.csv", numerals = "warn.loss")

#bc_2019_20 <- data_2019_20 %>%
#  filter(GEODGHR4 >= 59911 & GEODGHR4 <= 59953)

#bc_2017_18 <- data_2017_18 %>%
#  filter(GEODGHR4 >= 59911 & GEODGHR4 <= 59953)

#bc_2015_16 <- data_2015_16 %>%
#  filter(GEODGHR4 >= 59911 & GEODGHR4 <= 59953)

#saveRDS(bc_2019_20, "bc_2019_20.rds")
#saveRDS(bc_2017_18, "bc_2017_18.rds")
#saveRDS(bc_2015_16, "bc_2015_16.rds")


#cleaning dataset

bc_2019_20 <- readRDS("bc_2019_20.rds")
bc_2017_18 <- readRDS("bc_2017_18.rds")
bc_2015_16 <- readRDS("bc_2015_16.rds")

#recoding ages to new bins (note that the bins for 2019-2020 are ages 12-27, 18-34, 35-49, 50-64, 65+)
bc_2017_18 <- bc_2017_18 %>%
  mutate(DHHGAGE_new = case_when(
    DHHGAGE %in% c(3,4) ~ 1,      #ages 10-19
    DHHGAGE %in% c(5,6,7) ~ 2,    #ages 20-34
    DHHGAGE %in% c(8,9,10) ~ 3,   #ages 35-49
    DHHGAGE %in% c(11,12,13) ~ 4, #ages 50-64
    DHHGAGE >13 ~ 5,              #ages 65+
    TRUE ~ NA_real_
  )) %>%
  subset(!is.na(DHHGAGE_new))

bc_2015_16 <- bc_2015_16 %>%
  mutate(DHHGAGE_new = case_when(
    DHHGAGE %in% c(3,4) ~ 1,      #ages 10-19
    DHHGAGE %in% c(5,6,7) ~ 2,    #ages 20-34
    DHHGAGE %in% c(8,9,10) ~ 3,   #ages 35-49
    DHHGAGE %in% c(11,12,13) ~ 4, #ages 50-64
    DHHGAGE >13 ~ 5,              #ages 65+
    TRUE ~ NA_real_
  )) %>%
  subset(!is.na(DHHGAGE_new))

bc_2019_20 <- bc_2019_20 %>% rename(DHHGAGE_new = DHHGAGE) #renaming column so function below works

#new data frames
clean_data <- function(x) { #function to clean data frames
  x %>%
    select(GEODGHR4, DHHGAGE_new, DHH_SEX, PHC_005, PHC_010, PHC_020, PHC_035) %>%
    rename(service_area = GEODGHR4) %>%
    mutate(health_auth = case_when(
      service_area %in% c(59911, 59912, 59913, 59914) ~ "Interior",
      service_area %in% c(59921, 59922, 59923) ~ "Fraser",
      service_area %in% c(59931, 59932, 59933) ~ "Vancouver Coastal",
      service_area %in% c(59941, 59942, 59943) ~ "Vancouver Island",
      service_area %in% c(59951, 59952, 59953) ~ "Northern"
    )) %>%
    mutate(health_area = case_when(
      service_area == 59911 ~ "East Kootenay",
      service_area == 59912 ~ "Kootenay Boundary",
      service_area == 59913 ~ "Okanagan",
      service_area == 59914 ~ "Thompson Cariboo Shuswap",
      service_area == 59921 ~ "Fraser East",
      service_area == 59922 ~ "Fraser North",
      service_area == 59923 ~ "Fraser South",
      service_area == 59931 ~ "Richmond",
      service_area == 59932 ~ "Vancouver",
      service_area == 59933 ~ "North Shore/Coast Garibaldi",
      service_area == 59941 ~ "South Vancouver Island",
      service_area == 59942 ~ "Central Vancouver Island",
      service_area == 59943 ~ "North Vancouver Island",
      service_area == 59951 ~ "Northwest",
      service_area == 59952 ~ "Northern Interior",
      service_area == 59953 ~ "Northeast",
    )) %>%
    mutate(
      age_group = case_when(
        DHHGAGE_new == 1 ~ "10-19",
        DHHGAGE_new == 2 ~ "20-34",
        DHHGAGE_new == 3 ~ "35-49",
        DHHGAGE_new == 4 ~ "50-64",
        DHHGAGE_new == 5 ~ "65+",
        TRUE ~ "Unknown"
      )) %>%
    mutate(
      gender = case_when(
        DHH_SEX == 1 ~ "Male",
        DHH_SEX == 2 ~ "Female",
        TRUE ~ "Unknown")
    )
}

bc_2015_16_clean <- clean_data(bc_2015_16)
bc_2017_18_clean <- clean_data(bc_2017_18)
bc_2019_20_clean <- clean_data(bc_2019_20)

#consolidating data frames
bc_2015_16_clean <- bc_2015_16_clean %>%
  mutate(year = "2015/2016")
bc_2017_18_clean <- bc_2017_18_clean %>%
  mutate(year = "2017/2018")
bc_2019_20_clean <- bc_2019_20_clean %>%
  mutate(year = "2019/2020")

bc_all <- bind_rows(bc_2015_16_clean, bc_2017_18_clean, bc_2019_20_clean)

#cleaning PHC_005
phc005_ha <- bc_all %>%
  filter(PHC_005 %in% c(1, 2)) %>%
  group_by(year, health_auth, gender, age_group) %>%
  summarize(
    count_yes = sum(PHC_005 == 1, na.rm = TRUE),
    count_no = sum(PHC_005 == 2, na.rm = TRUE),
    percent = count_yes / (count_yes + count_no),
    .groups = "drop"
  )

phc005_hsda <- bc_all %>%
  filter(PHC_005 %in% c(1, 2)) %>%
  group_by(year, health_area, gender, age_group) %>%
  summarize(
    count_yes = sum(PHC_005 == 1, na.rm = TRUE),
    count_no = sum(PHC_005 == 2, na.rm = TRUE),
    percent = count_yes / (count_yes + count_no),
    .groups = "drop"
  )

#cleaning PHC_010
phc010_ha <- bc_all %>%
  filter(PHC_005 == 1, PHC_010 %in% 1:6) %>%
  group_by(year, health_auth, gender, age_group, PHC_010) %>%
  summarize(count = n(), .groups = "drop")

phc010_hsda <- bc_all %>%
  filter(PHC_005 == 1, PHC_010 %in% 1:6) %>%
  group_by(year, health_area, gender, age_group, PHC_010) %>%
  summarize(count = n(), .groups = "drop")

#cleaning PHC_020
phc020_ha <- bc_all %>%
  filter(PHC_020 %in% c(1, 2)) %>%
  group_by(year, health_auth, gender, age_group) %>%
  summarize(
    count_yes = sum(PHC_020 == 1, na.rm = TRUE),
    count_no = sum(PHC_020 == 2, na.rm = TRUE),
    percent = count_yes / (count_yes + count_no),
    .groups = "drop"
  )

phc020_hsda <- bc_all %>%
  filter(PHC_020 %in% c(1, 2)) %>%
  group_by(year, health_area, gender, age_group) %>%
  summarize(
    count_yes = sum(PHC_020 == 1, na.rm = TRUE),
    count_no = sum(PHC_020 == 2, na.rm = TRUE),
    percent = count_yes / (count_yes + count_no),
    .groups = "drop"
  )

#cleaning PHC_035
phc035_ha <- bc_all %>%
  filter(PHC_005 == 1, PHC_035 %in% 1:6) %>%  # include only answers 1-6 when PHC_005 is 1
  group_by(year, health_auth, gender, age_group, PHC_035) %>%
  summarize(count = n(), .groups = "drop")

phc035_hsda <- bc_all %>%
  filter(PHC_005 == 1, PHC_035 %in% 1:6) %>%
  group_by(year, health_area, gender, age_group, PHC_035) %>%
  summarize(count = n(), .groups = "drop")


#binary PHC_035
phc035_binary_ha <- bc_all %>%
  filter(PHC_005 == 1, PHC_035 %in% 1:7) %>%  # Include only valid responses 1-7
  mutate(long_wait = ifelse(PHC_035 >= 4, 1, 0)) %>%  # 1-3 short wait, 4-7 long wait
  group_by(year, health_auth, gender, age_group) %>%
  summarize(
    count_yes = sum(long_wait == 1, na.rm = TRUE),
    count_no = sum(long_wait == 0, na.rm = TRUE),
    percent = count_yes / (count_yes + count_no),
    .groups = "drop"
  )

phc035_binary_hsda <- bc_all %>%
  filter(PHC_005 == 1, PHC_035 %in% 1:7) %>%  # Include only valid responses 1-7
  mutate(long_wait = ifelse(PHC_035 >= 4, 1, 0)) %>%
  group_by(year, health_area, gender, age_group) %>%
  summarize(
    count_yes = sum(long_wait == 1, na.rm = TRUE),
    count_no = sum(long_wait == 0, na.rm = TRUE),
    percent = count_yes / (count_yes + count_no),
    .groups = "drop"
  )

# Create a function to pre-calculate all combinations for each phc005 and phc020
preprocess_measure_data <- function(base_data, geo_level_col) {
  # Calculate aggregates for all combinations of gender and age
  all_combos <- base_data %>%
    # For all data (no gender or age filter)
    group_by(year, !!sym(geo_level_col)) %>%
    summarize(
      gender = "All",
      age_group = "All",
      count_yes = sum(count_yes),
      count_no = sum(count_no),
      percent = sum(count_yes) / (sum(count_yes) + sum(count_no)),
      .groups = "drop"
    ) %>%
    # Combine with gender-only filters
    bind_rows(
      base_data %>%
        group_by(year, !!sym(geo_level_col), gender) %>%
        summarize(
          age_group = "All",
          count_yes = sum(count_yes),
          count_no = sum(count_no),
          percent = sum(count_yes) / (sum(count_yes) + sum(count_no)),
          .groups = "drop"
        )
    ) %>%
    # Combine with age-only filters
    bind_rows(
      base_data %>%
        group_by(year, !!sym(geo_level_col), age_group) %>%
        summarize(
          gender = "All",
          count_yes = sum(count_yes),
          count_no = sum(count_no),
          percent = sum(count_yes) / (sum(count_yes) + sum(count_no)),
          .groups = "drop"
        )
    ) %>%
    # Add the original data (specific gender and age combinations)
    bind_rows(
      base_data %>%
        group_by(year, !!sym(geo_level_col), gender, age_group) %>%
        summarize(
          count_yes = sum(count_yes),
          count_no = sum(count_no),
          percent = sum(count_yes) / (sum(count_yes) + sum(count_no)),
          .groups = "drop"
        )
    )

  return(all_combos)
}

# Apply preprocessing to each dataset
phc005_ha_processed <- preprocess_measure_data(phc005_ha, "health_auth")
phc005_hsda_processed <- preprocess_measure_data(phc005_hsda, "health_area")
phc020_ha_processed <- preprocess_measure_data(phc020_ha, "health_auth")
phc020_hsda_processed <- preprocess_measure_data(phc020_hsda, "health_area")
phc035_binary_ha_processed <- preprocess_measure_data(phc035_binary_ha, "health_auth")
phc035_binary_hsda_processed <- preprocess_measure_data(phc035_binary_hsda, "health_area")



#bc ha & hsda data
ha <- st_transform(health_ha(), crs = 4326) #health_ha calls in map data; st_transform transforms it to WGS84 format
hsda <- st_transform(health_hsda(), crs = 4326)

# Simplify geometry to reduce complexity
ha <- st_simplify(ha, dTolerance = 300)  # Adjust tolerance to smooth
hsda <- st_simplify(hsda, dTolerance = 300)

#Create lookup tables for each combination of geo level and measure
create_spatial_lookup <- function() {
  # Create ha lookups
  ha_phc005_lookup <- list()
  ha_phc020_lookup <- list()
  ha_phc035_lookup <- list()

  # Create hsda lookups
  hsda_phc005_lookup <- list()
  hsda_phc020_lookup <- list()
  hsda_phc035_lookup <- list()

  # Populate the lookups for each year, gender, and age group combination
  years <- c("2015/2016", "2017/2018", "2019/2020")
  genders <- c("All", "Male", "Female")
  age_groups <- c("All", "10-19", "20-34", "35-49", "50-64", "65+")

  for (y in years) {
    for (g in genders) {
      for (a in age_groups) {
        # Create a unique key for this combination
        key <- paste(y, g, a, sep = "_")

        # Filter and join for HA and HSDA variables
        ha_phc005_lookup[[key]] <- ha %>%
          left_join(
            filter(phc005_ha_processed, year == y, gender == g, age_group == a),
            by = c("HLTH_AUTHORITY_NAME" = "health_auth")
          )
        ha_phc020_lookup[[key]] <- ha %>%
          left_join(
            filter(phc020_ha_processed, year == y, gender == g, age_group == a),
            by = c("HLTH_AUTHORITY_NAME" = "health_auth")
          )

        ha_phc035_lookup[[key]] <- ha %>%
          left_join(
            filter(phc035_binary_ha_processed, year == y, gender == g, age_group == a),
            by = c("HLTH_AUTHORITY_NAME" = "health_auth")
          )

        hsda_phc005_lookup[[key]] <- hsda %>%
          left_join(
            filter(phc005_hsda_processed, year == y, gender == g, age_group == a),
            by = c("HLTH_SERVICE_DLVR_AREA_NAME" = "health_area")
          )

        hsda_phc020_lookup[[key]] <- hsda %>%
          left_join(
            filter(phc020_hsda_processed, year == y, gender == g, age_group == a),
            by = c("HLTH_SERVICE_DLVR_AREA_NAME" = "health_area")
          )
        hsda_phc035_lookup[[key]] <- hsda %>%
          left_join(
            filter(phc035_binary_hsda_processed, year == y, gender == g, age_group == a),
            by = c("HLTH_SERVICE_DLVR_AREA_NAME" = "health_area")
          )
      }
    }
  }

  return(list(
    ha_phc005 = ha_phc005_lookup,
    ha_phc020 = ha_phc020_lookup,
    ha_phc035 = ha_phc035_lookup,
    hsda_phc005 = hsda_phc005_lookup,
    hsda_phc020 = hsda_phc020_lookup,
    hsda_phc035 = hsda_phc035_lookup
  ))
}

# Generate the lookup tables
spatial_lookups <- create_spatial_lookup()


# Memoize the function
create_spatial_lookup_memoised <- memoise(create_spatial_lookup)
spatial_lookups <- create_spatial_lookup_memoised()

 #save into rds file if needed
#   saveRDS(spatial_lookups, "spatial_lookups.rds")

#import rds file
#spatial_lookups <- readRDS("spatial_lookups.rds")

#create color pallete
color_comb <- c("#a6cee3", "#1f78b4", "#33a02c","#fb9a99","#fdbf6f","#ff7f00","#b2df8a","#6a3d9a","#d9d9d9","#ffff99","#fdb462","#e0f3f8","#fb8072","#d9d9d9","#b3de69","#f46d43","#4575b4")






# Shiny app####
ui <- page_navbar(
  theme = bs_theme(version = 5, bootswatch = "minty", primary = "#808080"), #theme colors
  title = "Healthcare Access Trends in British Columbia",
  header = tagList(useShinyjs(),
                    tags$head(
                      tags$style(HTML("
                      .leaflet-control-gps a{ /*changes gps icon size */
                      width: 26px !important; 
                      height: 26px !important;
                      }
                      .leaflet-bottom.leaflet-left .leaflet-control { /* makes the bottom left a flex column to reorganize the icons (also reduce margin to fix spacing) */
                      margin-top: 0 !important; }
                      .leaflet-bottom.leaflet-left {
                      display: flex;
                      flex-direction: column;
                      gap: 10px;
                      align-items: flex-start !important;
                      }
                      .leaflet-control-search { order: 1 !important; } 
                      .leaflet-control-gps    { order: 2 !important; } 
                      .leaflet-control-zoom   { order: 3 !important; } 
                                      ")
                                 )
                      )
                   ),
  nav_spacer(),
  nav_panel(title = "Map",
            page_fillable(
              tags$style(HTML("
              #map-container {
              position: relative;
              margin: -16px -16px -5px -16px;
              padding: 0;
              min-width: 600px;
              height: 103.25%;
              }
              #map {
              height: 100%;
              min-width: 600px;
              min-height: 800px;
              }
              body {
              min-width: 600px
              } 
                              ")),
              div(id = "map-container",
                  leafletOutput("map", width = "100%", height = "100%")
                  
                  ,
                  absolutePanel(
                    fixed = FALSE, top = 25, left = 25, width = 325, draggable = F, id = "map_side_panel",
                                wellPanel(style = "background-color: rgba(241, 242, 243, 0.90);", #makes the absolute panel anti-flash white
                                          selectInput("selected_year", "Year",
                                                      choices = c("2015/2016",
                                                                  "2017/2018",
                                                                  "2019/2020"),
                                                      selected = "2015/2016"),
                                          selectInput(
                                            "measure",
                                            label = tagList("Measure",
                                                            actionLink("measure_info", label = NULL, 
                                                                       icon = icon("info-circle"),
                                                                       style = "align-items: baseline ")),
                                            choices = c("Appointment Wait Time >3 Days" = "PHC_035",
                                                        "Immediate Care Available" = "PHC_005",
                                                        "Has a Regular Provider" = "PHC_020"
                                            ),
                                            selected = "PHC_035"),
                                          selectInput("gender_filter", "Gender",
                                                      choices = c("All", "Male", "Female"),
                                                      selected = "All"),
                                          selectInput("age_filter", "Age Group",
                                                      choices = c("All", "10-19", "20-34", "35-49", "50-64", "65+"),
                                                      selected = "All"),
                                          radioButtons("geo_level", "Geographic Boundaries",
                                                       choices = c("Health Authority" = "HA",
                                                                   "Health Service Delivery Area" = "HSDA"),
                                                       selected = "HA"),
                                          actionButton("hide_button","Hide panel", class = "btn btn-light")
                                )
                  ),
                  absolutePanel(fixed = FALSE, top = 25, left = 25, id = "show_panel_container", style = "display:none;", #panel for 'show' button
                                actionButton("show_button", "Show panel", class = "btn btn-dark"))
              )
            )
  )
  
  ,
  nav_panel(title = "Charts",
            page_fluid(
              fluidRow(
                column(3,
                       wellPanel(
                         selectInput("graph_measure",
                                     label = tagList("Measure",
                                                     actionLink("measure_info", label = NULL, 
                                                                icon = icon("info-circle"),
                                                                style = "align-items: baseline")),
                                     choices = c("Appointment Wait Time >3 Days" = "PHC_035",
                                                 "Immediate Care Available" = "PHC_005",
                                                 "Has a Regular Provider" = "PHC_020"),
                                     selected = "PHC_035"),
                         selectInput("graph_gender", "Gender",
                                     choices = c("All", "Male", "Female"),
                                     selected = "All"),
                         selectInput("graph_age", "Age Group",
                                     choices = c("All", "10-19", "20-34", "35-49", "50-64", "65+"),
                                     selected = "All"),
                         radioButtons("graph_geo_level", "Geographic Boundaries",
                                      choices = c("Health Authority" = "HA",
                                                  "Health Service Delivery Area" = "HSDA"),
                                      selected = "HA"),
                         radioButtons("x_toggle", "X-Axis:", 
                                      choices = c("Geographic Boundaries", "Year"),
                                      selected = "Geographic Boundaries")
                       )
                ),
                column(9,
                       card(withSpinner(plotlyOutput("bar_plot", height = "600px")))
                )
              )
            )
  ),
  nav_spacer(),
  nav_item(actionLink("infoButton", label = "Information", icon = icon("info-circle"))),
  
)

server <- function(input, output, session) {
  
  #### Map server code####
  pop_data <- reactive({
    req(input$selected_year, input$gender_filter, input$age_filter,
        input$measure, input$geo_level)
    
    # Create the lookup key
    key <- paste(input$selected_year, input$gender_filter, input$age_filter, sep = "_")
    
    # Return the pre-joined data
    if (input$geo_level == "HA") {
      if (input$measure == "PHC_005") {
        return(spatial_lookups$ha_phc005[[key]])
      } else if (input$measure == "PHC_020") {
        return(spatial_lookups$ha_phc020[[key]])
      } else { # PHC_035
        return(spatial_lookups$ha_phc035[[key]])
      }
    } else { # HSDA
      if (input$measure == "PHC_005") {
        return(spatial_lookups$hsda_phc005[[key]])
      } else if (input$measure == "PHC_020") {
        return(spatial_lookups$hsda_phc020[[key]])
      } else { # PHC_035
        return(spatial_lookups$hsda_phc035[[key]])
      }
    }
  })
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, zoomSnap = 0.75, zoomDelta = 0.75, minZoom = 4.5)) %>%
      addTiles(options = tileOptions(minZoom = 4.5)) %>%
      setView(lng = -128, lat = 53.00, zoom = 5.6) %>%
      setMaxBounds(lng1 = -180, lat1 = 10, lng2 = -65, lat2 = 80) %>%
      addSearchOSM(options = searchOptions( #search address
        position = "bottomleft",      
        autoCollapse = TRUE,     
        minLength = 2,           # Minimum characters before searching
        zoom = 12                
      )) %>%
      addControlGPS(options = gpsOptions(
        position = "bottomleft",     
        setView = TRUE,          # Move map view to user location when found
        autoCenter = TRUE,       
        maxZoom = 12,            
        activate = FALSE         
      )) %>% 
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        // adjusts zoom control to bottom left.
        L.control.zoom({ position: 'bottomleft' }).addTo(map);
        // Function to recenter the map.
        function recenter() {
          map.invalidateSize();  // Refresh map dimensions.
          map.setView([53.00, -128], 5.6);  // Re-center the map.
        }
        // Listen for window resize events.
        window.addEventListener('resize', recenter);
      }
    ")
  })

  # Update map
  observe({
    data <- pop_data()
    req(data) # Ensure data is loaded
    
    #Color palette
    pal <- colorNumeric("viridis", domain = c(0, 1.5), reverse = TRUE, na.color = "transparent")
    
    # Function to get PHC_Q010 data for the region
    get_phc010_data <- function(region_name, is_ha = TRUE) {
      # Select appropriate dataframe based on geographic level
      if(is_ha) {
        df <- phc010_ha %>%
          filter(year == input$selected_year, health_auth == region_name)
        
        if(input$gender_filter != "All") {
          df <- df %>% filter(gender == input$gender_filter)
        }
        
        if(input$age_filter != "All") {
          df <- df %>% filter(age_group == input$age_filter)
        }
      } else {
        df <- phc010_hsda %>%
          filter(year == input$selected_year, health_area == region_name)
        
        if(input$gender_filter != "All") {
          df <- df %>% filter(gender == input$gender_filter)
        }
        
        if(input$age_filter != "All") {
          df <- df %>% filter(age_group == input$age_filter)
        }
      }
      
      # Aggregate data by PHC_010 response
      phc010_agg <- df %>%
        group_by(PHC_010) %>%
        summarize(count = sum(count), .groups = "drop")
      
      # Add labels for PHC_010 responses
      phc010_agg$response_label <- case_when(
        phc010_agg$PHC_010 == 1 ~ "Doctor's office",
        phc010_agg$PHC_010 == 2 ~ "Hospital outpatient clinic",
        phc010_agg$PHC_010 == 3 ~ "Community health centre",
        phc010_agg$PHC_010 == 4 ~ "Walk-in clinic",
        phc010_agg$PHC_010 == 5 ~ "Hospital emergency room",
        phc010_agg$PHC_010 == 6 ~ "Some other place",
        TRUE ~ "Unknown"
      )
      
      return(phc010_agg)
    }
    
    # Add function to get PHC_035 detailed data
    get_phc035_data <- function(region_name, is_ha = TRUE) {
      # Select appropriate dataframe based on geographic level
      if(is_ha) {
        df <- phc035_ha %>%
          filter(year == input$selected_year, health_auth == region_name)
        
        # Age and gender filters
        if(input$gender_filter != "All") {
          df <- df %>% filter(gender == input$gender_filter)
        }
        if(input$age_filter != "All") {
          df <- df %>% filter(age_group == input$age_filter)
        }
      } else { #else use HSDA data
        df <- phc035_hsda %>%
          filter(year == input$selected_year, health_area == region_name)
        
        if(input$gender_filter != "All") {
          df <- df %>% filter(gender == input$gender_filter)
        }
        if(input$age_filter != "All") {
          df <- df %>% filter(age_group == input$age_filter)
        }
      }
      
      # Aggregate data by PHC_035 response
      phc035_agg <- df %>%
        group_by(PHC_035) %>%
        summarize(count = sum(count), .groups = "drop")
      
      # Add labels for PHC_035 responses
      phc035_agg$response_label <- case_when(
        phc035_agg$PHC_035 == 1 ~ "Same day",
        phc035_agg$PHC_035 == 2 ~ "Next day",
        phc035_agg$PHC_035 == 3 ~ "2-3 days",
        phc035_agg$PHC_035 == 4 ~ "4-6 days",
        phc035_agg$PHC_035 == 5 ~ "1-2 weeks",
        phc035_agg$PHC_035 == 6 ~ "2 weeks - 1 month",
        TRUE ~ "Unknown" # Note: Original code used 1:6, but binary used 1:7. Assuming 1:6 is for detailed breakdown.
      )
      
      # Add binary grouping
      phc035_agg$wait_group <- ifelse(phc035_agg$PHC_035 <= 3, "≤3 days", ">3 days")
      
      return(phc035_agg)
    }
    
    # Generate click popup on map
    popup_contents <- lapply(1:nrow(data), function(i) {
      row <- data[i,]
      is_special_na_case <- FALSE # Flag for the specific NA case
      
      if(input$geo_level == "HA") {
        region_name <- row$HLTH_AUTHORITY_NAME
        is_ha <- TRUE
      } else {
        region_name <- row$HLTH_SERVICE_DLVR_AREA_NAME
        is_ha <- FALSE
        # Check for the specific NA cases
        if (input$selected_year == "2019/2020" && region_name == "Northeast" && is.na(row$percent)) {
          is_special_na_case <- TRUE
        }
      }
      
      # name for popup table
      measure_name <- case_when(
        input$measure == "PHC_005" ~ "Immediate Care Available",
        input$measure == "PHC_020" ~ "Has a Regular Provider",
        input$measure == "PHC_035" ~ "Appointment Wait Time >3 days",
        TRUE ~ input$measure
      )
      
      percent_display_line <- if (is_special_na_case) {
        "Percent: Not available<br>"
      } else if (is.na(row$percent)) {
        "Percent: NA<br>" # Handles other potential NAs
      } else {
        paste0("Percent: ", round(row$percent * 100, 1), "%<br>")
      }
      
      counts_display_lines <- if (is_special_na_case) { #lines for NA tables
        "Yes, count: Not available<br>No, count: Not available<br>Total responses: Not available"
      } else {
        # Handle potential NAs in counts 
        paste0("Yes, count: ", ifelse(is.na(row$count_yes), "NA", row$count_yes), "<br>",
               "No, count: ", ifelse(is.na(row$count_no), "NA", row$count_no), "<br>",
               "Total responses: ", ifelse(is.na(row$count_yes + row$count_no), "NA", row$count_yes + row$count_no))
      }
      
      # popup content content
      basic_info <- paste0(
        "<strong>", region_name, "</strong><br>",
        "Measure: ", measure_name, "<br>",
        "Year: ", input$selected_year, "<br>",
        percent_display_line,   
        counts_display_lines    
      )
      
        # Add PHC_010 and PHC_035 table only if data is available
      if (!is_special_na_case) {
        if(input$measure == "PHC_005") {
          phc010_data <- get_phc010_data(region_name, is_ha)
          total_phc010 <- sum(phc010_data$count, na.rm = TRUE) # Ensure NA counts don't break sum
          
          # Check if there's actually data to display
          if(nrow(phc010_data) > 0 && total_phc010 > 0) {
            # Create a HTML table for PHC_010 responses
            phc010_html <- "<br><br><strong>For those who have a place for immediate care, what kind of place is it:</strong><br>"
            phc010_html <- paste0(phc010_html, "<table style='width:100%; border-collapse:collapse; margin-top:10px;'>")
            phc010_html <- paste0(phc010_html, "<tr><th style='text-align:left;'>Response</th><th style='text-align:right;'>Count</th><th style='text-align:right;'>%</th></tr>")
            
            for(j in 1:nrow(phc010_data)) {
              row_data <- phc010_data[j,]
              # Avoid division by zero if total is somehow 0 despite nrow > 0
              pct <- if(total_phc010 > 0) round((row_data$count / total_phc010) * 100, 1) else 0
              phc010_html <- paste0(
                phc010_html,
                "<tr><td style='border-top:1px solid #ddd; padding:4px 0;'>", row_data$response_label,
                "</td><td style='border-top:1px solid #ddd; text-align:right; padding:4px 0;'>", row_data$count,
                "</td><td style='border-top:1px solid #ddd; text-align:right; padding:4px 0;'>", pct, "%</td></tr>"
              )
            }
            phc010_html <- paste0(phc010_html, "</table>")
            basic_info <- paste0(basic_info, phc010_html)
          }
        } else if(input$measure == "PHC_035") {
          # PHC_035 breakdown
          phc035_data <- get_phc035_data(region_name, is_ha)
          total_phc035 <- sum(phc035_data$count, na.rm = TRUE)
          
          if(nrow(phc035_data) > 0 && total_phc035 > 0) {
            # Create a HTML table for PHC_035 responses
            phc035_html <- "<br><br><strong>Wait time for a regular health care provider:</strong><br>"
            phc035_html <- paste0(phc035_html, "When seeking care for a minor health problem, how long is the wait before getting an appointment.<br>")
            phc035_html <- paste0(phc035_html, "<table style='width:100%; border-collapse:collapse; margin-top:10px;'>")
            phc035_html <- paste0(phc035_html, "<tr><th style='text-align:left;'>Response</th><th style='text-align:right;'>Count</th><th style='text-align:right;'>%</th><th style='text-align:center;'>Category</th></tr>")
            
            for(j in 1:nrow(phc035_data)) {
              row_data <- phc035_data[j,]
              pct <- if(total_phc035 > 0) round((row_data$count / total_phc035) * 100, 1) else 0
              phc035_html <- paste0(
                phc035_html,
                "<tr><td style='border-top:1px solid #ddd; padding:4px 0;'>", row_data$response_label,
                "</td><td style='border-top:1px solid #ddd; text-align:right; padding:4px 0;'>", row_data$count,
                "</td><td style='border-top:1px solid #ddd; text-align:right; padding:4px 0;'>", pct, "%",
                "</td><td style='border-top:1px solid #ddd; text-align:center; padding:4px 0;'>", row_data$wait_group, "</td></tr>"
              )
            }
            phc035_html <- paste0(phc035_html, "</table>")
            basic_info <- paste0(basic_info, phc035_html)
          }
        }
      } else {
        basic_info <- paste0(basic_info, "<br><br><i>Detailed data not available for this area and year.</i>") #custom label for northeast
      }
      
      return(paste0("<div style='min-width:300px; max-width:350px;'>", basic_info, "</div>"))
    }) 
    
    
    
    #leaflet map proxy info
    leafletProxy("map", data = data) %>%
      clearShapes() %>% #clears out polygons
      addPolygons(
        fillColor = ~pal(percent),
        fillOpacity = 0.7,
        weight = 1,
        color = "#444444",
        smoothFactor = 0.3,
        # Mouseover tooltip
        label = ~{
          # Determine the correct name for this row based on the single input$geo_level value
          current_region_name <- if (input$geo_level == "HA") {
            HLTH_AUTHORITY_NAME
          } else {
            HLTH_SERVICE_DLVR_AREA_NAME 
          }
          
          # Mouseover display text
          display_text <- ifelse(is.na(percent),
                                 # if Northeast region, then we use this.
                                 ifelse(input$geo_level == "HSDA" &
                                          input$selected_year == "2019/2020" &
                                          HLTH_SERVICE_DLVR_AREA_NAME == "Northeast", 
                                        "Not available", 
                                        "NA"),          
                                 paste0(round(percent * 100, 1), "%")
          )
          
          paste0(current_region_name, ": ", display_text)
        },
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        # Use the HTML popup content 
        popup = popup_contents,
        popupOptions = popupOptions(
          maxWidth = 350,
          minWidth = 300
        ),
        # Add highlight on mouseover
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      clearControls() %>%  # Clear previous legends
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(0,1), # Domain for legend
        title = "Percent",
        labFormat = labelFormat(
          transform = function(x) round(x * 100, 1),
          suffix = "%"
        ),
        opacity = 0.7,
        bins = seq(0, 1, by = 0.2),
        na.label = "Not available / NA" # NA label in legend
      )
  }) 
  
  observeEvent(input$hide_button, {
    hide("map_side_panel")
    show("show_panel_container")
  })
  
  observeEvent(input$show_button, {
    show("map_side_panel")
    hide("show_panel_container")
  })

  
  
  ####Graph server code ####
  #selecting data frame
  bar_data <- reactive({
    req(input$graph_measure, input$graph_gender, input$graph_age, input$graph_geo_level)
    
    if (input$graph_geo_level == "HA") {
      if (input$graph_measure == "PHC_005") {
        base_data <- phc005_ha_processed
      } else if (input$graph_measure == "PHC_020") {
        base_data <- phc020_ha_processed
      } else {
        base_data <- phc035_binary_ha_processed
      }
      geo_col <- "health_auth"
    } else {
      if (input$graph_measure == "PHC_005") {
        base_data <- phc005_hsda_processed
      } else if (input$graph_measure == "PHC_020") {
        base_data <- phc020_hsda_processed
      } else {
        base_data <- phc035_binary_hsda_processed
      }
      geo_col <- "health_area"
    }
    
    # Filter the data based on selected gender and age group
    filtered_data <- base_data %>%
      filter(gender == input$graph_gender, age_group == input$graph_age) %>%
      mutate(percent_display = round(percent * 100, 1)) # Convert to percentage for display
    
    return(list(data = filtered_data, geo_col = geo_col))
  })
  
  # Render bar plot
  output$bar_plot <- renderPlotly({
    req(input$x_toggle)
    plot_data <- bar_data()
    
    # Get the appropriate title based on the selected measure
    measure_title <- case_when(
      input$graph_measure == "PHC_005" ~ "Immediate Care Available",
      input$graph_measure == "PHC_020" ~ "Has a Regular Provider",
      input$graph_measure == "PHC_035" ~ "Appointment Wait Time >3 Days",
      TRUE ~ input$graph_measure
    )
    
    
    # Create a plotly graph; if statement to choose plot to show
    if (input$x_toggle == "Geographic Boundaries"){
      
      p <- plot_data$data %>%
        plot_ly(x = ~get(plot_data$geo_col), 
                y = ~percent_display, 
                color = ~year, 
                colors = color_comb,
                type = "bar",
                hoverinfo = "text",
                text = ~paste(get(plot_data$geo_col), "<br>Year:", year, 
                              "<br>Percent:", percent_display, "%",
                              "<br>Yes, count:", count_yes,
                              "<br>No, count:", count_no)) %>%
        layout(
          title = paste0(measure_title, " (Gender: ", input$graph_gender, ", Age: ", input$graph_age, ")"),
          xaxis = list(title = ifelse(input$graph_geo_level == "HA", "Health Authority", "Health Service Delivery Area"),
                       tickangle = 45),
          yaxis = list(title = "Percentage (%)", range = c(0, 100)),
          barmode = "group",
          hovermode = "closest",
          legend = list(title = list(text = "Year"))
        )
    }
    else {
      p <- plot_data$data %>%
        plot_ly(x = ~year, 
                y = ~percent_display, 
                color = ~get(plot_data$geo_col), 
                colors = color_comb,
                type = "bar",
                hoverinfo = "text",
                text = ~paste(get(plot_data$geo_col), "<br>Year:", year, 
                              "<br>Percent:", percent_display, "%",
                              "<br>Yes, count:", count_yes,
                              "<br>No, count:", count_no)) %>%
        layout(
          title = paste0(measure_title, " (Gender: ", input$graph_gender, ", Age: ", input$graph_age, ")"),
          xaxis = list(title = ifelse(input$graph_geo_level == "HA", "Health Authority", "Health Service Delivery Area"),
                       tickangle = 45),
          yaxis = list(title = "Percentage (%)", range = c(0, 100)),
          barmode = "group",
          hovermode = "closest",
          legend = list(title = list(text = "Year"))
        )
      
      return(p)
    }
  })
  
  #modal (pop-up) windows####
  
  # Popup for measure descriptions in the absolute panel
  observeEvent(input$measure_info, {
    showModal(modalDialog(
      title = "Measure Descriptions",
      HTML("
        <strong>Appointment Wait Time >3 Days:</strong> If a respondent requires immediate care for a minor health problem, is the wait time for an appointment longer than 3 days. <br><br>
        <strong>Immediate Care:</strong> When respondents require immediate care for a minor health problem, do they have a place to go to? If they do, what sort of place is it? <br><br>
        <strong>Regular Provider:</strong> Indicates whether respondents have a regular health care provider.
        <br><br> <i>Minor health problem: fever, headache, a sprained ankle, vomiting or an unexplained rash. </i>
      "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  #information panel
  info_modal <- modalDialog(title = "Understanding Healthcare Access in British Columbia",
                            HTML("
                            <h5> Current State of Care </h5>
                            <p>Primary care is a key first point of contact with British Columbia's health care system and plays a vital role in patient outcomes. Yet timely access to these services continues to be a significant concern for residents throughout the province. Many British Columbians face extended wait times for appointments, struggle to establish relationships with regular healthcare providers, or lack knowledge of where to seek immediate care when needed. These fundamental aspects of healthcare access vary significantly across regions, demographics, and service types, creating potential inequities in our healthcare system.</p>
                            <h5>About This Dashboard</h5>
                            <p> The goal of this dashboard is to offer a comprehensive view of healthcare access patterns throughout the province using data from the <b> Canadian Community Health Survey (CCHS)</b>. The dashboard visualizes key indicators of primary care accessibility, including appointment wait times, access to regular healthcare providers, and availability of immediate care options. By understanding these patterns, we can:</p>
                            <ul>
                            <li>Identify underserved regions and populations</li>
                            <li>Support targeted improvements in healthcare delivery</li>
                            <li>Inform resource allocation and policy development</li>
                            </ul>
                            <h5>Data Source and Methodology</h5>
                            <p>The data comes from CCHS cycles 2015/2016, 2017/2018, and 2019/2020<sup>1</sup>. The CCHS is a cross-sectional survey that collects information related to health status, healthcare utilization, and health determinants for the Canadian population. For the 2019/2020 cycle, no data was collected from the Northeast health service delivery area. The analyses focus specifically on British Columbia respondents and questions related to appointment waiting periods, regular provider relationships, and access to immediate care facilities. </p>
                            <h6>References</h6>
                            <ol style = 'font-size: 15px;'>
                            <li>Statistics Canada.<i> Canadian Community Health Survey: Public Use Microdata File, 2015/2016, 2017/2018, and 2019/2020</i>. Ottawa, ON. doi:<a href=' https://doi.org/10.25318/82m0013x-eng'> https://doi.org/10.25318/82m0013x-eng</a> </li>
                            </ol>
                            <br>
                            <i>Developed by Kenneth Zhang, Master of Public Health.<br>
                            Contact: <a href ='mailto:kkzhang@sfu.ca'>kkzhang@sfu.ca</a></i>
                            "
                            ),
                            footer = modalButton("Close"),
                            size = "l",
                            easyClose = T)
  
  showModal(info_modal) #opens up the window on start-up
  
  observeEvent(input$infoButton, { # button toggle for info modal
    showModal(info_modal)
  })
  
}



shinyApp(ui, server)



