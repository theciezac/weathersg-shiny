# install.packages("curl", dependencies = TRUE)
library(curl)
library(jsonlite)
library(dplyr)
library(shiny)
library(leaflet)
library(htmltools)

# Set environment of app to Singapore timezone for display of correct time on web page
Sys.setenv(TZ="Asia/Singapore")

# URL prefix of API calls
url.prefix <- "https://api.data.gov.sg/v1/environment/"

temp <- list("url.suffix" = "air-temperature")
rainfall <- list("url.suffix" = "rainfall")
humidity <- list("url.suffix" = "relative-humidity")
wind.dir <- list("url.suffix" = "wind-direction")
wind.speed <- list("url.suffix" = "wind-speed")


# Vector of list of vector readings

reformat.timestamp <- function(raw.timestamp) {
    split.ts <- strsplit(raw.timestamp, "T")
    date <- strsplit(split.ts[[1]][1], "-")
    year <- date[[1]][1]
    month <- date[[1]][2]
    day <- date[[1]][3]
    time <- strsplit(split.ts[[1]][2],"[+]")
    time <- strsplit(time[[1]][1], ":")
    hour <- time[[1]][1]
    minute <- time[[1]][2]
    second <- time[[1]][3]
    reformatted.timestamp <- paste0(day, "/", month, "/", year, " ", hour, ":", minute, ":", second)
}

doLoad <- function () {
    temp <- fromJSON("https://api.data.gov.sg/v1/environment/air-temperature")
    rainfall <- fromJSON("https://api.data.gov.sg/v1/environment/rainfall")
    rh <- fromJSON("https://api.data.gov.sg/v1/environment/relative-humidity")
    wind.dir <- fromJSON("https://api.data.gov.sg/v1/environment/wind-direction")
    wind.speed <- fromJSON("https://api.data.gov.sg/v1/environment/wind-speed")
    
    temp.api.status <<- temp$api_info$status
    rainfall.api.status <<- rainfall$api_info$status
    rh.api.status <<- rh$api_info$status
    wind.dir.api.status <<- wind.dir$api_info$status
    wind.speed.api.status <<- wind.speed$api_info$status
    
    temp.stations.df <- data.frame(temp$metadata$stations, stringsAsFactors = TRUE)
    temp.stations.df$latitude <- temp.stations.df$location$latitude
    temp.stations.df$longitude <- temp.stations.df$location$longitude
    temp.stations.df$location <- NULL
    
    rainfall.stations.df <- data.frame(rainfall$metadata$stations, stringsAsFactors = TRUE)
    rainfall.stations.df$latitude <- rainfall.stations.df$location$latitude
    rainfall.stations.df$longitude <- rainfall.stations.df$location$longitude
    rainfall.stations.df$location <- NULL
    
    rh.stations.df <- data.frame(rh$metadata$stations, stringsAsFactors = TRUE)
    rh.stations.df$latitude <- rh.stations.df$location$latitude
    rh.stations.df$longitude <- rh.stations.df$location$longitude
    rh.stations.df$location <- NULL
    
    wind.dir.stations.df <- data.frame(wind.dir$metadata$stations, stringsAsFactors = TRUE)
    wind.dir.stations.df$latitude <- wind.dir.stations.df$location$latitude
    wind.dir.stations.df$longitude <- wind.dir.stations.df$location$longitude
    wind.dir.stations.df$location <- NULL
    
    wind.speed.stations.df <- data.frame(wind.speed$metadata$stations, stringsAsFactors = TRUE)
    wind.speed.stations.df$latitude <- wind.speed.stations.df$location$latitude
    wind.speed.stations.df$longitude <- wind.speed.stations.df$location$longitude
    wind.speed.stations.df$location <- NULL
    
    stations.df <- merge(temp.stations.df, rainfall.stations.df, 
                         by=c("id", "device_id", "name", "latitude", "longitude"), all=TRUE)
    stations.df <- merge(stations.df, rh.stations.df, 
                         by=c("id", "device_id", "name", "latitude", "longitude"), all=TRUE)
    stations.df <- merge(stations.df, wind.dir.stations.df, 
                         by=c("id", "device_id", "name", "latitude", "longitude"), all=TRUE)
    stations.df <- merge(stations.df, wind.speed.stations.df, 
                         by=c("id", "device_id", "name", "latitude", "longitude"), all=TRUE)
    
    temp.timestamp <- temp$items$timestamp
    rainfall.timestamp <- rainfall$items$timestamp
    rh.timestamp <- rh$items$timestamp
    wind.dir.timestamp <- wind.dir$items$timestamp
    wind.speed.timestamp <- wind.speed$items$timestamp
    
    ref.temp.timestamp <<- reformat.timestamp(temp.timestamp)
    ref.rainfall.timestamp <<- reformat.timestamp(rainfall.timestamp)
    ref.rh.timestamp <<- reformat.timestamp(rh.timestamp)
    ref.wind.dir.timestamp <<- reformat.timestamp(wind.dir.timestamp)
    ref.wind.speed.timestamp <<- reformat.timestamp(wind.speed.timestamp)
    
    temp.readings <- data.frame(temp$items$readings, stringsAsFactors = TRUE)
    temp.readings$temp <- temp.readings$value
    temp.readings$value <- NULL
    
    rainfall.readings <- data.frame(rainfall$items$readings, stringsAsFactors = TRUE)
    rainfall.readings$rainfall <- rainfall.readings$value
    rainfall.readings$value <- NULL
    
    rh.readings <- data.frame(rh$items$readings, stringsAsFactors = TRUE)
    rh.readings$relative.humidity <- rh.readings$value
    rh.readings$value <- NULL
    
    wind.dir.readings <- data.frame(wind.dir$items$readings, stringsAsFactors = TRUE)
    wind.dir.readings$wind.direction <- wind.dir.readings$value
    wind.dir.readings$value <- NULL
    
    wind.speed.readings <- data.frame(wind.speed$items$readings, stringsAsFactors = TRUE)
    wind.speed.readings$wind.speed <- wind.speed.readings$value
    wind.speed.readings$value <- NULL
    
    readings.df <- full_join(temp.readings, rainfall.readings, by="station_id")
    readings.df <- full_join(readings.df, rh.readings, by="station_id")
    readings.df <- full_join(readings.df, wind.dir.readings, by="station_id")
    readings.df <- full_join(readings.df, wind.speed.readings, by="station_id")
    
    merge.df <<- merge(stations.df, readings.df, by.x="id", by.y="station_id")
    
    merge.df$label <<- paste0("<b>", merge.df$id, " (", 
                              merge.df$name, ")</b><br/>",
                              ifelse(is.na(merge.df$temp), "", 
                                     paste0("Temperature: ", merge.df$temp, "°C<br/>")),
                              ifelse(is.na(merge.df$rainfall), "",
                                     paste0("Rainfall: ", merge.df$rainfall, " mm<br/>")),
                              ifelse(is.na(merge.df$relative.humidity), "",
                                     paste0("Relative humidity: ", merge.df$relative.humidity, "%<br/>")),
                              ifelse(is.na(merge.df$wind.dir), "", 
                                     paste0("Wind direction: ", merge.df$wind.direction, "°<br/>")),
                              ifelse(is.na(merge.df$wind.speed), "",
                                     paste0("Wind speed: ", merge.df$wind.speed, " knots<br/>"))
    )
    
    leaflet.mapAll <<- leaflet(data=merge.df, options=leafletOptions(minZoom=11)) %>% 
        addTiles() %>% 
        addMarkers(~longitude, ~latitude, popup=~label)
    
    leaflet.mapTemp <<- leaflet(data=merge.df[!is.na(merge.df$temp),], 
                                options=leafletOptions(minZoom=11)) %>%
        addTiles() %>%
        addCircleMarkers(~longitude, ~latitude,
                         radius = 5,
                         weight = 3,
                         color="black",
                         fillColor = "red",
                         fillOpacity = 1,
                         stroke = TRUE,
                         label=~htmlEscape(paste0(temp, "°C")), 
                         labelOptions = labelOptions(noHide=TRUE))
    
    leaflet.mapRainfall <<- leaflet(data=merge.df[!is.na(merge.df$rainfall),], 
                                    options=leafletOptions(minZoom=11)) %>%
        addTiles() %>%
        addCircleMarkers(~longitude, ~latitude,
                         radius = 5,
                         weight = 3,
                         color="black",
                         fillColor = "blue",
                         fillOpacity = 1,
                         stroke = TRUE,
                         label=~htmlEscape(paste0(rainfall, "mm")), 
                         labelOptions = labelOptions(noHide=TRUE))
    
    leaflet.mapRH <<- leaflet(data=merge.df[!is.na(merge.df$relative.humidity),], 
                              options=leafletOptions(minZoom=11)) %>%
        addTiles() %>%
        addCircleMarkers(~longitude, ~latitude,
                         radius = 5,
                         weight = 3,
                         color="black",
                         fillColor = "blue",
                         fillOpacity = 1,
                         stroke = TRUE,
                         label=~htmlEscape(paste0(relative.humidity, "%")), 
                         labelOptions = labelOptions(noHide=TRUE))
    
    wind.icon <<- awesomeIcons(
        icon = "arrow-alt-circle-up",
        iconColor = "blue",
        library = "fa"
    )
    
    # leaflet.mapWind <<- leaflet(data=merge.df[(!is.na(merge.df$wind.dir) | 
    #                                                !is.na(merge.df$wind.speed)),], 
    #                           options=leafletOptions(minZoom=11)) %>%
    #     addTiles() %>%
    #     addMarkers(~longitude, ~latitude,
    #                      icon = wind.icon
    #                      label=~htmlEscape(paste0(wind.speed, " knots")), 
    #                      labelOptions = labelOptions(noHide=TRUE))
    
    update.times <<- data.frame("type" = c("Temperature", "Rainfall", "Relative humidity", "Wind direction", "Wind speed"), "time" = c(ref.temp.timestamp, ref.rainfall.timestamp, ref.rh.timestamp, ref.wind.dir.timestamp, ref.wind.speed.timestamp))
    
}

doLoad()

ui <- fluidPage(
    # App title
    titlePanel(
        h2("Realtime Weather Readings Across Singapore", align="center"),
        windowTitle = "Realtime Weather Readings Across Singapore"
    ),
    tags$style(type = "text/css", 
               "#map {
                    height: calc(100vh - 220px) !important;
               }
               
               .side {
                    font-size: 11px;
               }"),
    fluidRow(
        column(3,
               wellPanel(
                   div(align="center", actionButton("refresh", "Refresh"), br()),
                   div(class="side", align="center", htmlOutput("refreshTime"), br(),
                       "Last update times: ",
                       tableOutput("updateTime")),
                   # p(htmlOutput("updateTime")),
                   div(align="center", class="side", 
                       p(align="center", "Times displayed are in Singapore time.", br(), 
                         "Source: ", 
                         a(href="https://data.gov.sg/dataset/realtime-weather-readings", "data.gov.sg")))
               )),
        column(9, tabsetPanel(type= "tabs",
                              tabPanel("All Stations", leafletOutput("plotAll")),
                              tabPanel("Temperature", leafletOutput("plotTemp")),
                              tabPanel("Rainfall", leafletOutput("plotRainfall")),
                              tabPanel("Relative Humidity", leafletOutput("plotRH"))#,
                              # tabPanel("Wind", leafletOutput("plotWind"))
        ))
        # column(9, leafletOutput("map"))
    )
)

server <- function(input, output) {
    output$refreshTime <- renderUI({
        HTML(paste("Last refreshed at", format(Sys.time(), "%d/%m/%Y %H:%M:%S")))
    })
    
    output$updateTime <- renderTable(update.times, hover=TRUE, spacing="xs", colnames=FALSE, na="Not available")
    
    output$plotAll <- renderLeaflet(leaflet.mapAll)
    output$plotTemp <- renderLeaflet(leaflet.mapTemp)
    output$plotRainfall <- renderLeaflet(leaflet.mapRainfall)
    output$plotRH <- renderLeaflet(leaflet.mapRH)
    # output$plotWind <- renderLeaflet(leaflet.mapWind)
    
    observeEvent(input$refresh, {
        doLoad()
        output$refreshTime <- renderUI({
            HTML(paste("Last refreshed at", format(Sys.time(), "%d/%m/%Y %H:%M:%S")))
        })
        output$updateTime <- renderTable(update.times, hover=TRUE, spacing="xs", colnames=FALSE, na="Not available")
        output$plotAll <- renderLeaflet(leaflet.mapAll)
        output$plotTemp <- renderLeaflet(leaflet.mapTemp)
        output$plotRainfall <- renderLeaflet(leaflet.mapRainfall)
        output$plotRH <- renderLeaflet(leaflet.mapRH)
        # output$plotWind <- renderLeaflet(leaflet.mapWind)
    })
}

shinyApp(ui=ui,server=server)