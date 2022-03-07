### Bare-bones app for "Visualization of Data in Space and Time"
### South Coast UseR March Meeting
###
### Josh Cullen
### March 3, 2022


library(shiny)
library(dygraphs)
library(xts)
library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)



###################
#### Load data ####
###################

beard <- read.csv("Bearded_Seals.csv")
sese <- read.csv("Southern_Elephant_Seals.csv")


data <- beard %>%   #rename as 'data' so more general for code
  rename(id = ptt, date = date_time)
epsg <- 3571  #need to select for given projection of data





###################
#### Define UI ####
###################

ui <- fluidPage(
             sidebarLayout(
                        sidebarPanel(selectInput('animal_id', label = 'Select an ID',
                                                 choices = unique(data$id),
                                                 selected = unique(data$id)[1]),
                                     selectInput('var', label = 'Select a Variable',
                                                 choices = names(data)[names(data) != "id"],
                                                 selected = names(data)[names(data) != "id"][1])
                                     ),  #close sidebarPanel
                        mainPanel(dygraphOutput("lineplot"),
                                  leafletOutput('map')
                                  )  #close mainPanel
                      )  #close sidebarLayout
             
             )  #close fluidPage






#######################
#### Define Server ####
#######################

server <- function(input, output, session) {
    
    ### Modify data
    data$id<- as.character(data$id)  #convert from factor or numeric if needed
    data$date<- lubridate::as_datetime(data$date)  #ensure that in datetime format
    
    
    ### Make reactive data by ID (from selection in sidebar)
    dat.filt <- reactive({
      d<- data[data$id == input$animal_id, ]  #filters by ID
      return(d)
    })
    

    
    ### Generate plot of variable time series (from selection in sidebar)
    output$lineplot<- renderDygraph({
      
      dygraph(data = xts(x = dat.filt()[,input$var], order.by = dat.filt()$date)) %>%  #add data
        dySeries(label = input$var, strokeWidth = 1.5) %>%  #add label for var when hovering
        dyAxis("y", label = input$var, axisLabelFontSize = 16, axisLabelWidth = 75) %>%
        dyRangeSelector() %>%  #adds additional time series plot at very bottom
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, colors = "black") %>%  #aesthetics
        dyLegend(width = 270) %>%  #width of "legend" when hovering over time series
        dyUnzoom() %>%  #adds button to zoom out instead of needing to double-click
        dyCrosshair(direction = "vertical")  #adds bar to trace along w/ mouse
      
    })
    
    
    
    #######################################################
    ### Filter dat.filt() based on selected time window ###
    #######################################################
    
    # requires updates to dat.filt() by change in selected ID or in new time window selection before being triggered
    dat.filt.time<- eventReactive(list(dat.filt(), input$lineplot_date_window), {
      req(input$lineplot_date_window)  #to prevent warning from 'if' expression below
      
      # define start and end times for filtering the data
      start<- strptime(input$lineplot_date_window[[1]], format = "%Y-%m-%dT%H:%M:%S",
                     tz = tz(data$date))
      end<- strptime(input$lineplot_date_window[[2]], format = "%Y-%m-%dT%H:%M:%S",
                   tz = tz(data$date))
      
      # subset dat.filt() by time window
      if (start == min(dat.filt()$date) & end == max(dat.filt()$date)) {
        dat.filt()
      } else {
        subset = dplyr::filter(dat.filt(), date >= start & date <= end)
        return(subset)
      }
    }) %>% 
      debounce(millis = 500)  #add delay so map doesn't hang up
    
    
    
    ### Add basemap and greyed-out full track
    output$map <- renderLeaflet({
      
      # convert dat.filt() to sf object
      dat.filt.sf<- sf::st_as_sf(dat.filt(), coords = c("x","y"), crs = epsg) %>%
        sf::st_cast("LINESTRING")
      
      # create base leaflet map
      leaflet(data = dat.filt.sf) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%  #add satellite basemap
        addPolylines(lng = as.numeric(sf::st_coordinates(dat.filt.sf)[,1]),  #x coords
                     lat = as.numeric(sf::st_coordinates(dat.filt.sf)[,2]),  #y coords
                     weight = 2,
                     color = "lightgrey",
                     opacity = 0.4) %>%
        addScaleBar()  #add dynamic scale bar
    })
    
    
    
    ### Add highlighted track segment filtered by dygraph selection
    observe({
      
      req(dat.filt.time())  #Do this if dat.filt.time() is not null
      
      # convert dat.filt() to sf object; to replace when using clearShapes()
      dat.filt.sf<- sf::st_as_sf(dat.filt(), coords = c("x","y"), crs = epsg) %>%
        sf::st_cast("LINESTRING")
      
      # Track w/in dygraph time window
      df.sf<- sf::st_as_sf(dat.filt.time(), coords = c("x","y"), crs = epsg) %>%
        sf::st_cast("LINESTRING")
      
      # First point of filtered track
      df.start.pt<- sf::st_as_sf(dat.filt.time(), coords = c("x","y"), crs = epsg) %>%
        dplyr::slice(1)
      
      # Last point of filtered track
      df.end.pt<- sf::st_as_sf(dat.filt.time(), coords = c("x","y"), crs = epsg) %>%
        dplyr::slice(n())
       
      
      # Clear old selection on map and add new selection
      leafletProxy('map') %>%  #makes updates to basemap
        clearShapes() %>%  #clear the previously highlighted track segment
        clearMarkers() %>%  #clear the start and end points
        fitBounds(as.numeric(sf::st_bbox(df.sf)[1]),
                  as.numeric(sf::st_bbox(df.sf)[2]),  #define new extent of map
                  as.numeric(sf::st_bbox(df.sf)[3]),
                  as.numeric(sf::st_bbox(df.sf)[4])) %>%
        addPolylines(lng = as.numeric(sf::st_coordinates(dat.filt.sf)[,1]),  #add full track
                     lat = as.numeric(sf::st_coordinates(dat.filt.sf)[,2]),
                     weight = 2,
                     color = "lightgrey",
                     opacity = 0.4) %>%
        addPolylines(lng = as.numeric(sf::st_coordinates(df.sf)[,1]),  #add highlighted track segment of interest
                     lat = as.numeric(sf::st_coordinates(df.sf)[,2]),
                     weight = 2,
                     color = "darkturquoise",
                     opacity = 0.8) %>%
        addCircleMarkers(data = df.start.pt,  #add starting location as point
                         fillColor = "#5EF230",
                         stroke = FALSE,
                         fillOpacity = 0.8) %>%
        addCircleMarkers(data = df.end.pt,  #add ending location as point
                         fillColor = "red",
                         stroke = FALSE,
                         fillOpacity = 0.8)
      
    })
    
  }




#################
#### Run App ####
#################

shinyApp(ui = ui, server = server)

