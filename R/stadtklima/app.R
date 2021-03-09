## Stadtklima interactive mapping tool
## Corinna Grobe, Statistisches Amt Kanton Zürich (corinna.grobe@statistik.ji.zh.ch), last updated 29 December 2020

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("ploty", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# Loading data
tageswerte = readRDS("C:/gitrepos/AWEL_Interaktive_Stadtklima_Auswertungen/output/tageswerte_enriched.RDS")
meteo_schweiz = readRDS("C:/gitrepos/AWEL_Interaktive_Stadtklima_Auswertungen/output/meteo_schweiz.RDS")

# Date range
min_date <- as.Date(min(tageswerte$date),"%Y-%m-%d")
max_date <- as.Date(max(tageswerte$date),"%Y-%m-%d")

# Color scales for three legends (temperature, spatial position, exposure to sun)
diverging20 <- c("#071e46", "#072f6b", "#08529c", "#2171b5", "#4292c7", "#5aa0cd", "#78bfd6", "#aadce6", "#dbf5ff", "#f0fcff",
                 "#fff0f5", "#ffe0e0", "#fcbbaa", "#fc9272", "#fb6a4a", "#f03c2b", "#cc181e", "#a60f14", "#780a0f", "#5f0000")

col_raum <- c("#43aa8b", "#f3722c", "#f94144", "#90be6d", "#f9c74f")

col_strahlung <- c("#43aa8b", "#f9c74f", "#f94144")

# Color palettes to color scales
pal_tmin <- colorBin(palette = diverging20,
                   bins = c(-999, -35, -30, -25, -20, -15, -12, -9, -6, -3, 0, 3, 6, 9, 12, 15, 20, 25, 30, 35, 999),
                   domain = tageswerte$T_min,
                   na.color = "#808080" )

pal_tmax <- colorBin(palette = diverging20,
                        bins = c(-999, -35, -30, -25, -20, -15, -12, -9, -6, -3, 0, 3, 6, 9, 12, 15, 20, 25, 30, 35, 999),
                        domain = tageswerte$T_max,
                        na.color = "#808080" )

pal_raum <- colorFactor(palette = col_raum,
                     domain = tageswerte$Raumlage,
                     na.color = "#808080",
                     ordered = TRUE)

pal_strahlung <- colorFactor(palette = col_strahlung,
                             domain = tageswerte$Strahlungssituation,
                             na.color = "#808080",
                             ordered = TRUE)

# Defining legend labels for temperature
labels <- c("< -35 °C", "-35 bis -30 °C", "-30 bis -25 °C", "-25 bis -20 °C", "-20 bis -15 °C", "-15 bis -12 °C",
            "-12 bis -9 °C", "-9 bis -6 °C", "-6 bis -3 °C", "-3 bis 0 °C", "0 bis 3 °C", "3 bis 6 °C", 
            "6 bis 9 °C", "9 bis 12 °C", "12 bis 15 °C", "15 bis 20 °C", "20 bis 25 °C", "25 bis 30 °C",
            "30 bis 35 °C", "> 35 °C")


### SHINY UI ###
ui <- bootstrapPage(
    navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Stadtklima</a>'), id="nav",
               windowTitle = "Stadtklima",
               
               tabPanel("Messnetz",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("map", width="50%", height="100%"),
                            # Adding overlay panel
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, right = 10, width = "48%", fixed=TRUE,
                                          draggable = FALSE, height = "auto",
                                          
                                          # Sensor information
                                          span(tags$i(h5(textOutput("reactive_network_data_site"), align = "left")), style="color:#045a8d"),

                                          # Output reporting year_month
                                          h6(textOutput("selected_date_reactive"), align = "left"),
                                          # Output Messanordnung
                                          h6(textOutput("reactive_network_data_messanordnung"), align = "left"),
                                          # Output Sensortyp
                                          h6(textOutput("reactive_network_data_sensortyp"), align = "left"),
                                          # Output CH KOORDINATEN
                                          h6(textOutput("reactive_network_data_coords"), align = "left"),
                                          # Output Standorthöhe [m. ü. M.]
                                          h6(textOutput("reactive_network_data_masl"), align = "left"),
                                          # Output Sensorhöhe [m. ü. Boden]
                                          h6(textOutput("reactive_network_data_magl"), align = "left"),
                                          # Output Strahlungssituation (schattig, offen, etc.)
                                          h6(textOutput("reactive_network_data_strahlung"), align = "left"),
                                          # Output Raumlage (Stadt, Land, etc.)
                                          h6(textOutput("reactive_network_data_lage"), align = "left"),
                                          # Output Wärmeinsel
                                          h6(textOutput("reactive_network_data_waermeinsel"), align = "left"),

                                          # Plot daily average temperature in the past 365 days (Temperatur, Tagesmittel)
                                          # plotlyOutput("daily_plot", height="130px", width="100%"),
                                          sliderTextInput("selected_date",
                                                          label = h5("Select date"),
                                                          choices = format(unique(tageswerte$date), "%d.%m.%Y"),
                                                          selected = format(max_date, "%d.%m.%Y"),
                                                          grid = FALSE,
                                                          animate=animationOptions(interval = 1000, loop = FALSE))

                                          # sliderInput("selected_date",
                                          #             "Datum wählen",
                                          #             min = min_date,
                                          #             max  = max_date,
                                          #             value = max_date,
                                          #             timeFormat="%d.%m.%Y",
                                          #             animate=animationOptions(interval = 1000, loop = FALSE))
                                          )
                            )
                        ),
               tabPanel("Vergleich",
                        sidebarLayout(
                            sidebarPanel(
                                span(tags$i(h6("Hier kommt notwendiger Erklärungstext")), style="color:#045a8d"),
                                span(tags$i(h6("Zweite Zeile für noch mehr Erklärungstext")), style="color:#045a8d"),
                                # pickerInput("level_select", "Level:",   
                                #             choices = c("Global", "Continent", "Country", "US state"), 
                                #             selected = c("Country"),
                                #             multiple = FALSE),
                                # 
                                # pickerInput("region_select", "Country/Region:",   
                                #             choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
                                #             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                #             selected = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country)[1:10],
                                #             multiple = TRUE), 
                                # 
                                # pickerInput("outcome_select", "Outcome:",   
                                #             choices = c("Deaths per million", "Cases per million", "Cases (total)", "Deaths (total)"), 
                                #             selected = c("Deaths per million"),
                                #             multiple = FALSE),
                                # 
                                # pickerInput("start_date", "Plotting start date:",   
                                #             choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"), 
                                #             options = list(`actions-box` = TRUE),
                                #             selected = "Date",
                                #             multiple = FALSE), 
                                # 
                                # sliderInput("minimum_date",
                                #             "Minimum date:",
                                #             min = as.Date(cv_min_date,"%Y-%m-%d"),
                                #             max = as.Date(current_date,"%Y-%m-%d"),
                                #             value=as.Date(cv_min_date),
                                #             timeFormat="%d %b"),
                                sliderInput("selected_date", 
                                                label = h5("Datum wählen"),
                                            min = min_date,
                                            max  = max_date,
                                            value = max_date,
                                            timeFormat="%d.%m.%Y",
                                                animate=animationOptions(interval = 1000, loop = FALSE)),
                                
                                        "Wählen Sie den Sensor, den Standort und das Startdatum der Aufzeichnung aus den Dropdown-Menüs aus, um die Aufzeichnungen zu aktualisieren."
                                ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Tageswerte", plotlyOutput("daily_plot", width = "100%", height = "500px")),
                                    tabPanel("Stundenwerte",)
                                    )
                                )
                            )
                        ),
               tabPanel("Hitzetage & Tropennächte",
                        ),
               tabPanel("Wärmeinseln",
                        )
               )
    )


### SHINY SERVER ###
server <- function(input, output, session) {
    
    # create a reactive value that will store the click position
    # as described at https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/
    data_of_click <- reactiveValues(clickedMarker=NULL)
    
    # formatted date
    formatted_date = reactive({
        format(as.Date(input$selected_date, format= "%Y-%m-%d"), "%Y-%m-%d")
    })
    
    # store the click on a map marker (sensor)
    observeEvent(input$map_marker_click,{
        data_of_click$clickedMarker <- input$map_marker_click
    })
    
    # Reactive expression for the data subsetted to what the user selected
    # reactive_db = reactive({
    #     tageswerte %>% filter(date == formatted_date())
    # })
    
    reactive_db <- tageswerte %>% filter(date == max_date)
    
    reactive_network_data = reactive({
        tageswerte %>% filter(date == formatted_date() && sensor == data_of_click$clickedMarker$id)
    })
    
    # Leaftlet map with markers and overlay layers
    output$map <- renderLeaflet({ 
        # create base map for Messnetz
        leaflet(tageswerte) %>% 
            # Set default view and zoom level
            # Kanton Zürich: lng: 7.5929 - 9.7490; lat: 47.8095 - 47.0430
            setView(lng = mean(c(7.5929,9.7490)), lat = mean(c(47.8095,47.0430)), zoom = 11) %>% 
            
            # Add layers to the map by using layer functions (e.g. addTiles, addMarkers, addPolygons) to modify the map widget
            # Add default OpenStreetMap map tiles
            addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.35)) %>%
            addProviderTiles(providers$Stamen.TonerLabels) %>% 
            # Layers control
            addLayersControl(position = "bottomright",
                             baseGroups = c("Tagesminimum", "Tagesmaximum", "Raumlage", "Strahlungssituation"),
                             options = layersControlOptions(collapsed = FALSE)) %>% 
            hideGroup(c("Tagesmaximum", "Raumlage", "Strahlungssituation"))
    })
    
    # Incremental changes to the map (in this case, replacing the
    # markers when a new group is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observeEvent(input$map_groups,{
        proxy <- leafletProxy("map", data = reactive_db)
        proxy %>% clearMarkers()
        if(input$map_groups == "Raumlage") {
            proxy %>% addCircleMarkers(lng= ~E, 
                             lat= ~N, 
                             layerId = ~sensor,
                             color = "#011627",
                             fillColor = ~pal_raum(Raumlage), 
                             radius = 8,
                             opacity = 0.25,
                             fillOpacity = 1,
                             group = "Raumlage",
                             label = sprintf("<strong>%s</strong><br/>
                                              Sensor: %1.f<br/>
                                             Raumlage: %s",
                                             reactive_db$site,
                                             reactive_db$sensor,
                                             reactive_db$Raumlage) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#333"),
                                 textsize = "15px", direction = "auto"))
        } else if (input$map_groups == "Tagesminimum") {
            proxy %>% addCircleMarkers(lng= ~E, 
                    lat= ~N, 
                    layerId = ~sensor,
                    color = "#011627",
                    fillColor = ~pal_tmin(T_min), 
                    radius = 8,
                    opacity = 0.25,
                    fillOpacity = 1,
                    group = "Tagesminimum",
                    label = sprintf("<strong>%s (%1.f)</strong><br/>
                                             Datum: %s<br/>
                                             Tagesmittel: %5.1f °C<br/>
                                             Tagesminumim: %5.1f °C<br/>
                                             Tagesmaximum: %5.1f °C<br/>",
                                             reactive_db$site,
                                             reactive_db$sensor,
                                             reactive_db$date,
                                             reactive_db$T_mean,
                                             reactive_db$T_min,
                                             reactive_db$T_max) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#333"),
                                 textsize = "15px", direction = "auto"))
        } else if (input$map_groups == "Strahlungssituation") {
            proxy %>% addCircleMarkers(lng= ~E, 
                                       lat= ~N, 
                                       layerId = ~sensor,
                                       color = "#011627",
                                       fillColor = ~pal_strahlung(Strahlungssituation), 
                                       radius = 8,
                                       opacity = 0.25,
                                       fillOpacity = 1,
                                       group = "Strahlungssituation",
                                       label = sprintf("<strong>%s</strong><br/>
                                              Sensor: %1.f<br/>
                                            Exposition: %s",
                                                       reactive_db$site,
                                                       reactive_db$sensor,
                                                       reactive_db$Strahlungssituation) %>% lapply(htmltools::HTML),
                                       labelOptions = labelOptions(
                                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#333"),
                                           textsize = "15px", direction = "auto"))
        } else { 
            proxy %>% addCircleMarkers(lng= ~E, 
                             lat= ~N, 
                             layerId = ~sensor,
                             color = "#011627",
                             fillColor = ~pal_tmax(T_max), 
                             radius = 8,
                             opacity = 0.25,
                             fillOpacity = 1,
                             group = "Tagesmaximum",
                             label = sprintf("<strong>%s (%1.f)</strong><br/>
                                             Datum: %s<br/>
                                             Tagesmittel: %5.1f °C<br/>
                                             Tagesminumim: %5.1f °C<br/>
                                             Tagesmaximum: %5.1f °C<br/>",
                                             reactive_db$site,
                                             reactive_db$sensor,
                                             reactive_db$date,
                                             reactive_db$T_mean,
                                             reactive_db$T_min,
                                             reactive_db$T_max) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#333"),
                                 textsize = "15px", direction = "auto"))
        }
    })
    
    # dynamic legend rendering
    observeEvent(input$map_groups,{
        proxy <- leafletProxy("map", data = tageswerte)
        proxy %>% clearControls()
        if (input$map_groups == 'Tagesminimum') {
            proxy %>% addLegend("topright",
                                colors =  diverging20,
                                data = tageswerte,
                                values =  ~tageswerte$T_min,
                                labels = labels,
                                title = "Temperatur",
                                opacity = 1,
                                group = c("Tagesminimum")
            )
        }
        else if (input$map_groups == 'Tagesmaximum') {
            proxy %>% addLegend("topright",
                              colors =  diverging20,
                              data = tageswerte,
                              values =  ~tageswerte$T_max,
                              labels = labels,
                              title = "Temperatur",
                              opacity = 1,
                              group = c("Tagesmaximum")
            )
        }
        else if (input$map_groups == 'Raumlage') {
            proxy %>% addLegend("topright",
                              colors =  col_raum,
                              data = tageswerte,
                              values =  ~tageswerte$Raumlage,
                              labels = unique(tageswerte$Raumlage),
                              title = "Raumlage",
                              opacity = 1,
                              group = "Raumlage"
            )
        }
        else if (input$map_groups == 'Strahlungssituation') {
            proxy %>% addLegend("topright",
                              colors =  col_strahlung,
                              data = tageswerte,
                              values =  ~tageswerte$Strahlungssituation,
                              labels = c("schattig", "halbschattig", "sonnig"),
                              title = "Strahlungssituation",
                              opacity = 1,
                              group = "Strahlungssituation"
            )
        }
    })
    
    output$selected_date_reactive <- renderText({
        paste0("Datum: ", formatted_date()) 
    })
    
    output$reactive_network_data_site <- renderText({
        paste0(reactive_network_data()$site, " (ID: ", reactive_network_data()$sensor, ")")
    })
    
    output$reactive_network_data_messanordnung <- renderText({
        paste0("Messanordnung: ", reactive_network_data()$Messanordnung)
    })
    
    output$reactive_network_data_sensortyp <- renderText({
        paste0("Sensortyp: ", reactive_network_data()$Sensortyp)
    })
    
    output$reactive_network_data_coords <- renderText({
        paste0("CH Koordinaten: ", reactive_network_data()$xyKoord)
    })
    
    output$reactive_network_data_masl <- renderText({
        paste0("Standorthöhe: ", reactive_network_data()$`Standorthöhe (masl)`, " m. ü. M.")
    })
    
    output$reactive_network_data_magl <- renderText({
        paste0("Sensorhöhe: ", reactive_network_data()$`Sensorhöhe (magl)`, " m. ü. Boden")
    })
    
    output$reactive_network_data_strahlung <- renderText({
        paste0("Strahlungssituation: ", reactive_network_data()$Strahlungssituation)
    })
    
    output$reactive_network_data_lage <- renderText({
        paste0("Raumlage: ", reactive_network_data()$Raumlage)
    })
    
    output$reactive_network_data_waermeinsel <- renderText({
        paste0("Wärmeinsel: ", reactive_network_data()$`Wärmeinsel`)
    })
    

# TAB Vergleich -----------------------------------------------------------

    # Make a scatterplot of the selected point
    output$monthly_plot=renderPlotly({
        selected_sensor=data_of_click$clickedMarker$id
        if(is.null(selected_sensor)) {}
        else{ p <- ggplot(monatswerte_longer %>% 
                         filter(sensor == selected_sensor), 
                     aes(x = year_month, y = temperatur, group = monatswerte, color = monatswerte)) +
                geom_path() +
                geom_point() +
                theme_stat() +
                theme(legend.position="bottom") +
            scale_color_manual(values = c("monatsmaximum" = "#de7fac","monatsminimum" = "#d9bde2", "monatsmittel" = "#003f5c")) +
            # scale_x_date(labels = scales::date_format("%b %Y")) +
                labs(title = paste(selected_sensor),
                     x = NULL,
                     y = "Temperatur (°C)",
                     color = NULL)  
                ggplotly(p)
            }
        
    })
    
    # Make a line plot of the selected point
    output$daily_plot=renderPlotly({
        selected_sensor=data_of_click$clickedMarker$id
        if(is.null(selected_sensor)) {}
        else{data <- tageswerte %>% 
            filter(sensor == selected_sensor & date >= max(date)-365);
        
            p <- ggplot(data = data, aes(x = as.Date(date))) + 
            # highlighting calibration periods
            # geom_rect(data=tageswerte_enriched %>% filter(sensor == selected_sensor & date >= max(date)-365 & Messanordnung == "Kalibration"), inherit.aes=FALSE,
            #           aes(xmin=as.Date(von),xmax=as.Date(bis),ymin=-Inf,ymax=Inf,
            #               fill = "Kalibration"), alpha=0.2) +
            geom_line(aes(y = T_min), size = 0.25, color = "#ff6b6b") + 
            geom_line(aes(y = T_mean), size = 0.25, color = "#1a535c") +
            geom_line(aes(y = T_max), size = 0.25, color = "#4ecdc4") +
            scale_x_date(breaks = scales::date_breaks("months"),
                         labels = scales::date_format("%b")) +
            scale_y_continuous(limits = c(-10, 45) ) +
            labs(title = paste(data$site, ": ", selected_sensor),
                 x = paste(as.Date(min(data$date)), " bis ", as.Date(max(data$date))),
                 y = "Temperatur (°C)",
                 color = NULL) +
            theme_stat()
        ggplotly(p)
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
