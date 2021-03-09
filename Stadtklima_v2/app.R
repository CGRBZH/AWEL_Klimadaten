## Stadtklima V2
## Corinna Grobe, Statistisches Amt Kanton Zürich (corinna.grobe@statistik.ji.zh.ch), last updated 02. March 2021

##############################################################################
# Libraries
##############################################################################
# require(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(leaflet)
library(htmlTable)
library(ggridges)
library(DT)
library(shinyjs)

##############################################################################
# Data
##############################################################################
tageswerte = readRDS("tageswerte_komplett.RDS")
# meteo_schweiz = readRDS("C:/gitrepos/AWEL_Interaktive_Stadtklima_Auswertungen/output/meteo_schweiz.RDS")

##############################################################################
# Parameters 
##############################################################################
min_date <- as.Date(min(tageswerte$date),"%d.%m.%Y")
max_date <- as.Date(max(tageswerte$date),"%d.%m.%Y")
standorte <- rev(sort(unique(tageswerte$Standort)))
exposition <- unique(tageswerte$Strahlungssituation)
location <- unique(tageswerte$Raumlage)
heatspot <- unique(tageswerte$`Wärmeinsel`) # excluding 'nicht definiert' and 'NA' [c(2:4,6:7)]
summer <- c('Jun', 'Jul', 'Aug')

# Color scales for three legends (temperature (temp), location (loc), exposure to sun (exp))
col_temp <- c("#071e46", "#072f6b", "#08529c", "#2171b5", "#4292c7", "#5aa0cd", "#78bfd6", "#aadce6", "#dbf5ff", "#f0fcff",
                 "#fff0f5", "#ffe0e0", "#fcbbaa", "#fc9272", "#fb6a4a", "#f03c2b", "#cc181e", "#a60f14", "#780a0f", "#5f0000")

col_loc <- c("#F5F5F5", "#43aa8b", "#f3722c", "#f94144", "#90be6d", "#f9c74f")

col_exp <- c("#F5F5F5", "#43aa8b", "#f9c74f", "#f94144")

col_heat <- c("#F5F5F5", "#43aa8b", "#f3722c", "#f94144", "#808080", "#90be6d", "#f9c74f")

# Color palettes to color scales
pal_tmin <- leaflet::colorBin(palette = col_temp,
                     bins = c(-Inf, -35, -30, -25, -20, -15, -12, -9, -6, -3, 0, 3, 6, 9, 12, 15, 20, 25, 30, 35, Inf),
                     domain = tageswerte$T_min,
                     na.color = "#808080" )

pal_tmean <- leaflet::colorBin(palette = col_temp,
                     bins = c(-Inf, -35, -30, -25, -20, -15, -12, -9, -6, -3, 0, 3, 6, 9, 12, 15, 20, 25, 30, 35, Inf),
                     domain = tageswerte$T_mean,
                     na.color = "#808080" )

pal_tmax <- leaflet::colorBin(palette = col_temp,
                     bins = c(-Inf, -35, -30, -25, -20, -15, -12, -9, -6, -3, 0, 3, 6, 9, 12, 15, 20, 25, 30, 35, Inf),
                     domain = tageswerte$T_max,
                     na.color = "#808080" )

pal_loc <- leaflet::colorFactor(palette = col_loc,
                       domain = tageswerte$Raumlage,
                       na.color = "#808080",
                       ordered = TRUE)

pal_exp <- leaflet::colorFactor(palette = col_exp,
                       domain = tageswerte$Strahlungssituation,
                       na.color = "#808080",
                       ordered = TRUE)

pal_heat <- leaflet::colorFactor(palette = col_heat,
                       domain = tageswerte$`Wärmeinsel`,
                       na.color = "#808080",
                       ordered = TRUE)

# Defining legend labels for temperature
labels <- c("< -35 °C", "-35 bis -30 °C", "-30 bis -25 °C", "-25 bis -20 °C", "-20 bis -15 °C", "-15 bis -12 °C",
            "-12 bis -9 °C", "-9 bis -6 °C", "-6 bis -3 °C", "-3 bis 0 °C", "0 bis 3 °C", "3 bis 6 °C", 
            "6 bis 9 °C", "9 bis 12 °C", "12 bis 15 °C", "15 bis 20 °C", "20 bis 25 °C", "25 bis 30 °C",
            "30 bis 35 °C", "> 35 °C")

##############################################################################
# UI Side
##############################################################################
ui <- navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                 HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Stadtklima</a>'), id="nav",
                 windowTitle = "Stadtklima",
                 tabPanel("Messnetz",
                 fluidPage(
                     # tags$head(includeCSS("styles.css")),
                     fluidRow(
                         column(width = 6,
                                box(width = NULL, solidHeader = TRUE,
                                    tags$h4(HTML("<strong>Stadtklima-Messnetz</strong><p>")),
                                    span(HTML("<p>Zur Unterstützung der klimaangepassten Raumplanung betreibt das AWEL im Kanton Zürich seit dem Sommer 2019 ein Sensormessnetz für die Messung von Lufttemperatur und -feuchte. Das Stadtklima-Messnetz umfasste im Sommer 2020 rund 50 Sensoren. Ergänzt wird das Messnetz mit Stationen von «MeteoSchweiz».
                                                     Der MeteoSchweiz Klimanetz-Standort Zürich/Fluntern (SMA) gilt als repräsentativ für die allgemeine Situation im Kanton Zürich.</p>
                                                     <p><strong>Hinweis: </strong><i>Wählen Sie einen Punkt auf der Karte um mehr Informationen zu diesem Standort eingeblendet zu bekommen.</i></p>")),
                                    
                                    # sliderInput("animation", "",
                                    #             min = min_date, max = max_date,
                                    #             value = max_date, step = 1,
                                    #             animate =
                                    #                 animationOptions(interval = 300, loop = TRUE)),
                                    leafletOutput("map", height = 650)
                                )
                                    ),
                         column(width = 6, 
                                box(width = NULL, 
                                    tags$h4(HTML("<strong>Informationen zu diesem Standort</strong>")),
                                    span(HTML("<p>Die Grafik zeigt Tagesmaxima, Tagesmittel und Tagesminima der Lufttemperatur seit Mai 2019 an der auf der Karte ausgewählten Messstelle.</p>
                                                     <p><strong>Hinweis: </strong><i>Fahren Sie über die Grafik um die einzelnen Messwerte zu sehen oder ziehen einen Bereich auf, um das angezeigte Zeitfenster anzupassen.</i></p>")),
                                    plotlyOutput("daily_plot", height = 400),
                                    br()
                                   ),
                                box(width = NULL,
                                    uiOutput("sensor_table", height = 450)
                                )
                         )
                     ))),
                 tabPanel("Standortvergleich",
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  span(h4("Witterung Sommer 2020")),
                                  
                                  span(HTML("<p>Auf den mildesten Winter folgte 2020 der drittwärmste Frühling seit Messbeginn der MeteoSchweiz im Jahr 1864. Der Sommer 2020 zählte zwar nicht zu den extrem heissen, er setzte die Serie der sehr warmen Sommer mit einem Landesmittel von 14,1°C
                                          aber weiter fort. Vor der kräftigen Sommererwärmung ab den 1990-er Jahren gab es nur sehr selten Schweizer Sommer mit über 14°C im landesweiten Mittel (MeteoSchweiz, Klimabulletin 2020).</p>
                                            <p><strong>Beispiel Messstelle Zürich - Kaserne (534):</strong> Nach einem durchschnittlichen Juni hielt sich die Sommerhitze auch im Juli bis zum Monatsende in Grenzen. Im August kam es zu einer länger anhaltenden Hitzewelle.</p>")),
                                  
                                  
                                  pickerInput(inputId = "location_select", label = "Vergleichsstandorte (max. 5):",
                                              choices = standorte,
                                              options = list(`max-options` = 5, # Limit the number of selections
                                                             `max-options-text` = "Maximale Anzahl Standorte gewählt."),
                                              selected = standorte[1:3],
                                              multiple = TRUE),
                                  
                                  pickerInput("preset_select", "Presets:",   
                                              choices = list("",
                                                  `Wärmeinsel Zürich` = c("Bertastrasse // Friedhof Sihlfeld" = "Preset_1", 
                                                             "Uetliberg // Friedhof Sihlfeld // Perron HB" = "Preset_2", 
                                                             "Vulkanplatz // Oerlikerpark" = "Preset_3"),
                                                  `Wärmeinsel Winterthur` = c("Schulhaus-Altstadt // Neumarkt // Turnerstrasse // Elsau-Halden" = "Preset_4")),
                                              selected = NULL,
                                              options = list(`none-selected-text` = "Bitte auswählen!"),
                                              multiple = FALSE),
                                  
                                  pickerInput("temperature_select", "Temperaturwert:",   
                                              choices = c("Tagesminimum" = "T_min", "Tagesmittel" = "T_mean", "Tagesmaximum" = "T_max"), 
                                              selected = c("T_mean"),
                                              multiple = FALSE),
                                  
                                  # pickerInput(inputId = "exposition_select", label = "Nach Lageparametern filtern:",
                                  #             choices = list(
                                  #                 Strahlungssituation = c(exposition),
                                  #                 Raumlage = c(location),
                                  #                 Wärmeinsel = c(heatspot)),
                                  #             options = list(`actions-box` = TRUE, `none-selected-text` = "Bitte auswählen!"),
                                  #             selected = NULL,
                                  #             multiple = TRUE),
                                  
                                  pickerInput(inputId = "periode_select", label = "Zeitraum:",
                                              choices = c("Letzte 7 Tage", "Sommer", "Kalenderjahr", "Jahr (letzte 365 Tage)", "Seit Messbeginn"),
                                              options = list(`actions-box` = TRUE),
                                              selected = c("Jahr (letzte 365 Tage)"),
                                              multiple = FALSE),
                                  
                                  span(HTML("<p><strong>Hinweise zu den Daten:</strong></p>
                                            <p><strong>Verfügbare Standorte: </strong>Es stehen nur Standorte mit mindestens 90% Messtagen pro Monat zu Auswahl.</p>
                                            <p><strong>Hitzetage: </strong>T_max >= 30°C</p>
                                            <p><strong>Tropennächte: </strong>T_min >= 20°C</p>
                                            <p><strong>Sommer: </strong>Die Monate Juni, Juli und August gelten als Sommerperiode.</p>"))
                              ),
                              
                              mainPanel(
                                  tags$h4(HTML("<strong>Standortvergleich</strong>")),
                                  span(tags$i("Wählen Sie die Standorte, den Zeitraum für die Aufzeichnung und die Temperaturwerte aus den Dropdown-Menüs links aus, um Grafik und Tabelle zu aktualisieren.")),
                                  
                                  # verbatimTextOutput(outputId = "picker_exposition"),
                                  plotlyOutput("comparison_plot", width = "100%", height = "500px"),
                                  br(),
                                  uiOutput("comparison_table", width = "100%", height = "350px")                              )
                          )),
                 tabPanel("Hitzetage & Tropennächte",
                          sidebarLayout(
                              sidebarPanel(
                                  tags$h4(HTML("<strong>Hinweise zur Grafik</strong>")),
                                  span(HTML("<p>Die Abbildung zeigt die Anzahl Hitzetage und Tropennächte pro Raumlage über alle Sensoren während der <i>Sommermonate (Juni, Juli und August) 2019 und 2020</i>.</p>
                                            <p>Die Boxplots zeigen stets Minumum und Maximum (Punkte), 5 und 95 Protent Perzentile (Whisker) sowie den Median (Linie in der Box). Die Box umfasst das untere (25%) und das obere (75%) Quartil.</p>")),                                 
                                  img(src = "https://www.r-graph-gallery.com/img/other/boxplot_explanation.png", width = 500),
                                  br(),
                                  span(HTML("<p>Die Tabelle zeigt die meisten Messorte mit der zugehörigen Anzahl Hitzetage und Tropennächte für die Sommermonate 2019 und 2020. 
                                        Da einige der Standorte sonnenexponierter sind, erwärmen sich die Sensoren an solchen Standorten tagsüber im Extremfall deutlich. Aus diesem Grund ist die Anzahl Hitzetage mit Vorsicht zu interpretieren.</p>
                                            <p>Noch wichtiger als die Hitzebelastung tagsüber sind die nächtlichen Temperaturen für die Gesundheit der Bevölkerung. Der Mensch kann hohe Temperaturen tagsüber relativ gut ertragen, wenn es sich während der Nacht erholen kann. Die Werte der Anzahl Tropennächte sind somit gut belastbar - die Sensoren liefern ohne Strahlungseinfluss präzise Daten.</p>")),
                                  br(),
                                  tags$h4(HTML("<strong>Lesebeispiel</strong>")),
                                  span(HTML("<p>An der Raumlage «Land» gab es im Sommer 2019 im Median 24 Hitzetage (Linie in der Box). Im Sommer 2020 waren es im Median 14 Tage. Die Spanne der Anzahl Hitzetage reichte 2019 von 5 bis 29 Tage (Punkt und Whisker). Im Sommer 2020 lag die Spanne zwischen 1 und 18 Tagen."))
                              ),
                          mainPanel(
                              tags$h4(HTML("<strong>Hitzetage und Tropennächte nach Raumlage, Sommermonate 2019 und 2020</strong>")),
                              span(HTML("<p> Die Grafik verdeutlicht, dass im Sommer 2019 längere und grössere Hitzte herrschte. Im Median gab es im Sommer 2019 rund 35% mehr Hitzetage. Ausnahme ist die Raumlage «Land», bei welcher der Unterschied zwischen den Jahren deutlich grösser ist (rund 60% mehr Hitzetage). Dies ein Hinweis auf die intensivere Hitze 2019.</p>")),
                              plotOutput("heatspot_plot", width = "100%", height = 325),
                              br(),
                              tags$h4(HTML("<strong>Anzahl Hitzetage und Tropennächte je Standort, Sommermonate 2019 und 2020</strong>")),
                              DT::dataTableOutput("heatspot_table", width = "100%", height = "350px")
                          ))),
                 tabPanel("Wärmeinseln", 
                          fluidPage(
                              # tags$head(includeCSS("styles.css")),
                              fluidRow(
                                  column(width = 12,
                                         box(width = NULL, solidHeader = TRUE,
                                             tags$h4(HTML("<strong>Temperaturverteilung für die Wärmeinsel Zürich</strong>")),
                                             span(HTML("<p>In der folgenden Grafik ist die Erwärmung der Wärmeinsel Zürich in den Sommermonaten 2019 und 2020 dargestellt.</p>")),
                                             br(),
                                             plotOutput("ridgelines_plot", width = "100%")
                                             )
                                  )),
                              fluidRow(
                                  column(width = 12,
                                         box(width = NULL, solidHeader = TRUE,
                                             tags$h4(HTML("<strong>Temperaturverteilung je Raumlage für die Wärmeinsel Zürich</strong>")),
                                             span(HTML("<p>In der folgenden Grafik ist die Erwärmung der Wärmeinsel Zürich nach Raumlage in den Sommermonaten 2019 und 2020 dargestellt.</p>")),
                                             br(),
                                             prettyRadioButtons(
                                                 inputId = "radio3",
                                                 label = "Transekte wählen",
                                                 choices = c("Gesamte Wärmeinsel", "Transekte Zürich - Limmattal", "Transekte Zürich - Nord"),
                                                 shape = "round",
                                                 status = "info",
                                                 fill = TRUE,
                                                 inline = TRUE
                                             ),
                                             plotOutput("transect_plot", width = "100%")
                                         )
                                  )
                              )
                          )
                 ),
                 tabPanel("Test", 
                          fluidPage(
                            # tags$head(includeCSS("styles.css")),
                            fluidRow(
                              column(width = 12,
                                     box(width = NULL, solidHeader = TRUE,
                                         tags$h4(HTML("<strong>Einfluss der Raumlage auf die Hitzeentwicklung</strong>")),
                                         span(HTML("<p>In der folgenden Grafik ist die Erwärmung der Wärmeinsel Zürich in den Sommermonaten 2019 und 2020 dargestellt.</p>")),
                                         tags$br()
                                         )
                                     )),
                            fluidRow(
                              column(width = 2),
                              column(width = 8,
                                     box(width = NULL, solidHeader = T,
                                         tags$div(`class`="btn-group btn-group-toggle", `data-toggle`="buttons",
                                                  tags$label(`class`="btn btn-info btn-sm active",
                                                             tags$input(`type`="radio", `name`="options", `id`="option1", `autocomplete`="off", `checked`="", "Stadt: versiegelt / offen")),
                                                  tags$label(`class`="btn btn-info btn-sm",
                                                             tags$input(`type`="radio", `name`="options", `id`="option2", `autocomplete`="off", "Stadt: Parkgestaltung")),
                                                  tags$label(`class`="btn btn-info btn-sm",
                                                             tags$input(`type`="radio", `name`="options", `id`="option3", `autocomplete`="off", "Wärmeinsel Zürich")),
                                                  tags$label(`class`="btn btn-info btn-sm",
                                                             tags$input(`type`="radio", `name`="options", `id`="option4", `autocomplete`="off", "Wärmeinsel Winterthur")),
                                                  tags$label(`class`="btn btn-info btn-sm",
                                                             tags$input(`type`="radio", `name`="options", `id`="option5", `autocomplete`="off", "Selber vergleichen"))
                                         )
                                         )),
                              column(width = 2)
                            )
                          )
                 )
                 
)

##############################################################################
# Server Logic
##############################################################################
server <- function(input, output, session) {
    
    # Notification on tab 'Messnetz'
    # A notification ID
#     id <- NULL
#     
#     observe({
#         id <<- showNotification(
#             HTML("<h4>Ausgangslage</h4><p>Die Auswirkungen des Klimawandels sind während Hitzeperioden in Stadtgebieten noch intensiver spürbar, da sich städtische Gebiete im Vergleich zum Umland stärker erwärmen. 
#             Die Auswirkungen dieser städtischen Wärmeinsel werden durch das Bevölkerungswachstum und die zunehmende Verdichtung in Städten und Agglomerationen weiter verstärkt.</p>
#             <p>Die Baudirektion des Kantons Zürichs hat einen Massnahmenplan zur Anpassung an den Klimawandel erarbeitet. Das Amt für Abfall, Wasser, Energie und Luft (AWEL) hat zur Unterstützung der klimaangepassten Raumplanung unter anderem auch Klimaanalysekarten für Hitzesituationen  erarbeitet.<br/>
#             <h4>Stadtklima-Messnetz</h4><p>Um  diese  Aktivitäten  durch  Messungen  zu  unterstützen,  betreibt  das AWEL im Kanton Zürich seit dem Sommer 2019 ein Sensormessnetz für die Messung
# von Lufttemperatur und -feuchte. Das Stadtklima-Messnetz umfasste im Sommer 2020 rund 50 Sensoren. Ergänzt wird das Messnetz mit Stationen von «MeteoSchweiz».<p>Der MeteoSchweiz Klimanetz-Standort Zürich/Fluntern (SMA) gilt als repräsentativ für die allgemeine Situation im Kanton Zürich.</p>"),
#             duration = NA, 
#             closeButton = TRUE,
#             type = "default"
#         )
#     })
#     
#     observeEvent(input$show, {
#         id <<- showNotification(
#             HTML("<h4>Ausgangslage</h4><p>Die Auswirkungen des Klimawandels sind während Hitzeperioden in Stadtgebieten noch intensiver spürbar, da sich städtische Gebiete im Vergleich zum Umland stärker erwärmen. 
#             Die Auswirkungen dieser städtischen Wärmeinsel werden durch das Bevölkerungswachstum und die zunehmende Verdichtung in Städten und Agglomerationen weiter verstärkt.</p>
#             <p>Die Baudirektion des Kantons Zürichs hat einen Massnahmenplan zur Anpassung an den Klimawandel erarbeitet. Das Amt für Abfall, Wasser, Energie und Luft (AWEL) hat zur Unterstützung der klimaangepassten Raumplanung unter anderem auch Klimaanalysekarten für Hitzesituationen  erarbeitet.<br/>
#             <h4>Stadtklima-Messnetz</h4><p>Um  diese  Aktivitäten  durch  Messungen  zu  unterstützen,  betreibt  das AWEL im Kanton Zürich seit dem Sommer 2019 ein Sensormessnetz für die Messung
# von Lufttemperatur und -feuchte. Das Stadtklima-Messnetz umfasste im Sommer 2020 rund 50 Sensoren. Ergänzt wird das Messnetz mit Stationen von «MeteoSchweiz».<p>Der MeteoSchweiz Klimanetz-Standort Zürich/Fluntern (SMA) gilt als repräsentativ für die allgemeine Situation im Kanton Zürich.</p>"),
#             duration = NA, 
#             closeButton = TRUE,
#             type = "default"
#         )
#     })
    
    # observeEvent(input$remove, {
    #     removeNotification(req(id))
    # })
    
    
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
    
    # For map: Only based on date
    reactive_db <- tageswerte %>% filter(date == max_date)
    
    # For tables: Based on selected location and date
    # Reactive expression for the data subsetted to what the user selected
    reactive_network_data = reactive({
        selected_sensor = data_of_click$clickedMarker$id
        if(is.null(selected_sensor)){selected_sensor = 9999} # location Zürich/Fluntern as default
        if(selected_sensor == 9999) {
            reactive_db %>% filter(sensor == 9999)
        }
        else{
            reactive_db %>% filter(sensor == selected_sensor)
            }    
        })
    
    # For graphs: Based on selected location and date
    # transform into longer format for easy visualization
    reactive_network_data_longer = reactive({
        selected_sensor = data_of_click$clickedMarker$id
        if(is.null(selected_sensor)){selected_sensor = 9999} # location Zürich/Fluntern as default
        if(selected_sensor == 9999) {
            tageswerte %>% filter(sensor == 9999 & date >= max(date)-365) %>%
                tidyr::pivot_longer(cols = c("T_min", "T_mean", "T_max"), names_to = "T_name", values_to = "T_value") %>% 
                as.data.frame()
        }
        else{
            tageswerte %>% filter(sensor == selected_sensor & date >= max(date)-365) %>%
                tidyr::pivot_longer(cols = c("T_min", "T_mean", "T_max"), names_to = "T_name", values_to = "T_value") %>% 
                as.data.frame()
        }    
    })
    
    ### TAB MESSNETZ ###
    
    # Leaftlet map with markers and overlay layers
    output$map <- renderLeaflet({ 
        # create base map for Messnetz
        leaflet(reactive_db) %>% 
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
                             baseGroups = c("Tagesminimum", "Tagesmaximum", "Wärmeinsel", "Raumlage", "Strahlungssituation"),
                             options = layersControlOptions(collapsed = FALSE)) %>% 
            hideGroup(c("Tagesmaximum", "Wärmeinsel", "Raumlage", "Strahlungssituation"))
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
                                       fillColor = ~pal_loc(Raumlage), 
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
        } else if (input$map_groups == "Wärmeinsel") {
            proxy %>% addCircleMarkers(lng= ~E,
                                       lat= ~N,
                                       layerId = ~sensor,
                                       color = "#011627",
                                       fillColor = ~pal_heat(`Wärmeinsel`),
                                       radius = 8,
                                       opacity = 0.25,
                                       fillOpacity = 1,
                                       group = "Wärmeinsel",
                                       label = sprintf("<strong>%s (%1.f)</strong><br/>
                                             Wärmeinsel: %s<br/>",
                                                       reactive_db$site,
                                                       reactive_db$sensor,
                                                       reactive_db$`Wärmeinsel`) %>% lapply(htmltools::HTML),
                                       labelOptions = labelOptions(
                                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#333"),
                                           textsize = "15px", direction = "auto"))
        } else if (input$map_groups == "Strahlungssituation") {
            proxy %>% addCircleMarkers(lng= ~E, 
                                       lat= ~N, 
                                       layerId = ~sensor,
                                       color = "#011627",
                                       fillColor = ~pal_exp(Strahlungssituation), 
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
        proxy <- leafletProxy("map", data = reactive_db)
        proxy %>% clearControls()
        if (input$map_groups == 'Tagesminimum') {
            proxy %>% addLegend("topright",
                                colors =  col_temp,
                                data = reactive_db,
                                values =  ~reactive_db$T_min,
                                labels = labels,
                                title = "Temperatur",
                                opacity = 1,
                                group = c("Tagesminimum")
            )
        } else if (input$map_groups == 'Tagesmaximum') {
            proxy %>% addLegend("topright",
                                colors =  col_temp,
                                data = reactive_db,
                                values =  ~reactive_db$T_max,
                                labels = labels,
                                title = "Temperatur",
                                opacity = 1,
                                group = c("Tagesmaximum")
            )
        } else if (input$map_groups == 'Raumlage') {
            proxy %>% addLegend("topright",
                                colors =  col_loc,
                                data = reactive_db,
                                values =  ~reactive_db$Raumlage,
                                labels = location,
                                title = "Raumlage",
                                opacity = 1,
                                group = "Raumlage"
            )
        } else if (input$map_groups == 'Tagesmittel') {
            proxy %>% addLegend("topright",
                                colors =  col_temp,
                                data = reactive_db,
                                values =  ~reactive_db$T_mean,
                                labels = labels,
                                title = "Temperatur",
                                opacity = 1,
                                group = "Tagesmittel"
            )
        } else if (input$map_groups == 'Strahlungssituation') {
            proxy %>% addLegend("topright",
                                colors =  col_exp,
                                data = reactive_db,
                                values =  ~reactive_db$Strahlungssituation,
                                labels = exposition,
                                title = "Strahlungssituation",
                                opacity = 1,
                                group = "Strahlungssituation"
            )
        } else if (input$map_groups == 'Wärmeinsel') {
            proxy %>% addLegend("topright",
                                colors =  col_heat,
                                data = reactive_db,
                                values =  ~reactive_db$`Wärmeinsel`,
                                labels = heatspot,
                                title = "Wärmeinseln",
                                opacity = 1,
                                group = "Wärmeinsel"
            )
        }
    })
    
    # Make a line plot of daily values for the selected sensor
    output$daily_plot=renderPlotly({
        data = reactive_network_data_longer()
        selected_sensor = data_of_click$clickedMarker$id
        g1 = ggplot(data = data, aes(x = as.Date(date),
                                     text = paste0(as.character(date, format = "%d.%m.%Y"),"\n", T_name, ": ", round(T_value,1), "°C "))) +
            geom_line(aes(x = as.Date(date), y = T_value, color = T_name, group = 1), size = 0.25)  +
            # geom_point(data = data %>% filter(T_value >= 30), stat = "identity",
            #            aes(x = as.Date(date), y = T_value, color = T_name, group = 2), color = "#ff6b6b", size = 1.5, show.legend = NA) +
            scale_x_date(breaks = scales::date_breaks("months"),
                         labels = scales::date_format("%b")) +
            scale_y_continuous(limits = c(-10, 45)) +
            labs(title = data$Standort[1],
                 x = paste(as.character(min(data$date),"%d.%m.%Y"), " bis ", as.character(max(data$date),"%d.%m.%Y")),
                 y = "Temperatur (°C)",
                 color = "HItzetag") +
            theme_minimal() +
            scale_color_manual(values = c("#43aa8b", "#f3722c", "#f94144", "#808080", "#90be6d", "#f9c74f")) +
            theme(legend.title = element_blank(), legend.position = "top", plot.title = element_text(size=10), axis.title.x = element_text(size = 10))
        ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=10))) %>% 
            config(displaylogo = FALSE,
                   displayModeBar=TRUE,
                   modeBarButtonsToRemove = c(
                       'sendDataToCloud', 'autoScale2d',
                       'pan2d','select2d','lasso2d',
                       'toggleSpikelines', "zoomIn2d", "zoomOut2d", 'zoom2d'))
    })
    
    # Render a table with information on the sensor location (metadata)
    output$sensor_table <- renderUI({
        locations <- reactive_network_data()
        
            # Create a Bootstrap-styled table
            tags$table(class="table", 
                       # tags$div(
                       #     tags$h4(HTML("<strong>Informationen zu diesem Standort</strong>"))
                       # ),
                       tags$thead(
                       ),
                       tags$tbody(
                           tags$tr(class = "table-light",
                                   tags$th(scope = "row", "Standort"),
                                   tags$td(locations$Standort)
                           ),
                           tags$tr(class="table-light",
                                   tags$th(scope = "row", "CH Koordinaten"),
                                   tags$td(locations$xyKoord)
                           ),
                           tags$tr(class="table-light",
                                   tags$th(scope = "row", "Standorthöhe / Sensorhöhe"),
                                   tags$td(HTML(paste0(locations$`Standorthöhe (masl)`, " m. ü. M. / ", locations$`Sensorhöhe (magl)`, " m. ü. G." )))
                           ),
                           tags$tr(class="table-light",
                                   tags$th(scope = "row", "Raumlage"),
                                   tags$td(locations$Raumlage)
                           ),
                           tags$tr(class="table-light",
                                   tags$th(scope = "row", "Strahlungssituation"),
                                   tags$td(locations$Strahlungssituation)
                           ),
                           tags$tr(class="table-light",
                                   tags$th(scope = "row", "Wärmeinsel"),
                                   tags$td(locations$`Wärmeinsel`)
                                   ),
                           tags$tr(class="table-light",
                                   tags$th(scope = "row", "Eigentümer"),
                                   tags$td(locations$source)
                           )
                           )
            )
    })


### TAB STANDORTVERGLEICH ###
    
    # create a value that will store the selection of the temperature value to display in the graph
    # Based on https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html
    
    observeEvent(input$preset_select, {

        x <- input$preset_select

        if(is.null(x)) {
            updatePickerInput(
                session = session, inputId = "location_select",
                choices = standorte,
                selected = input$location_select
            )
        } 
        else if(x == "Preset_1") {
            updatePickerInput(
                session = session, inputId = "location_select",
                choices = standorte,
                selected = c("Zürich - Wiedikon-Bertastrasse (2816)", "Zürich - Sihlfeld (537)")
            )
        } 
        else if(x == "Preset_2") {
            updatePickerInput(
                session = session, inputId = "location_select",
                choices = standorte,
                selected = c("Zürich - Altuetliberg (2656)", "Zürich - Sihlfeld (537)", "Zürich - Perron-HB (2689)")
            )
        }
        else if(x == "Preset_3") {
            updatePickerInput(
                session = session, inputId = "location_select",
                choices = standorte,
                selected = c("Zürich - Vulkanplatz (2695)", "Zürich - Oerlikerpark (2696)")
            )
        }
        else if(x == "Preset_4") {
            updatePickerInput(
                session = session, inputId = "location_select",
                choices = standorte,
                selected = c("Winterthur  - Schulhaus-Altstadt (2691)", "Winterthur - Neumarkt (2686)", "Winterthur - Turnerstrasse (2684)", "Elsau - Halden (2805)")
            )
        }
    })
    
    df_comparison_table <- reactive({
        
        location <- c(input$location_select[1], input$location_select[2], input$location_select[3], input$location_select[4], input$location_select[5])
        
        periode <- input$periode_select
        
        if(periode == "Jahr (letzte 365 Tage)") {
            tageswerte %>% 
                filter(date >= max(tageswerte$date)-years(1) & Standort %in% location) %>% 
                select(date, Standort, Raumlage, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                group_by(Standort, Raumlage) %>%  
                summarise(Temperaturminmum = paste(round(min(T_min, na.rm = T), 1), "°C"),
                          Temperaturmittel = paste(round(mean(T_mean, na.rm = T), 1), "°C"),
                          Temperaturmaximum = paste(round(max(T_max, na.rm = T), 1), "°C"),
                          Hitzetage = sum(Hitzetag, na.rm = T),
                          `Tropennächte` = sum(Tropennacht, na.rm = T)) %>% 
                ungroup() %>% 
                as.data.frame()
        }
        else if(periode == "Kalenderjahr") {
            tageswerte %>% 
                filter(year == format(Sys.time(), "%Y") & Standort %in% location) %>% 
                select(date, Standort, Raumlage, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                group_by(Standort, Raumlage) %>% 
                summarise(Temperaturminmum = paste(round(min(T_min, na.rm = T), 1), "°C"),
                          Temperaturmittel = paste(round(mean(T_mean, na.rm = T), 1), "°C"),
                          Temperaturmaximum = paste(round(max(T_max, na.rm = T), 1), "°C"),
                          Hitzetage = sum(Hitzetag, na.rm = T),
                          `Tropennächte` = sum(Tropennacht, na.rm = T)) %>% 
                ungroup() %>% 
                as.data.frame()
            } 
        else if (periode == "Sommer") {                   
            tageswerte %>% 
                filter(date >= max(date)-years(1) & month %in% summer & Standort %in% location) %>% 
                select(date, Standort, Raumlage, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                group_by(Standort, Raumlage) %>% 
                summarise(Temperaturminmum = paste(round(min(T_min, na.rm = T), 1), "°C"),
                          Temperaturmittel = paste(round(mean(T_mean, na.rm = T), 1), "°C"),
                          Temperaturmaximum = paste(round(max(T_max, na.rm = T), 1), "°C"),
                          Hitzetage = sum(Hitzetag, na.rm = T),
                          `Tropennächte` = sum(Tropennacht, na.rm = T)) %>% 
                ungroup() %>% 
                as.data.frame()
            } 
        else if (periode == "Seit Messbeginn") {
            tageswerte %>% 
                filter(date >= min(date) & Standort %in% location) %>% 
                select(date, Standort, Raumlage, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                group_by(Standort, Raumlage) %>%  
                summarise(Temperaturminmum = paste(round(min(T_min, na.rm = T), 1), "°C"),
                          Temperaturmittel = paste(round(mean(T_mean, na.rm = T), 1), "°C"),
                          Temperaturmaximum = paste(round(max(T_max, na.rm = T), 1), "°C"),
                          Hitzetage = sum(Hitzetag, na.rm = T),
                          `Tropennächte` = sum(Tropennacht, na.rm = T)) %>% 
                ungroup() %>% 
                as.data.frame()
            } 
        else if (periode == "Letzte 7 Tage") { 
            tageswerte %>% 
                filter(date >= max(date)-weeks(1) & Standort %in% location) %>%  
                select(date, Standort, Raumlage, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                group_by(Standort, Raumlage) %>% 
                summarise(Temperaturminmum = paste(round(min(T_min, na.rm = T), 1), "°C"),
                          Temperaturmittel = paste(round(mean(T_mean, na.rm = T), 1), "°C"),
                          Temperaturmaximum = paste(round(max(T_max, na.rm = T), 1), "°C"),
                          Hitzetage = sum(Hitzetag, na.rm = T),
                          `Tropennächte` = sum(Tropennacht, na.rm = T)) %>% 
                ungroup() %>% 
                as.data.frame()
            } 
    })
    
    df_comparison_plot <- reactive({
        
        location <- c(input$location_select[1], input$location_select[2], input$location_select[3], input$location_select[4], input$location_select[5])
        
        periode <- input$periode_select
        
        if(periode == "Jahr (letzte 365 Tage)") {
            tageswerte %>% 
                filter(date >= max(tageswerte$date)-years(1) & Standort %in% location) %>% 
                select(date, Standort, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                tidyr::pivot_longer(cols = -c(date, Standort, Hitzetag, Tropennacht), names_to = "T_name", values_to = "T_value") %>% 
                filter(T_name == input$temperature_select) %>%
                mutate(T_name = case_when(T_name == 'T_min' ~ 'Tagesminimum',
                                          T_name == 'T_mean' ~ 'Tagesmittel',
                                       TRUE ~ 'Tagesmaximum')) %>% 
                as.data.frame()        
            }
        else if(periode == "Kalenderjahr") {
            tageswerte %>% 
                filter(year == format(Sys.time(), "%Y") & Standort %in% location) %>% 
                select(date, Standort, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                tidyr::pivot_longer(cols = -c(date, Standort, Hitzetag, Tropennacht), names_to = "T_name", values_to = "T_value") %>% 
                filter(T_name == input$temperature_select) %>% 
                mutate(T_name = case_when(T_name == 'T_min' ~ 'Tagesminimum',
                                          T_name == 'T_mean' ~ 'Tagesmittel',
                                          TRUE ~ 'Tagesmaximum')) %>% 
                as.data.frame()
            } 
        else if (periode == "Sommer") {                   
            tageswerte %>% 
                filter(date >= max(date)-years(1) & month %in% summer & Standort %in% location) %>% 
                select(date, Standort, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                tidyr::pivot_longer(cols = -c(date, Standort, Hitzetag, Tropennacht), names_to = "T_name", values_to = "T_value") %>% 
                filter(T_name == input$temperature_select) %>% 
                mutate(T_name = case_when(T_name == 'T_min' ~ 'Tagesminimum',
                                          T_name == 'T_mean' ~ 'Tagesmittel',
                                          TRUE ~ 'Tagesmaximum')) %>% 
                as.data.frame()
            } 
        else if (periode == "Seit Messbeginn") {
            tageswerte %>% 
                filter(date >= min(date) & Standort %in% location) %>% 
                select(date, Standort, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                tidyr::pivot_longer(cols = -c(date, Standort, Hitzetag, Tropennacht), names_to = "T_name", values_to = "T_value") %>% 
                filter(T_name == input$temperature_select) %>% 
                mutate(T_name = case_when(T_name == 'T_min' ~ 'Tagesminimum',
                                          T_name == 'T_mean' ~ 'Tagesmittel',
                                          TRUE ~ 'Tagesmaximum')) %>% 
                as.data.frame()
            } 
        else if (periode == "Letzte 7 Tage") { 
            tageswerte %>% 
                filter(date >= max(date)-weeks(1) & Standort %in% location) %>%  
                select(date, Standort, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
                tidyr::pivot_longer(cols = -c(date, Standort, Hitzetag, Tropennacht), names_to = "T_name", values_to = "T_value") %>% 
                filter(T_name == input$temperature_select) %>% 
                mutate(T_name = case_when(T_name == 'T_min' ~ 'Tagesminimum',
                                          T_name == 'T_mean' ~ 'Tagesmittel',
                                          TRUE ~ 'Tagesmaximum')) %>% 
                as.data.frame()
        } 
        
    })
    
    # Change the value of a picker input on the client 
    # Based on https://dreamrs.github.io/shinyWidgets/reference/updatePickerInput.html
    
    
     # Comparison Plot: Make a line plot of the selected location(s)
    output$comparison_plot=renderPlotly({
        
        g1 = ggplot(data = df_comparison_plot(), 
                    aes(x = as.Date(df_comparison_plot()$date), 
                        text = paste0(Standort,"\n", as.character(df_comparison_plot()$date, format = "%d.%m.%Y"),"\n", T_name, ": ", round(T_value,1), "°C "))) +
            geom_line(aes(x = as.Date(df_comparison_plot()$date), y = T_value, color = Standort, group = 1), size = 0.25)  +
            scale_x_date(breaks = scales::date_breaks("months"),
                         labels = scales::date_format("%b")) +
            scale_y_continuous(limits = c(-10, 45)) +
            labs(title = paste(df_comparison_plot()$T_name[1], "je Standort"),
                x = paste(as.character(min(df_comparison_plot()$date),"%d.%m.%Y"), " bis ", as.character(max(df_comparison_plot()$date),"%d.%m.%Y")),
                 y = "Temperatur (°C)",
                 color = NULL) +
            theme_minimal() +
            scale_color_manual(values = c("#43aa8b", "#f3722c", "#f94144", "#90be6d", "#f9c74f", "#808080")) +
            theme(legend.title = element_blank(), 
                  legend.position = "", 
                  plot.title = element_text(size=10), 
                  axis.title.x = element_text(size = 10))
        ggplotly(g1, tooltip = c("text")) %>% 
            layout(legend = list(font = list(size=10)),
                   xaxis = list(
                       zeroline=TRUE,
                       showline=TRUE,
                       title='',
                       ticks = "outside",
                       showspikes = TRUE,
                       spikemode= 'across+toaxis+marker',
                       spikesnap= 'hovered data',
                       spikedash = 'solid',
                       spikethickness = 0.5,
                       spikecolor = 'gray',
                       rangeselector = list(
                           buttons = list(
                               list(
                                   count = 3,
                                   label = "letzte 3 Monate",
                                   step = "month",
                                   stepmode = "backward"),
                               list(
                                   count = 6,
                                   label = "letzte 6 Monate",
                                   step = "month",
                                   stepmode = "backward"),
                               list(
                                   count = 1,
                                   label = "letztes Jahr",
                                   step = "year",
                                   stepmode = "backward"),
                               list(step = "all",
                                    label = "alles")
                               )
                           )
                       ),
                   hovermode='x unified',
                   hoverlabel=list(bgcolor="white", opacity = 0.5),
                   legend=list(orientation='h',
                               xanchor = 'center',
                               yanchor = 'top',
                               y=-0.05,
                               x=0.5)
                   
                   ) %>% 
            config(displaylogo = FALSE,
                   displayModeBar=TRUE,
                   modeBarButtonsToRemove = c(
                       'sendDataToCloud', 'autoScale2d',
                       'pan2d','select2d','lasso2d',
                       'toggleSpikelines', "zoomIn2d", "zoomOut2d", 'zoom2d'))
        })

    
    
    # Render a table with information on heat days and tropical nights
    output$comparison_table <- renderTable({
        
        df_comparison_table()
    })
    
### TAB HITZETAGE & TROPENNÄCHTE ###

    
    # Make a box plot of heat days and tropical night per Raumlage and year
    output$heatspot_plot=renderPlot({
        
        df <- tageswerte %>%  
            filter(month %in% summer & !Raumlage %in% c("nicht definiert", "Wald")) %>% 
            group_by(Standort, Raumlage, year) %>%  
            summarise(Hitzetage = sum(Hitzetag, na.rm = T),
                      `Tropennächte` = sum(Tropennacht, na.rm = T)) %>% 
            tidyr::pivot_longer(cols = -c(Standort, Raumlage, year), names_to = "Group", values_to = "Group_Value") %>% 
            ungroup() %>% 
            as.data.frame()

        g1 <- ggplot(data = df, aes(x = Raumlage, y = Group_Value, fill = Group)) +
            geom_boxplot(
                
                alpha=0.2,
                
                # Notch?
                notch=FALSE,
                
                # custom outliers
                outlier.colour="red",
                outlier.fill="red",
                outlier.size=3
                
            ) +
            facet_wrap(~year) +
            theme_minimal() +
            labs(y = "Anzahl",
                 x = NULL,
                 fill = NULL) +
            theme(axis.title.y = element_text(size = 14),
                  axis.text = element_text(size = 14),
                  axis.text.x = element_text(size = 14, face = "bold"), 
                  legend.text = element_text(size = 12),
                  strip.text.x = element_text(size = 16, colour = "#2F4F4F", face = "bold"))
        
        g1 
        
    })
    

    
    
    output$heatspot_table <- DT::renderDataTable({
        
        df <- tageswerte %>%  
            filter(month %in% summer & !Raumlage %in% c("nicht definiert", "Wald")) %>% 
            group_by(Standort, Raumlage, `Wärmeinsel`, year) %>%  
            summarise(Hitzetage = sum(Hitzetag, na.rm = T),
                      `Tropennächte` = sum(Tropennacht, na.rm = T)) %>%
            ungroup() %>% 
            as.data.frame()
        
        df <- df %>% 
            tidyr::pivot_wider(., names_from = year, names_sep = " - ", values_from = c(Hitzetage, `Tropennächte`)) %>% 
            tidyr::drop_na() %>%  
            arrange(desc(`Hitzetage - 2019`))
        
        DT::datatable(df, options = list(lengthMenu = c(5, 10, 20, 30, 40, 50), pageLength = 5))
    })
    
    output$picker_heatspot <- renderText({
        
        input$heatspot_select
    })
    
    ### TAB WÄRMEINSELN ###
    
    output$ridgelines_plot <- renderPlot({
        
        current_year <- format(Sys.time(), "%Y")
        
        df_ridgelines <- tageswerte %>% 
            filter(year <= current_year & month %in% summer & `Wärmeinsel` == "Zürich") %>%
            as.data.frame()
        # Documentation ggridges: https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
        
        gg <- ggplot(df_ridgelines, aes(x = T_min, y = factor(year), fill = stat(x))) + 
            geom_density_ridges_gradient(aes(x = T_min, y = factor(year), fill = stat(x)), scale = 0.9, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2) +
            scale_fill_viridis_c(name = "Temperatur (°C)", option = "C") +
            facet_grid(cols = vars(year)) +
            theme_minimal() +
            labs(y = NULL,
                 x = "Tagestiefsttemperatur (°C)")
        gg
    })
    
    df_transect_plot <- reactive({
        
        current_year <- format(Sys.time(), "%Y")
        
    
        if(input$radio3 == "Transekte Zürich - Nord") {
            tageswerte %>% 
                filter(year <= current_year & 
                           month %in% summer & 
                           `Wärmeinsel` == "Zürich" & 
                           Standort %in% c("Oberembrach - Schüler (2694)", "Zürich - Schulhaus-Borrweg (2660)", "Zürich - offene Rennbahn (2683)", "Zürich - Hardplatz (2652)", "Zürich - Bucheggplatz (2698)") &
                           !Raumlage %in% c("nicht definiert", "Wald") &
                           T_min >= 20) %>%
                group_by(Standort, Raumlage, year) %>%  
                summarise(Tagestiefsttemperatur = T_min) %>%
                ungroup() %>% 
                as.data.frame()
        } else if(input$radio3 == "Transekte Zürich - Limmattal") {
            tageswerte %>% 
                filter(year <= current_year & 
                           month %in% summer & 
                           `Wärmeinsel` == "Zürich" &
                           Standort %in% c("Schlieren - Brachweg (2658)", "Zürich - Grünau (2657)", "Zürich - Hardplatz (2652)", "Zürich - Paradeplatz (2680)", "Zürich - Sechseläutenplatz (2682)", "Zürich - Zürihorn (2679)") &
                           !Raumlage %in% c("nicht definiert", "Wald") &
                           T_min >= 20) %>%
                group_by(Standort, Raumlage, year) %>%  
                summarise(Tagestiefsttemperatur = T_min) %>%
                ungroup() %>% 
                as.data.frame()
            
        } else {
            
            tageswerte %>% 
                filter(year <= current_year & 
                           month %in% summer & 
                           `Wärmeinsel` == "Zürich" & 
                           !Raumlage %in% c("nicht definiert", "Wald") &
                           T_min >= 20) %>%
                group_by(Standort, Raumlage, year) %>%  
                summarise(Tagestiefsttemperatur = T_min) %>%
                ungroup() %>% 
                as.data.frame()
        }
        
        
    })
    
    # Make a box plot of heat days and tropical night per Raumlage and year
    output$transect_plot=renderPlot({
        
        g1 <- ggplot(data = df_transect_plot(), aes(y = Standort, x = Tagestiefsttemperatur, fill = Raumlage)) +
            geom_boxplot(
                
                alpha=0.2,
                
                # Notch?
                notch=FALSE,
                
                # custom outliers
                outlier.colour="red",
                outlier.fill="red",
                outlier.size=3
                
            ) +
            theme_minimal() +
            facet_wrap(~year) +
            labs(y = "Tagestiefsttemperatur",
                 x = NULL,
                 fill = "Raumlage") +
            theme(axis.title.y = element_text(size = 14),
                  axis.text = element_text(size = 14),
                  axis.text.x = element_text(size = 14, face = "bold"), 
                  legend.text = element_text(size = 12),
                  strip.text.x = element_text(size = 16, colour = "#2F4F4F", face = "bold"),
                  legend.position = "top")
        
        g1 
        
    })
    
    ### TAB TEST
    
    observeEvent(input$continue1,{
      if(nchar(input$question1)>0)
      {
        shinyjs::hide("div_1")
        shinyjs::show("div_2")
      }
      else
      {
        showModal(modalDialog(
          title = "Important message",
          "How about you actually answer the question before continuing?"
        ))
      }
    })
    
    observeEvent(input$continue2,{
      shinyjs::hide("div_2")
      shinyjs::show("div_3")
    })
    
    observeEvent(input$goback2,{
      shinyjs::hide("div_2")
      shinyjs::show("div_1")
    })
    
    observeEvent(input$goback3,{
      shinyjs::hide("div_3")
      shinyjs::show("div_2")
    })
    
    output$results <- renderText({paste0("Your answers were: '", input$question1,"' and '", input$question2, "'.") })
    
    
    
}
    
    
##############################################################################
# Run the application 
##############################################################################
shinyApp(ui = ui, server = server)
