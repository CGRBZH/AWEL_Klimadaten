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
library(shinycssloaders)
library(shinyjs)

##############################################################################
# Data
##############################################################################
tageswerte = readRDS("tageswerte_komplett.RDS")
stundenwerte = readRDS("stundenwerte.RDS")
sommerwerte = readRDS("sommerwerte.RDS")

##############################################################################
# Parameters 
##############################################################################
min_date <- as.Date(min(tageswerte$date),"%d.%m.%Y")
max_date <- as.Date(max(tageswerte$date),"%d.%m.%Y")
standorte <- rev(sort(unique(tageswerte$Standort)))
exposition <- unique(tageswerte$Strahlungssituation)
location <- unique(tageswerte$Raumlage)
heatspot <- unique(tageswerte$`Wärmeinsel`)
summer <- c('Jun', 'Jul', 'Aug')

PAGE_NUM <- 2
NUM_PAGES <- 4

col_temp <- c("#071e46", "#08529c", "#2171b5", "#4292c7", "#78bfd6", "#dbf5ff", 
              "#ffe0e0", "#ffcbca", "#ffa09b", "#ff726a", "#ff3b36", 
              "#f70019", "#cf0014", "#a80010", "#82000a", "#5f0000") # "#cc181e", "#a60f14", "#780a0f", "#5f0000"

# Defining legend labels for temperature
labels <- c("< -15 °C","-15 bis -12 °C", "-12 bis -9 °C", "-9 bis -6 °C", "-6 bis -3 °C", "-3 bis 0 °C", 
            "0 bis 3 °C", "3 bis 6 °C", "6 bis 9 °C", "9 bis 12 °C", "12 bis 15 °C", 
            "15 bis 20 °C", "20 bis 25 °C", "25 bis 30 °C", "30 bis 35 °C", "> 35 °C")

col_loc <- c("#F5F5F5", "#43aa8b", "#f3722c", "#f94144", "#90be6d", "#f9c74f")

col_exp <- c("#F5F5F5", "#43aa8b", "#f9c74f", "#f94144")

col_heat <- c("#F5F5F5", "#43aa8b", "#f3722c", "#f94144", "#808080", "#90be6d", "#f9c74f")

colors <- c("#1f77b4", "#d62728", "#2ca02c", "#9467bd",  "#ff7f0e")

# Color palettes to color scales
pal_tmin <- leaflet::colorBin(palette = col_temp,
                              bins = c(-Inf,-15, -12, -9, -6, -3, 0, 3, 6, 9, 12, 15, 20, 25, 30, 35, Inf),
                              domain = tageswerte$T_min,
                              na.color = "#808080" )

pal_tmean <- leaflet::colorBin(palette = col_temp,
                               bins = c(-Inf,-15, -12, -9, -6, -3, 0, 3, 6, 9, 12, 15, 20, 25, 30, 35, Inf),
                               domain = tageswerte$T_mean,
                               na.color = "#808080" )

pal_tmax <- leaflet::colorBin(palette = col_temp,
                              bins = c(-Inf,-15, -12, -9, -6, -3, 0, 3, 6, 9, 12, 15, 20, 25, 30, 35, Inf),
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

##############################################################################
# UI Side
##############################################################################
ui <- tagList(useShinyjs(), 
              tags$head(
                  tags$link(href = "styles.css", rel = "stylesheet")
                  ),
              # Statup Page: Loading message
              div(id = "loading-content", h2("Daten werden geladen...",
                  img(src = "https://www.web.statistik.zh.ch/awel_stadtklima/loader-bar.gif"))),
              # The main app code starts here
              hidden(
                  div(id = "app-content",
                      navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE,
                                 HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="https://www.zh.ch/de/umwelt-tiere/klima/klimakarte-daten.html#1132125233">Stadtklima</a>'), id="nav",
                                 windowTitle = "Stadtklima",
                                 tabPanel(title = "Messnetz",  
                                          fluidPage(
                                              tags$head(includeCSS("styles.css")),
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
                                    leafletOutput("map", height = 650) %>% withSpinner(type = 7, size = 0.5, color="#949592")
                                )
                                    ),
                         column(width = 6, 
                                box(width = NULL, 
                                    tags$h4(HTML("<strong>Informationen zum Standort</strong>")),
                                    span(HTML("<p>Die Grafik zeigt Tagesmaxima, Tagesmittel und Tagesminima der Lufttemperatur seit Mai 2019 an der auf der Karte ausgewählten Stationen</p>
                                                     <p><strong>Hinweis: </strong><i>Fahren Sie über die Grafik um die einzelnen Messwerte zu sehen oder ziehen einen Bereich auf, um das angezeigte Zeitfenster anzupassen.</i></p>")),
                                    plotlyOutput("daily_plot", height = 450) %>% withSpinner(type = 7, size = 0.5, color="#949592"),
                                   ),
                                box(width = NULL,
                                    uiOutput("sensor_table", height = 450)
                                )
                         )
                     ))),
                 tabPanel("Standortvergleich", 
                          fluidPage(
                              tags$head(includeCSS("styles.css")),
                              fluidRow(
                                  column(width = 12,
                                         box(width = NULL, solidHeader = TRUE,
                                             tags$h4(HTML("<strong>Einfluss der Raumlage auf die Hitzeentwicklung</strong>")),
                                             span(HTML("<p>Mit dem Vergleich ausgewählter Stationen können zahlreiche Facetten der städtischen Hitzeentwicklung verdeutlicht werden.
                                                       Anhand von vier Beispielen werden einige Einflussfaktoren auf die Hitzeentwicklung vorgestellt.
                                                       Im Tab «Standorte vergleichen» können Sie selber Stationen wählen und vergleichen.</p>")),
                                             tags$br()
                                         )
                                  )),
                              fluidRow(
                                  tabsetPanel(selected = "Stadt: versiegelt / offen", 
                                              type = "tabs",
                                              tabPanel("Stadt: versiegelt / offen",
                                                       column(width = 5,
                                                              br(),
                                                              tags$h4(HTML("<strong>Einfluss versiegelter Bauweise auf die Hitzeentwicklung in der Stadt Zürich</strong>")),
                                                              span(HTML("<p>Es werden zwei geographisch nah beieinanderliegende, aber doch sehr unterschiedliche Stationen in der Stadt Zürich gegenübergestellt.
                                                        Bei «Zürich, Bertrastrasse» handelt es sich um einen Messort mit stark versiegelter Umgebung und ohne nennenswerte Kaltluftzufuhr in der Nacht.
                                                                  Der nur wenige Meter entfernt liegenede «Friedhof Sihlfeld» liegt im Grünen und profitiert in der Nacht von Frischluft, welche vom Uetliberg herunter strömt sowie der lokalen Abkühlung durch den Park.</p>")),
                                                              box(width = 6, solidHeader = T,
                                                                  tags$h6(HTML("<strong>Bertastrasse</strong>")),
                                                                  tags$img(src = "https://www.web.statistik.zh.ch/awel_stadtklima/Bertastrasse.jpg", width = "100%"),
                                                                  ),
                                                              box(width = 6, solidHeader = T,
                                                                  tags$h6(HTML("<strong>Friedhof Sihlfeld</strong>")),
                                                                  tags$img(src = "https://www.web.statistik.zh.ch/awel_stadtklima/Friedhof_Sihlfeld.jpg", width = "100%")
                                                                  ),
                                                              box(width = 12,
                                                                  leafletOutput("map_vgl1", height = 250)
                                                                  )
                                                              ),
                                                       column(width = 7,
                                                              box(width = NULL, solidHeader = T,
                                                                  br(),
                                                                  tags$h4(HTML("<strong>Temperaturverlauf an den Messstellen Bertastrasse und Friedhof Sihlfeld im Sommer 2020</strong>")),
                                                                  plotlyOutput("comparison_plot2", width = "100%", height = "450px") %>% withSpinner(type = 7, size = 0.5, color="#949592"),
                                                                  uiOutput("comparison_table2", width = "85%")
                                                                  )
                                                              )
                                                       ),
                                              tabPanel("Stadt: Parkgestaltung",
                                                       column(width = 5,
                                                              br(),
                                                              tags$h4(HTML("<strong>Einfluss der Parkgestaltung auf die Hitzeentwicklung</strong>")),
                                                              span(HTML("<p>Der «Vulkanplatz» in Zürich-Altstetten und der «Oerlikerpark» sind ungefähr gleich gross. 
                                                                            Der Vulkanplatz ist aber stärker versiegelt und weniger begrünt. 
                                                                            Der Vergleich der Messwerte soll aufzeigen, ob sich dadurch bereits messbare Temperaturunterschiede ergeben.</p>")),
                                                              box(width = 6, solidHeader = T,
                                                                  tags$h6(HTML("<strong>Vulkanplatz</strong>")),
                                                                  tags$img(src = "https://www.web.statistik.zh.ch/awel_stadtklima/Vulkanplatz_Altstetten.jpg", width = "100%"),
                                                                  leafletOutput("map_vgl2a", height = 300)
                                                                  ),
                                                              box(width = 6, solidHeader = T,
                                                                  tags$h6(HTML("<strong>Oerlikerpark</strong>")),
                                                                  tags$img(src = "https://www.web.statistik.zh.ch/awel_stadtklima/Oerlikerpark.jfif", width = "100%"),
                                                                  leafletOutput("map_vgl2b", height = 300)
                                                                  )
                                                       ),
                                                       column(width = 7,
                                                              box(width = NULL, solidHeader = T,
                                                                  br(),
                                                                  tags$h4(HTML("<strong>Temperaturverlauf an den Messstellen Vulkanplatz und Oerlikerpark im Sommer 2020</strong>")),
                                                                  span(HTML("<p>An heissen Sommertagen ist die Maximaltemperatur am «Vulkanplatz» etwas höher.
                                                                            Dies kann aber auch durch unterschiedliche Strahlungsverhältnisse und stärkere Sensorerwärmung verursacht sein.
                                                                            In der Nacht kühlt es im «Oerlikerpark» schneller und stärker ab.</p>")),
                                                                  plotlyOutput("comparison_plot3", width = "100%", height = "450px") %>% withSpinner(type = 7, size = 0.5, color="#949592"),
                                                                  uiOutput("comparison_table3", width = "85%")
                                                                  
                                                              )
                                                       )
                                              ),
                                              tabPanel("Zürich: Stadt - Land",
                                                       column(width = 5,
                                                              br(),
                                                              tags$h4(HTML("<strong>Uetliberg - Friedhof Sihlfeld - Perron am Zürich HB</strong>")),
                                                              span(HTML("<p>Das «Perron HB» am Zürich HB ist einen Messort mit stark versiegelter Umgebung ohne nennenwerte Zufuhr kühler Luft währende der Nacht.
                                                                        Die Messstelle «Friedhof Sihlfeld» liegt im Grünen und profitiert in der Nacht von Frischluft, welche den Uetliberg herunterströmt sowie der lokalen Auskühlung durch den Park.
                                                                        Die Station am «Uetliberg» befindet sich im Wald auf der Krete der Albiskette.</p>")),
                                                              leafletOutput("map_vgl3", height = 450),
                                                              tags$i("Wärmeinsel Zürich, Messstellen «Uetliberg», «Friedhof Sihlfeld» und «Perron HB».")
                                                       ),
                                                       column(width = 7,
                                                              box(width = NULL, solidHeader = T,
                                                                  br(),
                                                                  tags$h4(HTML("<strong>Temperaturverlauf an den Messstellen Uetliberg, Friedhof Sihlfeld und Perron HB im Sommer 2020</strong>")),
                                                                  span(HTML("<p>Während sich das Maxima der Messstellen «Friedhof Sihlfeld» und «Perron HB» kaum unterscheiden, ist es in der Nacht am «Friedhof Sihlfeld»
                                                                            3°C kühler. Die Station «Uetliberg» zeigt einen schwach ausgeprägten Tagesgang: Während der Nacht befindet sich die Messstelle oberhalb der Inversion 
                                                                            in der warmen Luft und tagsüber hält sich die Erwärmung aufgrund der Waldlage und der Höhe in Grenzen.</p>")),
                                                                  plotlyOutput("comparison_plot4", width = "100%", height = "450px") %>% withSpinner(type = 7, size = 0.5, color="#949592"),
                                                                  uiOutput("comparison_table4", width = "85%")
                                                              )
                                                       )
                                              ),
                                              tabPanel("Winterthur: Stadt - Land",
                                                       column(width = 5,
                                                              br(),
                                                              tags$h4(HTML("Innerstädtische Messstellen im Vergleich zur ländlichen Referenz")),
                                                              span(HTML("<p>Der Vergleich der innerstädtischen Messstellen «Schulhaus-Altstadt», «Neumarkt» und «Turnerstrasse» mit der ländlichen Referenz «Elsau - Halden»
                                                                        zeigt auch in Witerthur einen deutlichen Wärmeinseleffekt.</p>")),
                                                              leafletOutput("map_vgl4", height = 450),
                                                              tags$i("Wärmeinsel Winterthur, Messstellen «Schulhaus-Altstadt», «Neumarkt» und «Turnerstrasse» mit der ländlichen Referenz «Elsau - Halden».")
                                                              
                                                       ),
                                                       column(width = 7,
                                                              box(width = NULL, solidHeader = T,
                                                                  br(),
                                                                  tags$h4(HTML("Temperaturverlauf der Wärmeinsel Winterthur im Sommer 2020")),
                                                                  span(HTML("<p>Die Tiefsttemperaturen der innerstädtischen Messstellen sind praktisch gleich und ca. 3°C höher als diejenigen der 
                                                                            ländlichen Referenzstation in Elsau. Die Tagesmaxima sind ebenfalls höher. Die Abweichungen der Stationen «Turnerstrasse» und
                                                                            «Neumarkt» sind wahrscheinlich mindestens durch längere direkte Sonneneinstrahlung und entsprechende Sensorerwärmung verursacht.</p>")),
                                                                  plotlyOutput("comparison_plot5", width = "100%", height = "450px") %>% withSpinner(type = 7, size = 0.5, color="#949592"),
                                                                  uiOutput("comparison_table5", width = "85%")
                                                              )
                                                       )                                              ),
                                              tabPanel("Standorte vergleichen",
                                                       sidebarLayout(
                                                           sidebarPanel(
                                                               br(),
                                                               tags$i(HTML("<strong>Hinweis:</strong> Wählen Sie die Standorte, den Zeitraum und die Temperaturwerte aus den Auswahl-Menüs aus, um Grafik und Tabelle zu aktualisieren.</br>")),
                                                               br(),
                                                               
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
                                                               
                                                               pickerInput(inputId = "periode_select", label = "Zeitraum:",
                                                                           choices = c("Letzte 7 Tage", "Sommer", "Kalenderjahr", "Jahr (letzte 365 Tage)", "Seit Messbeginn"),
                                                                           options = list(`actions-box` = TRUE),
                                                                           selected = c("Jahr (letzte 365 Tage)"),
                                                                           multiple = FALSE),
                                                               ),
                                                           
                                                           mainPanel(
                                                               br(),
                                                               plotlyOutput("comparison_plot", width = "100%", height = "450px") %>% withSpinner(type = 7, size = 0.5, color="#949592"),
                                                               br(),
                                                               uiOutput("comparison_table", width = "100%", height = "350px")
                                                           )
                                                       )
                                              )
                                  )
                              )
                          )
                 ),
                 tabPanel("Hitzetage & Tropennächte",
                          fluidPage(
                              useShinyjs(),
                              fluidRow(
                                  column(width = 12,
                                         box(width = NULL, 
                                             h4("Hitzetage und Tropennächte in Stadt und Umland, Sommer 2019 und 2020"),
                                             p("Die folgenden Resultate basieren auf den Daten, welche während der Sommermonate 2019 und 2020 erhoben wurden."),
                                             br()
                                         )
                                  )),
                              fluidRow(
                                  tabsetPanel(selected = "Hitzetage in der Stadt", 
                                              type = "tabs",
                                              tabPanel("Hitzetage in der Stadt", height = "100%", 
                                                       fluidRow(
                                                           column(width = 12,
                                                                  box(width = 11,
                                                                      hidden(
                                                                          lapply(seq(NUM_PAGES), function(i) {
                                                                              div(class = "page",
                                                                                  id = paste0("step", i),
                                                                                  if(i == 1){
                                                                                      div(
                                                                                          h5("Einfluss der Stadt auf die Anzahl Hitzetag"),
                                                                                          p("Wird der Temperaturverlauf aller Sensoren in der Stadt für den Sommer 2019 mit denen im Sommer 2020 gegenübergestellt, so zeigt sich, dass die Durchschnittstemperatur 2019 um fast 2°C höherlag. 
                                                                                          Rot markiert die Hitzetage, also Tage an denen die Höchsttemperatur 30°C und mehr beträgt.
                                                                                            Die Hitzetage waren 2019 zahlreicher und die Bandbreite der Temperaturen ist wesentlich grösser als 2020."),
                                                                                          br(),
                                                                                          box(width = 6,
                                                                                              plotOutput("summer2019", width = "100%") %>% withSpinner(type = 7, size = 0.5, color="#949592")),
                                                                                          box(width = 6,
                                                                                              plotOutput("summer2020", width = "100%") %>% withSpinner(type = 7, size = 0.5, color="#949592"))
                                                                                      )
                                                                                  } else if (i == 2) {
                                                                                      div(
                                                                                          tags$h4(HTML("<strong>Verteilung der Hitzetage in der Stadt, Sommer 2019 und 2020</strong>")),
                                                                                          span(HTML("<p>Die Verteilung der Hitzetage lässt sich auch mit einem sogenannten Boxplot darstellen. 
                                                                                          Der Boxplot fasst die Messwerte zusammen: Die senkrechten Linien reichen vom Minimum bis zum Maximum der Anzahl Tage. 
                                                                                                Die Box markiert den Bereich, in dem die Hälfte der Hitzetage liegt. Die Linie in der Box zeigt den Median. 
                                                                                                    Im Juni 2019 liegt der Median bei rund 12 Tagen. Im Juni 2020 war es nur 1 Tag (Median).</p>")),
                                                                                          tags$br(),
                                                                                          plotOutput("summermonths", width = "100%") %>% withSpinner(type = 7, size = 0.5, color="#949592")
                                                                                      )
                                                                                  } else if (i == 3) {
                                                                                      div(
                                                                                          tags$h4(HTML("<strong>Hitzetage in der Stadt, Sommer 2019 und 2020</strong>")),
                                                                                          span(HTML("<p>Zusammengefasst nach Jahren, ist die grössere Hitze 2019 deutlich zu erkennen.
                                                                                                    Die Spanne der Anzahl Hitzetage reichte 2019 von 1 bis 43 Tage. Im Sommer 2020 lag die Spanne zwischen 0 und 32 Tagen.</p>")),
                                                                                          tags$br(),
                                                                                          box(width = 6,
                                                                                              plotOutput("summeryear_2019", width = "100%") %>% withSpinner(type = 7, size = 0.5, color="#949592")
                                                                                          ),
                                                                                          box(width = 6,
                                                                                              plotOutput("summeryear_2020", width = "100%") %>% withSpinner(type = 7, size = 0.5, color="#949592")
                                                                                          )
                                                                                      )
                                                                                  } else if (i == 4) {
                                                                                      div(
                                                                                          tags$h4(HTML("<strong>Hitzetage nach Raumlage im Vergleich, Sommer 2019 und 2020</strong>")),
                                                                                          tags$p("Die Abbildung zeigt die Anzahl Hitzetage je nach Raumlage über alle Sensoren während der Sommermonate (Juni, Juli und August) 2019 und 2020."),
                                                                                          span(HTML("<p>Die Grafik verdeutlicht, dass im Sommer 2019 längere und grössere Hitzte herrschte. Im Median gab es im Sommer 2019 rund 35% mehr Hitzetage. 
                                                                                          An der Raumlage «Land» gab es im Sommer 2019 im Median 24 Hitzetage. Im Sommer 2020 waren es 14 Tage.                                                                                                    Dies ein Hinweis auf die intensivere Hitze 2019.</p>")),
                                                                                          tags$br(),
                                                                                          plotOutput("summerraumlage", width = "100%") %>% withSpinner(type = 7, size = 0.5, color="#949592")
                                                                                      )
                                                                                  }
                                                                              )
                                                                          })
                                                                      )
                                                                  ),
                                                                  box(width = 1, solidHeader = T, align="center",
                                                                      tags$br(),
                                                                      tags$br(),
                                                                      actionButton("prevBtn", label = NULL, icon = icon("chevron-up"), class="btn btn-sm"),
                                                                      tags$br(),
                                                                      tags$br(),
                                                                      textOutput("page_index"),
                                                                      tags$br(),
                                                                      actionButton("nextBtn", label = NULL, icon = icon("chevron-down"), class="btn btn-sm"),
                                                                      tags$br(),
                                                                      tags$br()
                                                                  )
                                                           )
                                                       )
                                              ),
                                              tabPanel("Wärmeinseleffekte", height = "100%", 
                                                       fluidRow(
                                                           column(width = 12,
                                                                  box(width = 12, solidHeader = TRUE,
                                                                      tags$h4(HTML("<strong>Überwärmung der Wärmeinsel Zürich</strong>")),
                                                                      span(HTML("<p>Der nächtliche Wärmeinseleffekt zeigt sich auch deutlich bei der Darstellung der Tagestiefstwerte entlang von Transekten, welche die Stadt Zürich durchqueren.
                                                                            Im Folgenden betrachten wir die beiden Transekten «Zürich - Limmattal» und «Zürich - Nord-Ost».</p>
                                                                                <p>Transekte «Zürich - Limmattal»: Von Schlieren über Hardplatz und Sechseläutenplatz zum Zürichhorn.</p>
                                                                                <p>Transekte «Zürich - Nord-Ost»: Von Oberembrach im Norden bis in den Friesenberg im Osten von Zürich.</p>")),
                                                                      prettyRadioButtons(
                                                                          inputId = "radioTransect",
                                                                          label = "",
                                                                          choices = c("Transekte Zürich - Limmattal", "Transekte Zürich - Nord-Ost"),
                                                                          shape = "round",
                                                                          status = "primary",
                                                                          fill = TRUE,
                                                                          inline = TRUE
                                                                          ))
                                                                  ),
                                                           column(width = 12,
                                                                  box(width = 4,
                                                                      leafletOutput("transect_map", height = 450) %>% withSpinner(type = 7, size = 0.5, color="#949592")
                                                                      ),
                                                                  box(width = 8,
                                                                      plotOutput("transect_plot", width = "100%") %>% withSpinner(type = 7, size = 0.5, color="#949592")
                                                                      )
                                                           )
                                                       )
                                              ),
                                              tabPanel("Datentabelle",
                                                       column(width = 12,
                                                              tags$h4(HTML("<strong>Anzahl Tropennächte, Sommer 2019 und Sommer 2020 im Verhleich</strong>")),
                                                              span(HTML("<p>Der Sommer 2020 zählt zwar auch zu den sehr warmen Sommern, im Sommer 2019, welcher Rang 3 in der Hitliste belegt, 
                                                                        stellten sich aber deutlich mehr Hitzetage und Tropennächte ein.</p>")),
                                                              tags$p("Die Tabelle zeigt die meisten Messorte mit der zugehörigen Anzahl Hitzetage und Tropennächte für die Sommermonate 2019 und 2020. Da einige der Standorte sonnenexponierter sind, erwärmen sich die Sensoren an solchen Standorten tagsüber im Extremfall deutlich. Aus diesem Grund ist die Anzahl Hitzetage mit Vorsicht zu interpretieren."),
                                                              tags$p("Noch wichtiger als die Hitzebelastung tagsüber sind die nächtlichen Temperaturen für die Gesundheit der Bevölkerung. Der Mensch kann hohe Temperaturen tagsüber relativ gut ertragen, wenn es sich während der Nacht erholen kann. Die Werte der Anzahl Tropennächte sind somit gut belastbar - die Sensoren liefern ohne Strahlungseinfluss präzise Daten."),
                                                              DT::dataTableOutput("heatspot_table", width = "100%", height = "250px")
                                                              )
                                                       )
                                              )
                                  )
                              )
                          ),
                 tabPanel("Hinweise zu den Daten", icon = icon("info-circle"),
                          fluidPage(
                              tags$head(includeCSS("styles.css")),
                              fluidRow(
                                  column(width = 12,
                                         box(width = NULL, solidHeader = TRUE,
                                             tags$h3(HTML("<strong>Hinweise zu den Daten</strong>")),
                                             tags$p(HTML("Seit Mai 2019 betreibt das AWEL des Kantons Zürich ein Messnetz aus rund 40 Lufttemperatur und -feuchte Sensoren (LoRa). Die Sensor-Standorte befinden sich hauptsächlich im städtischen Raum, verdichtet in der Stadt Zürich, aber auch in weiteren Gemeinden im Kanton Zürich. Ziel des Messnetzes ist es, Messdaten für Stadtklima-Analysen bereitzustellen, damit Aspekte des Lokalklimas und Klimawandels stärker sichtbar werden und in der Raumplanung und Stadtentwicklung berücksichtigt werden können. Mit Hilfe dieser Daten lassen sich u.a. die Auswirkungen von Hitzewellen im städtischen Raum beschreiben. Das Messnetz wird durch das AWEL betrieben.")),
                                             tags$br(),
                                             tags$h4(HTML("<strong>Definitionen:</strong>")),
                                             tags$p(HTML("<p><strong>Hitzetage:</strong> Tage mit einer Temperatur (T_max) von 30°C und mehr.</p>
                                             <p><strong>Tropennächte:</strong> Nächte mit einer Temperatur (T_min) von 20°C und mehr.</p>
                                                         <p><strong>Sommer:</strong> Die Monate Juni, Juli und August gelten als Sommerperiode.</p>")),
                                             tags$br(),
                                             tags$h4(HTML("<strong>Datenbezug:</strong>")),
                                             tags$p(HTML("Die erhobenen Messdaten werden der interessierten Öffentlichkeit auf <a href='https://opendata.swiss/de/dataset/lufttemperatur-und-luftfeuchte-lora-sensor-messwerte'>opendata.swiss</a> laufend frei zur Verfügung gestellt.")),
                                             tags$p(HTML("Weitere Angaben zur Datensatz-Struktur und den verwendeten Sensoren sind im <a href='https://opendata.swiss/de/dataset/lufttemperatur-und-luftfeuchte-lora-sensor-messwerte/resource/c3af14da-6105-4527-a93b-fba317faa923'>Daten-Beschrieb</a> zu finden.")),
                                             tags$br(),
                                             tags$h4(HTML("<strong>Datenbehandlung:</strong>")),
                                             tags$p(HTML("Bei der Interpretation der Daten ist zu beachten, dass die Sensoren mit einem passiven, nicht aktiv belüfteten Strahlungsschild (Lamellen) ausgestattet sind. 
                                                         Vergleichmessungen zeigen, dass bei direkter, intensiver Sonneneinstrahlung auf den Strahlungsschild Abweichungen zwischen Messwert und eigentlicher Lufttemperatur von bis zu 5°C auftreten können. Ist der Strahlungsschutz beschattet, so sind die Messungen bezüglich Strahlungseffekt auf rund 0.5°C genau. Die Sensoren wurden dementsprechend wenn möglich an lokal grösstenteils beschatteten Standorten aufgehängt. In der Nacht sind die Messdaten erfahrungsgemäss sehr genau.")),
                                             tags$p(HTML("Aus diesem Grund sind die Anzahl Hitzetage mit Vorsicht zu interpretieren. Die Werte der Anzahl Tropennächte sind hingenen gut belastbar - die Sensoren liefern ohne Stralungseinfluss sehr präzise Daten.")),
                                             tags$br(),
                                             tags$h4(HTML("<strong>Kontaktstelle:</strong>")),
                                             tags$p(HTML("<a href='joerg.sintermann@bd.zh.ch'>Amt für Abfall, Wasser, Energie und Luft des Kantons Zürich, Abteilung Luft</a>"))
                                             
                                             )
                                             )
                                  )
                                  )
                              )
                          )
    )
    ))


##############################################################################
# Server Logic
##############################################################################
server <- function(input, output, session) {
    
    ### Loading Screen
    # Simulate work being done for 3 seconds
    Sys.sleep(3)
    
    # Hide the loading message when the rest of the server function has executed
    hide(id = "loading-content", anim = TRUE, animType = "fade")    
    show("app-content")
    
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
        if(is.null(selected_sensor)){selected_sensor = "Zürich/Fluntern (9999)"} # location Zürich/Fluntern as default
        if(selected_sensor == "Zürich/Fluntern (9999)") {
            reactive_db %>% filter(Standort == "Zürich/Fluntern (9999)") 
        }
        else{
            reactive_db %>% filter(Standort == selected_sensor) 
            }    
        })
    
    # For graphs: Based on selected location and date
    # transform into longer format for easy visualization
    reactive_network_data_longer = reactive({
        selected_sensor = data_of_click$clickedMarker$id
        if(is.null(selected_sensor)){selected_sensor = "Zürich/Fluntern (9999)"} # location Zürich/Fluntern as default
        if(selected_sensor == "Zürich/Fluntern (9999)") {
            tageswerte %>% filter(Standort == "Zürich/Fluntern (9999)" & date >= max(date)-365) %>%
                tidyr::pivot_longer(cols = c("T_min", "T_mean", "T_max"), names_to = "T_name", values_to = "T_value") %>% 
                as.data.frame()
        }
        else{
            tageswerte %>% filter(Standort == selected_sensor & date >= max(date)-365) %>%
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
            # addProviderTiles(providers$Stamen.Terrain) %>% 
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
                                       layerId = ~Standort,
                                       color = ~pal_loc(Raumlage),
                                       fillColor = ~pal_loc(Raumlage), 
                                       radius = 8,
                                       opacity = 0.75,
                                       fillOpacity = 1,
                                       group = "Raumlage",
                                       label = sprintf("<strong>%s</strong><br/>
                                              Sensor: %1.f<br/>
                                             Raumlage: %s",
                                                       reactive_db$site,
                                                       reactive_db$sensor,
                                                       reactive_db$Raumlage) %>% lapply(htmltools::HTML),
                                       labelOptions = labelOptions(
                                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
                                           textsize = "15px", direction = "auto"))
        } else if (input$map_groups == "Tagesminimum") {
            proxy %>% addCircleMarkers(lng= ~E,
                                       lat= ~N,
                                       layerId = ~Standort,
                                       color = "black",
                                       fillColor = ~pal_tmin(T_min),
                                       radius = 8,
                                       opacity = 0.75,
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
                                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
                                           textsize = "15px", direction = "auto"))
        } else if (input$map_groups == "Wärmeinsel") {
            proxy %>% addCircleMarkers(lng= ~E,
                                       lat= ~N,
                                       layerId = ~Standort,
                                       color = ~pal_heat(`Wärmeinsel`),
                                       fillColor = ~pal_heat(`Wärmeinsel`), 
                                       radius = 8,
                                       opacity = 0.75,
                                       fillOpacity = 1,
                                       group = "Wärmeinsel",
                                       label = sprintf("<strong>%s (%1.f)</strong><br/>
                                             Wärmeinsel: %s<br/>",
                                                       reactive_db$site,
                                                       reactive_db$sensor,
                                                       reactive_db$`Wärmeinsel`) %>% lapply(htmltools::HTML),
                                       labelOptions = labelOptions(
                                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
                                           textsize = "15px", direction = "auto"))
        } else if (input$map_groups == "Strahlungssituation") {
            proxy %>% addCircleMarkers(lng= ~E, 
                                       lat= ~N, 
                                       layerId = ~Standort,
                                       color = ~pal_exp(Strahlungssituation),
                                       fillColor = ~pal_exp(Strahlungssituation), 
                                       radius = 8,
                                       opacity = 0.75,
                                       fillOpacity = 1,
                                       group = "Strahlungssituation",
                                       label = sprintf("<strong>%s</strong><br/>
                                              Sensor: %1.f<br/>
                                            Exposition: %s",
                                                       reactive_db$site,
                                                       reactive_db$sensor,
                                                       reactive_db$Strahlungssituation) %>% lapply(htmltools::HTML),
                                       labelOptions = labelOptions(
                                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
                                           textsize = "15px", direction = "auto"))
        } else {
            proxy %>% addCircleMarkers(lng= ~E,
                                       lat= ~N,
                                       layerId = ~Standort,
                                       color = "black",
                                       fillColor = ~pal_tmax(T_max),
                                       radius = 8,
                                       opacity = 0.75,
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
                                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
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
      selected_sensor = data_of_click$clickedMarker$id
      

      fig <- if(is.null(selected_sensor)){selected_sensor = "Zürich/Fluntern (9999)"} 
      if(selected_sensor == "Zürich/Fluntern (9999)") {
        plot_ly(data = tageswerte, x = ~date)  %>% 
          filter(Standort == "Zürich/Fluntern (9999)") %>% 
          add_lines(y = ~round(T_min,1), name = 'Tagesminimum', mode = 'lines', line=list(color = colors[1], width = 1)) %>% 
          # add_lines(y = ~round(T_mean,1), name = 'T_mean', type = 'scatter', mode = 'lines', line = list(color = 'black', width = 1)) %>% 
          add_lines(y = ~round(T_max,1), name = 'Tagesmaximum', mode = 'lines', line=list(color = colors[2], width = 1)) %>% 
          layout(title = "",
                 xaxis = list(title = "",
                              zeroline=FALSE,
                              showline=FALSE,
                              ticks = "outside",
                              showspikes = TRUE,
                              spikemode= 'across',
                              spikesnap= 'hovered data',
                              spikedash = 'dashed',
                              spikethickness = 0.5,
                              spikecolor = 'gray',
                              rangeselector = list(
                                buttons = list(
                                  list(step = "all",
                                       label = "Gesamter Zeitraum"),
                                  list(
                                    count = 7,
                                    label = "Letzte 7 Tage",
                                    step = "day",
                                    stepmode = "backward"),
                                  list(
                                    count = 3,
                                    label = "Letzte 3 Monate",
                                    step = "month",
                                    stepmode = "backward"),
                                  # list(
                                  #   count = 6,
                                  #   label = "Letzte 6 Monate",
                                  #   step = "month",
                                  #   stepmode = "backward"),
                                  list(
                                    count = 1,
                                    label = "Letzte 12 Monate",
                                    step = "year",
                                    stepmode = "backward"),
                                  list(
                                    count = 1,
                                    label = "Aktuelles Jahr",
                                    step = "year",
                                    stepmode = "todate")
                                )),
                              rangeslider = list(type = "date"),
                              thickness = 0.05),
                 yaxis = list(title = "",
                              ticksuffix = "°C",
                              ticks = "outside",
                              zeroline = TRUE,
                              showline = FALSE,
                              range = c(-15,42)),
                 hovermode = 'x unified', # 'compare'
                 annotations = list(
                   list(x = 0.0 , y = 1.0,
                        text = "<b>Temperaturverlauf, Tageswerte</b>",
                        showarrow = F,
                        xref='paper', yref='paper',
                        font=list(size=12,
                                  color='black',
                                  face = 'bold',
                                  family = 'Arial'))),
                 showlegend = TRUE,
                 legend = list(x = 0.5, 
                               y = -0.35, 
                               orientation = "h", 
                               traceorder = "reversed",
                               xanchor = 'center',
                               yanchor = 'top'),
                 margin=list(t = 10, b = 35, r = 10, l = 10)
          ) %>%
          config(displaylogo = FALSE,
                 displayModeBar=TRUE,
                 modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d',
                                            'pan2d','select2d','lasso2d',
                                            'toggleSpikelines', "zoomIn2d", "zoomOut2d", 'zoom2d')
          )
      }
      else{
        plot_ly(data = tageswerte, x = ~date)  %>% 
          filter(Standort == data_of_click$clickedMarker$id)  %>% 
          add_lines(y = ~round(T_min,1), name = 'Tagesminimum', mode = 'lines', line=list(color = colors[1], width = 1)) %>% 
          # add_lines(y = ~round(T_mean,1), name = 'T_mean', type = 'scatter', mode = 'lines', line = list(color = 'black', width = 1)) %>% 
          add_lines(y = ~round(T_max,1), name = 'Tagesmaximum', mode = 'lines', line=list(color = colors[2], width = 1)) %>% 
          layout(title = "",
                 xaxis = list(title = "",
                              zeroline=FALSE,
                              showline=FALSE,
                              ticks = "outside",
                              showspikes = TRUE,
                              spikemode= 'across',
                              spikesnap= 'hovered data',
                              spikedash = 'dashed',
                              spikethickness = 0.5,
                              spikecolor = 'gray',
                              rangeselector = list(
                                buttons = list(
                                  list(step = "all",
                                       label = "Gesamter Zeitraum"),
                                  list(
                                    count = 7,
                                    label = "Letzten 7 Tage",
                                    step = "day",
                                    stepmode = "backward"),
                                  list(
                                    count = 3,
                                    label = "Letzte 3 Monate",
                                    step = "month",
                                    stepmode = "backward"),
                                  # list(
                                  #   count = 6,
                                  #   label = "Letzte 6 Monate",
                                  #   step = "month",
                                  #   stepmode = "backward"),
                                  list(
                                    count = 1,
                                    label = "Letzte 12 Monate",
                                    step = "year",
                                    stepmode = "backward"),
                                  list(
                                    count = 1,
                                    label = "Aktuelles Jahr",
                                    step = "year",
                                    stepmode = "todate")
                                )),
                              rangeslider = list(type = "date"),
                              thickness = 0.05),
                 yaxis = list(title = "",
                              ticksuffix = "°C",
                              ticks = "outside",
                              zeroline = TRUE,
                              showline = FALSE,
                              range = c(-15,42)),
                 hovermode = 'x unified', # 'compare'
                 annotations = list(
                   list(x = 0.0 , y = 1.0,
                        text = "<b>Temperaturverlauf, Tageswerte</b>",
                        showarrow = F,
                        xref='paper', yref='paper',
                        font=list(size=12,
                                  color='black',
                                  face = 'bold',
                                  family = 'Arial'))),
                 showlegend = TRUE,
                 legend = list(x = 0.5, 
                               y = -0.35, 
                               orientation = "h", 
                               traceorder = "reversed",
                               xanchor = 'center',
                               yanchor = 'top'),
                 margin=list(t = 10, b = 35, r = 10, l = 10)
          ) %>%
          config(displaylogo = FALSE,
                 displayModeBar=TRUE,
                 modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d',
                                            'pan2d','select2d','lasso2d',
                                            'toggleSpikelines', "zoomIn2d", "zoomOut2d", 'zoom2d')
          )
      } 
      
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
    
    df_comparison_table2 <- reactive({
        
        location <- c("Zürich - Wiedikon-Bertastrasse (2816)", "Zürich - Sihlfeld (537)")
        
        tageswerte %>% 
          filter(date >= "2020-06-01" & date < "2020-09-01" & Standort %in% location) %>% 
          select(date, Standort, Raumlage, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
          group_by(Standort, Raumlage) %>% 
          summarise(Temperaturminmum = paste(round(min(T_min, na.rm = T), 1), "°C"),
                    Temperaturmittel = paste(round(mean(T_mean, na.rm = T), 1), "°C"),
                    Temperaturmaximum = paste(round(max(T_max, na.rm = T), 1), "°C"),
                    Hitzetage = sum(T_max >= 30, na.rm = T),
                    `Tropennächte` = sum(T_min >= 20, na.rm = T)) %>% 
          ungroup() %>% 
          as.data.frame()
    })
    
    df_comparison_table3 <- reactive({
        
        location <- c("Zürich - Oerlikerpark (2696)", "Zürich - Vulkanplatz (2695)")
        
        tageswerte %>% 
          filter(date >= "2020-06-01" & date < "2020-09-01" & Standort %in% location) %>% 
          select(date, Standort, Raumlage, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
          group_by(Standort, Raumlage) %>% 
          summarise(Temperaturminmum = paste(round(min(T_min, na.rm = T), 1), "°C"),
                    Temperaturmittel = paste(round(mean(T_mean, na.rm = T), 1), "°C"),
                    Temperaturmaximum = paste(round(max(T_max, na.rm = T), 1), "°C"),
                    Hitzetage = sum(T_max >= 30, na.rm = T),
                    `Tropennächte` = sum(T_min >= 20, na.rm = T)) %>% 
          ungroup() %>% 
          as.data.frame()
    })
    
    df_comparison_table4 <- reactive({
        
        location <- c("Zürich - Altuetliberg (2656)", "Zürich - Perron-HB (2689)", "Zürich - Sihlfeld (537)")
        
        tageswerte %>% 
          filter(date >= "2020-06-01" & date < "2020-09-01" & Standort %in% location) %>% 
          select(date, Standort, Raumlage, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
          group_by(Standort, Raumlage) %>% 
          summarise(Temperaturminmum = paste(round(min(T_min, na.rm = T), 1), "°C"),
                    Temperaturmittel = paste(round(mean(T_mean, na.rm = T), 1), "°C"),
                    Temperaturmaximum = paste(round(max(T_max, na.rm = T), 1), "°C"),
                    Hitzetage = sum(T_max >= 30, na.rm = T),
                    `Tropennächte` = sum(T_min >= 20, na.rm = T)) %>% 
          ungroup() %>% 
          as.data.frame()
    })
    
    df_comparison_table5 <- reactive({
        
        location <- c("Elsau - Halden (2805)", "Winterthur - Neumarkt (2686)", "Winterthur - Turnerstrasse (2684)", "Winterthur  - Schulhaus-Altstadt (2691)")
        
        tageswerte %>% 
          filter(date >= "2020-06-01" & date < "2020-09-01" & Standort %in% location) %>% 
          select(date, Standort, Raumlage, T_min, T_mean, T_max, Hitzetag, Tropennacht) %>% 
          group_by(Standort, Raumlage) %>% 
          summarise(Temperaturminmum = paste(round(min(T_min, na.rm = T), 1), "°C"),
                    Temperaturmittel = paste(round(mean(T_mean, na.rm = T), 1), "°C"),
                    Temperaturmaximum = paste(round(max(T_max, na.rm = T), 1), "°C"),
                    Hitzetage = sum(T_max >= 30, na.rm = T),
                    `Tropennächte` = sum(T_min >= 20, na.rm = T)) %>% 
          ungroup() %>% 
          as.data.frame()
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
    
    # Comparison Plot: Make a line plot of the selected location(s)
    output$comparison_plot=renderPlotly({
        
        g1 = ggplot(data = df_comparison_plot(), 
                    aes(x = as.Date(df_comparison_plot()$date), 
                        text = paste0(Standort,"\n", as.character(df_comparison_plot()$date, format = "%d.%m.%Y"),"\n", T_name, ": ", round(T_value,1), "°C "))) +
            geom_line(aes(x = as.Date(df_comparison_plot()$date), y = T_value, color = Standort, group = 1), size = 0.25)  +
            scale_x_date("",
                         breaks = scales::date_breaks("months"),
                         labels = scales::label_date_short()) +
            scale_y_continuous("",
                               limits = c(min(df_comparison_plot()$T_value), max(df_comparison_plot()$T_value)),
                               breaks = scales::breaks_extended(5),
                               labels = scales::label_number(suffix = "°C", accuracy = 1)  
            ) + 
            labs(title = paste(df_comparison_plot()$T_name[1], "je Standort")) +
            theme_minimal() +
            scale_color_manual(values = colors) +
            theme(plot.title = element_text(size = 12, colour = "#3e3f3a", face = "bold"),
                  axis.title.y = element_text(size = 10, colour = "#3e3f3a",),
                  axis.text = element_text(size = 10, colour = "#3e3f3a",),
                  axis.text.x = element_text(size = 10, colour = "#3e3f3a", face = "bold"), 
                  strip.text.x = element_text(size = 12, colour = "#3e3f3a", face = "bold", hjust = 0),
                  legend.title = element_blank(),
                  legend.position = "top")
        
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
                       spikecolor = '#3e3f3a',
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
    
    output$comparison_plot2=renderPlotly({
      
      location <- c("Zürich - Wiedikon-Bertastrasse (2816)", "Zürich - Sihlfeld (537)")
      
      plot_ly(data = stundenwerte, line = list(width = 1)) %>% 
        filter(Standort %in% location) %>% 
        select(date, Standort, T_mean, time) %>% 
        add_lines(x = ~time, y = ~T_mean, color= ~Standort, mode="lines", colors = "Set1") %>% 
        layout(title = "",
               xaxis = list(
                 title = "",
                 type = "date",
                 range=c('2020-06-01', '2020-08-31'),
                 zeroline=FALSE,
                 showline=FALSE,
                 ticks = "outside",
                 showspikes = TRUE,
                 spikemode= 'across',
                 spikesnap= 'hovered data',
                 spikedash = 'dashed',
                 spikethickness = 0.5,
                 spikecolor = 'gray',
                 rangeselector = list(
                   buttons = list(
                     list(step = "all",
                          label = "Gesamter Sommer"),
                     list(
                       count = 168,
                       label = "7 Tage",
                       step = "hour",
                       stepmode = "backward")
                     )),
                 rangeslider = list(type = "date"),
                 thickness = 0.05),
               yaxis = list(title = "",
                            ticksuffix = "°C",
                            ticks = "outside",
                            zeroline = FALSE,
                            showline = FALSE,
                            range = c(0,40)),
               hovermode = 'compare', # 'compare'
               annotations = list(
                 list(x = 0.0 , y = 1,
                      text = "<b>Temperaturverlauf, Stundenmittel</b>",
                      showarrow = F,
                      xref='paper', yref='paper',
                      font=list(size=12,
                                color='black',
                                face = 'bold',
                                family = 'Arial'))),
               showlegend = TRUE,
               legend = list(x = 0.5, 
                             y = -0.5, 
                             orientation = "h", 
                             traceorder = "normal",
                             xanchor = 'center',
                             yanchor = 'top'),
               margin=list(t = 25, b = 50, r = 10, l = 10)
        ) %>%
        config(displaylogo = FALSE,
               displayModeBar=TRUE,
               modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d',
                                          'pan2d','select2d','lasso2d',
                                          'toggleSpikelines', "zoomIn2d", "zoomOut2d", 'zoom2d')
        )
    })
    
    output$comparison_plot3=renderPlotly({
      
      location <- c("Zürich - Oerlikerpark (2696)", "Zürich - Vulkanplatz (2695)")
      
      plot_ly(data = stundenwerte, line = list(width = 1)) %>% 
        filter(Standort %in% location) %>% 
        select(date, Standort, T_mean, time) %>% 
        add_lines(x = ~time, y = ~T_mean, color= ~Standort, mode="lines", colors = "Set1") %>% 
        layout(title = "",
               xaxis = list(
                 title = "",
                 type = "date",
                 range=c('2020-06-01', '2020-08-31'),
                 zeroline=FALSE,
                 showline=FALSE,
                 ticks = "outside",
                 showspikes = TRUE,
                 spikemode= 'across',
                 spikesnap= 'hovered data',
                 spikedash = 'dashed',
                 spikethickness = 0.5,
                 spikecolor = 'gray',
                 rangeselector = list(
                   buttons = list(
                     list(step = "all",
                          label = "Gesamter Sommer"),
                     list(
                       count = 168,
                       label = "7 Tage",
                       step = "hour",
                       stepmode = "backward")
                   )),
                 rangeslider = list(type = "date"),
                 thickness = 0.05),
               yaxis = list(title = "",
                            ticksuffix = "°C",
                            ticks = "outside",
                            zeroline = FALSE,
                            showline = FALSE,
                            range = c(0,40)),
               hovermode = 'compare', # 'compare'
               annotations = list(
                 list(x = 0.0 , y = 1,
                      text = "<b>Temperaturverlauf, Stundenmittel</b>",
                      showarrow = F,
                      xref='paper', yref='paper',
                      font=list(size=12,
                                color='black',
                                face = 'bold',
                                family = 'Arial'))),
               showlegend = TRUE,
               legend = list(x = 0.5, 
                             y = -0.5, 
                             orientation = "h", 
                             traceorder = "normal",
                             xanchor = 'center',
                             yanchor = 'top'),
               margin=list(t = 25, b = 50, r = 10, l = 10)
        ) %>%
        config(displaylogo = FALSE,
               displayModeBar=TRUE,
               modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d',
                                          'pan2d','select2d','lasso2d',
                                          'toggleSpikelines', "zoomIn2d", "zoomOut2d", 'zoom2d')
        )
    })

    
    output$comparison_plot4=renderPlotly({
      
      location <- c("Zürich - Altuetliberg (2656)", "Zürich - Perron-HB (2689)", "Zürich - Sihlfeld (537)")
      
      plot_ly(data = stundenwerte, line = list(width = 1)) %>% 
        filter(Standort %in% location) %>% 
        select(date, Standort, T_mean, time) %>% 
        add_lines(x = ~time, y = ~T_mean, color= ~Standort, mode="lines", colors = "Set1") %>% 
        layout(title = "",
               xaxis = list(
                 title = "",
                 type = "date",
                 range=c('2020-06-01', '2020-08-31'),
                 zeroline=FALSE,
                 showline=FALSE,
                 ticks = "outside",
                 showspikes = TRUE,
                 spikemode= 'across',
                 spikesnap= 'hovered data',
                 spikedash = 'dashed',
                 spikethickness = 0.5,
                 spikecolor = 'gray',
                 rangeselector = list(
                   buttons = list(
                     list(step = "all",
                          label = "Gesamter Sommer"),
                     list(
                       count = 168,
                       label = "7 Tage",
                       step = "hour",
                       stepmode = "backward")
                   )),
                 rangeslider = list(type = "date"),
                 thickness = 0.05),
               yaxis = list(title = "",
                            ticksuffix = "°C",
                            ticks = "outside",
                            zeroline = FALSE,
                            showline = FALSE,
                            range = c(0,40)),
               hovermode = 'compare', # 'compare'
               annotations = list(
                 list(x = 0.0 , y = 1,
                      text = "<b>Temperaturverlauf, Stundenmittel</b>",
                      showarrow = F,
                      xref='paper', yref='paper',
                      font=list(size=12,
                                color='black',
                                face = 'bold',
                                family = 'Arial'))),
               showlegend = TRUE,
               legend = list(x = 0.5, 
                             y = -0.5, 
                             orientation = "h", 
                             traceorder = "normal",
                             xanchor = 'center',
                             yanchor = 'top'),
               margin=list(t = 25, b = 50, r = 10, l = 10)
        ) %>%
        config(displaylogo = FALSE,
               displayModeBar=TRUE,
               modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d',
                                          'pan2d','select2d','lasso2d',
                                          'toggleSpikelines', "zoomIn2d", "zoomOut2d", 'zoom2d')
        )
      
    })
    
    output$comparison_plot5=renderPlotly({
      
      location <- c("Elsau - Halden (2805)", "Winterthur - Neumarkt (2686)", "Winterthur - Turnerstrasse (2684)", "Winterthur  - Schulhaus-Altstadt (2691)")

      plot_ly(data = stundenwerte, line = list(width = 1)) %>% 
        filter(Standort %in% location) %>% 
        select(date, Standort, T_mean, time) %>% 
        add_lines(x = ~time, y = ~T_mean, color= ~Standort, mode="lines", colors = "Set1") %>% 
        layout(title = "",
               xaxis = list(
                 title = "",
                 type = "date",
                 range=c('2020-06-01', '2020-08-31'),
                 zeroline=FALSE,
                 showline=FALSE,
                 ticks = "outside",
                 showspikes = TRUE,
                 spikemode= 'across',
                 spikesnap= 'hovered data',
                 spikedash = 'dashed',
                 spikethickness = 0.5,
                 spikecolor = 'gray',
                 rangeselector = list(
                   buttons = list(
                     list(step = "all",
                          label = "Gesamter Sommer"),
                     list(
                       count = 168,
                       label = "7 Tage",
                       step = "hour",
                       stepmode = "backward")
                   )),
                 rangeslider = list(type = "date"),
                 thickness = 0.05),
               yaxis = list(title = "",
                            ticksuffix = "°C",
                            ticks = "outside",
                            zeroline = FALSE,
                            showline = FALSE,
                            range = c(0,40)),
               hovermode = 'compare', # 'compare'
               annotations = list(
                 list(x = 0.0 , y = 1,
                      text = "<b>Temperaturverlauf, Stundenmittel</b>",
                      showarrow = F,
                      xref='paper', yref='paper',
                      font=list(size=12,
                                color='black',
                                face = 'bold',
                                family = 'Arial'))),
               showlegend = TRUE,
               legend = list(x = 0.5, 
                             y = -0.5, 
                             orientation = "h", 
                             traceorder = "reversed",
                             xanchor = 'center',
                             yanchor = 'top'),
               margin=list(t = 25, b = 50, r = 10, l = 10)
        ) %>%
        config(displaylogo = FALSE,
               displayModeBar=TRUE,
               modeBarButtonsToRemove = c('sendDataToCloud', 'autoScale2d',
                                          'pan2d','select2d','lasso2d',
                                          'toggleSpikelines', "zoomIn2d", "zoomOut2d", 'zoom2d')
        )
    })

    
    
    # Render a table with information on heat days and tropical nights
    output$comparison_table <- renderTable({
        
        df_comparison_table()
    })
    
    # Render a table with information on heat days and tropical nights
    output$comparison_table2 <- renderTable({
        
        df_comparison_table2()
    })
    
    # Render a table with information on heat days and tropical nights
    output$comparison_table3 <- renderTable({
        
        df_comparison_table3()
    })
    
    # Render a table with information on heat days and tropical nights
    output$comparison_table4 <- renderTable({
        
        df_comparison_table4()
    })
    
    # Render a table with information on heat days and tropical nights
    output$comparison_table5 <- renderTable({
        
        df_comparison_table5()
    })
    
    # Leaftlet map for locations "Sihlfeld" and "Bertastrasse"
    output$map_vgl1 <- renderLeaflet({
        data <- tageswerte %>% filter(date == max(date) & Standort %in% c("Zürich - Sihlfeld (537)", "Zürich - Wiedikon-Bertastrasse (2816)"))
        leaflet(data) %>% 
            
            setView(lng = 8.5107, lat = 47.3731, zoom = 15) %>% 
            
            addProviderTiles(providers$Esri.WorldImagery) %>% 
            addCircleMarkers(lng= ~E, 
                             lat= ~N, 
                             layerId = ~Standort,
                             color = colors[c(2,1)],
                             fill = 'white',
                             radius = 10,
                             opacity = 1,
                             fillOpacity = 0.5,
                             label = sprintf("<strong>%s</strong><br/>
                                              Sensor: %1.f",
                                             data$site,
                                             data$sensor) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
                                 textsize = "15px", direction = "auto"))
    })
    
    # Leaftlet map for location "Vulkanplatz"
    output$map_vgl2a <- renderLeaflet({
        data <- tageswerte %>% filter(date == max(date) & Standort == "Zürich - Vulkanplatz (2695)")
        leaflet(data) %>% 
            
            setView(lng = 8.49042, lat = 47.39237, zoom = 17) %>% 
            
            addProviderTiles(providers$Esri.WorldImagery) %>% 
            addCircleMarkers(lng= ~E, 
                             lat= ~N, 
                             layerId = ~Standort,
                             color = colors[1],
                             fill = 'white',
                             radius = 10,
                             opacity = 1,
                             fillOpacity = 0.5,)
    })
    
    # Leaftlet map for location "Oerlikerpark"
    output$map_vgl2b <- renderLeaflet({
        data <- tageswerte %>% filter(date == max(date) & Standort == "Zürich - Oerlikerpark (2696)")
        leaflet(data) %>% 
            
            setView(lng = 8.53884, lat = 47.41471, zoom = 17) %>% 
            
            addProviderTiles(providers$Esri.WorldImagery) %>% 
            addCircleMarkers(lng= ~E, 
                             lat= ~N, 
                             layerId = ~Standort,
                             color = colors[2],
                             fill = 'white',
                             radius = 10,
                             opacity = 1,
                             fillOpacity = 0.5)
    })
    
    # Leaftlet map for location "Wärmeinsel Zürich"
    output$map_vgl3 <- renderLeaflet({
        data <- tageswerte %>% filter(date == max(date) & Standort %in% c("Zürich - Altuetliberg (2656)", "Zürich - Perron-HB (2689)", "Zürich - Sihlfeld (537)"))
        leaflet(data) %>% 
            
            setView(lng = 8.5352, lat = 47.3639, zoom = 13) %>% 
            
            addProviderTiles(providers$Esri.WorldImagery) %>% 
            addCircleMarkers(lng= ~E, 
                             lat= ~N, 
                             layerId = ~Standort,
                             color = colors[c(3,2,1)],
                             fill = 'white',
                             radius = 10,
                             opacity = 1,
                             fillOpacity = 0.5,
                             label = sprintf("<strong>%s</strong><br/>
                                              Sensor: %1.f",
                                             data$site,
                                             data$sensor) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
                                 textsize = "15px", direction = "auto"))
    })
    
    # Leaftlet map for location "Wärmeinsel Winterthur"
    output$map_vgl4 <- renderLeaflet({
        data <- tageswerte %>% filter(date == max(date) & Standort %in% c("Elsau - Halden (2805)", "Winterthur - Neumarkt (2686)", "Winterthur - Turnerstrasse (2684)", "Winterthur - Schulhaus-Altstadt (2691)"))
        leaflet(data) %>% 
            
            setView(lng = 8.7722, lat = 47.4919, zoom = 13) %>% 
            
            addProviderTiles(providers$Esri.WorldImagery) %>% 
            addCircleMarkers(lng= ~E, 
                             lat= ~N, 
                             layerId = ~Standort,
                             color = colors[c(3,1,4,2)],
                             fill = 'white',
                             radius = 10,
                             opacity = 1,
                             fillOpacity = 0.5,
                             label = sprintf("<strong>%s</strong><br/>
                                              Sensor: %1.f",
                                             data$site,
                                             data$sensor) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
                                 textsize = "15px", direction = "auto"))
    })
    
### TAB HITZETAGE & TROPENNÄCHTE ###

   df_transect_plot <- reactive({
        
        current_year <- format(Sys.time(), "%Y")
        
    
        if(input$radioTransect == "Transekte Zürich - Nord-Ost") {
            tageswerte %>% 
                filter(year <= current_year & 
                           month %in% summer & 
                           # `Wärmeinsel` == "Zürich" & 
                           site %in% c("Oberembrach - Schüler", "Zürich - Schulhaus-Borrweg", "Zürich - offene Rennbahn", "Zürich - Hardplatz", "Zürich - Bucheggplatz") &
                           # Standort %in% c("Oberembrach - Schüler (2694)", "Zürich - Schulhaus-Borrweg (2660)", "Zürich - offene Rennbahn (2683)", "Zürich - Hardplatz (2652)", "Zürich - Bucheggplatz (2698)") &
                           !Raumlage %in% c("nicht definiert", "Wald")) %>%
                group_by(site, Raumlage, year) %>%  
                # group_by(Standort, Raumlage, year) %>%  
                summarise(Tagestiefsttemperatur = T_min) %>%
                ungroup() %>% 
                as.data.frame()
        } else if(input$radioTransect == "Transekte Zürich - Limmattal") {
            tageswerte %>% 
                filter(year <= current_year & 
                           month %in% summer & 
                           # `Wärmeinsel` == "Zürich" &
                           site %in% c("Schlieren - Brachweg", "Zürich - Grünau", "Zürich - Hardplatz", "Zürich - Paradeplatz", "Zürich - Sechseläutenplatz", "Zürich - Zürihorn") &
                           # Standort %in% c("Schlieren - Brachweg (2658)", "Zürich - Grünau (2657)", "Zürich - Hardplatz (2652)", "Zürich - Paradeplatz (2680)", "Zürich - Sechseläutenplatz (2682)", "Zürich - Zürihorn (2679)") &
                           !Raumlage %in% c("nicht definiert", "Wald") ) %>%
                group_by(site, Raumlage, year) %>%  
                # group_by(Standort, Raumlage, year) %>%  
                summarise(Tagestiefsttemperatur = T_min) %>%
                ungroup() %>% 
                as.data.frame()
            
        } 
    })
    
    # Make a box plot of heat days per Raumlage and year
    output$transect_plot=renderPlot({
        
      farben <- c("Land" = colors[1], "Stadtrand" = colors[4], "Stadt/Grünfläche" = colors[3], "Stadt" = colors[2])
      
        g1 <- ggplot(data = df_transect_plot(), aes(y = site, x = Tagestiefsttemperatur, fill = Raumlage)) +
            geom_boxplot(outlier.shape = NA, alpha = 0.5) +
            stat_summary(fun = mean, 
                         shape = 21, 
                         fill = "black", 
                         fun.min = min, 
                         fun.max = max, 
                         size = 0.75) +
            theme_minimal() +
            scale_fill_manual(values= farben) +
            scale_x_continuous("Tagestiefstwerte",
                               limits = c(15,28),
                               breaks = scales::breaks_extended(4),
                               labels = scales::label_number(suffix = "°C", accuracy = 1)  
            ) +
            labs(y = NULL,
                 fill = NULL) +
            facet_wrap(~year) +
            theme(plot.title = element_text(size = 16, colour = "#3e3f3a", face = "bold"),
                  axis.title = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text.x = element_text(size = 14, colour = "#3e3f3a", face = "bold"), 
                  strip.text.x = element_text(size = 16, colour = "#3e3f3a", face = "bold", hjust = 0),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14, colour = "#3e3f3a"),
                  legend.position = "top",
                  panel.spacing = unit(4, "lines"))
        
        g1 
        
    })
    
    
    output$heatspot_table <- DT::renderDataTable({
        
        df <- tageswerte %>%  
            filter(month %in% summer & !Raumlage %in% c("nicht definiert", "Wald")) %>% 
            group_by(site, Raumlage, `Wärmeinsel`, year) %>%  
            summarise(Hitzetage = sum(Hitzetag, na.rm = T),
                      `Tropennächte` = sum(Tropennacht, na.rm = T)) %>%
            ungroup() %>% 
            as.data.frame()
        
        df <- df %>% 
            tidyr::pivot_wider(., names_from = year, names_sep = " ", values_from = c(Hitzetage, `Tropennächte`)) %>% 
            tidyr::drop_na() %>%  
            arrange(desc(`Hitzetage 2019`))
        
        DT::datatable(df, options = list(lengthMenu = c(5, 10, 20, 30, 40, 50), pageLength = 10))
    })
    
    output$picker_heatspot <- renderText({
        
        input$heatspot_select
    })
    
    # Leaftlet map for Transekten
    output$transect_map <- renderLeaflet({
        if(input$radioTransect == "Transekte Zürich - Limmattal"){
            data <- tageswerte %>% filter(date == max(date) & Standort %in% c("Zürich - Zürihorn (2679)", 
                                                                              "Zürich - Sechseläutenplatz (2682)", 
                                                                              "Zürich - Paradeplatz (2680)", 
                                                                              "Zürich - Hardplatz (2652)",
                                                                              "Zürich - Grünau (2657)",
                                                                              "Schlieren - Brachweg (2658)"))
            leaflet(data) %>% 
                
                setView(lng = 8.5222, lat = 47.3731, zoom = 12) %>% 
                
                addProviderTiles(providers$Esri.WorldImagery) %>% 
                addCircleMarkers(lng= ~E, 
                                 lat= ~N, 
                                 layerId = ~Standort,
                                 color = colors[2], 
                                 radius = 6,
                                 opacity = 1,
                                 fillOpacity = 0,
                                 label = sprintf("<strong>%s</strong><br/>
                                              Sensor: %1.f",
                                                 data$site,
                                                 data$sensor) %>% lapply(htmltools::HTML),
                                 labelOptions = labelOptions(
                                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
                                     textsize = "15px", direction = "auto"))
            
        } else if(input$radioTransect == "Transekte Zürich - Nord-Ost") {
            data <- tageswerte %>% filter(date == max(date) & Standort %in% c("Zürich - Schulhaus-Borrweg (2660)", 
                                                                              "Zürich - offene Rennbahn (2683)", 
                                                                              "Zürich - Hardplatz (2652)", 
                                                                              "Zürich - Bucheggplatz (2698)",
                                                                              "Oberembrach - Schüler (2694)"))
            leaflet(data) %>% 
                
                setView(lng = 8.5848, lat = 47.4056, zoom = 11) %>% 
                
                addProviderTiles(providers$Esri.WorldImagery) %>% 
                addCircleMarkers(lng= ~E, 
                                 lat= ~N, 
                                 layerId = ~Standort,
                                 color = colors[1], 
                                 radius = 6,
                                 opacity = 1,
                                 fillOpacity = 0,
                                 label = sprintf("<strong>%s</strong><br/>
                                              Sensor: %1.f",
                                                 data$site,
                                                 data$sensor) %>% lapply(htmltools::HTML),
                                 labelOptions = labelOptions(
                                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#3e3f3a"),
                                     textsize = "15px", direction = "auto"))
        }
        
    })

    
    
    ## Navigation Buttons 1: Standorte
    rv <- reactiveValues(page = 1)

    observe({
        toggleState(id = "prevBtn", condition = rv$page > 1)
        toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
        hide(selector = ".page")
        show(paste0("step", rv$page))
    })
    
    navPage <- function(direction) {
        rv$page <- rv$page + direction
    }
    
    observeEvent(input$prevBtn, navPage(-1))
    observeEvent(input$nextBtn, navPage(1))
    
    output$page_index <- renderText({
        paste0(rv$page, " / ", NUM_PAGES)
    })
    
    ## Navigation Buttons 2: Transects
    page_start <- reactiveValues(page = 1)
    
    observe({
        toggleState(id = "upBtn", condition = page_start$page > 1)
        toggleState(id = "dwnBtn", condition = page_start$page < PAGE_NUM)
        hide(selector = ".pages")
        show(paste0("step", page_start$page))
    })
    
    navTransect <- function(direction) {
        page_start$page <- page_start$page + direction
    }
    
    observeEvent(input$upBtn, navTransect(-1))
    observeEvent(input$dwnBtn, navTransect(1))
    
    output$page_id <- renderText({
        paste0(page_start$page, " / ", PAGE_NUM)
    })
    
    output$summer2019 <- renderPlot({
        
        df1 <- sommerwerte %>% filter(year == 2019)
        
        mean_2019 <- round(mean(df1$T_max, na.rm = T), 1)
        
        summer2019 <- ggplot(df1, aes(x = date, y = T_max, group = Standort)) +
            geom_path(color = "#b9b9b9") +
            geom_point(color = ifelse(df1$T_max >= 30, colors[2], "#b9b9b9"), size = 1.5) +
            geom_hline(aes(yintercept= mean_2019), 
                       linetype= "dashed", 
                       color = "black") +
            geom_text(aes(x = date[10], 
                          y = mean_2019 - 2, 
                          label = paste("Ø: ", mean_2019, "°C")),
                      colour = "black", 
                      size = 5) +
            geom_text(x = df1$date[42], 
                      y = 42, 
                      label = "Hitzetage", 
                      color = colors[2],
                      size = 5) +
            scale_y_continuous("Tagesmaximum",
                               limits = c(0, 45),
                               breaks = scales::breaks_extended(5),
                               labels = scales::label_number(suffix = "°C", accuracy = 1)  
            ) +             theme_minimal() +
            labs(title = df1$year[1],
                 x = NULL) +
            theme(plot.title = element_text(size = 16, colour = "#3e3f3a", face = "bold"),
                  axis.title.y = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text.x = element_text(size = 14, colour = "#3e3f3a", face = "bold"), 
                  strip.text.x = element_text(size = 16, colour = "#3e3f3a", face = "bold", hjust = 0),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14, colour = "#3e3f3a"),
                  legend.position = "none")
        
        summer2019
    })

    
    output$summer2020 <- renderPlot({
        
        df2 <- sommerwerte %>% filter(year == 2020)
        
        mean_2020 <- round(mean(df2$T_max, na.rm = T), 1)
        
        summer2020 <- ggplot(df2, aes(x = date, y = T_max, group = Standort, color = Hitzetag)) +
            geom_path(color = "#b9b9b9") +
            geom_point(color = ifelse(df2$T_max >= 30, colors[2], "#b9b9b9"), size = 1.5) +
            ylim(0, 45) + 
            geom_hline(yintercept= mean_2020, 
                       linetype= "dashed", 
                       color = "black") +
            geom_text(aes(x = date[10], 
                          y = mean_2020 - 2, 
                          label = paste("Ø: ", mean_2020, "°C")),
                      colour = "black", size = 5) +
            geom_text(x = df2$date[40], 
                      y = 37, 
                      label = "Hitzetage", 
                      color = colors[2], size = 5) +
            theme_minimal() +
            labs(title = df2$year[1],
                 y = "Tagesmaximum (°C)",
                 x = NULL) +
            theme(plot.title = element_text(size = 16, colour = "#3e3f3a", face = "bold"),
                  axis.title.y = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text.x = element_text(size = 14, colour = "#3e3f3a", face = "bold"), 
                  strip.text.x = element_text(size = 16, colour = "#3e3f3a", face = "bold", hjust = 0),
                  legend.text = element_text(size = 14, colour = "#3e3f3a"),
                  legend.title = element_blank(),
                  legend.position = "none")
        
        summer2020
    })
    
    output$summermonths <- renderPlot({
        
        htage_month <- sommerwerte %>% 
            group_by(year, month, Standort) %>% 
            summarise(htag = sum(Hitzetag, na.rm = T))
        
        gg <- ggplot(htage_month, aes(x = month, y = htag, group = month, fill = as.factor(year))) +
            geom_point(size = 3, aes(fill = as.factor(year), color = as.factor(year))) +
            geom_boxplot(outlier.shape = 16, alpha = 0.5) +
            stat_summary(fun = mean, shape = 21, 
                         fill = "black", 
                         fun.min = min, 
                         fun.max = max, 
                         size = 0.75) +
            scale_fill_manual(values= colors[c(1,4)]) +
            facet_wrap(~year) +
            theme_minimal() +
            labs(y = "Anzahl Hitzetage",
                 x = NULL,
                 color = NULL) +
            theme(plot.title = element_text(size = 16, colour = "#3e3f3a", face = "bold"),
                  axis.title.y = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text.x = element_text(size = 14, colour = "#3e3f3a", face = "bold"), 
                  strip.text.x = element_text(size = 16, colour = "#3e3f3a", face = "bold", hjust = 0),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14, colour = "#3e3f3a"),
                  legend.position = "none",
                  panel.spacing = unit(4, "lines"))
        
        gg
    })
    
    output$summeryear_2019 <- renderPlot({
        
        htage_year <- sommerwerte %>% 
            filter(year == 2019)  %>% 
            group_by(year, Standort) %>% 
            summarise(htag = sum(T_max >= 30, na.rm = T))
        
        gg <- ggplot(htage_year, aes(x = as.factor(year), y = htag, fill = colors[1])) +
            geom_boxplot(outlier.shape = NA, alpha = 0.5) +
            stat_summary(fun = mean, 
                         shape = 21, 
                         fill = "black", 
                         fun.min = min, 
                         fun.max = max, 
                         size = 0.75) +
            ylim(0,43) +
            theme_minimal() +
            scale_fill_manual(values= colors[1]) +
            labs(title = htage_year$year[1], 
                 x = NULL,
                 y = "Anzahl Hitzetage",
                 fill = NULL) +
            theme(plot.title = element_text(size = 16, colour = "#3e3f3a", face = "bold"),
                  axis.title.y = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text.x = element_blank(), 
                  strip.text.x = element_text(size = 16, colour = "#3e3f3a", face = "bold", hjust = 0),
                  legend.text = element_text(size = 14, colour = "#3e3f3a"),
                  legend.title = element_blank(),
                  legend.position = "none")
        
        gg
    })
    
    output$summeryear_2020 <- renderPlot({
        
        htage_year <- sommerwerte %>% 
            filter(year == 2020) %>% 
            group_by(year, Standort) %>% 
            summarise(htag = sum(T_max >=30, na.rm = T))
        
        gg <- ggplot(htage_year, aes(x = as.factor(year), y = htag, fill = colors[4])) +
            geom_boxplot(alpha = 0.5) +
            stat_summary(fun = mean, 
                         shape = 21, 
                         fill = "black", 
                         fun.min = min, 
                         fun.max = max, 
                         size = 0.75) +
            ylim(0,43) +
            theme_minimal() +
            scale_fill_manual(values= colors[4]) +
            labs(title = htage_year$year[1], 
                 x = NULL,
                 y = "Anzahl Hitzetage",
                 fill = "Raumlage") +
            theme(plot.title = element_text(size = 16, colour = "#3e3f3a", face = "bold"),
                  axis.title.y = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text.x = element_blank(), 
                  strip.text.x = element_text(size = 16, colour = "#3e3f3a", face = "bold", hjust = 0),
                  legend.text = element_text(size = 14, colour = "#3e3f3a"),
                  legend.title = element_blank(),
                  legend.position = "none")
        
        gg
    })
    
    output$summerraumlage <- renderPlot({
        
        htag_raumlage <- tageswerte %>%  
            filter(!Raumlage %in% c("nicht definiert", "Wald") & month %in% summer) %>% 
            group_by(year, Raumlage, Standort) %>% 
            summarise(htag = sum(Hitzetag, na.rm = T))
        
        farben <- c("Land" = colors[1], "Stadtrand" = colors[4], "Stadt/Grünfläche" = colors[3], "Stadt" = colors[2])
        
        gg <- ggplot(htag_raumlage, aes(x = reorder(Raumlage, htag), y = htag, group = Raumlage, fill = reorder(Raumlage, htag))) +
            geom_boxplot(outlier.shape = NA, alpha = 0.5) +
            stat_summary(fun = mean, 
                         shape = 21, 
                         fill = "black", 
                         fun.min = min, 
                         fun.max = max, 
                         size = 0.75) +
            theme_minimal() +
            scale_fill_manual(values= farben) +
            labs(x = NULL,
                 y = "Anzahl Hitzetage",
                 fill = "Raumlage") +
            facet_wrap(~year) +
            theme(plot.title = element_text(size = 16, colour = "#3e3f3a", face = "bold"),
                  axis.title.y = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text = element_text(size = 14, colour = "#3e3f3a",),
                  axis.text.x = element_text(size = 14, colour = "#3e3f3a", face = "bold"), 
                  strip.text.x = element_text(size = 16, colour = "#3e3f3a", face = "bold", hjust = 0),
                  legend.text = element_text(size = 14, colour = "#3e3f3a"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  panel.spacing = unit(4, "lines"))
        
        gg
    })
    

    
}
    
    
##############################################################################
# Run the application 
##############################################################################
shinyApp(ui = ui, server = server)
