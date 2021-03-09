
# Sourcing required packages & libraries ----------------------------------

source("set-up.R")

# Loading data source files -----------------------------------------------

metadata <- fread("http://www.web.statistik.zh.ch/awel/LoRa/metadata/AWEL_LoRa_Logbuch.csv", 
                  header = T, 
                  skip = 2, 
                  sep = ";", 
                  select = c(1:17), 
                  na.strings = c("", "NA")) %>% 
  filter(Standort !="NA") %>% 
  mutate(von = as.POSIXct(strptime(von, "%d.%m.%Y")),
         bis = as.POSIXct(strptime(bis, "%d.%m.%Y")))


ogd_data <- readRDS("./output/stadtklima_daten.RDS") %>% 
  mutate(date = as.Date(starttime)) %>%
  select(-(starttime))

meteo_schweiz <- readRDS("./output/meteo_schweiz.RDS")


# Data cleaning: Metadata -------------------------------------------------
start_date <- min(ogd_data$date)
end_date <- max(ogd_data$date)

metadata_cleaned <- metadata %>% 
  
  # Step 1: Fill in all 'fortlaufend' with max(ogd_data$date) 
  mutate(bis = case_when(is.na(as.Date(bis)) ~ end_date, TRUE ~ as.Date(bis)),
         
         # Consistently fill in Messanordnung in case it's calibrating
         Messanordnung = case_when(Standort == "Kalibration" ~ "Kalibration",
                                   TRUE ~ "Messung")) %>% 
  
  # Step 2: Filter `metadata` to only contain 'bis' dates after the 'min(ogd_data$date)'
  filter(bis >= min(ogd_data$date)) %>% 
  
  mutate(von = case_when(as.Date(von) < start_date ~ start_date,
                         TRUE ~ as.Date(von))) %>% 
  
  # Step 3: Only keeping what is neccessary
  select(Standort, `Sensor ID`, von, bis, Sensortyp, xyKoord, `Standorthöhe (masl)`, `Sensorhöhe (magl)`, Raumlage, `Wärmeinsel`, Strahlungssituation, Messanordnung)


# Comparing metadata and ogd_data -----------------------------------------
summary(metadata_cleaned) # ISSUE #4: In Metadaten: strange value for Sensorhöhe (magl)
summary(ogd_data)

## Unique sensor
length(unique(metadata_cleaned$`Sensor ID`)) == length(unique(ogd_data$sensor))

## setdiff(x, y) --> Logic: in x, but not in y
setdiff(unique(ogd_data$sensor), unique(metadata_cleaned$`Sensor ID`)) # all sensors from ogd_data are also in metadata
setdiff(unique(metadata_cleaned$`Sensor ID`), unique(ogd_data$sensor)) # NO ISSUE: 3 sensors from metadata are not in ogd_data

## Unique site names (ISSUE #2: In Metadata: Inkonsistentes Naming der Sites) --> AWEL to fix in source files
length(unique(ogd_data$site)) == length(unique(metadata_cleaned$Standort))

ogd_sites <- unique(ogd_data$site)
metadata_sites <- unique(metadata_cleaned$Standort)

setdiff(ogd_sites, metadata_sites) # 4 sites from ogd_data are not in metadata
setdiff(metadata_sites, ogd_sites) # 9 sites from metadata are not in ogd_data

## Date range (ISSUES --> AWEL to fix in source files)
range(ogd_data$date) 

range(metadata_cleaned$von) # ISSUE #1 In Metadata: 'von' date not plausible
range(metadata_cleaned$bis) 

# Data preparation: Competing & filling metadata ---------------------

metadata_enriched <- metadata_cleaned %>% 
  
  #  Filling missing dates and values within each group of sensor combination
  group_by(`Sensor ID`) %>%

    # ‘seq.Date’ works only for Date data type, so changing it by using as.Date
  mutate(date = as.Date(von)) %>%

    # ‘seq.Date’ populates a sequence of Date data for the period that is configured
    # ‘complete’ will add rows for the missing dates.
    # Date range should be the same as in the 'ogd_data'
  complete(date = seq.Date(as.Date(min(ogd_data$date)), as.Date(max(ogd_data$date)), by="day")) %>% 
  
  fill(3:13)

# Exporting metadata --------------------------------------------

saveRDS(metadata_enriched, "./output/metadata_enriched.RDS")

# Merging metadata with daily values ----------------------------------

tageswerte = readRDS("./output/tageswerte.RDS")
tageswerte_enriched <- tageswerte %>% 
  
  #JOining the metadata
  left_join(metadata_enriched, by = c("sensor" = "Sensor ID", "date" = "date")) %>% 
  
  # Only keeping the required columns
  select(-c(Standort, x, y, von, bis, Sensortyp)) %>% 
  
  # Adding source (owner of the sensor / data) to later be able to join with the meteo_schweiz dataset
  mutate(source = "AWEL Kanton Zürich")

saveRDS(tageswerte_enriched, "./output/tageswerte_enriched.RDS")


# Merging meteo_schweiz to daily values -----------------------------------

meteo_schweiz_extended <- meteo_schweiz %>% 
  
  # Filter all records to match start and end of the ogd timeseries
  filter(between(date, min(ogd_data$date), max(ogd_data$date))) %>% 
  
  # Create the additional variables in meteo_schweiz to later be able to join with the tageswerte_enriched dataset
  mutate(sensor = as.numeric(9999),
         Messanordnung = "Messung",
         `Wärmeinsel` = "Zürich/Fluntern",
         `Sensorhöhe (magl)` = 1,
         Strahlungssituation = "nicht definiert",
         Raumlage = "nicht definiert",
         xyKoord = paste0(x, " / ", y)) %>%
  
  rename("Standorthöhe (masl)" = "masl") %>% 
  
  select(-c(x, y)) 
  
  # Join the two data frames (datasets) vertically
tageswerte_komplett <- bind_rows(meteo_schweiz_extended, tageswerte_enriched) %>% 
  
  mutate(Standort = paste0(site, " (", sensor, ")")) %>% 
  
  as.data.frame()

saveRDS(tageswerte_komplett, "./output/tageswerte_komplett.RDS")

# Checking data quality after merge ---------------------------------------------------

## Issue 5: Diskrepanz zwischen Metadaten und Messdaten
yy <- tageswerte_komplett %>% 
  filter(is.na(T_mean) & Messanordnung == "Messung")

table(yy$date) 

## Visual check: Wrap by sensor - site combination (remember: sensors can move to new site)
cols <- c("tagesmaximum" = "#D62828", "tagesminimum" = "#003049", "tagesmittel" = "#F77F00")
fill <- c("Kalibration" = "#eae2b7")

plot <-ggplot(data = tageswerte_komplett %>% filter(sensor == 9999 & date >= max(date)-365), aes(x = as.Date(date))) + 
  geom_line(aes(y = T_min), size = 0.5, color = "#003049") + 
  geom_line(aes(y = T_mean), size = 0.5, color = "#F77F00") +
  geom_line(aes(y = T_max), size = 0.5, color = "#D62828") +
  # facet_wrap(vars(sensor, site)) +
  theme_stat()

plot

