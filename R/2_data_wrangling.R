
# SOURCE REQUIRED PACKAGES / LIBRARIES --------------------------------------

source("set-up.R")

# Auswertungsfunktion
source("temperature_metrics.r")

# IMPORT 'KLIMA' DATA -------------------------------------------------------

klima <- readRDS("./output/stadtklima_daten.RDS")


# TRANSFORM COORDINATES FROM LV95 TO WGS84 --------------------------------

coords_LV95 <- klima %>%  
  distinct(., site, sensor, x, y) %>% 
  select("E" = x, "N" = y) 

saveRDS(coords_LV95, "./output/coords_LV95.RDS")

coords_lv95 <- readRDS("./output/coords_LV95.RDS")

## Create a Spatial points data frame for data, where E (East) and N (North) are the fields with coordinates
sp::coordinates(coords_lv95) <- ~ E + N

## Tell R our data it's on LV95, EPSG=2056
coords_lv95@proj4string <- CRS("+init=epsg:2056")

## Check the original coords
# coords_lv95@coords

## Plot the data after transformation to check
plot(coords_lv95)

## Transform the data to WGS84
coords_wgs84 <- sp::spTransform(coords_lv95, CRS("+init=epsg:4326"))

## Look at the transformed coords
# test2@coords

## Plot the data to check
plot(coords_wgs84)

saveRDS(coords_wgs84, "./output/coords_wgs84.RDS")
# write.table(coords_WGS84, "coords_wgs84.csv", sep = ",")

## Check length of data sets
nrow(coords_wgs84@coords) == nrow(coords_lv95@coords)

## Merging coordinates to 'temp values'klima' data-frame
coords_WGS84 <- readRDS("./output/coords_WGS84.RDS")

messnetz <- klima %>%  
  distinct(., site, sensor, x, y) %>%
  mutate(E = coords_WGS84$E,
        N = coords_WGS84$N)

## Exporting the final coordinates for the network
messnetz <- saveRDS(messnetz, "./output/messnetz.RDS")
messnetz <- readRDS("./output/messnetz.RDS")


# DATA PROCESSING: STUNDENWERTE -------------------------------------------

stundenwerte <- klima %>% 
  mutate(date = as.Date(starttime),
         hour = hour(starttime)) %>% 
  group_by(date, hour, sensor, site, x, y) %>% 
  summarise(T_min = min(temperature, na.rm = T),
            T_mean = mean(temperature, na.rm = T),
            T_max = max(temperature, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(day = day(date),
         month = month(date, label = TRUE),
         year = year(date),
         standort = paste(site, "(", sensor, ")"))

saveRDS(stundenwerte, "./output/stundenwerte.RDS")

stundenwerte_populated <- stundenwerte %>% 
  # Filling missing dates and values within each group of unique sensor / site combination
  group_by(sensor, site) %>% 
  
  # ‘seq.Date’ populates a sequence of Date data for the period that is configured
  # ‘complete’ will add rows for the missing dates.
  # Add all date column combinations that should be pre-filled
  complete(date = seq.Date(min(date), max(date), by="day")) %>% 
  
  # Add all date column combinations that should be copied from the above line and filled into the newly added rows
  fill(x, y, year, month, day, standort) %>% 
  
  ungroup() %>% 
  
  # Joining WGS84 transformed coordinates
  left_join(messnetz) %>% 
  
  select(-c(x,y))

saveRDS(stundenwerte_populated, "./output/stundenwerte.RDS")

# DATA PROCESSING: TAGESWERTE ---------------------------------------------

## Calculating daily minimum, daily maximum, daily mean, heat days, tropical nights
tageswerte <- temperature_metrics(klima, temperature, date = as_date(starttime), site, sensor, x, y) %>% 
  # Adding 'year_month' for input selection in shiny app
  mutate( 
    year = year(date),  
    month = month(date, label = TRUE) 
  )

# DATA PROCESSING: POPULATE MISSING DATES ---------------------------------

## The problem is that we don’t have all the dates during the tracked period in the data. 
## Instead, we have only the dates when values are actually measured. 
## To merge with the metadata we need all dates.

## Reference: https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5

## To populate missing dates we are working with ‘complete’ from ‘tidyr’ package and ‘seq.Date’ from base R
## Basic structure: complete(Date = seq.Date(<start_date>, <end_date>, by=<date_unit>))
tageswerte_populated <- tageswerte %>%
  
  # Filling missing dates and values within each group of unique sensor / site combination
  group_by(sensor, site) %>% 
  
  # ‘seq.Date’ works only for Date data type, so changing it by using as.Date
  # mutate(date = as.Date(date)) %>%
  
  # ‘seq.Date’ populates a sequence of Date data for the period that is configured
  # ‘complete’ will add rows for the missing dates.
  # Add all date column combinations that should be pre-filled
  complete(date = seq.Date(min(date), max(date), by="day")) %>% 
  
  # Add all date column combinations that should be copied from the above line and filled into the newly added rows
  fill(x, y, year, month) %>% 
  
  ungroup() %>% 
  
  # Joining WGS84 transformed coordinates
  left_join(messnetz)

## Check: We should have introduced NA for the calculated values 
na <- which(is.na(tageswerte_populated$T_min))

check_new_rows <- tageswerte_populated %>% filter(is.na(T_min))


## Exporting daily values
saveRDS(tageswerte_populated, "./output/tageswerte.RDS")


# PLOTTING: TAGESWERTE -----------------------------------------

target_sensor <- 531

# Kalender plots 
tageswerte_populated %>% 
  dplyr::filter(sensor == target_sensor) %>%  
  ggcalendar(x = "date", z = "T_max") + 
  cal_month_border() + 
  scale_fill_viridis_c(option = "A", direction = -1, na.value = NA) + 
  cal_label(aes(label = ifelse(Hitzetag, "x", NA)))
  

tageswerte_populated %>% 
  dplyr::filter(sensor == target_sensor) %>%  
  ggcalendar(x = "date", z = "Hitzetag") + 
  cal_month_border() + 
  cal_label(aes(label = mday(date))) + 
  scale_fill_manual(values = c("steelblue", "red3"))
