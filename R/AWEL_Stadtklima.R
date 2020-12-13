source("set-up.R")

# Load files directly from opendata.swiss
base_url <- "http://www.web.statistik.zh.ch/awel/LoRa/data/"
listcsv <- c("AWEL_Sensors_LoRa_201905.csv",
             "AWEL_Sensors_LoRa_201906.csv",
             "AWEL_Sensors_LoRa_201907.csv",
             "AWEL_Sensors_LoRa_201908.csv",
             "AWEL_Sensors_LoRa_201909.csv",
             "AWEL_Sensors_LoRa_201910.csv",
             "AWEL_Sensors_LoRa_201911.csv",
             "AWEL_Sensors_LoRa_201912.csv",
             "AWEL_Sensors_LoRa_202001.csv", 
             "AWEL_Sensors_LoRa_202002.csv", 
             "AWEL_Sensors_LoRa_202003.csv",
             "AWEL_Sensors_LoRa_202004.csv",
             "AWEL_Sensors_LoRa_202005.csv",
             "AWEL_Sensors_LoRa_202006.csv",
             "AWEL_Sensors_LoRa_202007.csv",
             "AWEL_Sensors_LoRa_202008.csv",
             "AWEL_Sensors_LoRa_202009.csv",
             "AWEL_Sensors_LoRa_202010.csv",
             "AWEL_Sensors_LoRa_202011.csv")
ldf <- list() # creates a list

for(f in 1:length(listcsv)){
  ldf[[f]] <- read_delim(paste0(base_url, listcsv[f]), ";", escape_double = FALSE, trim_ws = TRUE) %>%
    mutate(starttime = as.POSIXct(starttime, '%Y-%m-%d %H:%M'),
           date = strptime(starttime, '%Y-%m-%d'),
           year = strftime(date, "%Y"),
           month = strftime(date, "%m"),
           day = strftime(date, "%d"),
           hour = strftime(starttime, '%H:%M'))
  }

# length(is.na(ldf[[1]]))

# Bindind all data frames into one data frame with daily mean values for temp. and humidity
klima <- bind_rows(ldf)

saveRDS(klima, "stadtklima_daten.RDS")

# Transform coordinates from LV95 to WGS84

library(sp)
library(ggmap)
library(tmaptools)

coords_LV95 <- klima %>%  
  distinct(., site, sensor, x, y) %>% 
  select("E" = x, "N" = y) 
write.table(coords_LV95, "coords_LV95.csv", sep = ",")

coords_lv95 <- read.csv("coords_LV95.csv")

# Create a Spatial points data frame for data, where E (East) and N (North) are the fields with coordinates
sp::coordinates(coords_lv95) <- ~ E + N

# Tell R our data it's on LV95, EPSG=2056
coords_lv95@proj4string <- CRS("+init=epsg:2056")

# Check the original coords
# coords_lv95@coords

# Plot the data after transformation to check
plot(coords_lv95)

# Transform the data to WGS84
coords_wgs84 <- sp::spTransform(coords_lv95, CRS("+init=epsg:4326"))

# Look at the transformed coords
# test2@coords

# Plot the data to check
plot(coords_wgs84)

coords_WGS84 <- coords_wgs84@coords

write.table(coords_WGS84, "coords_wgs84.csv", sep = ",")

# Check length of data sets
nrow(coords_wgs84@coords) == nrow(coords_lv95@coords)

# Merging coordinates to 'temp values'klima' data-frame
coords_WGS84 <- read.csv("coords_WGS84.csv")

stationennetz <- klima %>%  
  distinct(., site, sensor, x, y) %>%
  mutate(E = coords_WGS84$E,
        N = coords_WGS84$N)


# Tageswerte: min/max. Tagestemp., Hitzetag, Tropennacht
klima_tageswerte <- klima %>%
  # Tageswerte
  group_by(site, sensor, date, year, month, day, x, y) %>%
  summarise(daily_min_temp = min(temperature),
            daily_max_temp = max(temperature),
            daily_mean_temp = mean(temperature)) %>%
  ungroup() %>% 
  # Kalendertag an dem maximale, 10-Minuten-gemittelte Temperatur > 30°C
  mutate(hitzetag = case_when(daily_max_temp > 30 ~ 1,TRUE ~ 2),
         #Kalendertag an dem minimale, 10-Minuten-gemittelte Temperatur > 20°C
         tropennacht = case_when(daily_min_temp > 20 ~ 1,TRUE ~ 2))

n_hitzetage <- klima_tageswerte %>% 
  group_by(site, sensor, x, y, year, month) %>% 
  tally(hitzetag == 1, name = "n_hitzetage")

n_tropennaechte <- klima_tageswerte %>% 
  group_by(site, sensor, x, y, year, month) %>% 
  tally(tropennacht == 1, name = "n_tropennacht")

monatswerte <- n_hitzetage %>% 
  merge(n_tropennaechte)



