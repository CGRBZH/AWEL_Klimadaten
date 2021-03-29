# SOURCE REQUIRED PACKAGES / LIBRARIES / HELPER FUNCTIONS --------------------------------------

source("set-up.R")

# Load helper function
source("temperature_metrics.r")

# IMPORT DATA FILES -------------------------------------------------------

klima <- readRDS("./output/stadtklima_daten.RDS") %>% 
  as.data.frame()

messnetz <- readRDS("./output/messnetz.RDS")

# CALCULATING STUNDENWERTE -------------------------------------------
## Attention: The following operations are computationally intensive and take some time to complete

df <- klima %>% 
  filter(starttime >= "2020-06-01 00:00:00" & starttime < "2020-09-01 00:00:00") %>% 
  mutate(date = as.Date(starttime),
         time = format(starttime, "%Y-%m-%d %H"))

stundenwerte <- temperature_metrics(df, temperature, time, date, sensor, site, x, y) %>% 
  mutate(Standort = paste0(site, " (", sensor, ")"),
         month = month(date, label = TRUE)) %>% 
  select(-c(T_sd, T_median, Hitzetag, Tropennacht))

# CALCULATING STUNDENWERTE: POPULATE MISSING DATES ------------------------

## The problem is that we don’t have all the dates during the recorded period in the data. 
## Instead, we have only the dates when values are actually recorded. 
## To merge with the metadata we need consecutive dates from start up to now.

## To populate missing dates we are working with ‘complete’ from ‘tidyr’ package and ‘seq.Date’ from base R
## Basic structure: complete(Date = seq.Date(<start_date>, <end_date>, by=<date_unit>))
## Reference: https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5

stundenwerte_populated <- stundenwerte %>%
  # Only selecting summer period for hourly values
  filter(month %in% c("Jun", "Jul", "Aug")) %>% 

  # Filling missing dates and values within each group of unique sensor / site combination
  group_by(sensor, site) %>% 
  
  # ‘seq.Date’ populates a sequence of Date data for the period that is configured
  # ‘complete’ will add rows for the missing dates.
  # Add all date column combinations that should be pre-filled
  complete(date = seq.Date(min(date), max(date), by="day")) %>% 
  
  # Add all date column combinations that should be copied from the above line and filled into the newly added rows
  fill(x, y, month, time, Standort) %>% 
  
  ungroup() %>% 
  
  # Joining WGS84 transformed coordinates
  left_join(messnetz) %>% 
  
  select(-c(x,y, sensor, site))

# CALCULATING TAGESWERTE ---------------------------------------------

## Calculating daily minimum, daily maximum, daily mean, heat days, tropical nights
tageswerte <- temperature_metrics(klima, temperature, date = as_date(starttime), site, sensor, x, y) %>% 
  # Adding 'year_month' for input selection in shiny app
  mutate( 
    year = year(date),  
    month = month(date, label = TRUE) 
  ) %>% 
  select(-c(T_sd, T_median))

# CALCULATING TAGESWERTE: POPULATE MISSING DATES ---------------------------------

tageswerte_populated <- tageswerte %>%
  
  # Filling missing dates and values within each group of unique sensor / site combination
  group_by(sensor, site) %>% 
  
  # Add all date column combinations that should be pre-filled
  complete(date = seq.Date(min(date), max(date), by="day")) %>% 
  
  # Add all date column combinations that should be copied from the above line and filled into the newly added rows
  fill(x, y, year, month) %>% 
  
  ungroup() %>% 
  
  # Joining WGS84 transformed coordinates
  left_join(messnetz)

# SAVE OUTPUT FILES --------------------------------------------------------

saveRDS(stundenwerte_populated, "./output/stundenwerte.RDS")
saveRDS(stundenwerte_populated, "./Stadtklima_v2/stundenwerte.RDS")
saveRDS(tageswerte_populated, "./output/tageswerte.RDS")


# SOME PLOTS: TAGESWERTE -----------------------------------------

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

