# SOURCE REQUIRED PACKAGES / LIBRARIES / HELPER FUNCTIONS --------------------------------------

source("set-up.R")

# LOAD DATA FILES ---------------------------------------------------------

tageswerte <- readRDS("./output/tageswerte_komplett.RDS")

klima <- readRDS("./output/stadtklima_daten.RDS")

# DATA PROCESSING: LOCATION SELECTION -------------------------------------
## For the comparability of the sites, the analysis only includes sites 
## that have a threshold of 70% readings per day during the summer months (Jun, Jul, Aug).
## Summer period = 91 days; 144 readings per day (6 readings per hour * 24 hours)
## Threshold (70%): 100 readings per day

locations <- klima %>% 
  select(starttime, site, sensor, temperature) %>% 
  mutate(date = as.Date(starttime),
         month = month(date, label = TRUE), 
         year = year(date)) %>%
  
  filter(month %in% c("Jun", "Jul", "Aug")) %>% 
  
  # Filling missing dates and values within each group of unique sensor / site combination
  group_by(sensor, site) %>% 
  
  complete(date = seq.Date(min(date), max(date), by="day")) %>% 
  
  fill(starttime, site, sensor, year, month) %>% 
  
  ungroup() %>% 
  
  group_by(site, sensor, starttime) %>% 
  
  summarise(valid_reading = if_else(!is.na(temperature), TRUE, FALSE)) %>% 
  
  ungroup() %>% 
  
  mutate(date = as.Date(starttime)) %>% 
  
  group_by(site, sensor, date) %>% 
  
  summarise(n_readings = sum(valid_reading == TRUE)) %>% 
  
  ungroup()

valid_locations <- locations %>%
  mutate(year = year(date),
         Standort = paste0(site, " (", sensor, ")")) %>% 
  group_by(Standort, year) %>% 
  filter(!any(n_readings < 100))

loc <- lapply(split(valid_locations$Standort, valid_locations$year), unique)


## filter locations in b (2020) that are also in a (2019)
a <- as.data.frame(loc$`2019`)
a <- rename(a, location = colnames(a))
b <- as.data.frame(loc$`2020`)
b <- rename(b, location = colnames(b))

locations <- semi_join(b,a, by = "location")

# ALTERNATIVE FILTERING --------------------------------------------

## Filters according to AWEL
### Raumlage: Stadt
### Zeit: Sommermonate - Jun., Jul., Aug.
### MASL: <= 650

summer_filtered <- tageswerte %>% 
  filter(Raumlage == "Stadt" & 
           month %in% c("Jun", "Jul", "Aug") & 
           `Standorthöhe (masl)` <= 650) %>% 
  group_by(year, month, Standort) %>% 
  summarise(n = n(),
            sum_na = sum(is.na(T_max))) %>% 
  filter(n >= 27 & sum_na < 3) %>% 
  tidyr::pivot_wider(names_from = Standort, values_from = n) %>% 
  arrange(year, month) %>% 
  janitor::adorn_totals(name = "Total") %>% 
  select(-c(month, sum_na)) %>% 
  filter(year == "Total") %>% 
  tidyr::pivot_longer(cols = -(year), names_to = "Standort", values_to = "Total") %>% 
  filter(Total > 120)

standorte <- unique(summer_filtered$Standort)


# CALCULATING SUMMER VALUES FOR VALID LOCATIONS ----------------------------

summer <- tageswerte %>%  filter(Standort %in% locations$location &
                                   Raumlage == "Stadt" & 
                                   month %in% c("Jun", "Jul", "Aug") & 
                                   `Standorthöhe (masl)` <= 650) %>% 
  select(-c(T_sd, T_median))

# SAVE OUTPUT FILE --------------------------------------------------------
saveRDS(summer, "./Stadtklima_v2/sommerwerte.RDS")

