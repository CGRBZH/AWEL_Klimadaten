## R-code rOstluft Beispiel einer Datenabfrage für MeteoSchweiz Daten und 
## einfache Auswertungen / plots für Standort Zürich/Fluntern:  
  
require(rOstluft) 
require(rOstluft.plot) 
require(dplyr) 
require(tidyr) 
require(lubridate) 
require(ggplot2) 



### initialise rOstluft data store 
### -------------------------------------------------------------------------------------------------------------------- 
# make sure, your rOstluft AWS S3 credentials are contained in R-project .Renvirion file or in user HOME directory 
# see https://ostluft.github.io/rOstluft/articles/articles/tutorial.html 
aq <- store_aqmet()




### datastore content, check for 10 min air temperature date with datasource MeteoSchweiz & ETHZ 
### -------------------------------------------------------------------------------------------------------------------- 
content <- aq$get_content() 
meta <- aq$get_meta() 
sites <- 
  meta %>%  
  bind_rows_with_factor_columns() %>%  
  dplyr::filter(source %in% c("MeteoSchweiz", "ETHZ/IAC") & parameter == "T") 

unique(sites$site) 
# => we (Kt ZH) might be interested in c("Zürich/Fluntern", "Zürich/Kloten", "Zürich/Affoltern", "Neerach", "Lucketen",  
# "Oberrütti/Büelhof", "ETHZ_CHN-Gebäude", "ETHZ_Hönggerberg") <= "Uetliberg" & "Regensdorf/GubriSt" are at towers,  
# so not comparable to other sites 

# target_sites <- c("Zürich/Fluntern", "Zürich/Kloten", "Zürich/Affoltern", "Neerach", "Lucketen",  
#                   "Oberrütti/Büelhof", "ETHZ_CHN-Gebäude", "ETHZ_Hönggerberg") 

target_site <- c("Zürich/Fluntern") 

# check this year's aqmet contents for target_sites 
dplyr::filter(content , interval == "min10", site %in% target_site & parameter == "T" & n > 0 & year %in% c(2019, 2020, year(today()))) 




### get all those data (and include Hr parameter for fun => might be used for PET index later on) 
### I picked year 2019 because heat-wise more is going on compared to 2020, but as seen above, data for 2020 are available 
### -------------------------------------------------------------------------------------------------------------------- 
data <- aq$get(site = target_site, year = c(2019, 2020, 2021), interval = "min10", filter = parameter %in% c("T")) 
str(data) 

# you can turn these long data format into wide format (compatible with openair package) 
data_wide <-  
  data %>%  
  rolf_to_openair() %>%  
  dplyr::rename(starttime = date) 
str(data_wide) 

# add site coordinates etc 
data_wide <- left_join(data_wide, dplyr::select(sites, site, x, y, masl, source), by = "site") 
data_wide 





### some plots for temperature metrics in Zürich/Fluntern (Klimamessnetz Standort MeteoSchweiz => ausschlaggebend für Zeitstrahl-Übersicht Hitzetage etc) 
### try it out... 
### -------------------------------------------------------------------------------------------------------------------- 
temperature_metrics <- function(data) { 
  data %>%  
    group_by(date = as_date(starttime), site, x, y, masl, source) %>%  
    dplyr::summarise( 
      T_min = min(T, na.rm = TRUE), 
      T_max = max(T, na.rm = TRUE), 
      T_mean = mean(T, na.rm = TRUE), 
      T_sd = sd(T, na.rm = TRUE), 
      T_median = median(T, na.rm = TRUE), 
      Hitzetag = ifelse(T_max > 30, TRUE, FALSE), 
      Tropennacht = ifelse(T_min > 20, TRUE, FALSE) 
    ) %>%  
    ungroup() 
} 

summer <- c("Jun", "Jul", "Aug") 

d <-  
  temperature_metrics(data_wide) %>%  
  mutate( 
    year = year(date),  
    month = month(date, label = TRUE) 
  ) 



# eine Art Zeitstrahl für Hitzetage & Tropennächte 
d %>%  
  dplyr::select(date, month, site, Hitzetag, Tropennacht) %>%  
  dplyr::filter(site == target_site & month %in% summer) %>%  
  gather(stat, value, -date, -month, -site) %>%  
  ggplot(aes(x = date, y = 1, fill = value)) + 
  geom_tile() + 
  facet_grid(stat~month, scales = "free_x", switch = "y") + 
  theme_void(base_size = 18) + 
  scale_fill_brewer() 


# eine Art Sommer-Zeitstrahl für Tmax & T_min 
d %>%  
  dplyr::select(date, month, site, T_max, T_min) %>%  
  dplyr::filter(site == target_site & month %in% summer) %>%  
  gather(stat, value, -date, -month, -site) %>%  
  ggplot(aes(x = date, y = 1, fill = value)) + 
  geom_tile() + 
  facet_grid(stat~month, scales = "free_x", switch = "y") + 
  theme_void(base_size = 18) + 
  scale_fill_viridis_c(option = "A", direction = -1) 


# oder nur T_max (engere Farbskala) 
d %>%  
  dplyr::select(date, month, site, T_max) %>%  
  dplyr::filter(site == target_site & month %in% summer) %>%  
  ggplot(aes(x = date, y = 1, fill = T_max)) + 
  geom_tile() + 
  facet_grid(.~month, scales = "free_x", switch = "y") + 
  theme_void(base_size = 18) + 
  scale_fill_viridis_c(option = "A", direction = -1) 


# Kalender plots 
d %>% 
  dplyr::filter(site == target_site) %>%  
  ggcalendar(x = "date", z = "T_max") + 
  cal_month_border() + 
  cal_label(aes(label = ifelse(Hitzetag, "x", NA))) + 
  scale_fill_viridis_c(option = "A", direction = -1, na.value = NA) 

d %>% 
  dplyr::filter(site == target_site) %>%  
  ggcalendar(x = "date", z = "Hitzetag") + 
  cal_month_border() + 
  cal_label(aes(label = mday(date))) + 
  scale_fill_manual(values = c("steelblue", "red3")) 


# Tages-Jahresgang der Temperatur auf Basis min10 
data_wide %>%  
  dplyr::filter(site == target_site) %>%  
  ggyearday(time = starttime, z = T) + 
  scale_fill_viridis_squished(limits = c(NA, 30), breaks = seq(-20, 30, 5), option = "A", direction = -1, na.value = NA) 




# TRANSFORM COORDINATES FROM LV95 TO WGS84 --------------------------------

coords_LV95 <- d %>%  
  distinct(., site, x, y) %>% 
  select("E" = x, "N" = y) 

## Create a Spatial points data frame for data, where E (East) and N (North) are the fields with coordinates
sp::coordinates(coords_LV95) <- ~ E + N

## Tell R our data it's on LV95, EPSG=2056
coords_LV95@proj4string <- CRS("+init=epsg:2056")

## Check the original coords
# coords_lv95@coords

## Plot the data after transformation to check
plot(coords_LV95)

## Transform the data to WGS84
coords_wgs84 <- sp::spTransform(coords_LV95, CRS("+init=epsg:4326"))

## Look at the transformed coords
# test2@coords

## Plot the data to check
plot(coords_wgs84)

E <- coords_WGS84$E

d <- d %>%  
  mutate(E = ifelse(x == coords_LV95$E, 8.565718, NA),
         N = ifelse(y == coords_LV95$N, 47.37792, NA))

# Exporting MeteoSchweiz Data
saveRDS(d, "./output/meteo_schweiz.RDS")
