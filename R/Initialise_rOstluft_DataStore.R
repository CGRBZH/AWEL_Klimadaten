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

target_sites <- c("Zürich/Fluntern", "Zürich/Kloten", "Zürich/Affoltern", "Neerach", "Lucketen",  
                  "Oberrütti/Büelhof", "ETHZ_CHN-Gebäude", "ETHZ_Hönggerberg") 

# check this year's aqmet contents for target_sites 
dplyr::filter(content , interval == "min10", site %in% target_sites & parameter == "T" & n > 0 & year == year(today())) 




### get all those data (and include Hr parameter for fun => might be used for PET index later on) 
### I picked year 2019 because heat-wise more is going on compared to 2020, but as seen above, data for 2020 are available 
### -------------------------------------------------------------------------------------------------------------------- 
data <- aq$get(site = target_sites, year = 2019, interval = "min10", filter = parameter %in% c("T", "Hr")) 
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
  data_wide %>%  
    group_by(date = as_date(starttime), site) %>%  
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
  data_wide %>%  
  temperature_metrics() %>%  
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
  geom_raster() + 
  facet_grid(stat~month, scales = "free_x", switch = "y") + 
  theme_void(base_size = 18) + 
  scale_fill_brewer() 


# eine Art Sommer-Zeitstrahl für Tmax & T_min 
d %>%  
  dplyr::select(date, month, site, T_max, T_min) %>%  
  dplyr::filter(site == target_site & month %in% summer) %>%  
  gather(stat, value, -date, -month, -site) %>%  
  ggplot(aes(x = date, y = 1, fill = value)) + 
  geom_raster() + 
  facet_grid(stat~month, scales = "free_x", switch = "y") + 
  theme_void(base_size = 18) + 
  scale_fill_viridis_c(option = "A", direction = -1) 


# oder nur T_max (engere Farbskala) 
d %>%  
  dplyr::select(date, month, site, T_max) %>%  
  dplyr::filter(site == target_site & month %in% summer) %>%  
  ggplot(aes(x = date, y = 1, fill = T_max)) + 
  geom_raster() + 
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