library(readr)
library(dplyr)
library(ggplot2)
library(statR)

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
             "AWEL_Sensors_LoRa_202007.csv")
ldf <- list() # creates a list

for(f in 1:length(listcsv)){
  ldf[[f]] <- read_delim(paste0(base_url, listcsv[f]), ";", escape_double = FALSE, trim_ws = TRUE) %>%
    mutate(starttime = as.POSIXct(starttime, , '%Y-%m-%d %H:%M', tz = "CEST" ),
           date = strptime(starttime, '%Y-%m-%d'),
           year = strftime(date, "%Y"),
           month = strftime(date, "%m"),
           day = strftime(date, "%d"),
           hour = strftime(starttime, '%H:%M', tz = "CEST"))
  }

# length(is.na(ldf[[1]]))

# Bindind all 2020 data frames into one data frame with daily mean values for temp. and humidity
klima <- bind_rows(ldf)

write.csv(klima, "LoRa_Klimadaten_GesamterZeitraum.csv")

# Tageswerte
klima_tageswerte <- klima %>%
  # Tageswerte
  group_by(site, sensor, year, month, day) %>%
  summarise(daily_mean_temp = mean(temperature),
            daily_mean_humid = mean(humidity)) %>%
  ungroup()

# theme_set(theme_stat())

get_colour <- function(df){
  colfunc <- colorRampPalette(c("blue", "red"))
  my_colour <- colfunc(12)
  
  klima %>%
    group_by(month) %>%
    summarise(monthly_mean_temp = round(mean(temperature), 1)) %>%
    arrange(monthly_mean_temp) %>%
    pull(month) %>%
    as.integer() -> my_order
  
  my_colour[match(1:12, my_order)]
}

my_colour <- get_colour(monthly_temp_531)

monthly_temp_531 <- klima %>%
  filter(sensor == 531) %>%
  # Tageswerte
  group_by(site, sensor, year, month) %>%
  summarise(monthly_mean_temp = mean(temperature),
            monthly_mean_humid = mean(humidity)) %>%
  ungroup()

daily_temp_531 <- klima %>%
  filter(sensor == 531) %>%
  # Tageswerte
  group_by(site, sensor, year, month, day, date) %>%
  summarise(daily_mean_temp = mean(temperature),
            daily_mean_humid = mean(humidity)) %>%
  ungroup()

par(mfrow=c(1,2))
ggplot(subset(daily_temp_531, year == 2020), aes(day, daily_mean_temp, color = month)) +
  geom_point(aes(group = month), size = 2) +
  geom_smooth(aes(group = month), method = "loess") +
  ylim(0,30) +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = my_colour) +
  labs(title = "Durchschnittliche Tagestemperatur pro Monat", 
       subtitle = "Bülach - Feldermösli: Januar - Juli 2020", 
       y = "Temperatur (°C)") +
  facet_wrap(~month) +
  NULL

ggplot(subset(daily_temp_531, year == 2019), aes(day, daily_mean_temp, color = month)) +
  geom_point(aes(group = month), size = 2) +
  geom_smooth(aes(group = month), method = "loess") +
  ylim(0,30) +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = my_colour) +
  labs(title = "Durchschnittliche Tagestemperatur pro Monat", 
       subtitle = "Bülach - Feldermösli: April - Dez. 2019", 
       y = "Temperatur (°C)") +
  facet_wrap(~month) +
  NULL

ggplot(daily_temp_531, aes(day, daily_mean_temp, color = month)) +
  geom_point(aes(group = month), size = 2) +
  geom_smooth(aes(group = month), method = "loess") +
  ylim(0,30) +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = my_colour) +
  labs(title = "Durchschnittliche Tagestemperatur pro Monat", 
       subtitle = "Bülach - Feldermösli: 2019 und 2020", 
       y = "Temperatur (°C)") +
  facet_wrap(~month + year, ncol = 4) +
  # facet_grid(month ~ year)
  # facet_grid(~month) +
  NULL


ggplot(monthly_temp_531, aes(month, monthly_mean_temp, color = year)) +
  geom_line(aes(group = year), color = "#A9A9A9") +
  geom_point(size = 2) +
  ylim(0,30) +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "top") +
  scale_color_manual(values = c("#DC143C","#00BFFF")) +
  labs(title = "Durchschnittliche Monatstemperatur", 
       subtitle = "Bülach - Feldermösli: Januar 2019 - Juli 2020", 
       y = "Temperatur (°C)",
       color = NULL) +
  facet_wrap(~year) +
  NULL

coeff <- 3.333333
temperatureColor <- "#DC143C"
humidityColor <- "#00BFFF"

ggplot(data = subset(monthly_temp_531, year == 2020), mapping = aes(x = month, y = monthly_mean_humid, group = 1)) + 
  geom_bar(stat = "identity", fill = humidityColor) + 
  
  # Scale data to match desired scale
  geom_line(mapping = aes(y = monthly_mean_temp * coeff), color=temperatureColor, size=1.5) + 
  scale_y_continuous(limits = c(0, 100), "Luftfeuchtigkeit [%]", 
  
                     # Reverse transformation to match data
                     sec.axis = sec_axis(~ . / coeff, name = "Temperatur [°C]")) + 
  theme(axis.title.y = element_text(color = humidityColor, face = "bold", size = 12),
        axis.text.y = element_text(color = humidityColor),
        axis.title.y.right = element_text(color = temperatureColor, face = "bold", size = 12),
        axis.text.y.right = element_text(color = temperatureColor)) +
  labs(title = "Durchschnittstemperatur und Luftfeuchtigkeit",
       subtitle = "Bülach - Feldermösli: Januar - Juli 2020",
       x = NULL)


library(viridis)     ## color palette
library(ggjoy)       ## ridges
library(hrbrthemes)  ## plot theme

dat <- klima %>%
  filter(year == 2020 & sensor == 531) %>%
  mutate(starttime = as.POSIXct(starttime, , '%Y-%m-%d %H:%M', tz = "CEST" ),
         date = strptime(starttime, '%Y-%m-%d'),
         month = strftime(date, "%m"))

#scales
mins <- min(dat$temperature)
maxs <- max(dat$temperature)
mean <- mean(dat$temperature)

#plot

## in black and white
ggplot(dat,aes(x = temperature, y = month, height = ..density..))+
  geom_joy(aes(group = month)) +
  scale_x_continuous(limits = c(mins,maxs))+
  theme_ipsum(grid=F)+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 180, hjust = 1))+
  labs(title='Temperatures in Pittsburgh',
       subtitle='Median temperatures (Fahrenheit) by month for 2016\nData: Original CSV from the Weather Underground', 
       x = "Mean Tempterature [ºF]")

## in color
ggplot(dat, aes(x = temperature, y = month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Temp. [ºC]", option = "C") +
  labs(title = 'Temperaturen in Bülach',
       subtitle = 'Durchschnittstemperatur (°C) pro Monat für 2020\nData: Original CSV von AWEL via opendata.swiss', 
       x = "∅ Temperatur (°C)") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())