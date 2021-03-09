library(tidyverse)
library(ggplot2)
library(ggridges)
library(lubridate)


stadtklima_daten <- readRDS("C:/gitrepos/AWEL_Interaktive_Stadtklima_Auswertungen/output/tageswerte_komplett.RDS")

current_year <- format(Sys.time(), "%Y")

df_ridgelines <- stadtklima_daten %>% 
  filter(year <= current_year & month >= "Mai" & month <= "Dez" & Wärmeinsel == "Zürich") %>%
  as.data.frame()
# Documentation ggridges: https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

gg <- ggplot(df_ridgelines, aes(x = T_min, y = factor(year), fill = stat(x), group = year)) + 
  geom_density_ridges_gradient(aes(x = T_min, y = factor(year), fill = stat(x)), scale = 0.9, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_viridis_c(name = "Temperatur (°C)", option = "C") +
  theme_minimal() +
  labs(y = NULL,
       x = "Tagestiefsttemperatur (°C)")
gg

tageswerte = readRDS("C:/gitrepos/AWEL_Interaktive_Stadtklima_Auswertungen/output/tageswerte_komplett.RDS")

df <- tageswerte %>%  
  filter(month %in% c("Jun", "Jul", "Aug") & !Raumlage %in% c("Wald") & Wärmeinsel == "Zürich/Fluntern") %>% 
  group_by(Standort, Raumlage, year) %>%  
  summarise(Hitzetage = sum(Hitzetag, na.rm = T),
            Tropennächte = sum(Tropennacht, na.rm = T)) %>% 
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
  theme_minimal()

df2 <- tageswerte %>%  
  filter(month %in% c("Jun", "Jul", "Aug") & Wärmeinsel == "Zürich" & year == 2020) %>% 
  group_by(date, Wärmeinsel) %>%  
  summarise(T_max = max(T_max)) %>% 
  # tidyr::pivot_longer(cols = -c( date, Wärmeinsel), names_to = "Group", values_to = "Group_Value") %>% 
  ungroup() %>% 
  as.data.frame()

g2 <- ggplot(df2, aes(x = date, y = T_max)) +
  geom_line(aes( color = ifelse(T_max >= 30,'red','green')), size = 0.5) +
  theme_minimal()
g2
