# Funktion für einfache (gruppierte) Temperaturangaben
#***********************************************************

# Erstellt eine (gruppierte) Auswertung mit
#   - Min
#   - Mean
#   - Max
#   - Median
#   - Standardabweichung
#   - Hitzetag (T_max >= 30)
#   - Tropennacht (T_min >= 20)

# Verwendung der Funktion
#*************************
# temperature_metrics(df, var, [grp1, grp2, ...])
## df -> data.frame
## var -> Auswertungsvariable(n)
## grp1 -> Groupingvariable 1 (optional)
## grp2 -> Groupingvariable 2 (optional)

# Beispiel

# temperature_metrics <- function(data, temperature, date = as_date(starttime), site, sensor)

require(tidyverse)

temperature_metrics <- function(df, var, ...) {
  group_by <- quos(...)
  var <- enquo(var)
  df %>%  
    group_by(!!!group_by) %>%  
    dplyr::summarise( 
      T_min = round(min(!!var, na.rm = TRUE),1), 
      T_max = round(max(!!var, na.rm = TRUE),1), 
      T_mean = round(mean(!!var, na.rm = TRUE),1), 
      T_sd = sd(!!var, na.rm = TRUE), 
      T_median = median(!!var, na.rm = TRUE), 
      Hitzetag = ifelse(T_max > 30, TRUE, FALSE), 
      Tropennacht = ifelse(T_min > 20, TRUE, FALSE) 
    ) %>%  
    ungroup() 
} 
