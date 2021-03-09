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
             "AWEL_Sensors_LoRa_202011.csv",
             "AWEL_Sensors_LoRa_202012.csv",
             "AWEL_Sensors_LoRa_202101.csv")
ldf <- list() # creates a list

# If outside of Leunet: adjust proxy setting in .Rprofile
for(f in 1:length(listcsv)){
  ldf[[f]] <- read_delim(paste0(base_url, listcsv[f]), ";", escape_double = FALSE, trim_ws = TRUE)
}

# length(is.na(ldf[[1]]))

# Bindind all data frames into one data frame with daily mean values for temp. and humidity
klima <- bind_rows(ldf)

saveRDS(klima, "./output/stadtklima_daten.RDS")
