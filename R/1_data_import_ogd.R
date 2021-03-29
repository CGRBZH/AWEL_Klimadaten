# SOURCE REQUIRED PACKAGES / LIBRARIES / HELPER FUNCTIONS --------------------------------------

source("set-up.R")

# LOADING OGD DATA --------------------------------------------------------

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
             "AWEL_Sensors_LoRa_202101.csv",
             "AWEL_Sensors_LoRa_202102.csv",
             "AWEL_Sensors_LoRa_202103.csv")
ldf <- list() # creates a list

# If outside of Leunet: adjust proxy setting in .Rprofile
for(f in 1:length(listcsv)){
  ldf[[f]] <- read_delim(paste0(base_url, listcsv[f]), ";", escape_double = FALSE, trim_ws = TRUE)
}

# length(is.na(ldf[[1]]))

# Binding all data frames into one data frame
klima <- bind_rows(ldf) 

# LOADING METADATA --------------------------------------------------------

metadata <- fread("http://www.web.statistik.zh.ch/awel/LoRa/metadata/AWEL_LoRa_Logbuch.csv", 
                  header = T, 
                  skip = 2, 
                  sep = ";", 
                  select = c(1:17), 
                  na.strings = c("", "NA")) %>% 
  filter(Standort !="NA") %>% 
  mutate(von = as.POSIXct(strptime(von, "%d.%m.%Y")),
         bis = as.POSIXct(strptime(bis, "%d.%m.%Y")))

# SAVE OUTPUT FILES --------------------------------------------------------

saveRDS(klima, "./output/stadtklima_daten.RDS")
saveRDS(metadata, "./output/metadata.RDS")
