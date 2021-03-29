# SOURCE REQUIRED PACKAGES / LIBRARIES --------------------------------------

source("set-up.R")


# IMPORT DATA FILES -------------------------------------------------------

klima <- readRDS("./output/stadtklima_daten.RDS") %>% 
  as.data.frame()


# TRANSFORM COORDINATES FROM LV95 TO WGS84 --------------------------------

coords_lv95 <- klima %>%  
  distinct(., site, sensor, x, y) %>% 
  select("E" = x, "N" = y) 

# saveRDS(coords_LV95, "./output/coords_LV95.RDS")

## Create a Spatial points data frame for data, where E (East) and N (North) are the fields with coordinates
sp::coordinates(coords_lv95) <- ~ E + N

## Tell R our data it's on LV95, EPSG=2056
coords_lv95@proj4string <- CRS("+init=epsg:2056")

## Check the original coords
# coords_lv95@coords

## Plot the data after transformation to check
plot(coords_lv95)

## Transform the data to WGS84
coords_wgs84 <- sp::spTransform(coords_lv95, CRS("+init=epsg:4326"))

## Look at the transformed coords
# test2@coords

## Plot the data to check
plot(coords_wgs84)

# saveRDS(coords_wgs84, "./output/coords_wgs84.RDS")

## Check length of data sets
nrow(coords_wgs84@coords) == nrow(coords_lv95@coords)

## Merging coordinates to 'klima' data-frame
messnetz <- klima %>%  
  distinct(., site, sensor, x, y) %>%
  mutate(E = coords_wgs84$E,
         N = coords_wgs84$N)

# SAVE OUTPUT FILES --------------------------------------------------------

messnetz <- saveRDS(messnetz, "./output/messnetz.RDS")
