#Final project
#Diego Machado
#Jake Tweedle
#Isaiah Vaughnn


#load libraries
library(tidyverse)
library(leaflet)

#read data files in
cardio <- read_delim("data/cardio_train.csv", delim= ";")
vital <- read_csv("data/Data1.csv")
vital1 <- spread(vital,Break_Out_Category,Break_Out)


temp <- vital1 %>%
  select(Year, Age, GeoLocation, Topic, Data_Value_Type, Data_Value) %>% na.omit()
temp <- temp %>% mutate(GeoLocation = str_remove_all(GeoLocation, "\\("), 
                        GeoLocation = str_remove_all(GeoLocation, "\\)"))
temp <-  temp %>%
  separate(GeoLocation,into = c("Latitude", "Longitude"),  ",")
temp <- temp %>% mutate(Latitude = as.numeric(Latitude),
                        Longitude = as.numeric(Longitude))

temp <- temp %>% top_n(n=-100)

temp %>% leaflet(options = leafletOptions(zoomSnap=1)) %>%
  addTiles() %>% setView(-74.00,40.71,zoom=12) %>% addMarkers(~Longitude, ~Latitude)


temp %>% group_by(Topic) %>%
  ggplot() + geom_bar(aes(Topic, fill=Topic))


#wow cool
