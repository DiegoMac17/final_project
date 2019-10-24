#Final project
#Diego Machado
#Jake Tweedle
#Isaiah Vaughnn


#load libraries
library(tidyverse)
library(leaflet)

#read data files in
cardio <- read_csv2("data/cardio_train.csv")
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


#Cardio Code
mycardio <- cardio %>%
  rename("ID" = id,
         "Age" = age,
         "Gender" = gender,
         "Height" = height,
         "Weight" = weight,
         "Systolic blood pressure" = ap_hi,
         "Diastolic blood pressure" = ap_lo,
         "Cholesterol" = cholesterol,
         "Glucose" = gluc,
         "Smoke" = smoke,
         "Alcohol" = alco,
         "Active" = active,
         "Cardio" = cardio)

glimpse(mycardio)
mycardio <- mycardio %>%
  mutate(Age = as.numeric(Age)/365) %>%
  mutate(Gender = case_when(Gender == "1" ~ "Female", 
                            Gender == "2" ~ "Male")) %>%
  mutate(Weight = as.numeric(Weight)*2.205) %>%
  mutate(Smoke = case_when(Smoke == "0" ~ "No", 
                           Smoke == "1" ~ "Yes")) %>%
  mutate(Alcohol = case_when(Alcohol == "0" ~ "No", 
                             Alcohol == "1" ~ "Yes")) %>%
  mutate(Active = case_when(Active == "0" ~ "No", 
                            Active == "1" ~ "Yes")) %>%
  mutate(Cardio = case_when(Cardio == "0" ~ "No", 
                            Cardio == "1" ~ "Yes")) %>%
  mutate(Height = as.numeric(Height)/2.54) %>%
  mutate(HeightFeet = floor(Height/12)) %>%
  mutate(HeightInches = round(Height - (12*HeightFeet), 1)) %>%
  mutate(HeightLong = paste(HeightFeet,"'",HeightInches))

mycardio$HeightFeet <- NULL
mycardio$HeightInches <- NULL

mycardio <- mycardio %>%
  select(ID:Height, HeightLong, Weight:Cardio)






