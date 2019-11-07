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



temp <- vital%>%
  select(Year, Break_Out_Category,Break_Out, GeoLocation, Topic,LocationAbbr,HighConfidenceLimit,LowConfidenceLimit,Data_Value) %>% 
  arrange(Topic) %>% 
  na.omit()
temp <- temp %>% mutate(GeoLocation = str_remove_all(GeoLocation, "\\("), 
                        GeoLocation = str_remove_all(GeoLocation, "\\)"))
temp <-  temp %>%
  separate(GeoLocation,into = c("Latitude", "Longitude"),  ",")
temp <- temp %>% mutate(Latitude = as.numeric(Latitude),
                        Longitude = as.numeric(Longitude))

temp <- temp %>% top_n(n=-100)

temp %>% leaflet(options = leafletOptions(zoomSnap=1)) %>%
  addTiles() %>% setView(-98.00,38.71,zoom=4) %>% addMarkers(~Longitude, ~Latitude)


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

<<<<<<< HEAD
# Gender     n
# <chr>  <int>
# 1 Female 45530
# 2 Male   24470
#Need to standardize the data.

facetCardio <- mycardio %>%
  select(Gender, Smoke:Cardio) %>%
  gather("Lifestyle", "Occurance", -Gender) %>%
  group_by(Gender, Lifestyle, Occurance) %>%
  count()

actFe <- facetCardio %>%
  filter(Gender == "Female" & Lifestyle == "Active" & Occurance == "Yes") %>%
  mutate(percent = n/45530) %>%
  select(Gender, Lifestyle, percent)
alcFe <- facetCardio %>%
  filter(Gender == "Female" & Lifestyle == "Alcohol" & Occurance == "Yes") %>%
  mutate(percent = n/45530) %>%
  select(Gender, Lifestyle, percent)
cardFe <- facetCardio %>%
  filter(Gender == "Female" & Lifestyle == "Cardio" & Occurance == "Yes") %>%
  mutate(percent = n/45530) %>%
  select(Gender, Lifestyle, percent)
smoFe <- facetCardio %>%
  filter(Gender == "Female" & Lifestyle == "Smoke" & Occurance == "Yes") %>%
  mutate(percent = n/45530) %>%
  select(Gender, Lifestyle, percent)

actMa <- facetCardio %>%
  filter(Gender == "Male" & Lifestyle == "Active" & Occurance == "Yes") %>%
  mutate(percent = n/45530) %>%
  select(Gender, Lifestyle, percent)
alcMa <- facetCardio %>%
  filter(Gender == "Male" & Lifestyle == "Alcohol" & Occurance == "Yes") %>%
  mutate(percent = n/45530) %>%
  select(Gender, Lifestyle, percent)
cardMa <- facetCardio %>%
  filter(Gender == "Male" & Lifestyle == "Cardio" & Occurance == "Yes") %>%
  mutate(percent = n/45530) %>%
  select(Gender, Lifestyle, percent)
smoMa <- facetCardio %>%
  filter(Gender == "Male" & Lifestyle == "Smoke" & Occurance == "Yes") %>%
  mutate(percent = n/45530) %>%
  select(Gender, Lifestyle, percent)

percentLife <- bind_rows(actFe, actMa, alcFe, alcMa, cardFe, cardMa, smoFe, smoMa)
percentLife$Occurance <- NULL


percentLife %>%
  ggplot() +
  geom_bar(mapping = aes(x = Gender, y = percent, fill = Lifestyle), stat = "identity") +
  facet_wrap(~Lifestyle, scales = "free_y")
  



=======
temp %>% 
  filter(Topic=="Acute Myocardial Infarction (Heart Attack)" & Year==2000 & LocationAbbr=="AL") %>% 
  view()
temp1 <-  temp%>% 
  group_by(LocationAbbr,Topic) %>% 
  summarise(avg=mean(Data_Value)) %>% 
  arrange(Topic)

temp2 <- temp1 %>% 
  group_by(Topic) %>% 
  top_n(5,avg)
temp2 %>% 
  ggplot()+
  geom_col(aes(reorder(LocationAbbr,avg),avg,fill=Topic))+
  coord_flip()+
  labs(title = "Coronary Heart Disease")+
  facet_wrap(~Topic,scales = "free",ncol = 3)
>>>>>>> bb9a31031f1f095d983e1e973f40d90d639d7473

# Gender     n
# <chr>  <int>
# 1 Female 45530
# 2 Male   24470
#Need to standardize the data.
# 
# facetCardio <- mycardio %>%
#   select(Gender, Smoke:Cardio) %>%
#   gather("Lifestyle", "Occurance", -Gender) %>%
#   group_by(Gender, Lifestyle, Occurance) %>%
#   count()
# 
# actFe <- facetCardio %>%
#   filter(Gender == "Female" & Lifestyle == "Active" & Occurance == "Yes") %>%
#   mutate(percent = n/45530) %>%
#   select(Gender, Lifestyle, percent)
# alcFe <- facetCardio %>%
#   filter(Gender == "Female" & Lifestyle == "Alcohol" & Occurance == "Yes") %>%
#   mutate(percent = n/45530) %>%
#   select(Gender, Lifestyle, percent)
# cardFe <- facetCardio %>%
#   filter(Gender == "Female" & Lifestyle == "Cardio" & Occurance == "Yes") %>%
#   mutate(percent = n/45530) %>%
#   select(Gender, Lifestyle, percent)
# smoFe <- facetCardio %>%
#   filter(Gender == "Female" & Lifestyle == "Smoke" & Occurance == "Yes") %>%
#   mutate(percent = n/45530) %>%
#   select(Gender, Lifestyle, percent)
# 
# actMa <- facetCardio %>%
#   filter(Gender == "Male" & Lifestyle == "Active" & Occurance == "Yes") %>%
#   mutate(percent = n/45530) %>%
#   select(Gender, Lifestyle, percent)
# alcMa <- facetCardio %>%
#   filter(Gender == "Male" & Lifestyle == "Alcohol" & Occurance == "Yes") %>%
#   mutate(percent = n/45530) %>%
#   select(Gender, Lifestyle, percent)
# cardMa <- facetCardio %>%
#   filter(Gender == "Male" & Lifestyle == "Cardio" & Occurance == "Yes") %>%
#   mutate(percent = n/45530) %>%
#   select(Gender, Lifestyle, percent)
# smoMa <- facetCardio %>%
#   filter(Gender == "Male" & Lifestyle == "Smoke" & Occurance == "Yes") %>%
#   mutate(percent = n/45530) %>%
#   select(Gender, Lifestyle, percent)
# 
# percentLife <- bind_rows(actFe, actMa, alcFe, alcMa, cardFe, cardMa, smoFe, smoMa)
# percentLife$Occurance <- NULL
# 
# 
# percentLife %>%
#   ggplot() +
#   geom_bar(mapping = aes(x = Gender, y = percent, fill = Lifestyle), stat = "identity") +
#   facet_wrap(~Lifestyle, scales = "free_y")



