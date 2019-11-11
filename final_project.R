#Final project
#Diego Machado
#Jake Tweedle
#Isaiah Vaughnn


####load libraries####
library(tidyverse)
library(leaflet)

####read data files####
cardio <- read_csv2("data/cardio_train.csv")
vital <- read_csv("data/Data1.csv")



#wrangle the data 
rate<- vital%>%
  select(GeoLocation,LocationAbbr,Topic,Data_Value_Type,Data_Value,Break_Out_Category,Break_Out) %>%
  mutate(GeoLocation = str_remove_all(GeoLocation, "\\("), 
                        GeoLocation = str_remove_all(GeoLocation, "\\)"),
         Topic=case_when(Topic=="Diseases of the Heart (Heart Disease)"~"Heart Disease",
                         Topic=="Acute Myocardial Infarction (Heart Attack)"~"Heart Attack",
                         TRUE~Topic)) %>% 
  separate(GeoLocation,into = c("Latitude", "Longitude"),  ",") %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  na.omit()

#average mortality rate for each state by topic
state_avg <- rate %>% 
  group_by(LocationAbbr,Topic,Data_Value_Type) %>% 
  summarise(avg=mean(Data_Value))

#top 5 states in mortality by topic
state_avg_highest<- state_avg %>% 
  group_by(Topic,Data_Value_Type) %>% 
  top_n(5,avg) %>% 
  arrange(Data_Value_Type,desc(avg))

#plot of the top 5 states in every topic 
state_avg_highest%>% 
  filter(Topic=="Heart Attack") %>% 
ggplot()+
geom_col(aes(reorder(LocationAbbr,avg),avg,fill=LocationAbbr))+
  facet_wrap(~Data_Value_Type,ncol = 2,scales = "free")+
  labs(title = "Average Mortality from Heart Attack",
       x=element_blank())+
  guides(fill=FALSE)+
  theme(plot.title = element_text(hjust = 0.5))

state_avg_highest%>% 
  filter(Topic=="Stroke") %>% 
  ggplot()+
  geom_col(aes(reorder(LocationAbbr,avg),avg,fill=LocationAbbr))+
  facet_wrap(~Data_Value_Type,ncol = 2,scales = "free")+
  labs(title = "Average Mortality from Stroke",
       x=element_blank())+
  guides(fill=FALSE)+
  theme(plot.title = element_text(hjust = 0.5))


state_avg_highest%>% 
  filter(Topic=="Major Cardiovascular Disease") %>% 
  ggplot()+
  geom_col(aes(reorder(LocationAbbr,avg),avg,fill=LocationAbbr))+
  facet_wrap(~Data_Value_Type,ncol = 2,scales = "free")+
  labs(title = "Average Mortality from Major Cardiovascular Diease",
       x=element_blank())+
  guides(fill=FALSE)+
  theme(plot.title = element_text(hjust = 0.5))


state_avg_highest%>% 
  filter(Topic=="Heart Failure") %>% 
  ggplot()+
  geom_col(aes(reorder(LocationAbbr,avg),avg,fill=LocationAbbr))+
  facet_wrap(~Data_Value_Type,ncol = 2,scales = "free")+
  labs(title = "Average Mortality from Heart Failure",
       x=element_blank())+
  guides(fill=FALSE)+
  theme(plot.title = element_text(hjust = 0.5))

state_avg_highest%>% 
  filter(Topic=="Heart Diease") %>% 
  ggplot()+
  geom_col(aes(reorder(LocationAbbr,avg),avg,fill=LocationAbbr))+
  facet_wrap(~Data_Value_Type,ncol = 2,scales = "free")+
  labs(title = "Average Mortality from Heart Diease",
       x=element_blank())+
  guides(fill=FALSE)+
  theme(plot.title = element_text(hjust = 0.5))

state_avg_highest%>% 
  filter(Topic=="Coronary Heart Disease") %>% 
  ggplot()+
  geom_col(aes(reorder(LocationAbbr,avg),avg,fill=LocationAbbr))+
  facet_wrap(~Data_Value_Type,ncol = 2,scales = "free")+
  labs(title = "Average Mortality from Coronary Heart Disease",
       x=element_blank())+
  guides(fill=FALSE)+
  theme(plot.title = element_text(hjust = 0.5))



#2 plots faceted by topic showing the highest age-standarized and crude average rates
state_avg_highest%>% 
  filter(Data_Value_Type=="Crude") %>% 
  ggplot()+
  geom_col(aes(reorder(LocationAbbr,avg),avg,fill=LocationAbbr))+
  facet_wrap(~Topic,ncol = 2,scales = "free")+
  labs(title = "Average Crude Mortality Rate",
       x=element_blank())+
  guides(fill=FALSE)+
  theme(plot.title = element_text(hjust = 0.5))

state_avg_highest%>% 
  filter(Data_Value_Type=="Age-Standardized") %>% 
  ggplot()+
  geom_col(aes(reorder(LocationAbbr,avg),avg,fill=LocationAbbr))+
  facet_wrap(~Topic,ncol = 3,scales = "free")+
  labs(title = "Average Age-Standardized Mortality Rate",
       x=element_blank())+
  guides(fill=FALSE)+
  theme(plot.title = element_text(hjust = 0.5))

#the above plots are the same just filtered for topic or faceted by topic. filtering produces 6 graphs for each topic.
#the plots faceted by topic produces two 
#We can decide which is a better visualization


#find the average mortality rate among genders for each topic
Gender_average <- rate%>% 
  filter(Break_Out_Category=="Gender") %>%
  select(-Break_Out_Category) %>% 
  rename(Gender = Break_Out) %>% 
  group_by(Topic,Gender,Data_Value_Type) %>% 
  summarise(avg=mean(Data_Value))

#Plot the average mortality for each gender by topic
Gender_average %>% 
  ggplot()+
  geom_col(aes(Gender,avg,fill=Data_Value_Type),position = "dodge")+
  facet_wrap(~Topic,scales = "free")+
  labs(title = "Average Mortality among Gender")+
  theme(plot.title = element_text(hjust = 0.5,size = 25))




temp %>% leaflet(options = leafletOptions(zoomSnap=1)) %>%
  addTiles() %>% setView(-98.00,38.71,zoom=4) %>% addMarkers(~Longitude, ~Latitude)

###################

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





####Joining and mapping####
vitalGender <- vital%>%
  select(Year, Break_Out_Category,Break_Out, GeoLocation, Topic,LocationAbbr,HighConfidenceLimit,LowConfidenceLimit,Data_Value) %>% 
  arrange(Topic) %>% 
  na.omit()
vitalGender <- vitalGender %>% filter(Break_Out_Category=="Gender") %>%
  select(-Break_Out_Category) %>% rename(Gender = Break_Out)

vitalGender <- vitalGender%>% mutate(GeoLocation = str_remove_all(GeoLocation, "\\("), 
                                     GeoLocation = str_remove_all(GeoLocation, "\\)")) %>% 
  separate(GeoLocation,into = c("Latitude", "Longitude"),  ",") %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  na.omit()

#cardioVital <- left_join(vitalGender, mycardio, by ="Gender")
#mycardio
cardioVitalMS <- left_join(vitalGender, mycardio, by ="Gender")


vitalGender %>% leaflet(options = leafletOptions(zoomSnap=1)) %>%
  addTiles() %>% setView(-98.00,38.71,zoom=4) %>% addMarkers(~Longitude, ~Latitude)





