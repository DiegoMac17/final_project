#Final project
#Diego Machado
#Jake Tweedle
#Isaiah Vaughnn


####load libraries####
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(socviz)
library(usmap)
library(maps)
library(cowplot)


####read data files####
cardio <- read_csv2("data/cardio_train.csv")
vital <- read_csv("data/Data1.csv")




#wrangle the data####
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



####Which states have the highest average mortality rates for each topic####

#average mortality rate for each state by topic
state_avg <- rate %>% 
  filter(Data_Value_Type=="Age-Standardized",Topic!="Major Cardiovascular Disease") %>% 
  group_by(LocationAbbr,Topic) %>% 
  summarise(avg=mean(Data_Value)) %>% 
  mutate(pct=avg/100000*100)

#top 5 states in mortality by topic
state_avg_highest<- state_avg %>% 
  group_by(Topic) %>% 
  top_n(5,pct) %>% 
  arrange(Topic,desc(pct))

#plot of the top 5 states in every topic 
state_avg_highest%>%
ggplot()+
geom_col(aes(reorder(LocationAbbr,pct),pct,fill=Topic),position = "dodge")+
  facet_wrap(~Topic,ncol = 2,scales = "free")+
  labs(title = "Average Mortality Among States",
       x=element_blank())+
  guides(fill=FALSE)+
  theme(plot.title = element_text(hjust = 0.5))




####Which gender has the highest average mortality rate for each topic####
#find the average mortality rate among genders for each topic
Gender_average <- rate%>% 
  filter(Break_Out_Category=="Gender",Topic!="Major Cardiovascular Disease",Data_Value_Type=="Age-Standardized") %>%
  select(-Break_Out_Category) %>% 
  rename(Gender = Break_Out) %>% 
  group_by(Topic,Gender) %>% 
  summarise(avg=mean(Data_Value)) %>% 
  mutate(pct=avg/100000*100) %>% 
  arrange(Topic,desc(pct))

#Plot the average mortality for each gender by topic
Gender_average %>% 
  ggplot()+
  geom_col(aes(Gender,pct,fill=Topic))+
  facet_wrap(~Topic,scales = "free_y")+
  labs(title = "Average Mortality among Gender")+
  theme(plot.title = element_text(hjust = 0.5,size = 25))+
  guides(fill=FALSE)




temp %>% leaflet(options = leafletOptions(zoomSnap=1)) %>%
  addTiles() %>% setView(-98.00,38.71,zoom=4) %>% addMarkers(~Longitude, ~Latitude)


#####Isaiah's Code####

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
  mutate(Weight = as.numeric(Weight)*2.205/10) %>%
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
  filter(Lifestyle == "Active" | Lifestyle == "Alcohol" | Lifestyle == "Smoke") %>%
  ggplot() +
  geom_bar(mapping = aes(x = Gender, y = percent, fill = Lifestyle), stat = "identity") +
  facet_wrap(~Lifestyle, scales = "free_y")

percentLife %>%
  filter(Lifestyle == "Cardio") %>%
  ggplot() +
  geom_bar(mapping = aes(x = Gender, y = percent, fill = percent), stat = "identity") +
  scale_fill_distiller(palette = "Set1") +
  labs(title = "Percentage of Gender with Cardiovascular Disease")
  

#People with VERY HIGH weights have distolic and systolic values that are off the charts

#mycolors <- palette(brewer.pal(n=2, name="Set1"))
mycolors <- c("green", "red")


mycardio %>%
  filter(`Diastolic blood pressure` < 200 & `Systolic blood pressure` < 300 & `Diastolic blood pressure` > 30 & `Systolic blood pressure` > 30) %>%
  filter(Weight < 500) %>%
  ggplot() +
  geom_point(mapping = aes(x = `Systolic blood pressure`,
                           y = `Diastolic blood pressure`,
                           color = Cardio),
             alpha = 0.1) +
  scale_color_manual(values = mycolors) +
  labs(title = "Blood Pressure with Cardiovascular Disease")

mycardio %>%
  filter(Cholesterol == "1" & Glucose == "1" & `Diastolic blood pressure` < 200 & `Systolic blood pressure` < 300 & `Diastolic blood pressure` > 30 & `Systolic blood pressure` > 30) %>%
  filter(Weight < 500) %>%
  ggplot() +
  geom_point(mapping = aes(x = `Systolic blood pressure`,
                           y = `Diastolic blood pressure`,
                           color = Cardio),
             alpha = 0.1) +
  scale_color_manual(values = mycolors) +
  labs(title = "Blood Pressure with Cardiovascular Disease")

mycardio %>%
  filter(`Diastolic blood pressure` < 1000) %>%
  arrange(desc(`Diastolic blood pressure`)) %>%
  top_n(10)

cardio %>%
  filter(ap_hi > 200) %>%
  arrange(desc(ap_hi)) %>%
  top_n(10)












####Joining and mapping####
#Select relevant variables 
vitalGender <- vital%>%
  select(Year, Break_Out_Category,Break_Out, GeoLocation, Topic,LocationAbbr,
         HighConfidenceLimit,LowConfidenceLimit,Data_Value,Data_Value_Type) %>% 
  arrange(Topic) %>% 
  na.omit()
#Use only the gender break out
vitalGender <- vitalGender %>% filter(Break_Out_Category=="Gender") %>%
  select(-Break_Out_Category) %>% rename(Gender = Break_Out)
#Divide geolocation into latitude and longitude
vitalGender <- vitalGender%>% mutate(GeoLocation = str_remove_all(GeoLocation, "\\("), 
                                     GeoLocation = str_remove_all(GeoLocation, "\\)")) %>% 
  separate(GeoLocation,into = c("Latitude", "Longitude"),  ",") %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  na.omit()




####What lifestyle combination has the highest mortality rates for stroke in men and female ?####
stroke_gender <- vitalGender %>%
  filter(Topic == "Stroke") %>% group_by(Gender) %>%
  summarise(avg_deathRate = mean(Data_Value))


cardioJoin <- mycardio %>%
  group_by(Gender, Smoke, Alcohol, Active ,Cardio) %>% summarise(n = n())


stroke_lifestyle <- left_join(cardioJoin, stroke_gender)

plot_stroke_f <- stroke_lifestyle %>% filter(Gender=="Female") %>%
  arrange(desc(n)) %>% head(1)
plot_stroke_m <- stroke_lifestyle %>% filter(Gender=="Male") %>%
  arrange(desc(n)) %>% head(1)

plot_stroke <- bind_rows(plot_stroke_m,plot_stroke_f)

plot_stroke %>% ggplot() +
  geom_col(aes(x=Gender, y= n, fill = avg_deathRate))



#### map of the us for stroke avg death rate ####
strokeM <- vitalGender %>% filter(Topic == "Stroke", Gender == "Male",Data_Value_Type=="Age-Standardized") %>%
  group_by(LocationAbbr) %>% summarise(avg_deathRate = mean(Data_Value)) %>% rename(state = LocationAbbr)

#create a data fram with the us states
us_states <- us_map("states")
#Rename abbr to state
us_states <- us_states %>% rename(state=abbr)
#create data frame with mortality rate for mapping
strokeMaleMap <- left_join(us_states, strokeM)
#find centroid and bind to death rate
centroid <- aggregate(data=strokeMaleMap,
                      cbind(x, y) ~ avg_deathRate, FUN=mean)
#plot avg  death rate in us state map
strokeMaleMap %>%
  ggplot(aes(x,y, group=group, fill=avg_deathRate)) + 
  geom_polygon(color = "darkgray", size = 0.5)+
  scale_fill_gradient(low = "orange", 
                      high = "purple",
                      na.value = "gray") +
  guides(fill= guide_legend(nrow=1)) +
  geom_text(mapping = aes(x,y, label=round(avg_deathRate)), color = "black",
            data = centroid,
            inherit.aes = FALSE )+
  theme_map()+
  coord_equal()+
  labs(title = " Average Death Rate for stroke in Males", fill = "Rate per 100,000") +
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "bottom")

strokeF <- vitalGender %>% filter(Topic == "Stroke", Gender == "Female")



#### Which vital factor contributes the most to mortality rate by age group? ####
#Relation between age groups and life styles and cardivascular disease

#Select relevant variables 
vitalAge <- vital%>%
  select(Year, Break_Out_Category,Break_Out, GeoLocation, Topic,LocationAbbr,
         HighConfidenceLimit,LowConfidenceLimit,Data_Value, Data_Value_Type) %>% 
  arrange(Topic) %>% 
  na.omit()
#prepare cardio data set
cardio_j <- cardio %>%
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
cardio_j <- cardio_j %>%
  mutate(Age = as.numeric(Age)/365) %>%
  mutate(Gender = case_when(Gender == "1" ~ "Female", 
                            Gender == "2" ~ "Male")) %>%
  mutate(Weight = as.numeric(Weight)*2.205/10)

#Use only age break out category
vitalAge <- vitalAge %>% filter(Break_Out_Category=="Age") %>%
  select(-Break_Out_Category) %>% rename(Age = Break_Out)

#Filter for age groups that match cardio data set and obtain mean death rate
vitalAgeRelevant <- vitalAge %>% filter(Data_Value_Type == "Crude",
                                        Age == "25-44"| Age == "45-64" | Age == "18-24") %>% group_by(Age) %>%
  mutate(percent_avg = mean(Data_Value)/1000) %>%
  distinct(Age, .keep_all = TRUE) %>% select(Age,percent_avg)

mycardioAgeGroup <- cardio_j %>%
  mutate(Age = if_else(condition = Age<44, true = "25-44", false ="45-64"))

mycardioAgeGroup <- mycardioAgeGroup %>% group_by(Age) %>%
  summarise(cardio_pavg = mean(Cardio)/100,
            alcohol_pavg = mean(Alcohol)/100,
            active_pavg = mean(Active)/100,
            smoke_pavg = mean(Smoke)/100)

vital_cardio_join_age <- left_join(vitalAgeRelevant,mycardioAgeGroup)

vital_cardio_join_44 <- vital_cardio_join_age %>%
  filter(Age=="45-64") %>%
  gather(AgeR,Pavg)

vital_cardio_join_age %>% ggplot()+
  geom_line(aes(Age, percent_avg)) +
  geom_line(aes(Age, cardio_pavg)) +
  geom_line(aes(Age, active_pavg)) +
  geom_line(aes(Age, smoke_pavg)) +
  geom_line(aes(Age, active_pavg)) +

#### What is the average mortality by state leaflet map? ####
vital_avg_state_all <- vital%>%
  select(Year, Break_Out_Category,Break_Out, GeoLocation, Topic,LocationAbbr,
         HighConfidenceLimit,LowConfidenceLimit,Data_Value, Data_Value_Type) %>% 
  arrange(Topic) %>% 
  na.omit()
vital_avg_state_all <- vital_avg_state_all %>%
  group_by(LocationAbbr) %>% mutate(avg = mean(Data_Value)) %>%
  distinct(LocationAbbr, .keep_all = TRUE)
vital_avg_state_all <- vital_avg_state_all%>% mutate(GeoLocation = str_remove_all(GeoLocation, "\\("), 
                                     GeoLocation = str_remove_all(GeoLocation, "\\)")) %>% 
  separate(GeoLocation,into = c("Latitude", "Longitude"),  ",") %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  na.omit()

vital_avg_state_all %>% leaflet(options = leafletOptions(zoomSnap=1)) %>%
  addTiles() %>% setView(-98.00,38.71,zoom=4) %>%
  addMarkers(~Longitude, ~Latitude)

#### Which vital factor contributes the most to mortality rate by gender? ####

age <- vitalAge %>% 
  group_by(Topic,Age) %>% 
  summarise(avg=mean(Data_Value))
  
  







