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
  select(GeoLocation,LocationAbbr,Topic,Data_Value_Type,Data_Value,Break_Out_Category,Break_Out,Year) %>%
  mutate(GeoLocation = str_remove_all(GeoLocation, "\\("), 
                        GeoLocation = str_remove_all(GeoLocation, "\\)"),
         Topic=case_when(Topic=="Diseases of the Heart (Heart Disease)"~"Heart Disease",
                         Topic=="Acute Myocardial Infarction (Heart Attack)"~"Heart Attack",
                         TRUE~Topic)) %>% 
  separate(GeoLocation,into = c("Latitude", "Longitude"),  ",") %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  rename("State"=LocationAbbr) %>% 
  na.omit()

####Has the average mortality rate from Cardiovascular Disease in the United States increased/decreased from 2000-2017?
yearly_avg <- rate %>% 
  filter(Break_Out!="18-24",Break_Out!="25-44",Break_Out!="45-64") %>% 
  group_by(Year,Break_Out_Category,Break_Out) %>% 
  summarise(avg=mean(Data_Value))

yearly_avg %>% 
  ggplot(aes(Year,avg,color=Break_Out))+
  geom_point()+
  geom_line(linetype="dotted")+
  facet_wrap(~Break_Out_Category,scales = "free")+
  labs(title = "Average Mortality from Cardiovascular Disease in the US",
       subtitle = "2000-2017")+
  guides(color=FALSE)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

####Which states have the highest average mortality rates for each topic####

#average mortality rate for each state by topic
state_avg <- rate %>% 
  filter(Data_Value_Type=="Age-Standardized") %>% 
  group_by(Topic,State) %>% 
  summarise(avg=mean(Data_Value)) %>% 
  mutate(pct=avg/1000)

#top 5 states in mortality by topic
state_avg_highest<- state_avg %>% 
  group_by(Topic) %>% 
  top_n(5,pct) %>% 
  arrange(Topic,desc(pct))

#plot of the top 5 states in every topic 
state_avg_highest %>% 
  ggplot()+
  geom_col(aes(reorder(State,pct),pct,fill=Topic))+
  facet_wrap(~Topic,scales = "free")+
  coord_flip()+
  labs(title = "Average Mortality Among States",
       x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=FALSE)



####Which gender has the highest average mortality rate for each topic####
#find the average mortality rate among genders for each topic
Gender_average <- rate%>% 
  filter(Break_Out_Category=="Gender",Data_Value_Type=="Age-Standardized") %>%
  select(-Break_Out_Category) %>% 
  rename(Gender = Break_Out) %>% 
  group_by(Topic,Gender) %>% 
  summarise(avg=mean(Data_Value)) %>% 
  mutate(pct=avg/1000) %>% 
  arrange(Topic,desc(pct))

#Plot the average mortality for each gender by topic
Gender_average %>% 
  ggplot()+
  geom_col(aes(Gender,pct,fill=Topic))+
  facet_wrap(~Topic,scales = "free_y")+
  labs(title = "Average Mortality among Gender",
       x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5,size = 25))+
  guides(fill=FALSE)

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
  scale_fill_distiller(palette = "Spectral") +
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

#Flipping Systolic and Diastolic
mycardio %>%
  filter(`Diastolic blood pressure` < 200 & `Systolic blood pressure` < 300 & `Diastolic blood pressure` > 30 & `Systolic blood pressure` > 30) %>%
  filter(Weight < 500) %>%
  ggplot() +
  geom_point(mapping = aes(x = `Diastolic blood pressure`,
                           y = `Systolic blood pressure`,
                           color = Cardio),
             alpha = 0.1) +
  scale_color_manual(values = mycolors) +
  labs(title = "Blood Pressure with Cardiovascular Disease")

#Weight
mycardio %>%
  filter(`Diastolic blood pressure` < 200 & `Systolic blood pressure` < 300 & `Diastolic blood pressure` > 30 & `Systolic blood pressure` > 30) %>%
  filter(Weight < 500) %>%
  ggplot() +
  geom_point(mapping = aes(x = Weight,
                           y = `Systolic blood pressure`,
                           color = Cardio),
             alpha = 0.1) +
  scale_color_manual(values = mycolors) +
  labs(title = "Blood Pressure with Cardiovascular Disease")


##########

mycardio %>%
  filter(Weight < 400) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = Weight), bins = 80)

mycardio %>%
  ggplot() +
  geom_histogram(mapping = aes(x = Age), bins = 8)

mycardio %>%
  filter(Age > 35) %>%
  group_by(Age) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_point(mapping = aes(x = Age, y = n, color = n))



facetCardio <- mycardio %>%
  select(Glucose, Cholesterol, Gender, Smoke:Cardio) %>%
  gather("Lifestyle", "Occurance", -Glucose, -Cholesterol, -Gender) %>%
  group_by(Glucose, Cholesterol, Gender, Lifestyle, Occurance) %>%
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


##########








#Cholesterol and Glucose
#Does high cholesteral/glucose increase rate of cardiovascular disease
myNewCardio <- mycardio %>%
  filter(Cholesterol == "3" & Cardio == "Yes")


mycardio %>%
  filter(`Diastolic blood pressure` < 1000) %>%
  arrange(desc(`Diastolic blood pressure`)) %>%
  top_n(10)

cardio %>%
  filter(ap_hi > 200) %>%
  arrange(desc(ap_hi)) %>%
  top_n(10)


####Joining and mapping####
#Select relevant variables from vital dataset
vitalJoinPlot <- vital%>%
  select(Year, Break_Out_Category,Break_Out, GeoLocation, Topic,LocationAbbr,
         HighConfidenceLimit,LowConfidenceLimit,Data_Value,Data_Value_Type) %>% 
  arrange(Topic) %>% 
  na.omit()
#Use only the gender break out
vitalJoinPlot <- vitalJoinPlot %>% filter(Break_Out_Category=="Gender") %>%
  select(-Break_Out_Category) %>% rename(Gender = Break_Out)
#Divide geolocation into latitude and longitude and prepare for plotting
vitalJoinPlot <- vitalJoinPlot%>% mutate(GeoLocation = str_remove_all(GeoLocation, "\\("), 
                                     GeoLocation = str_remove_all(GeoLocation, "\\)")) %>% 
  separate(GeoLocation,into = c("Latitude", "Longitude"),  ",") %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  na.omit()

<<<<<<< HEAD
=======



####What lifestyle combination has the highest mortality rates for stroke in male and female ?####
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

#### map of the us for stroke avg death rate (heat map) ####
strokeM <- vitalGender %>% filter(Data_Value_Type=="Age-Standardized") %>%
  group_by(LocationAbbr) %>% summarise(avg_deathRate = mean(Data_Value)) %>% rename(state = LocationAbbr)

>>>>>>> 347d3a317c824574baa0b4788f1092c4356f5a3c
#### map of the US for stroke avg death rate (heat map) ####
#Select only age standardized data and obtain average death rate per state
deathRateMap <- vitalJoinPlot %>%
  filter(Data_Value_Type=="Age-Standardized") %>%
  group_by(LocationAbbr) %>%
  summarise(avg_deathRate = mean(Data_Value)) %>% rename(state = LocationAbbr)

#create a data frame with the us states
us_states <- us_map("states")
#Rename abbr to state
us_states <- us_states %>% rename(state=abbr)
#create data frame with mortality rate for mapping
deathRateMapUS <- left_join(us_states, deathRateMap)
#find centroid and bind to death rate
centroid <- aggregate(data=deathRateMapUS,
                      cbind(x, y) ~ avg_deathRate, FUN=mean)
#plot avg  death rate in us state map
deathRateMapUS %>%
  ggplot(aes(x,y, group=group, fill=avg_deathRate)) + 
  geom_polygon(color = "darkgray", size = 0.5)+
  scale_fill_gradient(low = "orange", 
                      high = "blue",
                      na.value = "gray") +
  guides(fill= guide_legend(nrow=1)) +
  geom_text(mapping = aes(x,y, label=round(avg_deathRate)), color = "black",
            data = centroid,
            inherit.aes = FALSE )+
  theme_map()+
  coord_equal()+
  labs(title = " Average Death Rate - Age standarized", fill = "Rate per 100,000") +
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "bottom")


#### State plot -> From what county does the data for Mississippi comes from and what is the average mortality rate there?
#The county is Leake county with a mortality rate of 160 per 100,000. In this county we can find the Baptist Medical Center Leake.

#data frame for only Mississippi state and mortality rate
msPlot <- vitalJoinPlot %>%
  filter(LocationAbbr == "MS") %>%
  filter(Data_Value_Type=="Age-Standardized") %>%
  group_by(LocationAbbr) %>%
  summarise(avg_deathRate = mean(Data_Value), Latitude = mean(Latitude),
            Longitude = mean(Longitude)) %>% rename(state = LocationAbbr)
#create df for map of ms divided by countries
ms_counties <- map_data(map="county", region="mississippi")
#plot 
ms_counties %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "darkgray", fill = "cornsilk2", size = 0.5) +
  geom_text(mapping = aes(x=Longitude,y= Latitude, label=round(avg_deathRate)),
            data = msPlot,
            inherit.aes = FALSE)+
  coord_equal()+
  theme_map()+
  labs(title = "Mississippi County Mortality Rate per 100,000") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust = 0.5) ,
        legend.position = "bottom")
  



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
#prepare data for wrangling
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
                                        Age == "25-44"| Age == "45-64" | Age == "18-24") %>%
  group_by(Age) %>%
  mutate(`Death Rate` = mean(Data_Value)/1000) %>%
  distinct(Age, .keep_all = TRUE) %>% select(Age,`Death Rate`)
#convert ages to age range to match vital data set
mycardioAgeGroup <- cardio_j %>%
  mutate(Age = if_else(condition = Age<44, true = "25-44", false ="45-64"))
#obtain percent average for lifestyles 
mycardioAgeGroup <- mycardioAgeGroup %>% group_by(Age) %>%
  summarise(Cardio = mean(Cardio)*100,
            Alcohol = mean(Alcohol)*100,
            Active = mean(Active)*100,
            Smoke = mean(Smoke)*100)
#join vital and cardio data sets by age group
vital_cardio_join_age <- left_join(vitalAgeRelevant,mycardioAgeGroup)
#gather for ploting
vital_cardio_join_plot <- vital_cardio_join_age %>%
  gather(Pct_category,Pavg, -Age)
#plot for comparison between lifestyles and death rate by age group
vital_cardio_join_plot %>% 
  ggplot(aes(Age, Pavg, color = Pct_category, group =1 ))+
  geom_point(size = 3)+
  geom_line(linetype = "dotted") + 
  facet_wrap(~Pct_category, scales = "free") +
  labs(title = "Comparison of lifestyles with average death rates",
       x = "Age", y = "Average") + guides(color = FALSE) +
  theme(plot.title = element_text(hjust = 0.5))


#### What is the average mortality by state leaflet map? ####
#Select relevant variables
vital_avg_state_all <- vital%>%
  select(Year, Break_Out_Category,Break_Out, GeoLocation, Topic,LocationAbbr,
         HighConfidenceLimit,LowConfidenceLimit,Data_Value, Data_Value_Type) %>% 
  arrange(Topic) %>% 
  na.omit()
#prepare geolocation from vital for plotting
vital_avg_state_all <- vital_avg_state_all %>%
  group_by(LocationAbbr) %>% mutate(avg = mean(Data_Value)) %>%
  distinct(LocationAbbr, .keep_all = TRUE)
vital_avg_state_all <- vital_avg_state_all%>% mutate(GeoLocation = str_remove_all(GeoLocation, "\\("), 
                                     GeoLocation = str_remove_all(GeoLocation, "\\)")) %>% 
  separate(GeoLocation,into = c("Latitude", "Longitude"),  ",") %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  na.omit()
#filter data to use age standarized data only and get rid of umbrella disease
state_avg_leaflet <- rate %>% 
  filter(Data_Value_Type=="Age-Standardized",Topic!="Major Cardiovascular Disease") %>% 
  group_by(Longitude,Latitude,State,Topic) %>% 
  summarise(avg=mean(Data_Value)) %>% 
  mutate(pct=avg/1000)
#round averages for user friendly display
state_avg_leaflet <- state_avg_leaflet %>% select(-pct) %>%
  spread(Topic, avg) %>% mutate(`Coronary Heart Disease` = round(`Coronary Heart Disease`,2),
                                `Heart Attack` = round(`Heart Attack`,2),
                                `Heart Disease` = round(`Heart Disease`,2),
                                `Heart Failure` = round(`Heart Failure`, 2),
                                `Stroke` = round(`Stroke`, 2))
#create labels for display on leaflet
state_label <- sprintf("<b>Average mortality rate in: %s</b><br />Coronary Heart Disease:  %s<br/ >Heart Attack: %s<br/ >Heart Disease: %s<br/ >Heart Failure: %s<br/ >Stroke: %s",
                       state_avg_leaflet$State,
                    state_avg_leaflet$`Coronary Heart Disease`,
                    state_avg_leaflet$`Heart Attack`,
                    state_avg_leaflet$`Heart Disease`,
                    state_avg_leaflet$`Heart Failure`,
                    state_avg_leaflet$`Stroke`) %>%
  lapply(htmltools::HTML)
#Plot into leaflet 
state_avg_leaflet %>% leaflet(options = leafletOptions(zoomSnap=1)) %>%
  addTiles() %>% setView(-98.00,38.71,zoom=4) %>%
  addMarkers(~Longitude, ~Latitude, label = state_label, popup = state_label)

