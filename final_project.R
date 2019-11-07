#Final project
#Diego Machado
#Jake Tweedle
#Isaiah Vaughnn


#load libraries
library(tidyverse)

#read data files in
cardio <- read_delim("data/cardio_train.csv", delim= ";")
vital <- read_csv("data/Data1.csv")
vital1 <- spread(vital,Break_Out_Category,Break_Out)

