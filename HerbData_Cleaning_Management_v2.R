# Paige Snow
# OSU Field Experience 2019
# Text Mining & Data Management for Herb Project
# MANA Stats 2.0
# Useful resource: https://www.tidytextmining.com/tidytext.html

# Install packages needed. 
install.packages("tidyverse")
install.packages("stringr")
install.packages("tidytext")
install.packages("widyr")
install.packages("ggplot2")
install.packages("igraph")
install.packages("ggraph")
install.packages("devtools")
devtools::install_github("tidyverse/stringr")
install.packages("tm")
###################################################################################################
library(tidyverse)
library(stringr)
library(tm)
library(tidytext)
library(widyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(pastecs)
library(janitor)

citation("tidyverse")
citation("stringr")
citation("tidytext")
citation("janitor")
# Go through
# HerbData_Text_Mining_v2 first
head(HerbData)
view(HerbData)

###################################################################################################
# Herb Flag Y/N : Generating a yes/no variable if an observation used herbs 
# This flag variable should indicate YES if any herb was named in Herb_1Text-Herb_4Text, 
# if any herb was indicated for induction or augmentation (Induc_Aug), or if any herb was indicated to
# prevent hemorrhage (Blood_Actions)

HerbData <- HerbData %>% 
  mutate(HerbPresent.1yn = NA) %>% 
  mutate(HerbPresent.2yn = NA) %>% 
  mutate(HerbPresent.3yn = NA) %>% 
  mutate(HerbPresent.4yn = NA)

HerbData <- HerbData %>% 
  mutate(HerbPresent.1yn = str_detect(Herb_1Text, "^\\s*$")) %>% 
  mutate(HerbPresent.2yn = str_detect(Herb_2Text, "^\\s*$")) %>% 
  mutate(HerbPresent.3yn = str_detect(Herb_3Text, "^\\s*$"))%>% 
  mutate(HerbPresent.4yn = str_detect(Herb_4Text, "^\\s*$"))

HerbData %>% select(Herb_1Text, HerbPresent.1yn, Herb_2Text, HerbPresent.2yn, Herb_3Text, HerbPresent.3yn, Herb_4Text, HerbPresent.4yn) 

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$HerbPresent.yn <- ifelse(HerbData$HerbPresent.1yn & HerbData$HerbPresent.2yn & HerbData$HerbPresent.3yn & HerbData$HerbPresent.4yn == TRUE, 0, 1)
table(HerbData$HerbPresent.yn)

# Replace flag variable with 1, if herbs were used for induction or augmentation
HerbData$HerbPresent.yn[HerbData$Encour_Induc_CastorV02 == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Encour_Induc_CaulophV02 == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Encour_Induc_CohoshV02 == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Encour_Induc_PulsatV02 == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Encour_Induc_EvPrimOilV02 == 1] <- 1
table(HerbData$HerbPresent.yn)

HerbData$HerbPresent.yn[HerbData$Encour_Aug_CastorV02 == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Encour_Aug_CaulophV02 == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Encour_Aug_CohoshV02 == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Encour_Aug_PulsatV02 == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Encour_Aug_EvPrimOilV02 == 1] <- 1
table(HerbData$HerbPresent.yn)

# Replace flag variable with 1, if herbs were used for blood actions
HerbData$HerbPresent.yn[HerbData$Blood_Avoid_ShepPurse == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Blood_Avoid_Angelica == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Blood_Avoid_Motherwort == 1] <- 1
table(HerbData$HerbPresent.yn)
HerbData$HerbPresent.yn[HerbData$Blood_Actions_Herbs == 1] <- 1
table(HerbData$HerbPresent.yn)

warnings()
###################
# Create a flag for Blood Action Other Herbs
HerbData <- HerbData %>% 
  mutate(HerbActionOther.1yn = NA) 

HerbData <- HerbData %>% 
  mutate(HerbActionOther.1yn = str_detect(Blood_ActionsHerbs, "^\\s*$")) 

# Replace flag variable with 1, if pattern is in string. 
HerbData$HerbActionOther.yn <- ifelse(HerbData$HerbActionOther.1yn == TRUE, 0, 1)

####################
# Replace flag variable with 1, if herbs were used for blood actions
HerbData$HerbPresent.yn[HerbData$HerbActionOther.yn == 1] <- 1
table(HerbData$HerbPresent.yn)

# Check tabulations
table(HerbData$HerbPresent.yn, HerbData$HerbActionOther.yn)
table(HerbData$HerbActionOther.yn)

###################################################################################################

# Remove excess variables)
HerbData <- HerbData %>% select(-HerbPresent1.yn, -HerbPresent2.yn)

tabyl(HerbData$HerbPresent.yn, sort = TRUE)

###################################################################################################
# ### Compare to earlier subset sent by Marit....
# # Import 0409 herb subset V01.sav
# library(haven)
# X0409_herb_subset_V01 <- read_sav("0409 herb subset V01.sav")
# View(X0409_herb_subset_V01)
# 
# # Save as a new tibble and set order of columns
# HerbData2 <- X0409_herb_subset_V01
# head(HerbData2)
# 
# HerbData2 <- HerbData2 %>% 
#   mutate(HerbPresent.1yn = str_detect(Herb_1Text, "^\\s*$")) %>% 
#   mutate(HerbPresent.2yn = str_detect(Herb_2Text, "^\\s*$")) %>% 
#   mutate(HerbPresent.3yn = str_detect(Herb_3Text, "^\\s*$"))%>% 
#   mutate(HerbPresent.4yn = str_detect(Herb_4Text, "^\\s*$"))
# 
# HerbData2$HerbPresent.yn <- ifelse(HerbData2$HerbPresent.1yn & HerbData2$HerbPresent.2yn & HerbData2$HerbPresent.3yn & HerbData2$HerbPresent.4yn == TRUE, 0, 1)
# 
# tabyl(HerbData2$HerbPresent.yn, sort = TRUE)
###################################################################################################
###################################################################################################
# RASPBERRY: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(rasp.1yn = str_detect(Herb_1Text, "rasp") 
         | str_detect(Herb_1Text, "berry") 
         | str_detect(Herb_1Text, "berrry") 
         | str_detect(Herb_1Text, "rrl")
         | str_detect(Herb_1Text, "leaf")
         | str_detect(Herb_1Text, "rasb")
         | str_detect(Herb_1Text, "^rr")) %>% 
  mutate(rasp.2yn = str_detect(Herb_2Text, "rasp") 
         | str_detect(Herb_2Text, "berry") 
         | str_detect(Herb_2Text, "berrry") 
         | str_detect(Herb_2Text, "rrl")
         | str_detect(Herb_2Text, "leaf")
         | str_detect(Herb_2Text, "rasb")
         | str_detect(Herb_2Text, "^rr")) %>% 
  mutate(rasp.3yn = str_detect(Herb_3Text, "rasp") 
         | str_detect(Herb_3Text, "berry") 
         | str_detect(Herb_3Text, "berrry") 
         | str_detect(Herb_3Text, "rrl")
         | str_detect(Herb_3Text, "leaf")
         | str_detect(Herb_3Text, "rasb")
         | str_detect(Herb_3Text, "^rr"))%>% 
  mutate(rasp.4yn = str_detect(Herb_4Text, "rasp") 
         | str_detect(Herb_4Text, "berry") 
         | str_detect(Herb_4Text, "berrry") 
         | str_detect(Herb_4Text, "rrl")
         | str_detect(Herb_4Text, "leaf")
         | str_detect(Herb_4Text, "rasb")
         | str_detect(Herb_4Text, "^rr"))%>% 
  mutate(rasp.5yn = str_detect(Blood_ActionsHerbs, "rasp") 
         | str_detect(Blood_ActionsHerbs, "rrl")
         | str_detect(Blood_ActionsHerbs, "leaf")
         | str_detect(Blood_ActionsHerbs, "rasb")
         | str_detect(Blood_ActionsHerbs, "^rr"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(rasp.yn = 0) %>% mutate(rasp.reason = NA_character_) %>% 
  mutate(rasp.tri1 = NA_integer_) %>% mutate(rasp.tri2 = NA_integer_) %>% mutate(rasp.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$rasp.yn <- ifelse(HerbData$rasp.1yn | HerbData$rasp.2yn | HerbData$rasp.3yn | HerbData$rasp.4yn | HerbData$rasp.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$rasp.reason <- ifelse(HerbData$rasp.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$rasp.reason)
HerbData$rasp.reason <- ifelse(HerbData$rasp.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$rasp.reason)
HerbData$rasp.reason <- ifelse(HerbData$rasp.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$rasp.reason)
HerbData$rasp.reason <- ifelse(HerbData$rasp.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$rasp.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$rasp.tri1 <- ifelse(HerbData$rasp.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$rasp.tri1)
HerbData$rasp.tri1 <- ifelse(HerbData$rasp.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$rasp.tri1)
HerbData$rasp.tri1 <- ifelse(HerbData$rasp.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$rasp.tri1)
HerbData$rasp.tri1 <- ifelse(HerbData$rasp.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$rasp.tri1)

HerbData$rasp.tri2 <- ifelse(HerbData$rasp.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$rasp.tri2)
HerbData$rasp.tri2 <- ifelse(HerbData$rasp.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$rasp.tri2)
HerbData$rasp.tri2 <- ifelse(HerbData$rasp.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$rasp.tri2)
HerbData$rasp.tri2 <- ifelse(HerbData$rasp.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$rasp.tri2)

HerbData$rasp.tri3 <- ifelse(HerbData$rasp.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$rasp.tri3)
HerbData$rasp.tri3 <- ifelse(HerbData$rasp.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$rasp.tri3)
HerbData$rasp.tri3 <- ifelse(HerbData$rasp.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$rasp.tri3)
HerbData$rasp.tri3 <- ifelse(HerbData$rasp.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$rasp.tri3)

# Check for consistency
HerbData2 %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, rasp.yn, rasp.reason, rasp.tri1, rasp.tri2, rasp.tri3) %>% filter(rasp.yn == 0)
HerbData2 %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, rasp.yn, rasp.reason, rasp.tri1, rasp.tri2, rasp.tri3) %>% filter(rasp.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, rasp.1yn, rasp.yn) %>% filter(rasp.1yn == TRUE)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, rasp.2yn, rasp.yn) %>% filter(rasp.2yn == TRUE)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, rasp.3yn, rasp.yn) %>% filter(rasp.3yn == TRUE)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, rasp.4yn, rasp.yn) %>% filter(rasp.4yn == TRUE)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, rasp.5yn, rasp.yn) %>% filter(rasp.5yn == TRUE)

###################################################################################################
# ALFALFA: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(alfa.1yn = str_detect(Herb_1Text, "alfa") 
         | str_detect(Herb_1Text, "alpha")) %>% 
  mutate(alfa.2yn = str_detect(Herb_2Text, "alfa") 
         | str_detect(Herb_2Text, "alpha")) %>% 
  mutate(alfa.3yn = str_detect(Herb_3Text, "alfa") 
         | str_detect(Herb_3Text, "alpha"))%>% 
  mutate(alfa.4yn = str_detect(Herb_4Text, "alfa") 
         | str_detect(Herb_4Text, "alpha"))%>% 
  mutate(alfa.5yn = str_detect(Blood_ActionsHerbs, "alfa") 
         | str_detect(Blood_ActionsHerbs, "alpha"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(alfa.yn = 0) %>% mutate(alfa.reason = NA_character_) %>%
  mutate(alfa.tri1 = NA_integer_) %>% mutate(alfa.tri2 = NA_integer_) %>% mutate(alfa.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$alfa.yn <- ifelse(HerbData$alfa.1yn | HerbData$alfa.2yn | HerbData$alfa.3yn | HerbData$alfa.4yn | HerbData$alfa.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$alfa.reason <- ifelse(HerbData$alfa.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$alfa.reason)
HerbData$alfa.reason <- ifelse(HerbData$alfa.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$alfa.reason)
HerbData$alfa.reason <- ifelse(HerbData$alfa.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$alfa.reason)
HerbData$alfa.reason <- ifelse(HerbData$alfa.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$alfa.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$alfa.tri1 <- ifelse(HerbData$alfa.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$alfa.tri1)
HerbData$alfa.tri1 <- ifelse(HerbData$alfa.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$alfa.tri1)
HerbData$alfa.tri1 <- ifelse(HerbData$alfa.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$alfa.tri1)
HerbData$alfa.tri1 <- ifelse(HerbData$alfa.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$alfa.tri1)

HerbData$alfa.tri2 <- ifelse(HerbData$alfa.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$alfa.tri2)
HerbData$alfa.tri2 <- ifelse(HerbData$alfa.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$alfa.tri2)
HerbData$alfa.tri2 <- ifelse(HerbData$alfa.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$alfa.tri2)
HerbData$alfa.tri2 <- ifelse(HerbData$alfa.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$alfa.tri2)

HerbData$alfa.tri3 <- ifelse(HerbData$alfa.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$alfa.tri3)
HerbData$alfa.tri3 <- ifelse(HerbData$alfa.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$alfa.tri3)
HerbData$alfa.tri3 <- ifelse(HerbData$alfa.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$alfa.tri3)
HerbData$alfa.tri3 <- ifelse(HerbData$alfa.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$alfa.tri3)

# Check for consistency
HerbData2 %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, alfa.yn, alfa.reason, alfa.tri1, alfa.tri2, alfa.tri3) %>% filter(alfa.yn == 0)
HerbData2 %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, alfa.yn, alfa.reason, alfa.tri1, alfa.tri2, alfa.tri3) %>% filter(alfa.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, alfa.1yn, alfa.yn) %>% filter(alfa.1yn == TRUE)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, alfa.2yn, alfa.yn) %>% filter(alfa.2yn == TRUE)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, alfa.3yn, alfa.yn) %>% filter(alfa.3yn == TRUE)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, alfa.4yn, alfa.yn) %>% filter(alfa.4yn == TRUE)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, alfa.5yn, alfa.yn) %>% filter(alfa.5yn == TRUE)

###################################################################################################
# Evening Primrose Oil: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(primr.1yn = str_detect(Herb_1Text, "primro")|str_detect(Herb_1Text, "epo")) %>% 
  mutate(primr.2yn = str_detect(Herb_2Text, "primro")|str_detect(Herb_1Text, "epo")) %>% 
  mutate(primr.3yn = str_detect(Herb_3Text, "primro")|str_detect(Herb_1Text, "epo"))%>% 
  mutate(primr.4yn = str_detect(Herb_4Text, "primro")|str_detect(Herb_1Text, "epo"))%>% 
  mutate(primr.5yn = str_detect(Blood_ActionsHerbs, "primro")|str_detect(Blood_ActionsHerbs, "epo"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(primr.yn = 0) %>% mutate(primr.reason = NA_character_) %>%
  mutate(primr.tri1 = NA_integer_) %>% mutate(primr.tri2 = NA_integer_) %>% mutate(primr.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$primr.yn <- ifelse(HerbData$primr.1yn | HerbData$primr.2yn | HerbData$primr.3yn | HerbData$primr.4yn | HerbData$primr.5yn == TRUE, 1, 0)

# Replace flag variable with 1, if EPO was used for induction or augmentation
HerbData$primr.yn[HerbData$Encour_Induc_EvPrimOilV02 == 1] <- 1
HerbData$primr.yn[HerbData$Encour_Aug_EvPrimOilV02 == 1] <- 1

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$primr.reason <- ifelse(HerbData$primr.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$primr.reason)
HerbData$primr.reason <- ifelse(HerbData$primr.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$primr.reason)
HerbData$primr.reason <- ifelse(HerbData$primr.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$primr.reason)
HerbData$primr.reason <- ifelse(HerbData$primr.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$primr.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$primr.tri1 <- ifelse(HerbData$primr.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$primr.tri1)
HerbData$primr.tri1 <- ifelse(HerbData$primr.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$primr.tri1)
HerbData$primr.tri1 <- ifelse(HerbData$primr.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$primr.tri1)
HerbData$primr.tri1 <- ifelse(HerbData$primr.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$primr.tri1)

HerbData$primr.tri2 <- ifelse(HerbData$primr.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$primr.tri2)
HerbData$primr.tri2 <- ifelse(HerbData$primr.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$primr.tri2)
HerbData$primr.tri2 <- ifelse(HerbData$primr.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$primr.tri2)
HerbData$primr.tri2 <- ifelse(HerbData$primr.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$primr.tri2)

HerbData$primr.tri3 <- ifelse(HerbData$primr.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$primr.tri3)
HerbData$primr.tri3 <- ifelse(HerbData$primr.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$primr.tri3)
HerbData$primr.tri3 <- ifelse(HerbData$primr.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$primr.tri3)
HerbData$primr.tri3 <- ifelse(HerbData$primr.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$primr.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, primr.yn, primr.reason, primr.tri1, primr.tri2, primr.tri3) %>% filter(primr.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, primr.yn, primr.reason, primr.tri1, primr.tri2, primr.tri3) %>% filter(primr.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, primr.1yn, primr.yn) %>% filter(primr.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, primr.2yn, primr.yn) %>% filter(primr.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, primr.3yn, primr.yn) %>% filter(primr.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, primr.4yn, primr.yn) %>% filter(primr.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, primr.5yn, primr.yn) %>% filter(primr.5yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, Encour_Induc_EvPrimOilV02, Encour_Aug_EvPrimOilV02, primr.yn) %>% filter(Encour_Induc_EvPrimOilV02 == 1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, Encour_Induc_EvPrimOilV02, Encour_Aug_EvPrimOilV02, primr.yn) %>% filter(Encour_Aug_EvPrimOilV02 == 1)

###################################################################################################
###################################################################################################
# NETTLE: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(nettle.1yn = str_detect(Herb_1Text, "nettle") 
         | str_detect(Herb_1Text, "netle")| str_detect(Herb_1Text, "nttle")) %>% 
  mutate(nettle.2yn = str_detect(Herb_2Text, "nettle") 
         | str_detect(Herb_2Text, "netle") | str_detect(Herb_2Text, "nttle")) %>% 
  mutate(nettle.3yn = str_detect(Herb_3Text, "nettle") 
         | str_detect(Herb_3Text, "netle")| str_detect(Herb_3Text, "nttle"))%>% 
  mutate(nettle.4yn = str_detect(Herb_4Text, "nettle") 
         | str_detect(Herb_4Text, "netle")| str_detect(Herb_4Text, "nttle"))%>% 
  mutate(nettle.5yn = str_detect(Blood_ActionsHerbs, "nettle") 
         | str_detect(Blood_ActionsHerbs, "netle")| str_detect(Blood_ActionsHerbs, "nttle"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(nettle.yn = 0) %>% mutate(nettle.reason = NA_character_) %>%
  mutate(nettle.tri1 = NA_integer_) %>% mutate(nettle.tri2 = NA_integer_) %>% mutate(nettle.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$nettle.yn <- ifelse(HerbData$nettle.1yn | HerbData$nettle.2yn | HerbData$nettle.3yn | HerbData$nettle.4yn | HerbData$nettle.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$nettle.reason <- ifelse(HerbData$nettle.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$nettle.reason)
HerbData$nettle.reason <- ifelse(HerbData$nettle.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$nettle.reason)
HerbData$nettle.reason <- ifelse(HerbData$nettle.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$nettle.reason)
HerbData$nettle.reason <- ifelse(HerbData$nettle.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$nettle.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$nettle.tri1 <- ifelse(HerbData$nettle.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$nettle.tri1)
HerbData$nettle.tri1 <- ifelse(HerbData$nettle.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$nettle.tri1)
HerbData$nettle.tri1 <- ifelse(HerbData$nettle.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$nettle.tri1)
HerbData$nettle.tri1 <- ifelse(HerbData$nettle.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$nettle.tri1)

HerbData$nettle.tri2 <- ifelse(HerbData$nettle.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$nettle.tri2)
HerbData$nettle.tri2 <- ifelse(HerbData$nettle.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$nettle.tri2)
HerbData$nettle.tri2 <- ifelse(HerbData$nettle.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$nettle.tri2)
HerbData$nettle.tri2 <- ifelse(HerbData$nettle.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$nettle.tri2)

HerbData$nettle.tri3 <- ifelse(HerbData$nettle.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$nettle.tri3)
HerbData$nettle.tri3 <- ifelse(HerbData$nettle.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$nettle.tri3)
HerbData$nettle.tri3 <- ifelse(HerbData$nettle.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$nettle.tri3)
HerbData$nettle.tri3 <- ifelse(HerbData$nettle.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$nettle.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, nettle.yn, nettle.reason, nettle.tri1, nettle.tri2, nettle.tri3) %>% filter(nettle.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, nettle.yn, nettle.reason, nettle.tri1, nettle.tri2, nettle.tri3) %>% filter(nettle.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, nettle.1yn, nettle.yn) %>% filter(nettle.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, nettle.2yn, nettle.yn) %>% filter(nettle.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, nettle.3yn, nettle.yn) %>% filter(nettle.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, nettle.4yn, nettle.yn) %>% filter(nettle.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, nettle.5yn, nettle.yn) %>% filter(nettle.5yn ==1)

###################################################################################################
# Ensure "gentle" and "birth" are spelled correctly
HerbData$Herb_1Text <- gsub("gental", "gentle", HerbData$Herb_1Text, fixed=TRUE)
HerbData$Herb_2Text <- gsub("gental", "gentle", HerbData$Herb_2Text, fixed=TRUE)
HerbData$Herb_3Text <- gsub("gental", "gentle", HerbData$Herb_3Text, fixed=TRUE)
HerbData$Herb_4Text <- gsub("gental", "gentle", HerbData$Herb_4Text, fixed=TRUE)

HerbData$Herb_1Text <- gsub("bith", "birth", HerbData$Herb_1Text, fixed=TRUE)
HerbData$Herb_2Text <- gsub("bith", "birth", HerbData$Herb_2Text, fixed=TRUE)
HerbData$Herb_3Text <- gsub("bith", "birth", HerbData$Herb_3Text, fixed=TRUE)
HerbData$Herb_4Text <- gsub("bith", "birth", HerbData$Herb_4Text, fixed=TRUE)

# GENTLE BIRTH FORMULA: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(gbform.1yn = (str_detect(Herb_1Text, "formu") 
         | str_detect(Herb_1Text, "fomula") | str_detect(Herb_1Text, "fornula"))
         & str_detect(Herb_1Text, "gentle") & str_detect(Herb_1Text, "birth")) %>% 
  mutate(gbform.2yn = (str_detect(Herb_2Text, "formu") 
         | str_detect(Herb_2Text, "fomula") | str_detect(Herb_2Text, "fornula"))
         & str_detect(Herb_2Text, "gentle") & str_detect(Herb_2Text, "birth")) %>% 
  mutate(gbform.3yn = (str_detect(Herb_3Text, "formu") 
         | str_detect(Herb_3Text, "fomula") | str_detect(Herb_3Text, "fornula"))
         & str_detect(Herb_3Text, "gentle") & str_detect(Herb_3Text, "birth")) %>% 
  mutate(gbform.4yn = (str_detect(Herb_4Text, "formu") 
         | str_detect(Herb_4Text, "fomula") | str_detect(Herb_4Text, "fornula")
         & str_detect(Herb_4Text, "gentle") & str_detect(Herb_4Text, "birth"))) %>% 
  mutate(gbform.5yn = (str_detect(Blood_ActionsHerbs, "gentle") & str_detect(Blood_ActionsHerbs, "birth")))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(gbform.yn = 0) %>% mutate(gbform.reason = NA_character_) %>%
  mutate(gbform.tri1 = NA_integer_) %>% mutate(gbform.tri2 = NA_integer_) %>% mutate(gbform.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$gbform.yn <- ifelse(HerbData$gbform.1yn | HerbData$gbform.2yn | HerbData$gbform.3yn | HerbData$gbform.4yn | HerbData$gbform.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$gbform.reason <- ifelse(HerbData$gbform.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$gbform.reason)
HerbData$gbform.reason <- ifelse(HerbData$gbform.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$gbform.reason)
HerbData$gbform.reason <- ifelse(HerbData$gbform.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$gbform.reason)
HerbData$gbform.reason <- ifelse(HerbData$gbform.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$gbform.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$gbform.tri1 <- ifelse(HerbData$gbform.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$gbform.tri1)
HerbData$gbform.tri1 <- ifelse(HerbData$gbform.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$gbform.tri1)
HerbData$gbform.tri1 <- ifelse(HerbData$gbform.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$gbform.tri1)
HerbData$gbform.tri1 <- ifelse(HerbData$gbform.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$gbform.tri1)

HerbData$gbform.tri2 <- ifelse(HerbData$gbform.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$gbform.tri2)
HerbData$gbform.tri2 <- ifelse(HerbData$gbform.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$gbform.tri2)
HerbData$gbform.tri2 <- ifelse(HerbData$gbform.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$gbform.tri2)
HerbData$gbform.tri2 <- ifelse(HerbData$gbform.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$gbform.tri2)

HerbData$gbform.tri3 <- ifelse(HerbData$gbform.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$gbform.tri3)
HerbData$gbform.tri3 <- ifelse(HerbData$gbform.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$gbform.tri3)
HerbData$gbform.tri3 <- ifelse(HerbData$gbform.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$gbform.tri3)
HerbData$gbform.tri3 <- ifelse(HerbData$gbform.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$gbform.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, gbform.yn, gbform.reason, gbform.tri1, gbform.tri2, gbform.tri3) %>% filter(gbform.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, gbform.yn, gbform.reason, gbform.tri1, gbform.tri2, gbform.tri3) %>% filter(gbform.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, gbform.1yn, gbform.yn) %>% filter(gbform.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, gbform.2yn, gbform.yn) %>% filter(gbform.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, gbform.3yn, gbform.yn) %>% filter(gbform.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, gbform.4yn, gbform.yn) %>% filter(gbform.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, gbform.5yn, gbform.yn) %>% filter(gbform.5yn ==1)

###################################################################################################
# DANDELION: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(dande.1yn = str_detect(Herb_1Text, "dand") 
         | str_detect(Herb_1Text, "deand")) %>% 
  mutate(dande.2yn = str_detect(Herb_2Text, "dand") 
         | str_detect(Herb_2Text, "deand")) %>% 
  mutate(dande.3yn = str_detect(Herb_3Text, "dand") 
         | str_detect(Herb_3Text, "deand"))%>% 
  mutate(dande.4yn = str_detect(Herb_4Text, "dand") 
         | str_detect(Herb_4Text, "deand")) %>% 
  mutate(dande.5yn = str_detect(Blood_ActionsHerbs, "dand") 
         | str_detect(Blood_ActionsHerbs, "deand"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(dande.yn = 0) %>% mutate(dande.reason = NA_character_) %>%
  mutate(dande.tri1 = NA_integer_) %>% mutate(dande.tri2 = NA_integer_) %>% mutate(dande.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$dande.yn <- ifelse(HerbData$dande.1yn | HerbData$dande.2yn | HerbData$dande.3yn | HerbData$dande.4yn | HerbData$dande.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$dande.reason <- ifelse(HerbData$dande.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$dande.reason)
HerbData$dande.reason <- ifelse(HerbData$dande.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$dande.reason)
HerbData$dande.reason <- ifelse(HerbData$dande.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$dande.reason)
HerbData$dande.reason <- ifelse(HerbData$dande.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$dande.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$dande.tri1 <- ifelse(HerbData$dande.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$dande.tri1)
HerbData$dande.tri1 <- ifelse(HerbData$dande.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$dande.tri1)
HerbData$dande.tri1 <- ifelse(HerbData$dande.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$dande.tri1)
HerbData$dande.tri1 <- ifelse(HerbData$dande.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$dande.tri1)

HerbData$dande.tri2 <- ifelse(HerbData$dande.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$dande.tri2)
HerbData$dande.tri2 <- ifelse(HerbData$dande.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$dande.tri2)
HerbData$dande.tri2 <- ifelse(HerbData$dande.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$dande.tri2)
HerbData$dande.tri2 <- ifelse(HerbData$dande.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$dande.tri2)

HerbData$dande.tri3 <- ifelse(HerbData$dande.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$dande.tri3)
HerbData$dande.tri3 <- ifelse(HerbData$dande.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$dande.tri3)
HerbData$dande.tri3 <- ifelse(HerbData$dande.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$dande.tri3)
HerbData$dande.tri3 <- ifelse(HerbData$dande.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$dande.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, dande.yn, dande.reason, dande.tri1, dande.tri2, dande.tri3) %>% filter(dande.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, dande.yn, dande.reason, dande.tri1, dande.tri2, dande.tri3) %>% filter(dande.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, dande.1yn, dande.yn) %>% filter(dande.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, dande.2yn, dande.yn) %>% filter(dande.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, dande.3yn, dande.yn) %>% filter(dande.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, dande.4yn, dande.yn) %>% filter(dande.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, dande.5yn, dande.yn) %>% filter(dande.5yn ==1)

###################################################################################################
# Mothers Blend Tea: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(mblend.1yn = str_detect(Herb_1Text, "mot") 
         & str_detect(Herb_1Text, "blend")) %>% 
  mutate(mblend.2yn = str_detect(Herb_2Text, "mot") 
         & str_detect(Herb_2Text, "blend")) %>% 
  mutate(mblend.3yn = str_detect(Herb_3Text, "mot") 
         & str_detect(Herb_3Text, "blend")) %>% 
  mutate(mblend.4yn = str_detect(Herb_4Text, "mot") 
         & str_detect(Herb_4Text, "blend")) %>% 
  mutate(mblend.5yn = str_detect(Blood_ActionsHerbs, "mot") 
         & str_detect(Blood_ActionsHerbs, "blend"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(mblend.yn = 0) %>% mutate(mblend.reason = NA_character_) %>%
  mutate(mblend.tri1 = NA_integer_) %>% mutate(mblend.tri2 = NA_integer_) %>% mutate(mblend.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$mblend.yn <- ifelse(HerbData$mblend.1yn | HerbData$mblend.2yn | HerbData$mblend.3yn | HerbData$mblend.4yn | HerbData$mblend.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$mblend.reason <- ifelse(HerbData$mblend.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$mblend.reason)
HerbData$mblend.reason <- ifelse(HerbData$mblend.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$mblend.reason)
HerbData$mblend.reason <- ifelse(HerbData$mblend.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$mblend.reason)
HerbData$mblend.reason <- ifelse(HerbData$mblend.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$mblend.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$mblend.tri1 <- ifelse(HerbData$mblend.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$mblend.tri1)
HerbData$mblend.tri1 <- ifelse(HerbData$mblend.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$mblend.tri1)
HerbData$mblend.tri1 <- ifelse(HerbData$mblend.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$mblend.tri1)
HerbData$mblend.tri1 <- ifelse(HerbData$mblend.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$mblend.tri1)

HerbData$mblend.tri2 <- ifelse(HerbData$mblend.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$mblend.tri2)
HerbData$mblend.tri2 <- ifelse(HerbData$mblend.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$mblend.tri2)
HerbData$mblend.tri2 <- ifelse(HerbData$mblend.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$mblend.tri2)
HerbData$mblend.tri2 <- ifelse(HerbData$mblend.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$mblend.tri2)

HerbData$mblend.tri3 <- ifelse(HerbData$mblend.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$mblend.tri3)
HerbData$mblend.tri3 <- ifelse(HerbData$mblend.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$mblend.tri3)
HerbData$mblend.tri3 <- ifelse(HerbData$mblend.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$mblend.tri3)
HerbData$mblend.tri3 <- ifelse(HerbData$mblend.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$mblend.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, mblend.yn, mblend.reason, mblend.tri1, mblend.tri2, mblend.tri3) %>% filter(mblend.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, mblend.yn, mblend.reason, mblend.tri1, mblend.tri2, mblend.tri3) %>% filter(mblend.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, mblend.1yn, mblend.yn) %>% filter(mblend.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, mblend.2yn, mblend.yn) %>% filter(mblend.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, mblend.3yn, mblend.yn) %>% filter(mblend.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, mblend.4yn, mblend.yn) %>% filter(mblend.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, mblend.5yn, mblend.yn) %>% filter(mblend.5yn ==1)

###################################################################################################
# FLORADIX: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(flordx.1yn = str_detect(Herb_1Text, "flori") 
         | str_detect(Herb_1Text, "florad")| str_detect(Herb_1Text, "floro")
         | str_detect(Herb_1Text, "fluro")| str_detect(Herb_1Text, "flord")
         | str_detect(Herb_1Text, "flour")| str_detect(Herb_1Text, "flur")) %>% 
  mutate(flordx.2yn = str_detect(Herb_2Text, "flori") 
         | str_detect(Herb_2Text, "florad")| str_detect(Herb_2Text, "floro")
         | str_detect(Herb_2Text, "fluro")| str_detect(Herb_2Text, "flord")
         | str_detect(Herb_2Text, "flour")| str_detect(Herb_2Text, "flur")) %>% 
  mutate(flordx.3yn = str_detect(Herb_3Text, "flori") 
         | str_detect(Herb_3Text, "florad")| str_detect(Herb_3Text, "floro")
         | str_detect(Herb_3Text, "fluro")| str_detect(Herb_3Text, "flord")
         | str_detect(Herb_3Text, "flour")| str_detect(Herb_3Text, "flur"))%>% 
  mutate(flordx.4yn = str_detect(Herb_4Text, "flori") 
         | str_detect(Herb_4Text, "florad")| str_detect(Herb_4Text, "floro")
         | str_detect(Herb_4Text, "fluro")| str_detect(Herb_4Text, "flord")
         | str_detect(Herb_4Text, "flour")| str_detect(Herb_4Text, "flur")) %>% 
  mutate(flordx.5yn = str_detect(Blood_ActionsHerbs, "flori") 
         | str_detect(Blood_ActionsHerbs, "florad")| str_detect(Blood_ActionsHerbs, "floro")
         | str_detect(Blood_ActionsHerbs, "fluro")| str_detect(Blood_ActionsHerbs, "flord")
         | str_detect(Blood_ActionsHerbs, "flour")| str_detect(Blood_ActionsHerbs, "flur"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(flordx.yn = 0) %>% mutate(flordx.reason = NA_character_) %>%
  mutate(flordx.tri1 = NA_integer_) %>% mutate(flordx.tri2 = NA_integer_) %>% mutate(flordx.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$flordx.yn <- ifelse(HerbData$flordx.1yn | HerbData$flordx.2yn | HerbData$flordx.3yn | HerbData$flordx.4yn | HerbData$flordx.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$flordx.reason <- ifelse(HerbData$flordx.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$flordx.reason)
HerbData$flordx.reason <- ifelse(HerbData$flordx.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$flordx.reason)
HerbData$flordx.reason <- ifelse(HerbData$flordx.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$flordx.reason)
HerbData$flordx.reason <- ifelse(HerbData$flordx.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$flordx.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$flordx.tri1 <- ifelse(HerbData$flordx.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$flordx.tri1)
HerbData$flordx.tri1 <- ifelse(HerbData$flordx.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$flordx.tri1)
HerbData$flordx.tri1 <- ifelse(HerbData$flordx.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$flordx.tri1)
HerbData$flordx.tri1 <- ifelse(HerbData$flordx.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$flordx.tri1)

HerbData$flordx.tri2 <- ifelse(HerbData$flordx.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$flordx.tri2)
HerbData$flordx.tri2 <- ifelse(HerbData$flordx.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$flordx.tri2)
HerbData$flordx.tri2 <- ifelse(HerbData$flordx.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$flordx.tri2)
HerbData$flordx.tri2 <- ifelse(HerbData$flordx.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$flordx.tri2)

HerbData$flordx.tri3 <- ifelse(HerbData$flordx.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$flordx.tri3)
HerbData$flordx.tri3 <- ifelse(HerbData$flordx.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$flordx.tri3)
HerbData$flordx.tri3 <- ifelse(HerbData$flordx.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$flordx.tri3)
HerbData$flordx.tri3 <- ifelse(HerbData$flordx.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$flordx.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, flordx.yn, flordx.reason, flordx.tri1, flordx.tri2, flordx.tri3) %>% filter(flordx.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, flordx.yn, flordx.reason, flordx.tri1, flordx.tri2, flordx.tri3) %>% filter(flordx.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, flordx.1yn, flordx.yn) %>% filter(flordx.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, flordx.2yn, flordx.yn) %>% filter(flordx.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, flordx.3yn, flordx.yn) %>% filter(flordx.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, flordx.4yn, flordx.yn) %>% filter(flordx.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, flordx.5yn, flordx.yn) %>% filter(flordx.5yn ==1)

###################################################################################################
# ECHINACEA: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(echina.1yn = str_detect(Herb_1Text, "echin") 
         | str_detect(Herb_1Text, "ecin")| str_detect(Herb_1Text, "ecch")
         | str_detect(Herb_1Text, "ecci")| str_detect(Herb_1Text, "ecen")| str_detect(Herb_1Text, "echan")) %>% 
  mutate(echina.2yn = str_detect(Herb_2Text, "echin") 
         | str_detect(Herb_2Text, "ecin")| str_detect(Herb_2Text, "ecch")
         | str_detect(Herb_2Text, "ecci")| str_detect(Herb_2Text, "ecen")| str_detect(Herb_2Text, "echan")) %>% 
  mutate(echina.3yn = str_detect(Herb_3Text, "echin") 
         | str_detect(Herb_3Text, "ecin")| str_detect(Herb_3Text, "ecch")
         | str_detect(Herb_3Text, "ecci")| str_detect(Herb_3Text, "ecen")| str_detect(Herb_3Text, "echan"))%>% 
  mutate(echina.4yn = str_detect(Herb_4Text, "echin") 
         | str_detect(Herb_4Text, "ecin")| str_detect(Herb_4Text, "ecch")
         | str_detect(Herb_4Text, "ecci")| str_detect(Herb_4Text, "ecen")| str_detect(Herb_4Text, "echan")) %>% 
  mutate(echina.5yn = str_detect(Blood_ActionsHerbs, "echin") 
         | str_detect(Blood_ActionsHerbs, "ecin")| str_detect(Blood_ActionsHerbs, "ecch")
         | str_detect(Blood_ActionsHerbs, "ecci")| str_detect(Blood_ActionsHerbs, "ecen")| str_detect(Blood_ActionsHerbs, "echan"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(echina.yn = 0) %>% mutate(echina.reason = NA_character_) %>%
  mutate(echina.tri1 = NA_integer_) %>% mutate(echina.tri2 = NA_integer_) %>% mutate(echina.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$echina.yn <- ifelse(HerbData$echina.1yn | HerbData$echina.2yn | HerbData$echina.3yn | HerbData$echina.4yn | HerbData$echina.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$echina.reason <- ifelse(HerbData$echina.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$echina.reason)
HerbData$echina.reason <- ifelse(HerbData$echina.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$echina.reason)
HerbData$echina.reason <- ifelse(HerbData$echina.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$echina.reason)
HerbData$echina.reason <- ifelse(HerbData$echina.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$echina.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$echina.tri1 <- ifelse(HerbData$echina.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$echina.tri1)
HerbData$echina.tri1 <- ifelse(HerbData$echina.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$echina.tri1)
HerbData$echina.tri1 <- ifelse(HerbData$echina.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$echina.tri1)
HerbData$echina.tri1 <- ifelse(HerbData$echina.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$echina.tri1)

HerbData$echina.tri2 <- ifelse(HerbData$echina.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$echina.tri2)
HerbData$echina.tri2 <- ifelse(HerbData$echina.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$echina.tri2)
HerbData$echina.tri2 <- ifelse(HerbData$echina.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$echina.tri2)
HerbData$echina.tri2 <- ifelse(HerbData$echina.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$echina.tri2)

HerbData$echina.tri3 <- ifelse(HerbData$echina.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$echina.tri3)
HerbData$echina.tri3 <- ifelse(HerbData$echina.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$echina.tri3)
HerbData$echina.tri3 <- ifelse(HerbData$echina.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$echina.tri3)
HerbData$echina.tri3 <- ifelse(HerbData$echina.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$echina.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, echina.yn, echina.reason, echina.tri1, echina.tri2, echina.tri3) %>% filter(echina.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, echina.yn, echina.reason, echina.tri1, echina.tri2, echina.tri3) %>% filter(echina.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, echina.1yn, echina.yn) %>% filter(echina.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, echina.2yn, echina.yn) %>% filter(echina.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, echina.3yn, echina.yn) %>% filter(echina.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, echina.4yn, echina.yn) %>% filter(echina.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, echina.5yn, echina.yn) %>% filter(echina.5yn ==1)

###################################################################################################
# PULSATILLA: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(pulsat.1yn = str_detect(Herb_1Text, "puls") 
         | str_detect(Herb_1Text, "pusatilla")| str_detect(Herb_1Text, "puslatilla")) %>% 
  mutate(pulsat.2yn = str_detect(Herb_2Text, "puls") 
         | str_detect(Herb_2Text, "pusatilla")| str_detect(Herb_2Text, "puslatilla")) %>% 
  mutate(pulsat.3yn = str_detect(Herb_3Text, "puls") 
         | str_detect(Herb_3Text, "pusatilla")| str_detect(Herb_3Text, "puslatilla"))%>% 
  mutate(pulsat.4yn = str_detect(Herb_4Text, "puls") 
         | str_detect(Herb_4Text, "pusatilla")| str_detect(Herb_4Text, "puslatilla")) %>% 
  mutate(pulsat.5yn = str_detect(Blood_ActionsHerbs, "puls") 
         | str_detect(Blood_ActionsHerbs, "pusatilla")| str_detect(Blood_ActionsHerbs, "puslatilla"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(pulsat.yn = 0) %>% mutate(pulsat.reason = NA_character_) %>%
  mutate(pulsat.tri1 = NA_integer_) %>% mutate(pulsat.tri2 = NA_integer_) %>% mutate(pulsat.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$pulsat.yn <- ifelse(HerbData$pulsat.1yn | HerbData$pulsat.2yn | HerbData$pulsat.3yn | HerbData$pulsat.4yn | HerbData$pulsat.5yn == TRUE, 1, 0)

# Replace flag variable with 1, if EPO was used for induction or augmentation
HerbData$primr.yn[HerbData$Encour_Induc_PulsatV02 == 1] <- 1
HerbData$primr.yn[HerbData$Encour_Aug_PulsatV02 == 1] <- 1

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$pulsat.reason <- ifelse(HerbData$pulsat.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$pulsat.reason)
HerbData$pulsat.reason <- ifelse(HerbData$pulsat.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$pulsat.reason)
HerbData$pulsat.reason <- ifelse(HerbData$pulsat.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$pulsat.reason)
HerbData$pulsat.reason <- ifelse(HerbData$pulsat.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$pulsat.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$pulsat.tri1 <- ifelse(HerbData$pulsat.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$pulsat.tri1)
HerbData$pulsat.tri1 <- ifelse(HerbData$pulsat.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$pulsat.tri1)
HerbData$pulsat.tri1 <- ifelse(HerbData$pulsat.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$pulsat.tri1)
HerbData$pulsat.tri1 <- ifelse(HerbData$pulsat.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$pulsat.tri1)

HerbData$pulsat.tri2 <- ifelse(HerbData$pulsat.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$pulsat.tri2)
HerbData$pulsat.tri2 <- ifelse(HerbData$pulsat.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$pulsat.tri2)
HerbData$pulsat.tri2 <- ifelse(HerbData$pulsat.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$pulsat.tri2)
HerbData$pulsat.tri2 <- ifelse(HerbData$pulsat.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$pulsat.tri2)

HerbData$pulsat.tri3 <- ifelse(HerbData$pulsat.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$pulsat.tri3)
HerbData$pulsat.tri3 <- ifelse(HerbData$pulsat.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$pulsat.tri3)
HerbData$pulsat.tri3 <- ifelse(HerbData$pulsat.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$pulsat.tri3)
HerbData$pulsat.tri3 <- ifelse(HerbData$pulsat.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$pulsat.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, pulsat.yn, pulsat.reason, pulsat.tri1, pulsat.tri2, pulsat.tri3) %>% filter(pulsat.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, pulsat.yn, pulsat.reason, pulsat.tri1, pulsat.tri2, pulsat.tri3) %>% filter(pulsat.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, pulsat.1yn, pulsat.yn) %>% filter(pulsat.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, pulsat.2yn, pulsat.yn) %>% filter(pulsat.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, pulsat.3yn, pulsat.yn) %>% filter(pulsat.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, pulsat.4yn, pulsat.yn) %>% filter(pulsat.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, pulsat.5yn, pulsat.yn) %>% filter(pulsat.5yn ==1)

###################################################################################################
# CHLOROPHYLL: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(chloro.1yn = str_detect(Herb_1Text, "chlorop") | str_detect(Herb_1Text, "chlorrap")
         | str_detect(Herb_1Text, "chlorrop")| str_detect(Herb_1Text, "chloryp")
         | str_detect(Herb_1Text, "cholorp")| str_detect(Herb_1Text, "chorop")) %>% 
  mutate(chloro.2yn = str_detect(Herb_2Text, "chlorop") | str_detect(Herb_2Text, "chlorrap")
         | str_detect(Herb_2Text, "chlorrop")| str_detect(Herb_2Text, "chloryp")
         | str_detect(Herb_2Text, "cholorp")| str_detect(Herb_2Text, "chorop")) %>% 
  mutate(chloro.3yn = str_detect(Herb_3Text, "chlorop") | str_detect(Herb_3Text, "chlorrap")
         | str_detect(Herb_3Text, "chlorrop")| str_detect(Herb_3Text, "chloryp")
         | str_detect(Herb_3Text, "cholorp")| str_detect(Herb_3Text, "chorop")) %>% 
  mutate(chloro.4yn = str_detect(Herb_4Text, "chlorop") | str_detect(Herb_4Text, "chlorrap")
         | str_detect(Herb_4Text, "chlorrop")| str_detect(Herb_4Text, "chloryp")
         | str_detect(Herb_4Text, "cholorp")| str_detect(Herb_4Text, "chorop")) %>% 
  mutate(chloro.5yn = str_detect(Blood_ActionsHerbs, "chlorop") | str_detect(Blood_ActionsHerbs, "chlorrap")
         | str_detect(Blood_ActionsHerbs, "chlorrop")| str_detect(Blood_ActionsHerbs, "chloryp")
         | str_detect(Blood_ActionsHerbs, "cholorp")| str_detect(Blood_ActionsHerbs, "chorop"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(chloro.yn = 0) %>% mutate(chloro.reason = NA_character_) %>%
  mutate(chloro.tri1 = NA_integer_) %>% mutate(chloro.tri2 = NA_integer_) %>% mutate(chloro.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$chloro.yn <- ifelse(HerbData$chloro.1yn | HerbData$chloro.2yn | HerbData$chloro.3yn | HerbData$chloro.4yn | HerbData$chloro.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$chloro.reason <- ifelse(HerbData$chloro.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$chloro.reason)
HerbData$chloro.reason <- ifelse(HerbData$chloro.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$chloro.reason)
HerbData$chloro.reason <- ifelse(HerbData$chloro.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$chloro.reason)
HerbData$chloro.reason <- ifelse(HerbData$chloro.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$chloro.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$chloro.tri1 <- ifelse(HerbData$chloro.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$chloro.tri1)
HerbData$chloro.tri1 <- ifelse(HerbData$chloro.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$chloro.tri1)
HerbData$chloro.tri1 <- ifelse(HerbData$chloro.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$chloro.tri1)
HerbData$chloro.tri1 <- ifelse(HerbData$chloro.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$chloro.tri1)

HerbData$chloro.tri2 <- ifelse(HerbData$chloro.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$chloro.tri2)
HerbData$chloro.tri2 <- ifelse(HerbData$chloro.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$chloro.tri2)
HerbData$chloro.tri2 <- ifelse(HerbData$chloro.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$chloro.tri2)
HerbData$chloro.tri2 <- ifelse(HerbData$chloro.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$chloro.tri2)

HerbData$chloro.tri3 <- ifelse(HerbData$chloro.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$chloro.tri3)
HerbData$chloro.tri3 <- ifelse(HerbData$chloro.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$chloro.tri3)
HerbData$chloro.tri3 <- ifelse(HerbData$chloro.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$chloro.tri3)
HerbData$chloro.tri3 <- ifelse(HerbData$chloro.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$chloro.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, chloro.yn, chloro.reason, chloro.tri1, chloro.tri2, chloro.tri3) %>% filter(chloro.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, chloro.yn, chloro.reason, chloro.tri1, chloro.tri2, chloro.tri3) %>% filter(chloro.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, chloro.1yn, chloro.yn) %>% filter(chloro.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, chloro.2yn, chloro.yn) %>% filter(chloro.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, chloro.3yn, chloro.yn) %>% filter(chloro.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, chloro.4yn, chloro.yn) %>% filter(chloro.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, chloro.5yn, chloro.yn) %>% filter(chloro.5yn ==1)

###################################################################################################
###################################################################################################
# ARNICA: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
 a
  mutate(arnica.2yn = str_detect(Herb_2Text, "arnica") | str_detect(Herb_2Text, "arnia")
         | str_detect(Herb_2Text, "arinca")| str_detect(Herb_2Text, "arnic")) %>% 
  mutate(arnica.3yn = str_detect(Herb_3Text, "arnica") | str_detect(Herb_3Text, "arnia")
         | str_detect(Herb_3Text, "arinca")| str_detect(Herb_3Text, "arnic"))%>% 
  mutate(arnica.4yn = str_detect(Herb_4Text, "arnica") | str_detect(Herb_4Text, "arnia")
         | str_detect(Herb_4Text, "arinca")| str_detect(Herb_4Text, "arnic")) %>% 
  mutate(arnica.5yn = str_detect(Blood_ActionsHerbs, "arnica") | str_detect(Blood_ActionsHerbs, "arnia")
         | str_detect(Blood_ActionsHerbs, "arinca")| str_detect(Blood_ActionsHerbs, "arnic"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(arnica.yn = 0) %>% mutate(arnica.reason = NA_character_) %>%
  mutate(arnica.tri1 = NA_integer_) %>% mutate(arnica.tri2 = NA_integer_) %>% mutate(arnica.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$arnica.yn <- ifelse(HerbData$arnica.1yn | HerbData$arnica.2yn | HerbData$arnica.3yn | HerbData$arnica.4yn | HerbData$arnica.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$arnica.reason <- ifelse(HerbData$arnica.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$arnica.reason)
HerbData$arnica.reason <- ifelse(HerbData$arnica.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$arnica.reason)
HerbData$arnica.reason <- ifelse(HerbData$arnica.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$arnica.reason)
HerbData$arnica.reason <- ifelse(HerbData$arnica.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$arnica.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$arnica.tri1 <- ifelse(HerbData$arnica.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$arnica.tri1)
HerbData$arnica.tri1 <- ifelse(HerbData$arnica.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$arnica.tri1)
HerbData$arnica.tri1 <- ifelse(HerbData$arnica.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$arnica.tri1)
HerbData$arnica.tri1 <- ifelse(HerbData$arnica.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$arnica.tri1)

HerbData$arnica.tri2 <- ifelse(HerbData$arnica.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$arnica.tri2)
HerbData$arnica.tri2 <- ifelse(HerbData$arnica.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$arnica.tri2)
HerbData$arnica.tri2 <- ifelse(HerbData$arnica.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$arnica.tri2)
HerbData$arnica.tri2 <- ifelse(HerbData$arnica.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$arnica.tri2)

HerbData$arnica.tri3 <- ifelse(HerbData$arnica.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$arnica.tri3)
HerbData$arnica.tri3 <- ifelse(HerbData$arnica.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$arnica.tri3)
HerbData$arnica.tri3 <- ifelse(HerbData$arnica.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$arnica.tri3)
HerbData$arnica.tri3 <- ifelse(HerbData$arnica.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$arnica.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, arnica.yn, arnica.reason, arnica.tri1, arnica.tri2, arnica.tri3) %>% filter(arnica.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, arnica.yn, arnica.reason, arnica.tri1, arnica.tri2, arnica.tri3) %>% filter(arnica.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, arnica.1yn, arnica.yn) %>% filter(arnica.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, arnica.2yn, arnica.yn) %>% filter(arnica.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, arnica.3yn, arnica.yn) %>% filter(arnica.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, arnica.4yn, arnica.yn) %>% filter(arnica.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, arnica.5yn, arnica.yn) %>% filter(arnica.5yn ==1)

###################################################################################################
# GARLIC: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(garlic.1yn = str_detect(Herb_1Text, "garl")) %>% 
  mutate(garlic.2yn = str_detect(Herb_2Text, "garl")) %>% 
  mutate(garlic.3yn = str_detect(Herb_3Text, "garl")) %>% 
  mutate(garlic.4yn = str_detect(Herb_4Text, "garl")) %>% 
  mutate(garlic.5yn = str_detect(Blood_ActionsHerbs, "garl"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(garlic.yn = 0) %>% mutate(garlic.reason = NA_character_) %>%
  mutate(garlic.tri1 = NA_integer_) %>% mutate(garlic.tri2 = NA_integer_) %>% mutate(garlic.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$garlic.yn <- ifelse(HerbData$garlic.1yn | HerbData$garlic.2yn | HerbData$garlic.3yn | HerbData$garlic.4yn | HerbData$garlic.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$garlic.reason <- ifelse(HerbData$garlic.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$garlic.reason)
HerbData$garlic.reason <- ifelse(HerbData$garlic.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$garlic.reason)
HerbData$garlic.reason <- ifelse(HerbData$garlic.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$garlic.reason)
HerbData$garlic.reason <- ifelse(HerbData$garlic.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$garlic.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$garlic.tri1 <- ifelse(HerbData$garlic.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$garlic.tri1)
HerbData$garlic.tri1 <- ifelse(HerbData$garlic.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$garlic.tri1)
HerbData$garlic.tri1 <- ifelse(HerbData$garlic.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$garlic.tri1)
HerbData$garlic.tri1 <- ifelse(HerbData$garlic.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$garlic.tri1)

HerbData$garlic.tri2 <- ifelse(HerbData$garlic.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$garlic.tri2)
HerbData$garlic.tri2 <- ifelse(HerbData$garlic.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$garlic.tri2)
HerbData$garlic.tri2 <- ifelse(HerbData$garlic.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$garlic.tri2)
HerbData$garlic.tri2 <- ifelse(HerbData$garlic.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$garlic.tri2)

HerbData$garlic.tri3 <- ifelse(HerbData$garlic.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$garlic.tri3)
HerbData$garlic.tri3 <- ifelse(HerbData$garlic.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$garlic.tri3)
HerbData$garlic.tri3 <- ifelse(HerbData$garlic.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$garlic.tri3)
HerbData$garlic.tri3 <- ifelse(HerbData$garlic.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$garlic.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, garlic.yn, garlic.reason, garlic.tri1, garlic.tri2, garlic.tri3) %>% filter(garlic.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, garlic.yn, garlic.reason, garlic.tri1, garlic.tri2, garlic.tri3) %>% filter(garlic.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, garlic.1yn, garlic.yn) %>% filter(garlic.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, garlic.2yn, garlic.yn) %>% filter(garlic.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, garlic.3yn, garlic.yn) %>% filter(garlic.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, garlic.4yn, garlic.yn) %>% filter(garlic.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, garlic.5yn, garlic.yn) %>% filter(garlic.5yn ==1)

###################################################################################################
# COHOSH: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(cohosh.1yn = str_detect(Herb_1Text, "coh") 
         | str_detect(Herb_1Text, "coh")) %>% 
  mutate(cohosh.2yn = str_detect(Herb_2Text, "coh") 
         | str_detect(Herb_2Text, "coh")) %>% 
  mutate(cohosh.3yn = str_detect(Herb_3Text, "coh") 
         | str_detect(Herb_3Text, "coh")) %>% 
  mutate(cohosh.4yn = str_detect(Herb_4Text, "coh") 
         | str_detect(Herb_4Text, "coh")) %>% 
  mutate(cohosh.5yn = str_detect(Blood_ActionsHerbs, "coh") 
         | str_detect(Blood_ActionsHerbs, "coh"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(cohosh.yn = 0) %>% mutate(cohosh.reason = NA_character_) %>%
  mutate(cohosh.tri1 = NA_integer_) %>% mutate(cohosh.tri2 = NA_integer_) %>% mutate(cohosh.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$cohosh.yn <- ifelse(HerbData$cohosh.1yn | HerbData$cohosh.2yn | HerbData$cohosh.3yn | HerbData$cohosh.4yn | HerbData$cohosh.5yn == TRUE, 1, 0)

# Replace flag variable with 1, if EPO was used for induction or augmentation
HerbData$primr.yn[HerbData$Encour_Induc_CohoshV02 == 1] <- 1
HerbData$primr.yn[HerbData$Encour_Aug_CohoshV02 == 1] <- 1

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$cohosh.reason <- ifelse(HerbData$cohosh.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$cohosh.reason)
HerbData$cohosh.reason <- ifelse(HerbData$cohosh.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$cohosh.reason)
HerbData$cohosh.reason <- ifelse(HerbData$cohosh.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$cohosh.reason)
HerbData$cohosh.reason <- ifelse(HerbData$cohosh.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$cohosh.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$cohosh.tri1 <- ifelse(HerbData$cohosh.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$cohosh.tri1)
HerbData$cohosh.tri1 <- ifelse(HerbData$cohosh.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$cohosh.tri1)
HerbData$cohosh.tri1 <- ifelse(HerbData$cohosh.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$cohosh.tri1)
HerbData$cohosh.tri1 <- ifelse(HerbData$cohosh.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$cohosh.tri1)

HerbData$cohosh.tri2 <- ifelse(HerbData$cohosh.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$cohosh.tri2)
HerbData$cohosh.tri2 <- ifelse(HerbData$cohosh.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$cohosh.tri2)
HerbData$cohosh.tri2 <- ifelse(HerbData$cohosh.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$cohosh.tri2)
HerbData$cohosh.tri2 <- ifelse(HerbData$cohosh.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$cohosh.tri2)

HerbData$cohosh.tri3 <- ifelse(HerbData$cohosh.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$cohosh.tri3)
HerbData$cohosh.tri3 <- ifelse(HerbData$cohosh.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$cohosh.tri3)
HerbData$cohosh.tri3 <- ifelse(HerbData$cohosh.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$cohosh.tri3)
HerbData$cohosh.tri3 <- ifelse(HerbData$cohosh.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$cohosh.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, cohosh.yn, cohosh.reason, cohosh.tri1, cohosh.tri2, cohosh.tri3) %>% filter(cohosh.yn == 0)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, cohosh.yn, cohosh.reason, cohosh.tri1, cohosh.tri2, cohosh.tri3) %>% filter(cohosh.yn == 1)

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cohosh.1yn, cohosh.yn) %>% filter(cohosh.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cohosh.2yn, cohosh.yn) %>% filter(cohosh.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cohosh.3yn, cohosh.yn) %>% filter(cohosh.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cohosh.4yn, cohosh.yn) %>% filter(cohosh.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cohosh.5yn, cohosh.yn) %>% filter(cohosh.5yn ==1)

###################################################################################################
###################################################################################################
# Shepherds Purse: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(shepp.1yn = str_detect(Herb_1Text, "shep") 
         | str_detect(Herb_1Text, "purs")) %>% 
  mutate(shepp.2yn = str_detect(Herb_2Text, "shep") 
         | str_detect(Herb_2Text, "purs")) %>% 
  mutate(shepp.3yn = str_detect(Herb_3Text, "shep") 
         | str_detect(Herb_3Text, "purs")) %>% 
  mutate(shepp.4yn = str_detect(Herb_4Text, "shep") 
         | str_detect(Herb_4Text, "purs")) %>% 
  mutate(shepp.5yn = str_detect(Blood_ActionsHerbs, "shep") 
         | str_detect(Blood_ActionsHerbs, "purs"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(shepp.yn = 0) %>% mutate(shepp.reason = NA_character_) %>%
  mutate(shepp.tri1 = NA_integer_) %>% mutate(shepp.tri2 = NA_integer_) %>% mutate(shepp.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$shepp.yn <- ifelse(HerbData$shepp.1yn | HerbData$shepp.2yn | HerbData$shepp.3yn | HerbData$shepp.4yn | HerbData$shepp.5yn == TRUE, 1, 0)

# Replace flag variable with 1, if EPO was used for induction or augmentation
HerbData$shepp.yn[HerbData$Blood_Avoid_ShepPurse == 1] <- 1

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$shepp.reason <- ifelse(HerbData$shepp.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$shepp.reason)
HerbData$shepp.reason <- ifelse(HerbData$shepp.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$shepp.reason)
HerbData$shepp.reason <- ifelse(HerbData$shepp.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$shepp.reason)
HerbData$shepp.reason <- ifelse(HerbData$shepp.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$shepp.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$shepp.tri1 <- ifelse(HerbData$shepp.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$shepp.tri1)
HerbData$shepp.tri1 <- ifelse(HerbData$shepp.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$shepp.tri1)
HerbData$shepp.tri1 <- ifelse(HerbData$shepp.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$shepp.tri1)
HerbData$shepp.tri1 <- ifelse(HerbData$shepp.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$shepp.tri1)

HerbData$shepp.tri2 <- ifelse(HerbData$shepp.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$shepp.tri2)
HerbData$shepp.tri2 <- ifelse(HerbData$shepp.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$shepp.tri2)
HerbData$shepp.tri2 <- ifelse(HerbData$shepp.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$shepp.tri2)
HerbData$shepp.tri2 <- ifelse(HerbData$shepp.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$shepp.tri2)

HerbData$shepp.tri3 <- ifelse(HerbData$shepp.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$shepp.tri3)
HerbData$shepp.tri3 <- ifelse(HerbData$shepp.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$shepp.tri3)
HerbData$shepp.tri3 <- ifelse(HerbData$shepp.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$shepp.tri3)
HerbData$shepp.tri3 <- ifelse(HerbData$shepp.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$shepp.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, shepp.1yn, shepp.yn) %>% filter(shepp.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, shepp.2yn, shepp.yn) %>% filter(shepp.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, shepp.3yn, shepp.yn) %>% filter(shepp.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, shepp.4yn, shepp.yn) %>% filter(shepp.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, shepp.5yn, shepp.yn) %>% filter(shepp.5yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, Blood_Avoid_ShepPurse, shepp.yn) %>% filter(Blood_Avoid_ShepPurse == 1)

###################################################################################################
###################################################################################################
# Angelica: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(angelica.1yn = str_detect(Herb_1Text, "angeli") 
         | str_detect(Herb_1Text, "ageli")) %>% 
  mutate(angelica.2yn = str_detect(Herb_2Text, "angeli") 
         | str_detect(Herb_2Text, "ageli")) %>% 
  mutate(angelica.3yn = str_detect(Herb_3Text, "angeli") 
         | str_detect(Herb_3Text, "ageli")) %>% 
  mutate(angelica.4yn = str_detect(Herb_4Text, "angeli") 
         | str_detect(Herb_4Text, "ageli")) %>% 
  mutate(angelica.5yn = str_detect(Blood_ActionsHerbs, "angel") 
         | str_detect(Blood_ActionsHerbs, "ageli"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(angelica.yn = 0) %>% mutate(angelica.reason = NA_character_) %>%
  mutate(angelica.tri1 = NA_integer_) %>% mutate(angelica.tri2 = NA_integer_) %>% mutate(angelica.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$angelica.yn <- ifelse(HerbData$angelica.1yn | HerbData$angelica.2yn | HerbData$angelica.3yn | HerbData$angelica.4yn | HerbData$angelica.5yn == TRUE, 1, 0)

# Replace flag variable with 1, if EPO was used for induction or augmentation
HerbData$angelica.yn[HerbData$Blood_Avoid_Angelica == 1] <- 1

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$angelica.reason <- ifelse(HerbData$angelica.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$angelica.reason)
HerbData$angelica.reason <- ifelse(HerbData$angelica.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$angelica.reason)
HerbData$angelica.reason <- ifelse(HerbData$angelica.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$angelica.reason)
HerbData$angelica.reason <- ifelse(HerbData$angelica.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$angelica.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$angelica.tri1 <- ifelse(HerbData$angelica.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$angelica.tri1)
HerbData$angelica.tri1 <- ifelse(HerbData$angelica.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$angelica.tri1)
HerbData$angelica.tri1 <- ifelse(HerbData$angelica.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$angelica.tri1)
HerbData$angelica.tri1 <- ifelse(HerbData$angelica.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$angelica.tri1)

HerbData$angelica.tri2 <- ifelse(HerbData$angelica.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$angelica.tri2)
HerbData$angelica.tri2 <- ifelse(HerbData$angelica.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$angelica.tri2)
HerbData$angelica.tri2 <- ifelse(HerbData$angelica.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$angelica.tri2)
HerbData$angelica.tri2 <- ifelse(HerbData$angelica.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$angelica.tri2)

HerbData$angelica.tri3 <- ifelse(HerbData$angelica.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$angelica.tri3)
HerbData$angelica.tri3 <- ifelse(HerbData$angelica.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$angelica.tri3)
HerbData$angelica.tri3 <- ifelse(HerbData$angelica.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$angelica.tri3)
HerbData$angelica.tri3 <- ifelse(HerbData$angelica.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$angelica.tri3)

# Check for consistency

HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, angelica.1yn, angelica.yn) %>% filter(angelica.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, angelica.2yn, angelica.yn) %>% filter(angelica.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, angelica.3yn, angelica.yn) %>% filter(angelica.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, angelica.4yn, angelica.yn) %>% filter(angelica.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, angelica.5yn, angelica.yn) %>% filter(angelica.5yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, Blood_Avoid_Angelica, angelica.yn) %>% filter(Blood_Avoid_Angelica == 1)
###################################################################################################
###################################################################################################
# Hemhalt: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(hemhalt.1yn = str_detect(Herb_1Text, "hem") 
         & str_detect(Herb_1Text, "halt")) %>% 
  mutate(hemhalt.2yn = str_detect(Herb_2Text, "hem") 
         & str_detect(Herb_2Text, "halt")) %>% 
  mutate(hemhalt.3yn = str_detect(Herb_3Text, "hem") 
         & str_detect(Herb_3Text, "halt")) %>% 
  mutate(hemhalt.4yn = str_detect(Herb_4Text, "hem") 
         & str_detect(Herb_4Text, "halt")) %>% 
  mutate(hemhalt.5yn = str_detect(Blood_ActionsHerbs, "hem") 
         & str_detect(Blood_ActionsHerbs, "halt"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(hemhalt.yn = 0) %>% mutate(hemhalt.reason = NA_character_) %>%
  mutate(hemhalt.tri1 = NA_integer_) %>% mutate(hemhalt.tri2 = NA_integer_) %>% mutate(hemhalt.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$hemhalt.yn <- ifelse(HerbData$hemhalt.1yn | HerbData$hemhalt.2yn | HerbData$hemhalt.3yn | HerbData$hemhalt.4yn | HerbData$hemhalt.5yn == TRUE, 1, 0)

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$hemhalt.reason <- ifelse(HerbData$hemhalt.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$hemhalt.reason)
HerbData$hemhalt.reason <- ifelse(HerbData$hemhalt.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$hemhalt.reason)
HerbData$hemhalt.reason <- ifelse(HerbData$hemhalt.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$hemhalt.reason)
HerbData$hemhalt.reason <- ifelse(HerbData$hemhalt.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$hemhalt.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$hemhalt.tri1 <- ifelse(HerbData$hemhalt.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$hemhalt.tri1)
HerbData$hemhalt.tri1 <- ifelse(HerbData$hemhalt.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$hemhalt.tri1)
HerbData$hemhalt.tri1 <- ifelse(HerbData$hemhalt.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$hemhalt.tri1)
HerbData$hemhalt.tri1 <- ifelse(HerbData$hemhalt.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$hemhalt.tri1)

HerbData$hemhalt.tri2 <- ifelse(HerbData$hemhalt.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$hemhalt.tri2)
HerbData$hemhalt.tri2 <- ifelse(HerbData$hemhalt.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$hemhalt.tri2)
HerbData$hemhalt.tri2 <- ifelse(HerbData$hemhalt.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$hemhalt.tri2)
HerbData$hemhalt.tri2 <- ifelse(HerbData$hemhalt.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$hemhalt.tri2)

HerbData$hemhalt.tri3 <- ifelse(HerbData$hemhalt.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$hemhalt.tri3)
HerbData$hemhalt.tri3 <- ifelse(HerbData$hemhalt.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$hemhalt.tri3)
HerbData$hemhalt.tri3 <- ifelse(HerbData$hemhalt.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$hemhalt.tri3)
HerbData$hemhalt.tri3 <- ifelse(HerbData$hemhalt.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$hemhalt.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, hemhalt.1yn, hemhalt.yn) %>% filter(hemhalt.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, hemhalt.2yn, hemhalt.yn) %>% filter(hemhalt.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, hemhalt.3yn, hemhalt.yn) %>% filter(hemhalt.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, hemhalt.4yn, hemhalt.yn) %>% filter(hemhalt.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, hemhalt.5yn, hemhalt.yn) %>% filter(hemhalt.5yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, hemhalt.5yn, hemhalt.yn) %>% filter(hemhalt.5yn ==0)

###################################################################################################
###################################################################################################
# Castor oil: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(castor.1yn = str_detect(Herb_1Text, "cast")) %>% 
  mutate(castor.2yn = str_detect(Herb_2Text, "cast")) %>% 
  mutate(castor.3yn = str_detect(Herb_3Text, "cast")) %>% 
  mutate(castor.4yn = str_detect(Herb_4Text, "cast")) %>% 
  mutate(castor.5yn = str_detect(Blood_ActionsHerbs, "cast"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(castor.yn = 0) %>% mutate(castor.reason = NA_character_) %>%
  mutate(castor.tri1 = NA_integer_) %>% mutate(castor.tri2 = NA_integer_) %>% mutate(castor.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$castor.yn <- ifelse(HerbData$castor.1yn | HerbData$castor.2yn | HerbData$castor.3yn | HerbData$castor.4yn | HerbData$castor.5yn == TRUE, 1, 0)

# Replace flag variable with 1, if EPO was used for induction or augmentation
HerbData$castor.yn[HerbData$Encour_Induc_CastorV02 == 1] <- 1
HerbData$castor.yn[HerbData$Encour_Aug_CastorV02 == 1] <- 1

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$castor.reason <- ifelse(HerbData$castor.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$castor.reason)
HerbData$castor.reason <- ifelse(HerbData$castor.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$castor.reason)
HerbData$castor.reason <- ifelse(HerbData$castor.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$castor.reason)
HerbData$castor.reason <- ifelse(HerbData$castor.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$castor.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$castor.tri1 <- ifelse(HerbData$castor.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$castor.tri1)
HerbData$castor.tri1 <- ifelse(HerbData$castor.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$castor.tri1)
HerbData$castor.tri1 <- ifelse(HerbData$castor.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$castor.tri1)
HerbData$castor.tri1 <- ifelse(HerbData$castor.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$castor.tri1)

HerbData$castor.tri2 <- ifelse(HerbData$castor.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$castor.tri2)
HerbData$castor.tri2 <- ifelse(HerbData$castor.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$castor.tri2)
HerbData$castor.tri2 <- ifelse(HerbData$castor.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$castor.tri2)
HerbData$castor.tri2 <- ifelse(HerbData$castor.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$castor.tri2)

HerbData$castor.tri3 <- ifelse(HerbData$castor.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$castor.tri3)
HerbData$castor.tri3 <- ifelse(HerbData$castor.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$castor.tri3)
HerbData$castor.tri3 <- ifelse(HerbData$castor.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$castor.tri3)
HerbData$castor.tri3 <- ifelse(HerbData$castor.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$castor.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, castor.1yn, castor.yn) %>% filter(castor.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, castor.2yn, castor.yn) %>% filter(castor.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, castor.3yn, castor.yn) %>% filter(castor.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, castor.4yn, castor.yn) %>% filter(castor.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, castor.5yn, castor.yn) %>% filter(castor.5yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, Encour_Induc_CastorV02, Encour_Aug_CastorV02, castor.yn) %>% filter(Encour_Induc_CastorV02 == 1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, Encour_Induc_CastorV02, Encour_Aug_CastorV02, castor.yn) %>% filter(Encour_Aug_CastorV02 == 1)
###################################################################################################
# Caulophyllum: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(cauloph.1yn = str_detect(Herb_1Text, "caul")) %>% 
  mutate(cauloph.2yn = str_detect(Herb_2Text, "caul")) %>% 
  mutate(cauloph.3yn = str_detect(Herb_3Text, "caul")) %>% 
  mutate(cauloph.4yn = str_detect(Herb_4Text, "caul")) %>% 
  mutate(cauloph.5yn = str_detect(Blood_ActionsHerbs, "caul"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(cauloph.yn = 0) %>% mutate(cauloph.reason = NA_character_) %>%
  mutate(cauloph.tri1 = NA_integer_) %>% mutate(cauloph.tri2 = NA_integer_) %>% mutate(cauloph.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$cauloph.yn <- ifelse(HerbData$cauloph.1yn | HerbData$cauloph.2yn | HerbData$cauloph.3yn | HerbData$cauloph.4yn | HerbData$cauloph.5yn == TRUE, 1, 0)

# Replace flag variable with 1, if EPO was used for induction or augmentation
HerbData$cauloph.yn[HerbData$Encour_Induc_CaulophV02 == 1] <- 1
HerbData$cauloph.yn[HerbData$Encour_Aug_CaulophV02 == 1] <- 1

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$cauloph.reason <- ifelse(HerbData$cauloph.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$cauloph.reason)
HerbData$cauloph.reason <- ifelse(HerbData$cauloph.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$cauloph.reason)
HerbData$cauloph.reason <- ifelse(HerbData$cauloph.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$cauloph.reason)
HerbData$cauloph.reason <- ifelse(HerbData$cauloph.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$cauloph.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$cauloph.tri1 <- ifelse(HerbData$cauloph.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$cauloph.tri1)
HerbData$cauloph.tri1 <- ifelse(HerbData$cauloph.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$cauloph.tri1)
HerbData$cauloph.tri1 <- ifelse(HerbData$cauloph.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$cauloph.tri1)
HerbData$cauloph.tri1 <- ifelse(HerbData$cauloph.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$cauloph.tri1)

HerbData$cauloph.tri2 <- ifelse(HerbData$cauloph.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$cauloph.tri2)
HerbData$cauloph.tri2 <- ifelse(HerbData$cauloph.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$cauloph.tri2)
HerbData$cauloph.tri2 <- ifelse(HerbData$cauloph.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$cauloph.tri2)
HerbData$cauloph.tri2 <- ifelse(HerbData$cauloph.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$cauloph.tri2)

HerbData$cauloph.tri3 <- ifelse(HerbData$cauloph.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$cauloph.tri3)
HerbData$cauloph.tri3 <- ifelse(HerbData$cauloph.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$cauloph.tri3)
HerbData$cauloph.tri3 <- ifelse(HerbData$cauloph.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$cauloph.tri3)
HerbData$cauloph.tri3 <- ifelse(HerbData$cauloph.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$cauloph.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cauloph.1yn, cauloph.yn) %>% filter(cauloph.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cauloph.2yn, cauloph.yn) %>% filter(cauloph.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cauloph.3yn, cauloph.yn) %>% filter(cauloph.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cauloph.4yn, cauloph.yn) %>% filter(cauloph.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, cauloph.5yn, cauloph.yn) %>% filter(cauloph.5yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, Encour_Induc_CaulophV02, Encour_Aug_CaulophV02, cauloph.yn) %>% filter(Encour_Induc_CaulophV02 == 1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, Encour_Induc_CaulophV02, Encour_Aug_CaulophV02, cauloph.yn) %>% filter(Encour_Aug_CaulophV02 == 1)

###################################################################################################
HerbData_Cleaned$Blood_Avoid_Motherwort
HerbData_Cleaned$Blood_ActionsHerbs

# Ensure "mother" and "wort" are spelled correctly
HerbData$Herb_1Text <- gsub("motherw", "motherwort", HerbData$Herb_1Text, fixed=TRUE)
HerbData$Herb_1Text <- gsub("motherwart", "motherwort", HerbData$Herb_1Text, fixed=TRUE)
HerbData$Herb_1Text <- gsub("motherword", "motherwort", HerbData$Herb_1Text, fixed=TRUE)
HerbData$Herb_1Text <- gsub("motherwrt", "motherwort", HerbData$Herb_1Text, fixed=TRUE)
HerbData$Herb_1Text <- gsub("motherwt", "motherwort", HerbData$Herb_1Text, fixed=TRUE)
HerbData$Herb_1Text <- gsub("wart", "wort", HerbData$Herb_1Text, fixed=TRUE)

HerbData$Herb_2Text <- gsub("motherw", "motherwort", HerbData$Herb_2Text, fixed=TRUE)
HerbData$Herb_2Text <- gsub("motherwart", "motherwort", HerbData$Herb_2Text, fixed=TRUE)
HerbData$Herb_2Text <- gsub("motherword", "motherwort", HerbData$Herb_2Text, fixed=TRUE)
HerbData$Herb_2Text <- gsub("motherwrt", "motherwort", HerbData$Herb_2Text, fixed=TRUE)
HerbData$Herb_2Text <- gsub("motherwt", "motherwort", HerbData$Herb_2Text, fixed=TRUE)
HerbData$Herb_2Text <- gsub("wart", "wort", HerbData$Herb_2Text, fixed=TRUE)

HerbData$Herb_3Text <- gsub("motherw", "motherwort", HerbData$Herb_3Text, fixed=TRUE)
HerbData$Herb_3Text <- gsub("motherwart", "motherwort", HerbData$Herb_3Text, fixed=TRUE)
HerbData$Herb_3Text <- gsub("motherword", "motherwort", HerbData$Herb_3Text, fixed=TRUE)
HerbData$Herb_3Text <- gsub("motherwrt", "motherwort", HerbData$Herb_3Text, fixed=TRUE)
HerbData$Herb_3Text <- gsub("motherwt", "motherwort", HerbData$Herb_3Text, fixed=TRUE)
HerbData$Herb_3Text <- gsub("wart", "wort", HerbData$Herb_3Text, fixed=TRUE)


HerbData$Herb_4Text <- gsub("motherw", "motherwort", HerbData$Herb_4Text, fixed=TRUE)
HerbData$Herb_4Text <- gsub("motherwart", "motherwort", HerbData$Herb_4Text, fixed=TRUE)
HerbData$Herb_4Text <- gsub("motherword", "motherwort", HerbData$Herb_4Text, fixed=TRUE)
HerbData$Herb_4Text <- gsub("motherwrt", "motherwort", HerbData$Herb_4Text, fixed=TRUE)
HerbData$Herb_4Text <- gsub("motherwt", "motherwort", HerbData$Herb_4Text, fixed=TRUE)
HerbData$Herb_4Text <- gsub("wart", "wort", HerbData$Herb_4Text, fixed=TRUE)

HerbData$Blood_ActionsHerbs <- gsub("motherw", "motherwort", HerbData$Blood_ActionsHerbs, fixed=TRUE)
HerbData$Blood_ActionsHerbs <- gsub("motherwart", "motherwort", HerbData$Blood_ActionsHerbs, fixed=TRUE)
HerbData$Blood_ActionsHerbs <- gsub("motherword", "motherwort", HerbData$Blood_ActionsHerbs, fixed=TRUE)
HerbData$Blood_ActionsHerbs <- gsub("motherwrt", "motherwort", HerbData$Blood_ActionsHerbs, fixed=TRUE)
HerbData$Blood_ActionsHerbs <- gsub("motherwt", "motherwort", HerbData$Blood_ActionsHerbs, fixed=TRUE)
HerbData$Blood_ActionsHerbs <- gsub("wart", "wort", HerbData$Blood_ActionsHerbs, fixed=TRUE)

# Motherwort: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
HerbData <- HerbData %>% 
  mutate(motherwort.1yn = str_detect(Herb_1Text, "mot") 
         & str_detect(Herb_1Text, "wor")) %>% 
  mutate(motherwort.2yn = str_detect(Herb_2Text, "mot") 
         & str_detect(Herb_2Text, "wor")) %>% 
  mutate(motherwort.3yn = str_detect(Herb_3Text, "mot") 
         & str_detect(Herb_3Text, "wor")) %>% 
  mutate(motherwort.4yn = str_detect(Herb_4Text, "mot") 
         & str_detect(Herb_4Text, "wor")) %>% 
  mutate(motherwort.5yn = str_detect(Blood_ActionsHerbs, "mot") 
         & str_detect(Blood_ActionsHerbs, "wor"))

# Create one flag variable for this exposure (instead of 4), 
# one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
HerbData <- HerbData %>% mutate(motherwort.yn = 0) %>% mutate(motherwort.reason = NA_character_) %>%
  mutate(motherwort.tri1 = NA_integer_) %>% mutate(motherwort.tri2 = NA_integer_) %>% mutate(motherwort.tri3 = NA_integer_)

# Replace flag variable with 1, if pattern is in Herb_#Text string. 
HerbData$motherwort.yn <- ifelse(HerbData$motherwort.1yn | HerbData$motherwort.2yn | HerbData$motherwort.3yn | HerbData$motherwort.4yn | HerbData$motherwort.5yn == TRUE, 1, 0)

# Replace flag variable with 1, if EPO was used for induction or augmentation
HerbData$motherwort.yn[HerbData$Blood_Avoid_Motherwort == 1] <- 1

# Replace reason variable with Herb_#Reason if flag variable is true. 
HerbData$motherwort.reason <- ifelse(HerbData$motherwort.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$motherwort.reason)
HerbData$motherwort.reason <- ifelse(HerbData$motherwort.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$motherwort.reason)
HerbData$motherwort.reason <- ifelse(HerbData$motherwort.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$motherwort.reason)
HerbData$motherwort.reason <- ifelse(HerbData$motherwort.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$motherwort.reason)

# Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
HerbData$motherwort.tri1 <- ifelse(HerbData$motherwort.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$motherwort.tri1)
HerbData$motherwort.tri1 <- ifelse(HerbData$motherwort.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$motherwort.tri1)
HerbData$motherwort.tri1 <- ifelse(HerbData$motherwort.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$motherwort.tri1)
HerbData$motherwort.tri1 <- ifelse(HerbData$motherwort.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$motherwort.tri1)

HerbData$motherwort.tri2 <- ifelse(HerbData$motherwort.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$motherwort.tri2)
HerbData$motherwort.tri2 <- ifelse(HerbData$motherwort.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$motherwort.tri2)
HerbData$motherwort.tri2 <- ifelse(HerbData$motherwort.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$motherwort.tri2)
HerbData$motherwort.tri2 <- ifelse(HerbData$motherwort.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$motherwort.tri2)

HerbData$motherwort.tri3 <- ifelse(HerbData$motherwort.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$motherwort.tri3)
HerbData$motherwort.tri3 <- ifelse(HerbData$motherwort.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$motherwort.tri3)
HerbData$motherwort.tri3 <- ifelse(HerbData$motherwort.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$motherwort.tri3)
HerbData$motherwort.tri3 <- ifelse(HerbData$motherwort.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$motherwort.tri3)

# Check for consistency
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, motherwort.1yn, motherwort.yn) %>% filter(motherwort.1yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, motherwort.2yn, motherwort.yn) %>% filter(motherwort.2yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, motherwort.3yn, motherwort.yn) %>% filter(motherwort.3yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, motherwort.4yn, motherwort.yn) %>% filter(motherwort.4yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, motherwort.5yn, motherwort.yn) %>% filter(motherwort.5yn ==1)
HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, Blood_Avoid_Motherwort, motherwort.yn) %>% filter(Blood_Avoid_Motherwort == 1)
# ###################################################################################################
# # TEMPLATE: Generating a yes/no variable from a pattern in a string for each Herb_#Text variable. 
# HerbData <- HerbData %>% 
#   mutate(XXX.1yn = str_detect(Herb_1Text, "YYY") 
#          | str_detect(Herb_1Text, "YYY")) %>% 
#   mutate(XXX.2yn = str_detect(Herb_2Text, "YYY") 
#          | str_detect(Herb_2Text, "YYY")) %>% 
#   mutate(XXX.3yn = str_detect(Herb_3Text, "YYY") 
#          | str_detect(Herb_3Text, "YYY")) %>% 
#   mutate(XXX.4yn = str_detect(Herb_4Text, "YYY") 
#          | str_detect(Herb_4Text, "YYY")) %>% 
#   mutate(XXX.5yn = str_detect(Blood_ActionsHerbs, "YYY") 
#          | str_detect(Blood_ActionsHerbs, "YYY"))
# 
# # Create one flag variable for this exposure (instead of 4), 
# # one reason variable (instead of 4), and 3 trimester dose variables (instead of 12) where the value is null. 
# HerbData <- HerbData %>% mutate(XXX.yn = 0) %>% mutate(XXX.reason = NA_character_) %>%
#   mutate(XXX.tri1 = NA_integer_) %>% mutate(XXX.tri2 = NA_integer_) %>% mutate(XXX.tri3 = NA_integer_)
# 
# # Replace flag variable with 1, if pattern is in Herb_#Text string. 
# HerbData$XXX.yn <- ifelse(HerbData$XXX.1yn | HerbData$XXX.2yn | HerbData$XXX.3yn | HerbData$XXX.4yn | HerbData$XXX.5yn == TRUE, 1, 0)
# 
# # Replace flag variable with 1, if EPO was used for induction or augmentation
# HerbData$XXX.yn[HerbData$UUU == 1] <- 1
# HerbData$XXX.yn[HerbData$UUU == 1] <- 1
# 
# # Replace reason variable with Herb_#Reason if flag variable is true. 
# HerbData$XXX.reason <- ifelse(HerbData$XXX.1yn == TRUE, HerbData$Herb_1Reason,  HerbData$XXX.reason)
# HerbData$XXX.reason <- ifelse(HerbData$XXX.2yn == TRUE, HerbData$Herb_2Reason,  HerbData$XXX.reason)
# HerbData$XXX.reason <- ifelse(HerbData$XXX.3yn == TRUE, HerbData$Herb_3Reason,  HerbData$XXX.reason)
# HerbData$XXX.reason <- ifelse(HerbData$XXX.4yn == TRUE, HerbData$Herb_4Reason,  HerbData$XXX.reason)
# 
# # Replace trimester variables with Herb_#Tri# variables if flag variable is true. 
# HerbData$XXX.tri1 <- ifelse(HerbData$XXX.1yn == TRUE, HerbData$herb_1tri1V02,  HerbData$XXX.tri1)
# HerbData$XXX.tri1 <- ifelse(HerbData$XXX.2yn == TRUE, HerbData$Herb_2Tri1,  HerbData$XXX.tri1)
# HerbData$XXX.tri1 <- ifelse(HerbData$XXX.3yn == TRUE, HerbData$Herb_3Tri1,  HerbData$XXX.tri1)
# HerbData$XXX.tri1 <- ifelse(HerbData$XXX.4yn == TRUE, HerbData$Herb_4Tri1,  HerbData$XXX.tri1)
# 
# HerbData$XXX.tri2 <- ifelse(HerbData$XXX.1yn == TRUE, HerbData$herb_1tri2V02,  HerbData$XXX.tri2)
# HerbData$XXX.tri2 <- ifelse(HerbData$XXX.2yn == TRUE, HerbData$Herb_2Tri2,  HerbData$XXX.tri2)
# HerbData$XXX.tri2 <- ifelse(HerbData$XXX.3yn == TRUE, HerbData$Herb_3Tri2,  HerbData$XXX.tri2)
# HerbData$XXX.tri2 <- ifelse(HerbData$XXX.4yn == TRUE, HerbData$Herb_4Tri2,  HerbData$XXX.tri2)
# 
# HerbData$XXX.tri3 <- ifelse(HerbData$XXX.1yn == TRUE, HerbData$herb_1tri3V02,  HerbData$XXX.tri3)
# HerbData$XXX.tri3 <- ifelse(HerbData$XXX.2yn == TRUE, HerbData$Herb_2Tri3,  HerbData$XXX.tri3)
# HerbData$XXX.tri3 <- ifelse(HerbData$XXX.3yn == TRUE, HerbData$Herb_3Tri3,  HerbData$XXX.tri3)
# HerbData$XXX.tri3 <- ifelse(HerbData$XXX.4yn == TRUE, HerbData$Herb_4Tri3,  HerbData$XXX.tri3)
# 
# # Check for consistency
# HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, XXX.1yn, XXX.yn) %>% filter(XXX.1yn ==1)
# HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, XXX.2yn, XXX.yn) %>% filter(XXX.2yn ==1)
# HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, XXX.3yn, XXX.yn) %>% filter(XXX.3yn ==1)
# HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, XXX.4yn, XXX.yn) %>% filter(XXX.4yn ==1)
# HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, XXX.5yn, XXX.yn) %>% filter(XXX.5yn ==1)
# HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, UUU, UUU, XXX.yn) %>% filter(UUU == 1)
# HerbData %>% select(Herb_1Text, Herb_2Text, Herb_3Text, Herb_4Text, Blood_ActionsHerbs, UUU, UUU, XXX.yn) %>% filter(UUU == 1)
# ###################################################################################################

# OUTCOME VARIABLES
# no need for manipulation

# Infant outcomes
HerbData$NICU.yn
HerbData$dead
HerbData$LBW
HerbData$macro
HerbData$InfHosp_FlgR
HerbData$neo.trans.yn

# Maternal outcomes
HerbData$postdates.yn
HerbData$ActPrg_RxAntihypertensives
HerbData$Blood_Actions_BloodTrans
HerbData$Blood_Actions_DandC
HerbData$MomHosp_FlgR
HerbData$mompost.RT.infect.yn
HerbData$Csec_FlgR

###################
# Combine intrapartum and postpartum transport for birthing individual 
HerbData$ipm.trans.yn
HerbData$ppm.trans.yn

HerbData <- HerbData %>% mutate(mom_transport = 0)

HerbData$mom_transport[HerbData$ipm.trans.yn == 1] <- 1
HerbData$mom_transport[HerbData$ppm.trans.yn == 1] <- 1

# Combine intrapartum and postpartum transport for birthing individual 
HerbData$ipm.trans.yn
HerbData$ppm.trans.yn
HerbData$neo.trans.yn

HerbData <- HerbData %>% mutate(transport = 0)

HerbData$transport[HerbData$ipm.trans.yn == 1] <- 1
HerbData$transport[HerbData$ppm.trans.yn == 1] <- 1
HerbData$transport[HerbData$neo.trans.yn == 1] <- 1

###################
# Create a dichotomouse variable for blood loss, threshold == 1000. 
HerbData_Cleaned$Blood_Loss_ml

HerbData <- HerbData %>% mutate(blood_loss_cat = NA_integer_)
HerbData$blood_loss_cat[HerbData$Blood_Loss_ml < 1000 ] <- 0
HerbData$blood_loss_cat[HerbData$Blood_Loss_ml >= 1000] <- 1

###################
# Create a flag for sepsis or infection in the infant
# Create a flag for induction or augmentation with prostaglandins or pitocin
# Create a flag for pitocin or methergine use postpartum
HerbData <- HerbData %>% mutate(infant_infect.yn = NA_integer_) %>% mutate(pharm_induc_aug.yn = NA_integer_) %>%
  mutate(pharm_blood_avoid_action.yn = NA_integer_) 

# Fill flag for sepsis or infection in the infant
table(HerbData$infant_infect.yn)
HerbData$infant_infect.yn[HerbData$InfHealth_Probs_SepInf == 0] <- 0
table(HerbData$infant_infect.yn)
HerbData$infant_infect.yn[HerbData$InfHosp_Probs_SepInf == 0] <- 0
table(HerbData$infant_infect.yn)
HerbData$infant_infect.yn[HerbData$InfHealth_Probs_SepInf == 1] <- 1
table(HerbData$infant_infect.yn)
HerbData$infant_infect.yn[HerbData$InfHosp_Probs_SepInf == 1] <- 1
table(HerbData$infant_infect.yn)

table(HerbData$InfHealth_Probs_SepInf)
table(HerbData$InfHosp_Probs_SepInf)
table(HerbData$InfHealth_Probs_SepInf, HerbData$InfHosp_Probs_SepInf)
table(HerbData$infant_infect.yn)

# Fill flag for induction or augmentation with prostaglandins or pitocin
table(HerbData$pharm_induc_aug.yn)
HerbData$pharm_induc_aug.yn[HerbData$Encour_Induc_ProstagV02 == 0] <- 0
table(HerbData$pharm_induc_aug.yn)
HerbData$pharm_induc_aug.yn[HerbData$Encour_Induc_OxytocinV02 == 0] <- 0
table(HerbData$pharm_induc_aug.yn)
HerbData$pharm_induc_aug.yn[HerbData$Encour_Aug_ProstagV02 == 0] <- 0
table(HerbData$pharm_induc_aug.yn)
HerbData$pharm_induc_aug.yn[HerbData$Encour_Aug_OxytocinV02 == 0] <- 0
table(HerbData$pharm_induc_aug.yn)
HerbData$pharm_induc_aug.yn[HerbData$Encour_Induc_ProstagV02 == 1] <- 1
table(HerbData$pharm_induc_aug.yn)
HerbData$pharm_induc_aug.yn[HerbData$Encour_Induc_OxytocinV02 == 1] <- 1
table(HerbData$pharm_induc_aug.yn)
HerbData$pharm_induc_aug.yn[HerbData$Encour_Aug_ProstagV02 == 1] <- 1
table(HerbData$pharm_induc_aug.yn)
HerbData$pharm_induc_aug.yn[HerbData$Encour_Aug_OxytocinV02 == 1] <- 1
table(HerbData$pharm_induc_aug.yn)


table(HerbData$pharm_induc_aug.yn)
table(HerbData$Encour_Induc_ProstagV02)
table(HerbData$Encour_Induc_OxytocinV02)
table(HerbData$Encour_Aug_ProstagV02)
table(HerbData$Encour_Aug_OxytocinV02)
table(HerbData$Encour_Induc_ProstagV02, HerbData$Encour_Induc_OxytocinV02)
table(HerbData$Encour_Aug_ProstagV02, HerbData$Encour_Aug_OxytocinV02)

# Fill flag for pitocin or methergine use postpartum
table(HerbData$pharm_blood_avoid_action.yn)
HerbData$pharm_blood_avoid_action.yn[HerbData$Blood_Avoid_Oxytocin == 0] <- 0 
table(HerbData$pharm_blood_avoid_action.yn)
HerbData$pharm_blood_avoid_action.yn[HerbData$Blood_Avoid_Methergine == 0] <- 0 
table(HerbData$pharm_blood_avoid_action.yn)
HerbData$pharm_blood_avoid_action.yn[HerbData$Blood_Actions_Pitocin == 0] <- 0 
table(HerbData$pharm_blood_avoid_action.yn)
HerbData$pharm_blood_avoid_action.yn[HerbData$Blood_Actions_Pitocin == 0] <- 0 
table(HerbData$pharm_blood_avoid_action.yn)
HerbData$pharm_blood_avoid_action.yn[HerbData$Blood_Avoid_Oxytocin == 1] <- 1 
table(HerbData$pharm_blood_avoid_action.yn)
HerbData$pharm_blood_avoid_action.yn[HerbData$Blood_Avoid_Methergine == 1] <- 1 
table(HerbData$pharm_blood_avoid_action.yn)
HerbData$pharm_blood_avoid_action.yn[HerbData$Blood_Actions_Methergine == 1] <- 1 
table(HerbData$pharm_blood_avoid_action.yn)
HerbData$pharm_blood_avoid_action.yn[HerbData$Blood_Actions_Methergine == 1] <- 1 
table(HerbData$pharm_blood_avoid_action.yn)

table(HerbData$pharm_blood_avoid_action.yn)
table(HerbData$Blood_Avoid_Oxytocin)
table(HerbData$Blood_Avoid_Methergine)
table(HerbData$Blood_Actions_Pitocin)
table(HerbData$Blood_Actions_Methergine)
table(HerbData$Blood_Avoid_Oxytocin, HerbData$Blood_Avoid_Methergine)
table(HerbData$Blood_Actions_Pitocin, HerbData$Blood_Actions_Methergine)
#################
# Create a variable for perineal trauma
HerbData <- HerbData %>% mutate(peri_trauma.yn = NA_integer_)
table(HerbData$peri_trauma.yn)
HerbData$peri_trauma.yn[HerbData$lac.sev.global == 0] <- 0 
table(HerbData$peri_trauma.yn)
HerbData$peri_trauma.yn[HerbData$lac.sev.global == 1] <- 1
table(HerbData$peri_trauma.yn)
HerbData$peri_trauma.yn[HerbData$lac.sev.global == 2] <- 1
table(HerbData$peri_trauma.yn)
HerbData$peri_trauma.yn[HerbData$lac.sev.global == 3] <- 1
table(HerbData$peri_trauma.yn)
HerbData$peri_trauma.yn[HerbData$lac.sev.global == 4] <- 1
table(HerbData$peri_trauma.yn)
HerbData$peri_trauma.yn[HerbData$lac.sev.global == 5] <- 1
table(HerbData$peri_trauma.yn)
HerbData$peri_trauma.yn[HerbData$lac.sev.global == 6] <- 1
table(HerbData$peri_trauma.yn)
HerbData$peri_trauma.yn[HerbData$lac.sev.global == 7] <- 1
table(HerbData$peri_trauma.yn)
HerbData$peri_trauma.yn[HerbData$lac.sev.global == 99] <- 1
table(HerbData$peri_trauma.yn)
###################################################################################################
# drop extra columns
# filter out twin pregnancies
HerbData_Cleaned <- HerbData %>% select(
  -rasp.1yn,-rasp.2yn,-rasp.3yn,-rasp.4yn, -rasp.5yn,
  -alfa.1yn, -alfa.2yn, -alfa.3yn, -alfa.4yn, -alfa.5yn,
  -primr.1yn,-primr.2yn, -primr.3yn, -primr.4yn, -primr.5yn,
  -gbform.1yn, -gbform.2yn, -gbform.3yn, -gbform.4yn, -gbform.5yn,
  -nettle.1yn,-nettle.2yn,-nettle.3yn,-nettle.4yn, -nettle.5yn,
  -dande.1yn, -dande.2yn, -dande.3yn, -dande.4yn, -dande.5yn,
  -mblend.1yn, -mblend.2yn, -mblend.3yn, -mblend.4yn, -mblend.5yn,
  -flordx.1yn, -flordx.2yn, -flordx.3yn, -flordx.4yn, -flordx.5yn,
  -echina.1yn, -echina.2yn,-echina.3yn,-echina.4yn, - echina.5yn,
  -pulsat.1yn,-pulsat.2yn,-pulsat.3yn,-pulsat.4yn, -pulsat.5yn,
  -chloro.1yn, -chloro.2yn, -chloro.3yn, -chloro.4yn, -chloro.5yn,
  -arnica.1yn, -arnica.2yn, -arnica.3yn, -arnica.4yn, -arnica.5yn,
  -garlic.1yn, -garlic.2yn, -garlic.3yn, -garlic.4yn,-garlic.5yn,
  -cohosh.1yn, -cohosh.2yn, -cohosh.3yn, -cohosh.4yn, -cohosh.5yn,
  -shepp.1yn, -shepp.2yn, -shepp.3yn, -shepp.4yn, -shepp.5yn,
  -angelica.1yn, -angelica.2yn, -angelica.3yn, -angelica.4yn, -angelica.5yn,
  -hemhalt.1yn, -hemhalt.2yn, -hemhalt.3yn, -hemhalt.4yn, -hemhalt.5yn,
  -castor.1yn, -castor.2yn, -castor.3yn, -castor.4yn, -castor.5yn,
  -cauloph.1yn, -cauloph.2yn, -cauloph.3yn, -cauloph.4yn, -cauloph.5yn,
  -motherwort.1yn, -motherwort.2yn, -motherwort.3yn, -motherwort.4yn, -motherwort.5yn,
  -HerbPresent.1yn,-HerbPresent.2yn,-HerbPresent.3yn,-HerbPresent.4yn) %>% 
  filter(singleton.yn == 1) 

HerbData_Cleaned_NoCsec <- HerbData %>% select(
  -rasp.1yn,-rasp.2yn,-rasp.3yn,-rasp.4yn, -rasp.5yn,
  -alfa.1yn, -alfa.2yn, -alfa.3yn, -alfa.4yn, -alfa.5yn,
  -primr.1yn,-primr.2yn, -primr.3yn, -primr.4yn, -primr.5yn,
  -gbform.1yn, -gbform.2yn, -gbform.3yn, -gbform.4yn, -gbform.5yn,
  -nettle.1yn,-nettle.2yn,-nettle.3yn,-nettle.4yn, -nettle.5yn,
  -dande.1yn, -dande.2yn, -dande.3yn, -dande.4yn, -dande.5yn,
  -mblend.1yn, -mblend.2yn, -mblend.3yn, -mblend.4yn, -mblend.5yn,
  -flordx.1yn, -flordx.2yn, -flordx.3yn, -flordx.4yn, -flordx.5yn,
  -echina.1yn, -echina.2yn,-echina.3yn,-echina.4yn, - echina.5yn,
  -pulsat.1yn,-pulsat.2yn,-pulsat.3yn,-pulsat.4yn, -pulsat.5yn,
  -chloro.1yn, -chloro.2yn, -chloro.3yn, -chloro.4yn, -chloro.5yn,
  -arnica.1yn, -arnica.2yn, -arnica.3yn, -arnica.4yn, -arnica.5yn,
  -garlic.1yn, -garlic.2yn, -garlic.3yn, -garlic.4yn,-garlic.5yn,
  -cohosh.1yn, -cohosh.2yn, -cohosh.3yn, -cohosh.4yn, -cohosh.5yn,
  -shepp.1yn, -shepp.2yn, -shepp.3yn, -shepp.4yn, -shepp.5yn,
  -angelica.1yn, -angelica.2yn, -angelica.3yn, -angelica.4yn, -angelica.5yn,
  -hemhalt.1yn, -hemhalt.2yn, -hemhalt.3yn, -hemhalt.4yn, -hemhalt.5yn,
  -castor.1yn, -castor.2yn, -castor.3yn, -castor.4yn, -castor.5yn,
  -cauloph.1yn, -cauloph.2yn, -cauloph.3yn, -cauloph.4yn, -cauloph.5yn,
  -motherwort.1yn, -motherwort.2yn, -motherwort.3yn, -motherwort.4yn, -motherwort.5yn,
  -HerbPresent.1yn,-HerbPresent.2yn,-HerbPresent.3yn,-HerbPresent.4yn) %>% 
  filter(singleton.yn == 1) %>% filter(Csec_FlgR == 0)

HerbData_Cleaned_NoCsec$Csec_FlgR

HerbData_Cleaned
write.csv(HerbData_Cleaned, "HerbData_Cleaned_paige_V02.csv")
view(HerbData_Cleaned)

HerbData_Cleaned$birthplaceR2
