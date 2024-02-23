# Paige Snow
# OSU Field Experience 2019
# Descriptive Analysis Herb Project
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
install.packages("pastecs")
install.packages("janitor")
install.packages("Stack")
install.packages("sjlabelled")
install.packages("labelled")
###################################################################################################
library(tidyverse)
library(stringr)
#library(tm)
library(tidytext)
library(widyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(pastecs)
library(janitor)
library(Stack)
library(sjlabelled)
library(labelled)
# Go through
# HerbData_Text_Mining_v2 first
# HerbData_Cleaning_Management_v2 second

# Import cleaned data:
HerbData_Cleaned <- read.csv("~/Documents/MPH School/Field Experience/herb files/HerbData/HerbData_Cleaned.csv", stringsAsFactors=FALSE)

View(HerbData_Cleaned)

####################################################################################################################################################
####################################################################################################################################################
# MUST COPY AND PASTE -> ENTER CHISQ, DF, PVALUES MANUALLY
# MUST CHECK WHETHER TO USE PERCENT or VALID_PERCENT IN TABLE AND CHANGE CODE ACCORDINGLY
# PERINEAL TRAUMA AND BLOOD LOSS -> VAGINAL BIRTHS ONLY -> FILTER OR USE HerbData_Cleaned_NoCsec
###########################################################################################################
options(scipen=1000)
#options(digits=2)
# Herbs.yn <- data_frame(
# Raspberry = HerbData$rasp.yn,
# Alfalfa = HerbData$alfa.yn,
# EvenPrim = HerbData$primr.yn,
# GBFormula = HerbData$gbform.yn,
# Nettles = HerbData$nettle.yn,
# Dandelion = HerbData$dande.yn,
# MothersBlend = HerbData$mblend.yn,
# Floradix = HerbData$flordx.yn,
# Echinacea = HerbData$echina.yn,
# Pulsatilla = HerbData$pulsat.yn,
# Chlorophyll = HerbData$chloro.yn,
# Arnica = HerbData$arnica.yn,
# Garlic = HerbData$garlic.yn,
# Cohosh = HerbData$cohosh.yn
# )

#Herbs.stat.desc(Herbs.yn, basic=F)
###########################################################################################################
# Get count and frequency for each herb.
freq00 <- tabyl(HerbData_Cleaned$HerbPresent.yn) %>% mutate(Herb = "Any Herbal Product")
freq01 <- tabyl(HerbData_Cleaned$rasp.yn, sort = TRUE) %>% mutate(Herb = "Raspberry")
freq02 <- tabyl(HerbData_Cleaned$alfa.yn, sort = TRUE) %>% mutate(Herb = "Alfalfa")
freq03 <- tabyl(HerbData_Cleaned$primr.yn, sort = TRUE) %>% mutate(Herb = "EvenPrim")
freq04 <- tabyl(HerbData_Cleaned$gbform.yn, sort = TRUE) %>% mutate(Herb = "GBFormula")
freq05 <- tabyl(HerbData_Cleaned$nettle.yn, sort = TRUE) %>% mutate(Herb = "Nettles")
freq06 <- tabyl(HerbData_Cleaned$dande.yn, sort = TRUE) %>% mutate(Herb = "Dandelion")
freq07 <- tabyl(HerbData_Cleaned$mblend.yn, sort = TRUE) %>% mutate(Herb = "MothersBlend")
freq08 <- tabyl(HerbData_Cleaned$flordx.yn, sort = TRUE) %>% mutate(Herb = "Floradix")
freq09 <- tabyl(HerbData_Cleaned$echina.yn, sort = TRUE) %>% mutate(Herb = "Echinacea")
freq10 <- tabyl(HerbData_Cleaned$pulsat.yn, sort = TRUE) %>% mutate(Herb = "Pulsatilla")
freq11 <- tabyl(HerbData_Cleaned$chloro.yn, sort = TRUE) %>% mutate(Herb = "Chlorophyll")
freq12 <- tabyl(HerbData_Cleaned$arnica.yn, sort = TRUE) %>% mutate(Herb = "Arnica")
freq13 <- tabyl(HerbData_Cleaned$garlic.yn, sort = TRUE) %>% mutate(Herb = "Garlic")
freq14 <- tabyl(HerbData_Cleaned$cohosh.yn, sort = TRUE) %>% mutate(Herb = "Cohosh")
freq15 <- tabyl(HerbData_Cleaned$shepp.yn, sort = TRUE) %>% mutate(Herb = "ShepherdsPurse")
freq16 <- tabyl(HerbData_Cleaned$angelica.yn, sort = TRUE) %>% mutate(Herb = "Angelica")
freq17 <- tabyl(HerbData_Cleaned$hemhalt.yn, sort = TRUE) %>% mutate(Herb = "Hemhalt")
freq18 <- tabyl(HerbData_Cleaned$castor.yn, sort = TRUE) %>% mutate(Herb = "CastorOil")
freq19 <- tabyl(HerbData_Cleaned$cauloph.yn, sort = TRUE) %>% mutate(Herb = "Caulophyllum")
freq20 <- tabyl(HerbData_Cleaned$motherwort.yn, sort = TRUE) %>% mutate(Herb = "Motherwort")

#Standardize the column name across tables.
names(freq00)[names(freq00) == "HerbData_Cleaned$HerbPresent.yn"] <- "YN"
names(freq01)[names(freq01) == "HerbData_Cleaned$rasp.yn"] <- "YN"
names(freq02)[names(freq02) == "HerbData_Cleaned$alfa.yn"] <- "YN"
names(freq03)[names(freq03) == "HerbData_Cleaned$primr.yn"] <- "YN"
names(freq04)[names(freq04) == "HerbData_Cleaned$gbform.yn"] <- "YN"
names(freq05)[names(freq05) == "HerbData_Cleaned$nettle.yn"] <- "YN"
names(freq06)[names(freq06) == "HerbData_Cleaned$dande.yn"] <- "YN"
names(freq07)[names(freq07) == "HerbData_Cleaned$mblend.yn"] <- "YN"
names(freq08)[names(freq08) == "HerbData_Cleaned$flordx.yn"] <- "YN"
names(freq09)[names(freq09) == "HerbData_Cleaned$echina.yn"] <- "YN"
names(freq10)[names(freq10) == "HerbData_Cleaned$pulsat.yn"] <- "YN"
names(freq11)[names(freq11) == "HerbData_Cleaned$chloro.yn"] <- "YN"
names(freq12)[names(freq12) == "HerbData_Cleaned$arnica.yn"] <- "YN"
names(freq13)[names(freq13) == "HerbData_Cleaned$garlic.yn"] <- "YN"
names(freq14)[names(freq14) == "HerbData_Cleaned$cohosh.yn"] <- "YN"
names(freq15)[names(freq15) == "HerbData_Cleaned$shepp.yn"] <- "YN"
names(freq16)[names(freq16) == "HerbData_Cleaned$angelica.yn"] <- "YN"
names(freq17)[names(freq17) == "HerbData_Cleaned$hemhalt.yn"] <- "YN"
names(freq18)[names(freq18) == "HerbData_Cleaned$castor.yn"] <- "YN"
names(freq19)[names(freq19) == "HerbData_Cleaned$cauloph.yn"] <- "YN"
names(freq20)[names(freq20) == "HerbData_Cleaned$motherwort.yn"] <- "YN"

# Stack Tables. 
Herbs.Freqs <-rbind(freq00, freq01, freq02, freq03, freq04, freq05, freq06, 
                    freq07, freq08, freq09, freq10, freq11, freq12, freq13, freq14,
                    freq15, freq16, freq17, freq18, freq19, freq20)

# Reorder the columns and filter out all the "no" rows.
Herbs.Freqs <- Herbs.Freqs %>% select(Herb, YN, n, percent) %>% filter(YN == 1) %>% arrange(desc(percent))
write.csv(Herbs.Freqs, "HerbData_CountFreq_paige_v02.csv")

# Delete unnecessary tables. 
rm(freq00, freq01, freq02, freq03, freq04, freq05, freq06, 
   freq07, freq08, freq09, freq10, freq11, freq12, freq13, freq14,
   freq15, freq16, freq17, freq18, freq19, freq20)

Herbs.Freqs

###################################################################################################
# Summary Statistic commands
tabyl(HerbOutcomes$, sort = TRUE)
summary(HerbOutcomes$)
stat.desc(HerbOutcomes$)
###################################################################################################
# Group observations by herb exposure (Y/N)
HerbYesData <- HerbData_Cleaned %>% filter(HerbPresent.yn == 1) 
HerbNoData <- HerbData_Cleaned %>% filter(HerbPresent.yn == 0)

# Identify Multiples (confirm none)
HerbData_Cleaned %>% filter(singleton.yn == 0) %>% select(id, indexR, singleton.yn, HerbPresent.yn)

tabyl(HerbData_Cleaned$HerbPresent.yn)
###################################################################################################
# Herb Present
desc.y0 <- tabyl(HerbYesData$HerbPresent.yn, sort = TRUE)%>% 
  mutate(Characteristics = "Herb Utilization") %>% mutate(sort = 0)%>% 
  mutate(ChiSq = NA_integer_) %>% mutate( df = NA_integer_) %>% mutate( pvalue = NA_integer_)
desc.n0 <- tabyl(HerbNoData$HerbPresent.yn, sort = TRUE)%>% 
  mutate(Characteristics = "Herb Utilization") %>% mutate(sort = 0)%>% 
  mutate(ChiSq = NA_integer_) %>% mutate( df = NA_integer_) %>% mutate( pvalue = NA_integer_)

#Standardize the column name across tables.
names(desc.y0)[names(desc.y0) == "HerbYesData$HerbPresent.yn"] <- "Level"
names(desc.y0)[names(desc.y0) == "percent"] <- "Yes_Percentage"
names(desc.y0)[names(desc.y0) == "n"] <- "Yes_n"

names(desc.n0)[names(desc.n0) == "HerbNoData$HerbPresent.yn"] <- "Level_n"
names(desc.n0)[names(desc.n0) == "percent"] <- "No_Percentage"
names(desc.n0)[names(desc.n0) == "n"] <- "No_n"

# Select columns of interest
desc.y0 <- desc.y0 %>% select(Level, Yes_n, Yes_Percentage) %>% mutate(Level = "NA")
desc.n0 <- desc.n0 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue) %>% mutate(Level_n = "NA")%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc0 <- cbind(desc.y0, desc.n0)

# Check for accuracy
desc0

# Remove excess Level_n variable)
desc0 <- desc0 %>% select(-Level_n)

# Remove excess tables
rm(desc.y0, desc.n0)
###################################################################################################
# Race and ethnicity
HerbData_Cleaned$race.eth
table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$race.eth)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$race.eth, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$race.eth.label <- as_factor(HerbYesData$race.eth)
HerbNoData$race.eth.label <- as_factor(HerbNoData$race.eth)

# Create table for yes, table for no
desc.y1 <- tabyl(HerbYesData$race.eth.label, sort = TRUE) %>% 
  mutate(Characteristics = "Race/Ethnicity") %>% mutate(sort = 1) %>% 
  mutate(ChiSq = 8.39) %>% mutate( df = 5) %>% mutate( pvalue = 0.14) 
desc.n1 <- tabyl(HerbNoData$race.eth.label, sort = TRUE) %>% 
  mutate(Characteristics = "Race/Ethnicity") %>% mutate(sort = 1) %>% 
  mutate(ChiSq = 8.39) %>% mutate( df = 5) %>% mutate( pvalue = 0.14)

#Standardize the column name across tables.
names(desc.y1)[names(desc.y1) == "HerbYesData$race.eth.label"] <- "Level"
names(desc.y1)[names(desc.y1) == "valid_percent"] <- "Yes_Percentage"
names(desc.y1)[names(desc.y1) == "n"] <- "Yes_n"

names(desc.n1)[names(desc.n1) == "HerbNoData$race.eth.label"] <- "Level_n"
names(desc.n1)[names(desc.n1) == "valid_percent"] <- "No_Percentage"
names(desc.n1)[names(desc.n1) == "n"] <- "No_n"

# Select columns of interest
desc.y1 <- desc.y1 %>% select(Level, Yes_n, Yes_Percentage)
desc.n1 <- desc.n1 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc1 <- cbind(desc.y1, desc.n1)

# Check for accuracy
desc1

# Remove excess Level_n variable)
desc1 <- desc1 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y1, desc.n1)
         
###################################################################################################
###################################################################################################

# Special Groups: plain
HerbData_Cleaned$plain
table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$plain)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$plain, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$plain.label <- as_factor(HerbYesData$plain)
HerbNoData$plain.label <- as_factor(HerbNoData$plain)

# Create table for yes, table for no
desc.y2 <- tabyl(HerbYesData$plain.label, sort = TRUE) %>% 
  mutate(Characteristics = "Belongs to Amish, Mennonite, or other Plain church") %>% mutate(sort = 2) %>% 
  mutate(ChiSq = 140) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002) 
desc.n2 <- tabyl(HerbNoData$plain.label, sort = TRUE) %>% 
  mutate(Characteristics = "Belongs to Amish Mennonite, or other Plain church ") %>% mutate(sort = 2) %>% 
  mutate(ChiSq = 140) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002)

#Standardize the column name across tables.
names(desc.y2)[names(desc.y2) == "HerbYesData$plain.label"] <- "Level"
names(desc.y2)[names(desc.y2) == "percent"] <- "Yes_Percentage"
names(desc.y2)[names(desc.y2) == "n"] <- "Yes_n"

names(desc.n2)[names(desc.n2) == "HerbNoData$plain.label"] <- "Level_n"
names(desc.n2)[names(desc.n2) == "percent"] <- "No_Percentage"
names(desc.n2)[names(desc.n2) == "n"] <- "No_n"

# Select columns of interest
desc.y2 <- desc.y2 %>% select(Level, Yes_n, Yes_Percentage)
desc.n2 <- desc.n2 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc2 <- cbind(desc.y2, desc.n2)

# Check for accuracy
desc2

# Remove excess Level_n variable)
desc2 <- desc2 %>% select(-Level_n)

# Remove excess tables
rm(desc.y2, desc.n2)
###################################################################################################

# Education
HerbData_Cleaned$mom.ed
HerbData_Cleaned$mom.ed
table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$mom.ed)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$mom.ed, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$mom.ed.label <- as_factor(HerbYesData$mom.ed)
HerbNoData$mom.ed.label <- as_factor(HerbNoData$mom.ed)

# Create table for yes, table for no
desc.y3 <- tabyl(HerbYesData$mom.ed.label, sort = TRUE) %>% 
  mutate(Characteristics = "Education") %>% mutate(sort = 3) %>% 
  mutate(ChiSq = 111) %>% mutate( df = 5) %>% mutate( pvalue = 0.0000000000000002) 
desc.n3 <- tabyl(HerbNoData$mom.ed.label, sort = TRUE) %>% 
  mutate(Characteristics = "Education") %>% mutate(sort = 3) %>% 
  mutate(ChiSq = 111) %>% mutate( df = 5) %>% mutate( pvalue = 0.0000000000000002)

#Standardize the column name across tables.
names(desc.y3)[names(desc.y3) == "HerbYesData$mom.ed.label"] <- "Level"
names(desc.y3)[names(desc.y3) == "valid_percent"] <- "Yes_Percentage"
names(desc.y3)[names(desc.y3) == "n"] <- "Yes_n"

names(desc.n3)[names(desc.n3) == "HerbNoData$mom.ed.label"] <- "Level_n"
names(desc.n3)[names(desc.n3) == "valid_percent"] <- "No_Percentage"
names(desc.n3)[names(desc.n3) == "n"] <- "No_n"

# Select columns of interest
desc.y3 <- desc.y3 %>% select(Level, Yes_n, Yes_Percentage)
desc.n3 <- desc.n3 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue) %>%
  mutate(missing_percent = 0.103620)

# Bind columns of yes and no
desc3 <- cbind(desc.y3, desc.n3)

# Check for accuracy
desc3

# Remove excess Level_n variable)
desc3 <- desc3 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y3, desc.n3)
###################################################################################################
# Mom Age
qqnorm(HerbData_Cleaned$Dem_MomAge)
HerbYesData$Dem_MomAge
HerbNoData$Dem_MomAge

stat.desc(HerbYesData$Dem_MomAge)
stat.desc(HerbNoData$Dem_MomAge)

t.test(x = HerbYesData$Dem_MomAge,
       y = HerbNoData$Dem_MomAge)

# Create an empty tibble
desc4 <- tibble(rows =1)

# Create Level column
desc4$Level <- as.factor("mean (SD)")

# Mean (yes herbs)
desc4$Yes_n <- mean(HerbYesData$Dem_MomAge)
# Standard deviation
desc4$Yes_Percentage <- sqrt(var(HerbYesData$Dem_MomAge))

# mean (no herbs)
desc4$No_n <- mean(HerbNoData$Dem_MomAge)
# Standard deviation
desc4$No_Percentage <- sqrt(var(HerbNoData$Dem_MomAge))

# Create extra columns
desc4$Characteristics <- "Age at first prenatal visit, mean (SD)"
desc4$sort <- 4
desc4$ChiSq <- NA_integer_
desc4$df <- NA_integer_
desc4$pvalue <- NA_integer_
desc4$missing_percent <- NA_integer_

# Check for accuracy
desc4

# Remove excess Level_n variable)
desc4 <- desc4 %>% select(-rows)

# median(HerbYesData$Dem_MomAge)
# Standard error
# sqrt(var(HerbYesData$Dem_MomAge)/7316)
# Confidence Interval
# mean(HerbYesData$Dem_MomAge) - 1.96*sqrt(var(HerbYesData$Dem_MomAge)/7316)
# mean(HerbYesData$Dem_MomAge) + 1.96*sqrt(var(HerbYesData$Dem_MomAge)/7316)

###################################################################################################

# BMI percent normal bmi and mean/sd of bmi)
HerbData_Cleaned$bmi
# qqnorm(HerbData_Cleaned$bmi)
# stat.desc(HerbData_Cleaned$bmi)
# summary(HerbData_Cleaned$bmi)
# 
# HerbYesData$bmi
# summary(HerbYesData$bmi)
# 
# HerbNoData$bmi
# summary(HerbNoData$bmi)

# Categorize BMI
HerbData_Cleaned$bmi.cat <- cut(HerbData_Cleaned$bmi, breaks=c(0, 18.5, 25, 30, +Inf),
                                labels=c("Underweight", "Normal", "Overweight", "Obese"))
HerbYesData$bmi.cat <- cut(HerbYesData$bmi, breaks=c(0, 18.5, 25, 30, +Inf),
                                labels=c("Underweight", "Normal", "Overweight", "Obese"))
HerbNoData$bmi.cat <- cut(HerbNoData$bmi, breaks=c(0, 18.5, 25, 30, +Inf),
                                labels=c("Underweight", "Normal", "Overweight", "Obese"))

table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$bmi.cat)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$bmi.cat, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$bmi.cat.label <- as_factor(HerbYesData$bmi.cat)
HerbNoData$bmi.cat.label <- as_factor(HerbNoData$bmi.cat)

# Create table for yes, table for no
desc.y5 <- tabyl(HerbYesData$bmi.cat.label, sort = TRUE) %>% 
  mutate(Characteristics = "BMI") %>% mutate(sort = 5) %>% 
  mutate(ChiSq = 8.41) %>% mutate( df = 3) %>% mutate( pvalue = 0.038) 
desc.n5 <- tabyl(HerbNoData$bmi.cat.label, sort = TRUE) %>% 
  mutate(Characteristics = "BMI") %>% mutate(sort = 5) %>% 
  mutate(ChiSq = 8.41) %>% mutate( df = 3) %>% mutate( pvalue = 0.038)

#Standardize the column name across tables.
names(desc.y5)[names(desc.y5) == "HerbYesData$bmi.cat.label"] <- "Level"
names(desc.y5)[names(desc.y5) == "valid_percent"] <- "Yes_Percentage"
names(desc.y5)[names(desc.y5) == "n"] <- "Yes_n"

names(desc.n5)[names(desc.n5) == "HerbNoData$bmi.cat.label"] <- "Level_n"
names(desc.n5)[names(desc.n5) == "valid_percent"] <- "No_Percentage"
names(desc.n5)[names(desc.n5) == "n"] <- "No_n"

# Select columns of interest
desc.y5 <- desc.y5 %>% select(Level, Yes_n, Yes_Percentage)
desc.n5 <- desc.n5 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc5 <- cbind(desc.y5, desc.n5)

# Check for accuracy
desc5

# Remove excess Level_n variable)
desc5 <- desc5 %>% select(-Level_n) %>% filter( Yes_Percentage >0)

# Remove excess tables
rm(desc.y5, desc.n5)
###################################################################################################
###################################################################################################

# Marital Status
HerbData_Cleaned$part.yn

tabyl(HerbYesData$part.yn, sort = TRUE)
tabyl(HerbNoData$part.yn, sort = TRUE)

table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$part.yn)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$part.yn, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$part.yn.label <- as_factor(HerbYesData$part.yn)
HerbNoData$part.yn.label <- as_factor(HerbNoData$part.yn)

# Create table for yes, table for no
desc.y6 <- tabyl(HerbYesData$part.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Marital Status") %>% mutate(sort = 6) %>% 
  mutate(ChiSq = 5.55) %>% mutate( df = 2) %>% mutate( pvalue = 0.062) 
desc.n6 <- tabyl(HerbNoData$part.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Marital Status") %>% mutate(sort = 6) %>% 
  mutate(ChiSq = 5.55) %>% mutate( df = 2) %>% mutate( pvalue = 0.062)

#Standardize the column name across tables.
names(desc.y6)[names(desc.y6) == "HerbYesData$part.yn.label"] <- "Level"
names(desc.y6)[names(desc.y6) == "percent"] <- "Yes_Percentage"
names(desc.y6)[names(desc.y6) == "n"] <- "Yes_n"

names(desc.n6)[names(desc.n6) == "HerbNoData$part.yn.label"] <- "Level_n"
names(desc.n6)[names(desc.n6) == "valid_percent"] <- "No_Percentage"
names(desc.n6)[names(desc.n6) == "n"] <- "No_n"

# Select columns of interest
desc.y6 <- desc.y6 %>% select(Level, Yes_n, Yes_Percentage, Characteristics) 
desc.n6 <- desc.n6 %>% select(Level_n, No_n, No_Percentage, sort, ChiSq, df, pvalue) %>% filter(No_Percentage > 0)%>%
  mutate(missing_percent = NA_integer_)


# Bind columns of yes and no
desc6 <- cbind(desc.y6, desc.n6)

# Check for accuracy
desc6

# Remove excess Level_n variable)
desc6 <- desc6 %>% select(-Level_n)

# Remove excess tables
rm(desc.y6, desc.n6)
###################################################################################################
#########################################################################################################

# Midwife Credentials
HerbData_Cleaned$midwife.typeV02
tabyl(HerbYesData$midwife.typeV02, sort = TRUE)
tabyl(HerbNoData$midwife.typeV02, sort = TRUE)

table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$midwife.typeV02)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$midwife.typeV02, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$midwife.typeV02.label <- as_factor(HerbYesData$midwife.typeV02)
HerbNoData$midwife.typeV02.label <- as_factor(HerbNoData$midwife.typeV02)

# Create table for yes, table for no
desc.y7 <- tabyl(HerbYesData$midwife.typeV02.label, sort = TRUE) %>% 
  mutate(Characteristics = "Midwife Credentials") %>% mutate(sort = 7) %>% 
  mutate(ChiSq = 186) %>% mutate( df = 3) %>% mutate( pvalue = 0.0000000000000002) 
desc.n7 <- tabyl(HerbNoData$midwife.typeV02.label, sort = TRUE) %>% 
  mutate(Characteristics = "Midwife Credentials") %>% mutate(sort = 7) %>% 
  mutate(ChiSq = 186) %>% mutate( df = 3) %>% mutate( pvalue = 0.0000000000000002)

#Standardize the column name across tables.
names(desc.y7)[names(desc.y7) == "HerbYesData$midwife.typeV02.label"] <- "Level"
names(desc.y7)[names(desc.y7) == "percent"] <- "Yes_Percentage"
names(desc.y7)[names(desc.y7) == "n"] <- "Yes_n"

names(desc.n7)[names(desc.n7) == "HerbNoData$midwife.typeV02.label"] <- "Level_n"
names(desc.n7)[names(desc.n7) == "percent"] <- "No_Percentage"
names(desc.n7)[names(desc.n7) == "n"] <- "No_n"

# Select columns of interest
desc.y7 <- desc.y7 %>% select(Level, Yes_n, Yes_Percentage)
desc.n7 <- desc.n7 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc7 <- cbind(desc.y7, desc.n7)

# Check for accuracy
desc7

# Remove excess Level_n variable)
desc7 <- desc7 %>% select(-Level_n)

# Remove excess tables
rm(desc.y7, desc.n7)
###################################################################################################
# Method of Payment
HerbData_Cleaned$medicaidpay.yn
tabyl(HerbYesData$medicaidpay.yn, sort = TRUE)
tabyl(HerbNoData$medicaidpay.yn, sort = TRUE)

table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$medicaidpay.yn)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$medicaidpay.yn, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$medicaidpay.yn.label <- as_factor(HerbYesData$medicaidpay.yn)
HerbNoData$medicaidpay.yn.label <- as_factor(HerbNoData$medicaidpay.yn)

# Create table for yes, table for no
desc.y8 <- tabyl(HerbYesData$medicaidpay.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Medicaid was Payer") %>% mutate(sort = 8) %>% 
  mutate(ChiSq = 96.3) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002) 
desc.n8 <- tabyl(HerbNoData$medicaidpay.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Medicaid was Payer") %>% mutate(sort = 8) %>% 
  mutate(ChiSq = 96.3) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002)

#Standardize the column name across tables.
names(desc.y8)[names(desc.y8) == "HerbYesData$medicaidpay.yn.label"] <- "Level"
names(desc.y8)[names(desc.y8) == "valid_percent"] <- "Yes_Percentage"
names(desc.y8)[names(desc.y8) == "n"] <- "Yes_n"

names(desc.n8)[names(desc.n8) == "HerbNoData$medicaidpay.yn.label"] <- "Level_n"
names(desc.n8)[names(desc.n8) == "valid_percent"] <- "No_Percentage"
names(desc.n8)[names(desc.n8) == "n"] <- "No_n"

# Select columns of interest
desc.y8 <- desc.y8 %>% select(Level, Yes_n, Yes_Percentage)
desc.n8 <- desc.n8 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc8 <- cbind(desc.y8, desc.n8)

# Check for accuracy
desc8

# Remove excess Level_n variable)
desc8 <- desc8 %>% select(-Level_n) %>% filter(Yes_Percentage >0)

# Remove excess tables
rm(desc.y8, desc.n8)
###################################################################################################
###################################################################################################
# Pregnancy Characteristics
## Mother is primiparous
HerbData_Cleaned$primip.yn
tabyl(HerbYesData$primip.yn, sort = TRUE)
tabyl(HerbNoData$primip.yn, sort = TRUE)

table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$primip.yn)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$primip.yn, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$primip.yn.label <- as_factor(HerbYesData$primip.yn)
HerbNoData$primip.yn.label <- as_factor(HerbNoData$primip.yn)

# Create table for yes, table for no
desc.y10 <- tabyl(HerbYesData$primip.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Primiparous") %>% mutate(sort = 10) %>% 
  mutate(ChiSq = 13.7) %>% mutate( df = 1) %>% mutate( pvalue = 0.00021) 
desc.n10 <- tabyl(HerbNoData$primip.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Primiparous") %>% mutate(sort = 10) %>% 
  mutate(ChiSq = 13.7) %>% mutate( df = 1) %>% mutate( pvalue = 0.00021)

#Standardize the column name across tables.
names(desc.y10)[names(desc.y10) == "HerbYesData$primip.yn.label"] <- "Level"
names(desc.y10)[names(desc.y10) == "percent"] <- "Yes_Percentage"
names(desc.y10)[names(desc.y10) == "n"] <- "Yes_n"

names(desc.n10)[names(desc.n10) == "HerbNoData$primip.yn.label"] <- "Level_n"
names(desc.n10)[names(desc.n10) == "valid_percent"] <- "No_Percentage"
names(desc.n10)[names(desc.n10) == "n"] <- "No_n"

# Select columns of interest
desc.y10 <- desc.y10 %>% select(Level, Yes_n, Yes_Percentage) %>% filter(Yes_Percentage >0)
desc.n10 <- desc.n10 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue) %>% filter(No_Percentage > 0)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc10 <- cbind(desc.y10, desc.n10)

# Check for accuracy
desc10

# Remove excess Level_n variable)
desc10 <- desc10 %>% select(-Level_n)

# Remove excess tables
rm(desc.y10, desc.n10)
###################################################################################################

## Mother is grandmultiparous
HerbData_Cleaned$grandmultip.yn
tabyl(HerbYesData$grandmultip.yn, sort = TRUE)
tabyl(HerbNoData$grandmultip.yn, sort = TRUE)

table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$grandmultip.yn)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$grandmultip.yn, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$grandmultip.yn.label <- as_factor(HerbYesData$grandmultip.yn)
HerbNoData$grandmultip.yn.label <- as_factor(HerbNoData$grandmultip.yn)

# Create table for yes, table for no
desc.y11 <- tabyl(HerbYesData$grandmultip.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Grandmultiparous") %>% mutate(sort = 11) %>% 
  mutate(ChiSq = 9.15) %>% mutate( df = 1) %>% mutate( pvalue = 0.0025) 
desc.n11 <- tabyl(HerbNoData$grandmultip.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Grandmultiparous") %>% mutate(sort = 11) %>% 
  mutate(ChiSq = 9.15) %>% mutate( df = 1) %>% mutate( pvalue = 0.0025)

#Standardize the column name across tables.
names(desc.y11)[names(desc.y11) == "HerbYesData$grandmultip.yn.label"] <- "Level"
names(desc.y11)[names(desc.y11) == "valid_percent"] <- "Yes_Percentage"
names(desc.y11)[names(desc.y11) == "n"] <- "Yes_n"

names(desc.n11)[names(desc.n11) == "HerbNoData$grandmultip.yn.label"] <- "Level_n"
names(desc.n11)[names(desc.n11) == "valid_percent"] <- "No_Percentage"
names(desc.n11)[names(desc.n11) == "n"] <- "No_n"

# Select columns of interest
desc.y11 <- desc.y11 %>% select(Level, Yes_n, Yes_Percentage)
desc.n11 <- desc.n11 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue) %>% mutate( missing_percent = 0.29)

# Bind columns of yes and no
desc11 <- cbind(desc.y11, desc.n11)

# Check for accuracy
desc11

# Remove excess Level_n variable)
desc11 <- desc11 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y11, desc.n11)
###################################################################################################
# Csection
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$Csec_FlgR.label <- as_factor(HerbYesData$Csec_FlgR)
HerbNoData$Csec_FlgR.label <- as_factor(HerbNoData$Csec_FlgR)

# Create table for yes, table for no
desc.y12 <- tabyl(HerbYesData$Csec_FlgR.label, sort = TRUE) %>% 
  mutate(Characteristics = "Cesarean Section") %>% mutate(sort = 12) %>% 
  mutate(ChiSq = 14.1) %>% mutate( df = 1) %>% mutate( pvalue = 0.00018) 
desc.n12 <- tabyl(HerbNoData$Csec_FlgR.label, sort = TRUE) %>% 
  mutate(Characteristics = "Cesarean Section") %>% mutate(sort = 12) %>% 
  mutate(ChiSq = 14.1) %>% mutate( df = 1) %>% mutate( pvalue = 0.00018)

#Standardize the column name across tables.
names(desc.y12)[names(desc.y12) == "HerbYesData$Csec_FlgR.label"] <- "Level"
names(desc.y12)[names(desc.y12) == "valid_percent"] <- "Yes_Percentage"
names(desc.y12)[names(desc.y12) == "n"] <- "Yes_n"

names(desc.n12)[names(desc.n12) == "HerbNoData$Csec_FlgR.label"] <- "Level_n"
names(desc.n12)[names(desc.n12) == "valid_percent"] <- "No_Percentage"
names(desc.n12)[names(desc.n12) == "n"] <- "No_n"

# Select columns of interest
desc.y12 <- desc.y12 %>% select(Level, Yes_n, Yes_Percentage)
desc.n12 <- desc.n12 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc12 <- cbind(desc.y12, desc.n12)

# Check for accuracy
desc12

# Remove excess Level_n variable)
desc12 <- desc12 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y12, desc.n12)

###################################################################################################

## TOLAC
### 0 = multiparous, no history of cesarean
### 1 - multiparous, history of cesarean and vaginal birth
### 2 - multiparous, history of cesarean only
### 3 - primiparous
HerbData_Cleaned$tolac.prev
tabyl(HerbYesData$tolac.prev, sort = TRUE)
tabyl(HerbNoData$tolac.prev, sort = TRUE)

table(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$tolac.prev)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$tolac.prev, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$tolac.prev.label <- as_factor(HerbYesData$tolac.prev)
HerbNoData$tolac.prev.label <- as_factor(HerbNoData$tolac.prev)

# Create table for yes, table for no
desc.y13 <- tabyl(HerbYesData$tolac.prev.label, sort = TRUE) %>% 
  mutate(Characteristics = "TOLAC Prevalence") %>% mutate(sort = 13) %>% 
  mutate(ChiSq = 50.6) %>% mutate( df = 3) %>% mutate( pvalue = 0.000000000061) 
desc.n13 <- tabyl(HerbNoData$tolac.prev.label, sort = TRUE) %>% 
  mutate(Characteristics = "TOLAC Prevalence") %>% mutate(sort = 13) %>% 
  mutate(ChiSq = 50.6) %>% mutate( df = 3) %>% mutate( pvalue = 0.000000000061)

#Standardize the column name across tables.
names(desc.y13)[names(desc.y13) == "HerbYesData$tolac.prev.label"] <- "Level"
names(desc.y13)[names(desc.y13) == "valid_percent"] <- "Yes_Percentage"
names(desc.y13)[names(desc.y13) == "n"] <- "Yes_n"

names(desc.n13)[names(desc.n13) == "HerbNoData$tolac.prev.label"] <- "Level_n"
names(desc.n13)[names(desc.n13) == "valid_percent"] <- "No_Percentage"
names(desc.n13)[names(desc.n13) == "n"] <- "No_n"

# Select columns of interest
desc.y13 <- desc.y13 %>% select(Level, Yes_n, Yes_Percentage)
desc.n13 <- desc.n13 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc13 <- cbind(desc.y13, desc.n13)

# Check for accuracy
desc13

# Remove excess Level_n variable)
desc13 <- desc13 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y13, desc.n13)

###################################################################################################
###################################################################################################
###################################################################################################
# Breech
HerbData_Cleaned$Breech_FlgR
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$Breech_FlgR, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$Breech_FlgR.label <- as_factor(HerbYesData$Breech_FlgR)
HerbNoData$Breech_FlgR.label <- as_factor(HerbNoData$Breech_FlgR)

# Create table for yes, table for no
desc.y19 <- tabyl(HerbYesData$Breech_FlgR.label, sort = TRUE) %>% 
  mutate(Characteristics = "Breech") %>% mutate(sort = 19) %>% 
  mutate(ChiSq = 60.3) %>% mutate( df = 1) %>% mutate( pvalue = 0.000000000000008) 
desc.n19 <- tabyl(HerbNoData$Breech_FlgR.label, sort = TRUE) %>% 
  mutate(Characteristics = "Breech") %>% mutate(sort = 19) %>% 
  mutate(ChiSq = 60.3) %>% mutate( df = 1) %>% mutate( pvalue = 0.000000000000008)

#Standardize the column name across tables.
names(desc.y19)[names(desc.y19) == "HerbYesData$Breech_FlgR.label"] <- "Level"
names(desc.y19)[names(desc.y19) == "valid_percent"] <- "Yes_Percentage"
names(desc.y19)[names(desc.y19) == "n"] <- "Yes_n"

names(desc.n19)[names(desc.n19) == "HerbNoData$Breech_FlgR.label"] <- "Level_n"
names(desc.n19)[names(desc.n19) == "valid_percent"] <- "No_Percentage"
names(desc.n19)[names(desc.n19) == "n"] <- "No_n"

# Select columns of interest
desc.y19 <- desc.y19 %>% select(Level, Yes_n, Yes_Percentage)
desc.n19 <- desc.n19 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc19 <- cbind(desc.y19, desc.n19)

# Check for accuracy
desc19

# Remove excess Level_n variable)
desc19 <- desc19 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y19, desc.n19)
###################################################################################################
###################################################################################################
# Maternal outcomes
# Postdate pregnancy
table(HerbData_Cleaned$postdates.yn)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$postdates.yn, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$postdates.yn.label <- as_factor(HerbYesData$postdates.yn)
HerbNoData$postdates.yn.label <- as_factor(HerbNoData$postdates.yn)

# Create table for yes, table for no
desc.y25 <- tabyl(HerbYesData$postdates.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Postdated Preg") %>% mutate(sort = 25) %>% 
  mutate(ChiSq = 117) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002) 
desc.n25 <- tabyl(HerbNoData$postdates.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Postdated Preg") %>% mutate(sort = 25) %>% 
  mutate(ChiSq = 117) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002)

#Standardize the column name across tables.
names(desc.y25)[names(desc.y25) == "HerbYesData$postdates.yn.label"] <- "Level"
names(desc.y25)[names(desc.y25) == "percent"] <- "Yes_Percentage"
names(desc.y25)[names(desc.y25) == "n"] <- "Yes_n"

names(desc.n25)[names(desc.n25) == "HerbNoData$postdates.yn.label"] <- "Level_n"
names(desc.n25)[names(desc.n25) == "percent"] <- "No_Percentage"
names(desc.n25)[names(desc.n25) == "n"] <- "No_n"

# Select columns of interest
desc.y25 <- desc.y25 %>% select(Level, Yes_n, Yes_Percentage)
desc.n25 <- desc.n25 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc25 <- cbind(desc.y25, desc.n25)

# Check for accuracy
desc25

# Remove excess Level_n variable)
desc25 <- desc25 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y25, desc.n25)
###################################################################################################
# Maternal outcomes
# maternal or infant transport
table(HerbData_Cleaned$transport)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$transport, correct=FALSE)

# Create table for yes, table for no
desc.y26 <- tabyl(HerbYesData$transport, sort = TRUE) %>% 
  mutate(Characteristics = "Maternal or Infant Transport") %>% mutate(sort = 26) %>% 
  mutate(ChiSq = 79.9) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002) 
desc.n26 <- tabyl(HerbNoData$transport, sort = TRUE) %>% 
  mutate(Characteristics = "Maternal or Infant Transport") %>% mutate(sort = 26) %>% 
  mutate(ChiSq = 79.9) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002)

#Standardize the column name across tables.
names(desc.y26)[names(desc.y26) == "HerbYesData$transport"] <- "Level"
names(desc.y26)[names(desc.y26) == "percent"] <- "Yes_Percentage"
names(desc.y26)[names(desc.y26) == "n"] <- "Yes_n"

names(desc.n26)[names(desc.n26) == "HerbNoData$transport"] <- "Level_n"
names(desc.n26)[names(desc.n26) == "percent"] <- "No_Percentage"
names(desc.n26)[names(desc.n26) == "n"] <- "No_n"

# Select columns of interest
desc.y26 <- desc.y26 %>% select(Level, Yes_n, Yes_Percentage)
desc.n26 <- desc.n26 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc26 <- cbind(desc.y26, desc.n26)

# Check for accuracy
desc26

# Remove excess Level_n variable)
desc26 <- desc26 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y26, desc.n26)
###################################################################################################
# Maternal outcomes
# perineal trauma
table(HerbData_Cleaned$peri_trauma.yn)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$peri_trauma.yn, correct=FALSE)

remove_var_label(HerbData_Cleaned$peri_trauma.yn)
remove_var_label(HerbYesData$peri_trauma.yn)
remove_var_label(HerbNoData$peri_trauma.yn)

# Create table for yes, table for no
desc.y27 <- tabyl(HerbYesData$peri_trauma.yn, sort = TRUE) %>% 
  mutate(Characteristics = "Perineal Trauma") %>% mutate(sort = 27) %>% 
  mutate(ChiSq = 4.12) %>% mutate( df = 1) %>% mutate( pvalue = 0.042) 
desc.n27 <- tabyl(HerbNoData$peri_trauma.yn, sort = TRUE) %>% 
  mutate(Characteristics = "Perineal Trauma") %>% mutate(sort = 27) %>% 
  mutate(ChiSq = 4.12) %>% mutate( df = 1) %>% mutate( pvalue = 0.042)

#Standardize the column name across tables.
names(desc.y27)[names(desc.y27) == "HerbYesData$peri_trauma.yn"] <- "Level"
names(desc.y27)[names(desc.y27) == "valid_percent"] <- "Yes_Percentage"
names(desc.y27)[names(desc.y27) == "n"] <- "Yes_n"

names(desc.n27)[names(desc.n27) == "HerbNoData$peri_trauma.yn"] <- "Level_n"
names(desc.n27)[names(desc.n27) == "valid_percent"] <- "No_Percentage"
names(desc.n27)[names(desc.n27) == "n"] <- "No_n"

# Select columns of interest
desc.y27 <- desc.y27 %>% select(Level, Yes_n, Yes_Percentage)
desc.n27 <- desc.n27 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc27 <- cbind(desc.y27, desc.n27)

# Check for accuracy
desc27

# Remove excess Level_n variable)
desc27 <- desc27 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y27, desc.n27)
###################################################################################################
# Maternal outcomes
# blood  loss > 1L
table(HerbData_Cleaned$blood_loss_cat)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$blood_loss_cat, correct=FALSE)

# Create table for yes, table for no
desc.y29 <- tabyl(HerbYesData$blood_loss_cat, sort = TRUE) %>% 
  mutate(Characteristics = "Maternal Blood Loss over 1L") %>% mutate(sort = 29) %>% 
  mutate(ChiSq = 81.218) %>% mutate( df = 1) %>% mutate( pvalue = 2.2e-16) 
desc.n29 <- tabyl(HerbNoData$blood_loss_cat, sort = TRUE) %>% 
  mutate(Characteristics = "Maternal Blood Loss over 1L") %>% mutate(sort = 29) %>% 
  mutate(ChiSq = 81.218) %>% mutate( df = 1) %>% mutate( pvalue = 2.2e-16)

#Standardize the column name across tables.
names(desc.y29)[names(desc.y29) == "HerbYesData$blood_loss_cat"] <- "Level"
names(desc.y29)[names(desc.y29) == "valid_percent"] <- "Yes_Percentage"
names(desc.y29)[names(desc.y29) == "n"] <- "Yes_n"

names(desc.n29)[names(desc.n29) == "HerbNoData$blood_loss_cat"] <- "Level_n"
names(desc.n29)[names(desc.n29) == "valid_percent"] <- "No_Percentage"
names(desc.n29)[names(desc.n29) == "n"] <- "No_n"

# Select columns of interest
desc.y29 <- desc.y29 %>% select(Level, Yes_n, Yes_Percentage)
desc.n29 <- desc.n29 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc29 <- cbind(desc.y29, desc.n29)

# Check for accuracy
desc29

# Remove excess Level_n variable)
desc29 <- desc29 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y29, desc.n29)
##################################
# Maternal outcomes
# blood transfusion
table(HerbData_Cleaned$Blood_Actions_BloodTrans)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$Blood_Actions_BloodTrans, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$Blood_Actions_BloodTrans.label <- as_factor(HerbYesData$Blood_Actions_BloodTrans)
HerbNoData$Blood_Actions_BloodTrans.label <- as_factor(HerbNoData$Blood_Actions_BloodTrans)

# Create table for yes, table for no
desc.y30 <- tabyl(HerbYesData$Blood_Actions_BloodTrans.label, sort = TRUE) %>% 
  mutate(Characteristics = "Maternal Blood Transfusion") %>% mutate(sort = 30) %>% 
  mutate(ChiSq = 20.4) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000064) 
desc.n30 <- tabyl(HerbNoData$Blood_Actions_BloodTrans.label, sort = TRUE) %>% 
  mutate(Characteristics = "Maternal Blood Transfusion") %>% mutate(sort = 30) %>% 
  mutate(ChiSq = 20.4) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000064)

#Standardize the column name across tables.
names(desc.y30)[names(desc.y30) == "HerbYesData$Blood_Actions_BloodTrans.label"] <- "Level"
names(desc.y30)[names(desc.y30) == "valid_percent"] <- "Yes_Percentage"
names(desc.y30)[names(desc.y30) == "n"] <- "Yes_n"

names(desc.n30)[names(desc.n30) == "HerbNoData$Blood_Actions_BloodTrans.label"] <- "Level_n"
names(desc.n30)[names(desc.n30) == "valid_percent"] <- "No_Percentage"
names(desc.n30)[names(desc.n30) == "n"] <- "No_n"

# Select columns of interest
desc.y30 <- desc.y30 %>% select(Level, Yes_n, Yes_Percentage)
desc.n30 <- desc.n30 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc30 <- cbind(desc.y30, desc.n30)

# Check for accuracy
desc30

# Remove excess Level_n variable)
desc30 <- desc30 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y30, desc.n30)
###################################################################################################
###################################################################################################
# Maternal outcomes
# mother hospitalized
table(HerbData_Cleaned$MomHosp_FlgR)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$MomHosp_FlgR.label <- as_factor(HerbYesData$MomHosp_FlgR)
HerbNoData$MomHosp_FlgR.label <- as_factor(HerbNoData$MomHosp_FlgR)

# Create table for yes, table for no
desc.y32 <- tabyl(HerbYesData$MomHosp_FlgR.label, sort = TRUE) %>% 
  mutate(Characteristics = "Mother Hospitalized") %>% mutate(sort = 32) %>% 
  mutate(ChiSq = 97.8) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002) 
desc.n32 <- tabyl(HerbNoData$MomHosp_FlgR.label, sort = TRUE) %>% 
  mutate(Characteristics = "Mother Hospitalized") %>% mutate(sort = 32) %>% 
  mutate(ChiSq = 97.8) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000000002)

#Standardize the column name across tables.
names(desc.y32)[names(desc.y32) == "HerbYesData$MomHosp_FlgR.label"] <- "Level"
names(desc.y32)[names(desc.y32) == "valid_percent"] <- "Yes_Percentage"
names(desc.y32)[names(desc.y32) == "n"] <- "Yes_n"

names(desc.n32)[names(desc.n32) == "HerbNoData$MomHosp_FlgR.label"] <- "Level_n"
names(desc.n32)[names(desc.n32) == "valid_percent"] <- "No_Percentage"
names(desc.n32)[names(desc.n32) == "n"] <- "No_n"

# Select columns of interest
desc.y32 <- desc.y32 %>% select(Level, Yes_n, Yes_Percentage)
desc.n32 <- desc.n32 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc32 <- cbind(desc.y32, desc.n32)

# Check for accuracy
desc32

# Remove excess Level_n variable)
desc32 <- desc32 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y32, desc.n32)
###################################################################################################
# Maternal outcomes
# mother postpartum infection

table(HerbData_Cleaned$mompost.RT.infect.yn)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$mompost.RT.infect.yn.label <- as_factor(HerbYesData$mompost.RT.infect.yn)
HerbNoData$mompost.RT.infect.yn.label <- as_factor(HerbNoData$mompost.RT.infect.yn)

# Create table for yes, table for no
desc.y33 <- tabyl(HerbYesData$mompost.RT.infect.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Mother Postpartum Infection") %>% mutate(sort = 33) %>% 
  mutate(ChiSq = 25.6) %>% mutate( df = 1) %>% mutate( pvalue = 0.00000043) 
desc.n33 <- tabyl(HerbNoData$mompost.RT.infect.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Mother Postpartum Infection") %>% mutate(sort = 33) %>% 
  mutate(ChiSq = 25.6) %>% mutate( df = 1) %>% mutate( pvalue = 0.00000043)

#Standardize the column name across tables.
names(desc.y33)[names(desc.y33) == "HerbYesData$mompost.RT.infect.yn.label"] <- "Level"
names(desc.y33)[names(desc.y33) == "valid_percent"] <- "Yes_Percentage"
names(desc.y33)[names(desc.y33) == "n"] <- "Yes_n"

names(desc.n33)[names(desc.n33) == "HerbNoData$mompost.RT.infect.yn.label"] <- "Level_n"
names(desc.n33)[names(desc.n33) == "valid_percent"] <- "No_Percentage"
names(desc.n33)[names(desc.n33) == "n"] <- "No_n"

# Select columns of interest
desc.y33 <- desc.y33 %>% select(Level, Yes_n, Yes_Percentage)
desc.n33 <- desc.n33 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc33 <- cbind(desc.y33, desc.n33)

# Check for accuracy
desc33

# Remove excess Level_n variable)
desc33 <- desc33 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y33, desc.n33)
###################################################################################################
# Infant outcomes
# low birth weight

table(HerbData_Cleaned$LBW)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$LBW, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$LBW.label <- as_factor(HerbYesData$LBW)
HerbNoData$LBW.label <- as_factor(HerbNoData$LBW)

# Create table for yes, table for no
desc.y34 <- tabyl(HerbYesData$LBW.label, sort = TRUE) %>% 
  mutate(Characteristics = "LBW") %>% mutate(sort = 34) %>% 
  mutate(ChiSq = 0.218) %>% mutate( df = 1) %>% mutate( pvalue = 0.64) 
desc.n34 <- tabyl(HerbNoData$LBW.label, sort = TRUE) %>% 
  mutate(Characteristics = "LBW") %>% mutate(sort = 34) %>% 
  mutate(ChiSq = 0.218) %>% mutate( df = 1) %>% mutate( pvalue = 0.64)

#Standardize the column name across tables.
names(desc.y34)[names(desc.y34) == "HerbYesData$LBW.label"] <- "Level"
names(desc.y34)[names(desc.y34) == "valid_percent"] <- "Yes_Percentage"
names(desc.y34)[names(desc.y34) == "n"] <- "Yes_n"

names(desc.n34)[names(desc.n34) == "HerbNoData$LBW.label"] <- "Level_n"
names(desc.n34)[names(desc.n34) == "valid_percent"] <- "No_Percentage"
names(desc.n34)[names(desc.n34) == "n"] <- "No_n"

# Select columns of interest
desc.y34 <- desc.y34 %>% select(Level, Yes_n, Yes_Percentage)
desc.n34 <- desc.n34 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc34 <- cbind(desc.y34, desc.n34)

# Check for accuracy
desc34

# Remove excess Level_n variable)
desc34 <- desc34 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y34, desc.n34)
###################################################################################################
# Infant outcomes
# macrosomia

table(HerbData_Cleaned$macro)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$macro, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$macro.label <- as_factor(HerbYesData$macro)
HerbNoData$macro.label <- as_factor(HerbNoData$macro)

# Create table for yes, table for no
desc.y35 <- tabyl(HerbYesData$macro.label, sort = TRUE) %>% 
  mutate(Characteristics = "Macrosomia") %>% mutate(sort = 35) %>% 
  mutate(ChiSq = 21.4) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000038) 
desc.n35 <- tabyl(HerbNoData$macro.label, sort = TRUE) %>% 
  mutate(Characteristics = "Macrosomia") %>% mutate(sort = 35) %>% 
  mutate(ChiSq = 21.4) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000038)

#Standardize the column name across tables.
names(desc.y35)[names(desc.y35) == "HerbYesData$macro.label"] <- "Level"
names(desc.y35)[names(desc.y35) == "valid_percent"] <- "Yes_Percentage"
names(desc.y35)[names(desc.y35) == "n"] <- "Yes_n"

names(desc.n35)[names(desc.n35) == "HerbNoData$macro.label"] <- "Level_n"
names(desc.n35)[names(desc.n35) == "valid_percent"] <- "No_Percentage"
names(desc.n35)[names(desc.n35) == "n"] <- "No_n"

# Select columns of interest
desc.y35 <- desc.y35 %>% select(Level, Yes_n, Yes_Percentage)
desc.n35 <- desc.n35 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc35 <- cbind(desc.y35, desc.n35)

# Check for accuracy
desc35

# Remove excess Level_n variable)
desc35 <- desc35 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y35, desc.n35)
###################################################################################################
# Infant outcomes
# infant infection

table(HerbData_Cleaned$infant_infect.yn)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$infant_infect.yn, correct=FALSE)

# Create table for yes, table for no
desc.y36 <- tabyl(HerbYesData$infant_infect.yn, sort = TRUE) %>% 
  mutate(Characteristics = "Infant Infection") %>% mutate(sort = 36) %>% 
  mutate(ChiSq = 4.5) %>% mutate( df = 1) %>% mutate( pvalue = 0.034) 
desc.n36 <- tabyl(HerbNoData$infant_infect.yn, sort = TRUE) %>% 
  mutate(Characteristics = "Infant Infection") %>% mutate(sort = 36) %>% 
  mutate(ChiSq = 4.5) %>% mutate( df = 1) %>% mutate( pvalue = 0.034)

#Standardize the column name across tables.
names(desc.y36)[names(desc.y36) == "HerbYesData$infant_infect.yn"] <- "Level"
names(desc.y36)[names(desc.y36) == "valid_percent"] <- "Yes_Percentage"
names(desc.y36)[names(desc.y36) == "n"] <- "Yes_n"

names(desc.n36)[names(desc.n36) == "HerbNoData$infant_infect.yn"] <- "Level_n"
names(desc.n36)[names(desc.n36) == "valid_percent"] <- "No_Percentage"
names(desc.n36)[names(desc.n36) == "n"] <- "No_n"

# Select columns of interest
desc.y36 <- desc.y36 %>% select(Level, Yes_n, Yes_Percentage)
desc.n36 <- desc.n36 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue) %>% mutate(missing_percent = 0.87)

# Bind columns of yes and no
desc36 <- cbind(desc.y36, desc.n36)

# Check for accuracy
desc36

# Remove excess Level_n variable)
desc36 <- desc36 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y36, desc.n36)
###################################################################################################
###################################################################################################
# Infant outcomes
# infant hospitalization
table(HerbData_Cleaned$InfHosp_FlgR)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$InfHosp_FlgR.label <- as_factor(HerbYesData$InfHosp_FlgR)
HerbNoData$InfHosp_FlgR.label <- as_factor(HerbNoData$InfHosp_FlgR)

# Create table for yes, table for no
desc.y38 <- tabyl(HerbYesData$InfHosp_FlgR.label, sort = TRUE) %>% 
  mutate(Characteristics = "Infant Hospitalized") %>% mutate(sort = 38) %>% 
  mutate(ChiSq = 51.1) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000009) 
desc.n38 <- tabyl(HerbNoData$InfHosp_FlgR.label, sort = TRUE) %>% 
  mutate(Characteristics = "Infant Hospitalized") %>% mutate(sort = 38) %>% 
  mutate(ChiSq = 51.1) %>% mutate( df = 1) %>% mutate( pvalue = 0.0000000000009)

#Standardize the column name across tables.
names(desc.y38)[names(desc.y38) == "HerbYesData$InfHosp_FlgR.label"] <- "Level"
names(desc.y38)[names(desc.y38) == "valid_percent"] <- "Yes_Percentage"
names(desc.y38)[names(desc.y38) == "n"] <- "Yes_n"

names(desc.n38)[names(desc.n38) == "HerbNoData$InfHosp_FlgR.label"] <- "Level_n"
names(desc.n38)[names(desc.n38) == "valid_percent"] <- "No_Percentage"
names(desc.n38)[names(desc.n38) == "n"] <- "No_n"

# Select columns of interest
desc.y38 <- desc.y38 %>% select(Level, Yes_n, Yes_Percentage)
desc.n38 <- desc.n38 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc38 <- cbind(desc.y38, desc.n38)

# Check for accuracy
desc38

# Remove excess Level_n variable)
desc38 <- desc38 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y38, desc.n38)
###################################################################################################
# Infant outcomes
# infant in NICU
table(HerbData_Cleaned$NICU.yn)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$NICU.yn.label <- as_factor(HerbYesData$NICU.yn)
HerbNoData$NICU.yn.label <- as_factor(HerbNoData$NICU.yn)

# Create table for yes, table for no
desc.y39 <- tabyl(HerbYesData$NICU.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Infant in NICU") %>% mutate(sort = 39) %>% 
  mutate(ChiSq = 7.57) %>% mutate( df = 1) %>% mutate( pvalue = 0.0059) 
desc.n39 <- tabyl(HerbNoData$NICU.yn.label, sort = TRUE) %>% 
  mutate(Characteristics = "Infant in NICU") %>% mutate(sort = 39) %>% 
  mutate(ChiSq = 7.57) %>% mutate( df = 1) %>% mutate( pvalue = 0.0059)

#Standardize the column name across tables.
names(desc.y39)[names(desc.y39) == "HerbYesData$NICU.yn.label"] <- "Level"
names(desc.y39)[names(desc.y39) == "valid_percent"] <- "Yes_Percentage"
names(desc.y39)[names(desc.y39) == "n"] <- "Yes_n"

names(desc.n39)[names(desc.n39) == "HerbNoData$NICU.yn.label"] <- "Level_n"
names(desc.n39)[names(desc.n39) == "valid_percent"] <- "No_Percentage"
names(desc.n39)[names(desc.n39) == "n"] <- "No_n"

# Select columns of interest
desc.y39 <- desc.y39 %>% select(Level, Yes_n, Yes_Percentage)
desc.n39 <- desc.n39 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc39 <- cbind(desc.y39, desc.n39)

# Check for accuracy
desc39

# Remove excess Level_n variable)
desc39 <- desc39 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y39, desc.n39)
###################################################################################################
# Infant outcomes
# infant died
table(HerbData_Cleaned$dead)
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$dead, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$dead.label <- as_factor(HerbYesData$dead)
HerbNoData$dead.label <- as_factor(HerbNoData$dead)

# Create table for yes, table for no
desc.y40 <- tabyl(HerbYesData$dead.label, sort = TRUE) %>% 
  mutate(Characteristics = "Infant Died") %>% mutate(sort = 40) %>% 
  mutate(ChiSq = 0.462) %>% mutate( df = 1) %>% mutate( pvalue = 0.5) 
desc.n40 <- tabyl(HerbNoData$dead.label, sort = TRUE) %>% 
  mutate(Characteristics = "Infant Died") %>% mutate(sort = 40) %>% 
  mutate(ChiSq = 0.462) %>% mutate( df = 1) %>% mutate( pvalue = 0.5)

#Standardize the column name across tables.
names(desc.y40)[names(desc.y40) == "HerbYesData$dead.label"] <- "Level"
names(desc.y40)[names(desc.y40) == "percent"] <- "Yes_Percentage"
names(desc.y40)[names(desc.y40) == "n"] <- "Yes_n"

names(desc.n40)[names(desc.n40) == "HerbNoData$dead.label"] <- "Level_n"
names(desc.n40)[names(desc.n40) == "percent"] <- "No_Percentage"
names(desc.n40)[names(desc.n40) == "n"] <- "No_n"

# Select columns of interest
desc.y40 <- desc.y40 %>% select(Level, Yes_n, Yes_Percentage)
desc.n40 <- desc.n40 %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)%>%
  mutate(missing_percent = NA_integer_)

# Bind columns of yes and no
desc40 <- cbind(desc.y40, desc.n40)

# Check for accuracy
desc40

# Remove excess Level_n variable)
desc40 <- desc40 %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.y40, desc.n40)
###################################################################################################
###################################################################################################
###################################################################################################
# COMPILE DESCRIPTIVE TABLE

descriptive01 <- rbind(desc0, desc1)
descriptive02 <- rbind(descriptive01, desc2)
descriptive03 <- rbind(descriptive02, desc3)
descriptive04 <- rbind(descriptive03, desc4)
descriptive05 <- rbind(descriptive04, desc5)
descriptive06 <- rbind(descriptive05, desc6)
descriptive07 <- rbind(descriptive06, desc7)
descriptive08 <- rbind(descriptive07, desc8)
descriptive09 <- rbind(descriptive08, desc10)
descriptive11 <- rbind(descriptive09, desc11)
descriptive12 <- rbind(descriptive11, desc12)
descriptive13 <- rbind(descriptive12, desc13)
descriptive14 <- rbind(descriptive13, desc19)
descriptive15 <- rbind(descriptive14, desc25)
descriptive16 <- rbind(descriptive15, desc26)
descriptive17 <- rbind(descriptive16, desc27)
descriptive18 <- rbind(descriptive17, desc29)
descriptive19 <- rbind(descriptive18, desc30)
descriptive20 <- rbind(descriptive19, desc32)
descriptive21 <- rbind(descriptive20, desc33)
descriptive22 <- rbind(descriptive21, desc34)
descriptive23 <- rbind(descriptive22, desc35)
descriptive24 <- rbind(descriptive23, desc36)
descriptive25 <- rbind(descriptive24, desc38)
descriptive26 <- rbind(descriptive25, desc39)
descriptive <- rbind(descriptive26, desc40)


descriptive <- descriptive %>% select(sort, Characteristics, Level, Yes_n, Yes_Percentage, No_n, No_Percentage, ChiSq, df, pvalue, missing_percent)

descriptive

write.csv(descriptive, "descriptive_table_paige_v02_v2.csv")

rm(descriptive01,descriptive02, descriptive03, descriptive04, descriptive05, descriptive06, descriptive07, descriptive08, 
    descriptive09, descriptive11, descriptive12, descriptive13, descriptive14, descriptive15, descriptive16,
    descriptive17, descriptive18, descriptive19, descriptive20, descriptive21, descriptive22, descriptive23, descriptive24,
   descriptive25, descriptive26, descriptive27, descriptive28, descriptive29, descriptive30, descriptive31, descriptive32,
   descriptive33, descriptive34, descriptive35, descriptive36, descriptive37, descriptive38, descriptive39)

rm(desc0, desc1, desc2, desc3, desc4, desc5, desc6, desc7, desc8, desc10, desc11, desc12, desc13, desc14,
   desc15, desc16, desc17, desc18, desc19, desc20, desc21, desc22, desc23, desc24, desc25, desc26, desc27, desc28,
   desc29, desc30, desc31, desc32, desc33, desc34, desc35, desc36, desc37, desc38, desc39, desc40)
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
# Infant outcomes
HerbData_Cleaned$NICU.yn
HerbData_Cleaned$dead
HerbData_Cleaned$LBW
HerbData_Cleaned$macro

HerbData_Cleaned$infant_infect.yn
HerbData_Cleaned$InfHosp_FlgR

HerbData_Cleaned$neo.trans.yn

# Maternal outcomes
HerbData_Cleaned$postdates.yn

HerbData_Cleaned$Prenat_Tests_GBS
HerbData_Cleaned$ActPrg_RxAntibiotics
HerbData_Cleaned$ActPrg_RxAntifungals
HerbData_Cleaned$ActPrg_RxAntiemetics
HerbData_Cleaned$ActPrg_RxAntihypertensives

HerbData_Cleaned$ipm.trans.yn
HerbData_Cleaned$ppm.trans.yn

HerbData_Cleaned$pharm_induc_aug.yn

HerbData_Cleaned$pharm_blood_avoid_action.yn

HerbData_Cleaned$Blood_Actions_BloodTrans
HerbData_Cleaned$Blood_Actions_DandC

HerbData_Cleaned$MomHosp_FlgR
HerbData_Cleaned$mompost.RT.infect.yn

###################################################################################################
# Infant outcomes
HerbData_Cleaned$NICU.yn
HerbData_Cleaned$dead
HerbData_Cleaned$Breech_WkLast

HerbData_Cleaned$Breech_Turns
HerbData_Cleaned$Breech_VersMidwAtt
HerbData_Cleaned$Breech_VersMidwSuc
HerbData_Cleaned$Breech_VersPhysAtt
HerbData_Cleaned$Breech_VersPhysSuc

HerbData_Cleaned$new_gramsV03
HerbData_Cleaned$LBW
HerbData_Cleaned$macro
HerbData_Cleaned$InfHealth_Probs_SepInf
HerbData_Cleaned$InfHosp_FlgR
HerbData_Cleaned$InfHosp_Probs_SepInf

HerbData_Cleaned$Pos_Concern2_Breech
HerbData_Cleaned$Pos_BabyDeliveryR
HerbData_Cleaned$breech.birth.yn

HerbData_Cleaned$neo.trans.yn


# Maternal outcomes
HerbData_Cleaned$postdates.yn
HerbData_Cleaned$PrvPrg_Occurs_GstGT42wksV02
HerbData_Cleaned$PrvPrg_Occurs_BreechV02

HerbData_Cleaned$Prenat_Tests_GBS
HerbData_Cleaned$ActPrg_RxAntibiotics
HerbData_Cleaned$ActPrg_RxAntifungals
HerbData_Cleaned$ActPrg_RxAntiemetics
HerbData_Cleaned$ActPrg_RxAntihypertensives


HerbData_Cleaned$Intent_LaborRV02
HerbData_Cleaned$birthplaceR2
HerbData_Cleaned$IndAug_FlgR
HerbData_Cleaned$Encour_Induc_ARMpre5V02
HerbData_Cleaned$Encour_Induc_ARMpost5V02
HerbData_Cleaned$Encour_Induc_ProstagV02
HerbData_Cleaned$Encour_Induc_OxytocinV02
HerbData_Cleaned$Encour_Induc_NippleStV02
HerbData_Cleaned$Encour_Induc_CastorV02
HerbData_Cleaned$Encour_Induc_StripMemV02
HerbData_Cleaned$Encour_Induc_IntercrsV02
HerbData_Cleaned$Encour_Induc_EnemaV02
HerbData_Cleaned$Encour_Induc_CaulophV02
HerbData_Cleaned$Encour_Induc_CohoshV02
HerbData_Cleaned$Encour_Induc_PulsatV02
HerbData_Cleaned$Encour_Induc_EvPrimOilV02
HerbData_Cleaned$Encour_InducRV02
HerbData_Cleaned$Encour_Aug_ARMpre5V02
HerbData_Cleaned$Encour_Aug_ARMpost5V02
HerbData_Cleaned$Encour_Aug_ProstagV02
HerbData_Cleaned$Encour_Aug_OxytocinV02
HerbData_Cleaned$Encour_Aug_NippleStV02
HerbData_Cleaned$Encour_Aug_CastorV02
HerbData_Cleaned$Encour_Aug_StripMemV02
HerbData_Cleaned$Encour_Aug_IntercrsV02
HerbData_Cleaned$Encour_Aug_EnemaV02
HerbData_Cleaned$Encour_Aug_CaulophV02
HerbData_Cleaned$Encour_Aug_CohoshV02
HerbData_Cleaned$Encour_Aug_PulsatV02
HerbData_Cleaned$Encour_Aug_EvPrimOilV02
HerbData_Cleaned$Encour_AugRV02
HerbData_Cleaned$Len_EarlyLab
HerbData_Cleaned$Len_Stage1
HerbData_Cleaned$Len_Stage2
HerbData_Cleaned$Len_Stage3
HerbData_Cleaned$Len_Rupture
HerbData_Cleaned$birth_year
HerbData_Cleaned$length.pregV02
HerbData_Cleaned$lengthpregfix

HerbData_Cleaned$lac.sev.global

HerbData_Cleaned$ipm.trans.yn
HerbData_Cleaned$ppm.trans.yn

HerbData_Cleaned$Blood_Avoid_Oxytocin
HerbData_Cleaned$Blood_Avoid_ShepPurse
HerbData_Cleaned$Blood_Avoid_Angelica
HerbData_Cleaned$Blood_Avoid_Methergine
HerbData_Cleaned$Blood_Avoid_Motherwort
HerbData_Cleaned$Blood_Avoid_Other
HerbData_Cleaned$Blood_AvoidR
HerbData_Cleaned$Blood_Loss_ml
HerbData_Cleaned$Blood_Actions_Pitocin
HerbData_Cleaned$Blood_Actions_Methergine
HerbData_Cleaned$Blood_Actions_OtherDrugs
HerbData_Cleaned$Blood_Actions_Herbs
HerbData_Cleaned$Blood_ActionsHerbs
HerbData_Cleaned$Blood_Actions_IVfluids
HerbData_Cleaned$Blood_Actions_FundalM
HerbData_Cleaned$Blood_Actions_NStim
HerbData_Cleaned$Blood_Actions_ExtBiman
HerbData_Cleaned$Blood_Actions_IntBiman
HerbData_Cleaned$Blood_Actions_BloodTrans
HerbData_Cleaned$Blood_Actions_DandC
HerbData_Cleaned$Blood_ActionsR

HerbData_Cleaned$MomHosp_FlgR
HerbData_Cleaned$mompost.RT.infect.yn
HerbData_Cleaned$MomPost_Infect_Yeast
HerbData_Cleaned$MomPost_Infect_Uterine
HerbData_Cleaned$MomPost_InfectR           

###################################################################################################
# Template
chisq.test(HerbData_Cleaned$HerbPresent.yn, HerbData_Cleaned$XXX, correct=FALSE)

# Create a variable to show label instead of number for level
HerbYesData$XXX.label <- as_factor(HerbYesData$XXX)
HerbNoData$XXX.label <- as_factor(HerbNoData$XXX)

# Create table for yes, table for no
desc.yWWW <- tabyl(HerbYesData$XXX.label, sort = TRUE) %>% 
  mutate(Characteristics = "YYYY") %>% mutate(sort = WWW) %>% 
  mutate(ChiSq = 5.55) %>% mutate( df = 5) %>% mutate( pvalue = 0.05) 
desc.nWWW <- tabyl(HerbNoData$XXX.label, sort = TRUE) %>% 
  mutate(Characteristics = "YYYY") %>% mutate(sort = WWW) %>% 
  mutate(ChiSq = 5.55) %>% mutate( df = 5) %>% mutate( pvalue = 0.05)

#Standardize the column name across tables.
names(desc.yWWW)[names(desc.yWWW) == "HerbYesData$XXX.label"] <- "Level"
names(desc.yWWW)[names(desc.yWWW) == "valid_percent"] <- "Yes_Percentage"
names(desc.yWWW)[names(desc.yWWW) == "n"] <- "Yes_n"

names(desc.nWWW)[names(desc.nWWW) == "HerbNoData$XXX.label"] <- "Level_n"
names(desc.nWWW)[names(desc.nWWW) == "valid_percent"] <- "No_Percentage"
names(desc.nWWW)[names(desc.nWWW) == "n"] <- "No_n"

# Select columns of interest
desc.yWWW <- desc.yWWW %>% select(Level, Yes_n, Yes_Percentage)
desc.nWWW <- desc.nWWW %>% select(Level_n, No_n, No_Percentage, Characteristics, sort, ChiSq, df, pvalue)

# Bind columns of yes and no
descWWW <- cbind(desc.yWWW, desc.nWWW)

# Check for accuracy
descWWW

# Remove excess Level_n variable)
descWWW <- descWWW %>% select(-Level_n) %>% filter(Yes_Percentage > 0)

# Remove excess tables
rm(desc.yWWW, desc.nWWW)
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
