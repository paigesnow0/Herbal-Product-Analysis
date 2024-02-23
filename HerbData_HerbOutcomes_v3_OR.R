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
####################################################################################################################################################
####################################################################################################################################################
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

# Go through
# HerbData_Text_Mining_v2 first
# HerbData_Cleaning_Management_v2 second

# Import cleaned data:
HerbData_Cleaned <- read.csv("~/Documents/MPH School/Field Experience/herb files/HerbData/HerbData_Cleaned_paige_V02.csv", stringsAsFactors=FALSE)

View(HerbData_Cleaned)
####################################################################################################################################################
####################################################################################################################################################
# MUST COPY AND PASTE -> ENTER PVALUES MANUALLY
# MUST CHECK WHETHER TO USE PERCENT or VALID_PERCENT IN TABLE AND CHANGE CODE ACCORDINGLY
# PERINEAL TRAUMA AND BLOOD LOSS -> VAGINAL BIRTHS ONLY -> FILTER OR USE HerbData_Cleaned_NoCsec
# If a row is missing from tabyl, use this:
XXX01 <- add_row(XXX01, Levelb = 1, d_UUU = 0, p_UUU = 0)
# USE Find: herb flag, to go to the top of each herb
# Find the template for each herb at the bottom of the document - replace herb variable where XXX, replace herb name where YYY
####################################################################################################################################################
####################################################################################################################################################

# All chi square tests in one place...
# primr.yn
# HERB FLAG
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$primr.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "percent"] <- "p_cesarean"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_cesarean"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "valid_percent"] <- "p_cesarean"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01 %>% mutate(Herb = "Evening Primrose Oil") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02 %>% mutate(Herb = "Evening Primrose Oil") %>% mutate(Level = "No") %>% filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(Herb, Level, Levelb, Levelb, n_cesarean, p_cesarean, pvalue )
primr.yn02 <- primr.yn02 %>% select(Herb, Level, Levelb, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
primr.yn101 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$primr.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "valid_percent"] <- "p_m_infection"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_m_infection"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "valid_percent"] <- "p_m_infection"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000089) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
primr.yn102 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$primr.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "valid_percent"] <- "p_m_hospital"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_m_hospital"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "valid_percent"] <- "p_m_hospital"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
primr.yn103 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$primr.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "valid_percent"] <- "p_peri_trauma"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_peri_trauma"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "valid_percent"] <- "p_peri_trauma"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.000017) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
primr.yn104 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$primr.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_blood_loss_1L"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.02) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
primr.yn105 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn105
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$primr.yn, HerbData_Cleaned$Blood_Actions_BloodTrans, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_blood_transfusion"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.09) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
primr.yn106 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$primr.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "percent"] <- "p_m_i_transport"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_m_i_transport"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "percent"] <- "p_m_i_transport"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
primr.yn107 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn107
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$primr.yn, HerbData_Cleaned$infant_infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "valid_percent"] <- "p_i_infection"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_i_infection"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "valid_percent"] <- "p_i_infection"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.33) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
primr.yn108 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$primr.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_i_hospitalized"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
primr.yn109 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$primr.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "valid_percent"] <- "p_nicu"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_nicu"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "valid_percent"] <- "p_nicu"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000012) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_nicu, p_nicu, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
primr.yn110 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$primr.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(primr.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(primr.yn == 0)
# Calculate n and percentage of outcome by herb exposure
primr.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
primr.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
primr.yn01 <- primr.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn02 <- primr.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
primr.yn01
primr.yn02
#Standardize the column name across tables.
names(primr.yn01)[names(primr.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(primr.yn01)[names(primr.yn01) == "percent"] <- "p_dead"
names(primr.yn01)[names(primr.yn01) == "n"] <- "n_dead"
names(primr.yn02)[names(primr.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(primr.yn02)[names(primr.yn02) == "percent"] <- "p_dead"
names(primr.yn02)[names(primr.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
primr.yn01 <- primr.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
primr.yn02 <- primr.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
primr.yn01 <- primr.yn01 %>% select(n_dead, p_dead, pvalue )
primr.yn02 <- primr.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
primr.yn111 <- rbind(primr.yn01, primr.yn02)
# Delete unnecessary tables
rm(primr.yn01, primr.yn02, HerbYesData01, HerbNoData01)
# Display final table
primr.yn111

#####
# Join all tables for one herb 
primr.yn010 <- cbind(primr.yn101, primr.yn102)
primr.yn009 <- cbind(primr.yn010, primr.yn103)
primr.yn008 <- cbind(primr.yn009, primr.yn104)
primr.yn007 <- cbind(primr.yn008, primr.yn105)
primr.yn006 <- cbind(primr.yn007, primr.yn106)
primr.yn005 <- cbind(primr.yn006, primr.yn107)
primr.yn004 <- cbind(primr.yn005, primr.yn108)
primr.yn003 <- cbind(primr.yn004, primr.yn109)
primr.yn002 <- cbind(primr.yn003, primr.yn110)
primr.yn001 <- cbind(primr.yn002, primr.yn111)

rm(primr.yn101, primr.yn102, primr.yn103, primr.yn104, primr.yn105, primr.yn106, primr.yn107, primr.yn108, primr.yn109, primr.yn110, primr.yn111)
rm(primr.yn002, primr.yn003, primr.yn004, primr.yn005, primr.yn006, primr.yn007, primr.yn008, primr.yn009, primr.yn010)

primr.yn001
####################################################################################################################################################
####################################################################################################################################################
# rasp.yn
# HERB FLAG
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$rasp.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "valid_percent"] <- "p_cesarean"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_cesarean"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "valid_percent"] <- "p_cesarean"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = "Raspberry Leaf Tea") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.048) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = "Raspberry Leaf Tea") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(Herb, Level, Levelb, Levelb, n_cesarean, p_cesarean, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(Herb, Level, Levelb, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
rasp.yn101 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$rasp.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "valid_percent"] <- "p_m_infection"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_m_infection"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "valid_percent"] <- "p_m_infection"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.21) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
rasp.yn102 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$rasp.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "valid_percent"] <- "p_m_hospital"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_m_hospital"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "valid_percent"] <- "p_m_hospital"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.02) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
rasp.yn103 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$rasp.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "valid_percent"] <- "p_peri_trauma"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_peri_trauma"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "valid_percent"] <- "p_peri_trauma"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.88) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
rasp.yn104 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$rasp.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_blood_loss_1L"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.065) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
rasp.yn105 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn105
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$rasp.yn, HerbData_Cleaned$Blood_Actions_BloodTrans, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_blood_transfusion"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.17) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
rasp.yn106 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$rasp.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "percent"] <- "p_m_i_transport"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_m_i_transport"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "percent"] <- "p_m_i_transport"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.031) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
rasp.yn107 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn107
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$rasp.yn, HerbData_Cleaned$infant_infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "valid_percent"] <- "p_i_infection"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_i_infection"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "valid_percent"] <- "p_i_infection"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.44) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
rasp.yn108 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$rasp.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_i_hospitalized"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.12) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
rasp.yn109 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$rasp.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "valid_percent"] <- "p_nicu"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_nicu"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "valid_percent"] <- "p_nicu"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.035) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_nicu, p_nicu, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
rasp.yn110 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$rasp.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(rasp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(rasp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
rasp.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
rasp.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
rasp.yn01 <- rasp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn02 <- rasp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
rasp.yn01
rasp.yn02
#Standardize the column name across tables.
names(rasp.yn01)[names(rasp.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(rasp.yn01)[names(rasp.yn01) == "percent"] <- "p_dead"
names(rasp.yn01)[names(rasp.yn01) == "n"] <- "n_dead"
names(rasp.yn02)[names(rasp.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(rasp.yn02)[names(rasp.yn02) == "percent"] <- "p_dead"
names(rasp.yn02)[names(rasp.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
rasp.yn01 <- rasp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.59) %>% filter(Levelb == 0 | Levelb ==1)
rasp.yn02 <- rasp.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
rasp.yn01 <- rasp.yn01 %>% select(n_dead, p_dead, pvalue )
rasp.yn02 <- rasp.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
rasp.yn111 <- rbind(rasp.yn01, rasp.yn02)
# Delete unnecessary tables
rm(rasp.yn01, rasp.yn02, HerbYesData01, HerbNoData01)
# Display final table
rasp.yn111

#####
# Join all tables for one herb 
rasp.yn010 <- cbind(rasp.yn101, rasp.yn102)
rasp.yn009 <- cbind(rasp.yn010, rasp.yn103)
rasp.yn008 <- cbind(rasp.yn009, rasp.yn104)
rasp.yn007 <- cbind(rasp.yn008, rasp.yn105)
rasp.yn006 <- cbind(rasp.yn007, rasp.yn106)
rasp.yn005 <- cbind(rasp.yn006, rasp.yn107)
rasp.yn004 <- cbind(rasp.yn005, rasp.yn108)
rasp.yn003 <- cbind(rasp.yn004, rasp.yn109)
rasp.yn002 <- cbind(rasp.yn003, rasp.yn110)
rasp.yn001 <- cbind(rasp.yn002, rasp.yn111)

rm(rasp.yn101, rasp.yn102, rasp.yn103, rasp.yn104, rasp.yn105, rasp.yn106, rasp.yn107, rasp.yn108, rasp.yn109, rasp.yn110, rasp.yn111)
rm(rasp.yn002, rasp.yn003, rasp.yn004, rasp.yn005, rasp.yn006, rasp.yn007, rasp.yn008, rasp.yn009, rasp.yn010)

rasp.yn001
####################################################################################################################################################
####################################################################################################################################################
# HERB FLAG
# castor.yn
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$castor.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "percent"] <- "p_cesarean"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_cesarean"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "valid_percent"] <- "p_cesarean"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01 %>% mutate(Herb = "Castor Oil") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02 %>% mutate(Herb = "Castor Oil") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
castor.yn02 <- castor.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
castor.yn101 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$castor.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "valid_percent"] <- "p_m_infection"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_m_infection"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "valid_percent"] <- "p_m_infection"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0052) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
castor.yn102 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$castor.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "valid_percent"] <- "p_m_hospital"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_m_hospital"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "valid_percent"] <- "p_m_hospital"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
castor.yn103 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$castor.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "valid_percent"] <- "p_peri_trauma"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_peri_trauma"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "valid_percent"] <- "p_peri_trauma"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.39) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
castor.yn104 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$castor.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_blood_loss_1L"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.014) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
castor.yn105 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn105
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$castor.yn, HerbData_Cleaned$Blood_Actions_BloodTrans, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_blood_transfusion"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.12) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
castor.yn106 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$castor.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "percent"] <- "p_m_i_transport"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_m_i_transport"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "percent"] <- "p_m_i_transport"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
castor.yn107 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn107
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$castor.yn, HerbData_Cleaned$infant_infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "valid_percent"] <- "p_i_infection"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_i_infection"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "valid_percent"] <- "p_i_infection"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.26) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
castor.yn108 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$castor.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_i_hospitalized"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
castor.yn109 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$castor.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "valid_percent"] <- "p_nicu"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_nicu"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "valid_percent"] <- "p_nicu"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00016) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_nicu, p_nicu, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
castor.yn110 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$castor.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(castor.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(castor.yn == 0)
# Calculate n and percentage of outcome by herb exposure
castor.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
castor.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
castor.yn01 <- castor.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn02 <- castor.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
castor.yn01
castor.yn02
#Standardize the column name across tables.
names(castor.yn01)[names(castor.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(castor.yn01)[names(castor.yn01) == "percent"] <- "p_dead"
names(castor.yn01)[names(castor.yn01) == "n"] <- "n_dead"
names(castor.yn02)[names(castor.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(castor.yn02)[names(castor.yn02) == "percent"] <- "p_dead"
names(castor.yn02)[names(castor.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
castor.yn01 <- castor.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.049) %>% filter(Levelb == 0 | Levelb ==1)
castor.yn02 <- castor.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
castor.yn01 <- castor.yn01 %>% select(n_dead, p_dead, pvalue )
castor.yn02 <- castor.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
castor.yn111 <- rbind(castor.yn01, castor.yn02)
# Delete unnecessary tables
rm(castor.yn01, castor.yn02, HerbYesData01, HerbNoData01)
# Display final table
castor.yn111

#####
# Join all tables for one herb 
castor.yn010 <- cbind(castor.yn101, castor.yn102)
castor.yn009 <- cbind(castor.yn010, castor.yn103)
castor.yn008 <- cbind(castor.yn009, castor.yn104)
castor.yn007 <- cbind(castor.yn008, castor.yn105)
castor.yn006 <- cbind(castor.yn007, castor.yn106)
castor.yn005 <- cbind(castor.yn006, castor.yn107)
castor.yn004 <- cbind(castor.yn005, castor.yn108)
castor.yn003 <- cbind(castor.yn004, castor.yn109)
castor.yn002 <- cbind(castor.yn003, castor.yn110)
castor.yn001 <- cbind(castor.yn002, castor.yn111)

rm(castor.yn101, castor.yn102, castor.yn103, castor.yn104, castor.yn105, castor.yn106, castor.yn107, castor.yn108, castor.yn109, castor.yn110, castor.yn111)
rm(castor.yn002, castor.yn003, castor.yn004, castor.yn005, castor.yn006, castor.yn007, castor.yn008, castor.yn009, castor.yn010)

castor.yn001
####################################################################################################################################################
####################################################################################################################################################
# HERB FLAG
# cauloph.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cauloph.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "valid_percent"] <- "p_cesarean"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_cesarean"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "valid_percent"] <- "p_cesarean"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = "Caulophyllum") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = "Caulophyllum") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
cauloph.yn101 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cauloph.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "valid_percent"] <- "p_m_infection"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_m_infection"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "valid_percent"] <- "p_m_infection"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000077) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
cauloph.yn102 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cauloph.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "valid_percent"] <- "p_m_hospital"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_m_hospital"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "valid_percent"] <- "p_m_hospital"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
cauloph.yn103 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$cauloph.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "valid_percent"] <- "p_peri_trauma"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_peri_trauma"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "valid_percent"] <- "p_peri_trauma"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.000000000000032) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
cauloph.yn104 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$cauloph.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_blood_loss_1L"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.014) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
cauloph.yn105 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn105
####
# Chi-Squared test
fisher.test(HerbData_Cleaned$cauloph.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_blood_transfusion"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0027) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
cauloph.yn106 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cauloph.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "percent"] <- "p_m_i_transport"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_m_i_transport"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "percent"] <- "p_m_i_transport"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
cauloph.yn107 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn107
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cauloph.yn, HerbData_Cleaned$infant_infect.yn, correct=FALSE)


# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "valid_percent"] <- "p_i_infection"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_i_infection"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "valid_percent"] <- "p_i_infection"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.17) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
cauloph.yn108 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cauloph.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_i_hospitalized"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
cauloph.yn109 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cauloph.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "valid_percent"] <- "p_nicu"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_nicu"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "valid_percent"] <- "p_nicu"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000023) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_nicu, p_nicu, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
cauloph.yn110 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$cauloph.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cauloph.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cauloph.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
cauloph.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
cauloph.yn01 <- cauloph.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cauloph.yn01
cauloph.yn02
#Standardize the column name across tables.
names(cauloph.yn01)[names(cauloph.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(cauloph.yn01)[names(cauloph.yn01) == "percent"] <- "p_dead"
names(cauloph.yn01)[names(cauloph.yn01) == "n"] <- "n_dead"
names(cauloph.yn02)[names(cauloph.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(cauloph.yn02)[names(cauloph.yn02) == "percent"] <- "p_dead"
names(cauloph.yn02)[names(cauloph.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cauloph.yn01 <- cauloph.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
cauloph.yn02 <- cauloph.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cauloph.yn01 <- cauloph.yn01 %>% select(n_dead, p_dead, pvalue )
cauloph.yn02 <- cauloph.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
cauloph.yn111 <- rbind(cauloph.yn01, cauloph.yn02)
# Delete unnecessary tables
rm(cauloph.yn01, cauloph.yn02, HerbYesData01, HerbNoData01)
# Display final table
cauloph.yn111

#####
# Join all tables for one herb 
cauloph.yn010 <- cbind(cauloph.yn101, cauloph.yn102)
cauloph.yn009 <- cbind(cauloph.yn010, cauloph.yn103)
cauloph.yn008 <- cbind(cauloph.yn009, cauloph.yn104)
cauloph.yn007 <- cbind(cauloph.yn008, cauloph.yn105)
cauloph.yn006 <- cbind(cauloph.yn007, cauloph.yn106)
cauloph.yn005 <- cbind(cauloph.yn006, cauloph.yn107)
cauloph.yn004 <- cbind(cauloph.yn005, cauloph.yn108)
cauloph.yn003 <- cbind(cauloph.yn004, cauloph.yn109)
cauloph.yn002 <- cbind(cauloph.yn003, cauloph.yn110)
cauloph.yn001 <- cbind(cauloph.yn002, cauloph.yn111)

rm(cauloph.yn101, cauloph.yn102, cauloph.yn103, cauloph.yn104, cauloph.yn105, cauloph.yn106, cauloph.yn107, cauloph.yn108, cauloph.yn109, cauloph.yn110, cauloph.yn111)
rm(cauloph.yn002, cauloph.yn003, cauloph.yn004, cauloph.yn005, cauloph.yn006, cauloph.yn007, cauloph.yn008, cauloph.yn009, cauloph.yn010)

cauloph.yn001
####################################################################################################################################################
####################################################################################################################################################
# HERB FLAG
# shepp.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$shepp.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "valid_percent"] <- "p_cesarean"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_cesarean"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "valid_percent"] <- "p_cesarean"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- add_row(shepp.yn01, Levelb =1, n_cesarean = 0, p_cesarean = 0)
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = "Shepherd's Purse") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000000000013) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = "Shepherd's Purse") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
shepp.yn101 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$shepp.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "valid_percent"] <- "p_m_infection"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_m_infection"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "valid_percent"] <- "p_m_infection"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.5) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
shepp.yn102 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$shepp.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "percent"] <- "p_m_hospital"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_m_hospital"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "valid_percent"] <- "p_m_hospital"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.000079) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
shepp.yn103 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$shepp.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "valid_percent"] <- "p_peri_trauma"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_peri_trauma"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "valid_percent"] <- "p_peri_trauma"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.16) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
shepp.yn104 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$shepp.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_blood_loss_1L"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
shepp.yn105 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn105
####
# Fisher's test
fisher.test(HerbData_Cleaned$shepp.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "percent"] <- "p_blood_transfusion"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_blood_transfusion"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0007) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
shepp.yn106 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$shepp.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "percent"] <- "p_m_i_transport"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_m_i_transport"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "percent"] <- "p_m_i_transport"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.000000000023) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
shepp.yn107 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$shepp.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "valid_percent"] <- "p_i_infection"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_i_infection"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "valid_percent"] <- "p_i_infection"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.23) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
shepp.yn108 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$shepp.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "percent"] <- "p_i_hospitalized"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_i_hospitalized"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000015) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
shepp.yn109 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$shepp.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "valid_percent"] <- "p_nicu"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_nicu"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "valid_percent"] <- "p_nicu"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.095) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_nicu, p_nicu, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
shepp.yn110 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$shepp.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(shepp.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(shepp.yn == 0)
# Calculate n and percentage of outcome by herb exposure
shepp.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
shepp.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
shepp.yn01 <- shepp.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn02 <- shepp.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
shepp.yn01
shepp.yn02
#Standardize the column name across tables.
names(shepp.yn01)[names(shepp.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(shepp.yn01)[names(shepp.yn01) == "percent"] <- "p_dead"
names(shepp.yn01)[names(shepp.yn01) == "n"] <- "n_dead"
names(shepp.yn02)[names(shepp.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(shepp.yn02)[names(shepp.yn02) == "percent"] <- "p_dead"
names(shepp.yn02)[names(shepp.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
shepp.yn01 <- shepp.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
shepp.yn02 <- shepp.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
shepp.yn01 <- shepp.yn01 %>% select(n_dead, p_dead, pvalue )
shepp.yn02 <- shepp.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
shepp.yn111 <- rbind(shepp.yn01, shepp.yn02)
# Delete unnecessary tables
rm(shepp.yn01, shepp.yn02, HerbYesData01, HerbNoData01)
# Display final table
shepp.yn111

#####
# Join all tables for one herb 
shepp.yn010 <- cbind(shepp.yn101, shepp.yn102)
shepp.yn009 <- cbind(shepp.yn010, shepp.yn103)
shepp.yn008 <- cbind(shepp.yn009, shepp.yn104)
shepp.yn007 <- cbind(shepp.yn008, shepp.yn105)
shepp.yn006 <- cbind(shepp.yn007, shepp.yn106)
shepp.yn005 <- cbind(shepp.yn006, shepp.yn107)
shepp.yn004 <- cbind(shepp.yn005, shepp.yn108)
shepp.yn003 <- cbind(shepp.yn004, shepp.yn109)
shepp.yn002 <- cbind(shepp.yn003, shepp.yn110)
shepp.yn001 <- cbind(shepp.yn002, shepp.yn111)

rm(shepp.yn101, shepp.yn102, shepp.yn103, shepp.yn104, shepp.yn105, shepp.yn106, shepp.yn107, shepp.yn108, shepp.yn109, shepp.yn110, shepp.yn111)
rm(shepp.yn002, shepp.yn003, shepp.yn004, shepp.yn005, shepp.yn006, shepp.yn007, shepp.yn008, shepp.yn009, shepp.yn010)

shepp.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# angelica.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$angelica.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "percent"] <- "p_cesarean"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_cesarean"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "valid_percent"] <- "p_cesarean"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- add_row(angelica.yn01, Levelb =1, n_cesarean = 0, p_cesarean = 0)
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = "Angelica") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000000000019) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = "Angelica") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
angelica.yn101 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$angelica.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "percent"] <- "p_m_infection"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_m_infection"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "valid_percent"] <- "p_m_infection"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.91) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
angelica.yn102 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$angelica.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "percent"] <- "p_m_hospital"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_m_hospital"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "valid_percent"] <- "p_m_hospital"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.55) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
angelica.yn103 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$angelica.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "valid_percent"] <- "p_peri_trauma"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_peri_trauma"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "valid_percent"] <- "p_peri_trauma"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.063) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
angelica.yn104 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$angelica.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_blood_loss_1L"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
angelica.yn105 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$angelica.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "percent"] <- "p_blood_transfusion"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_blood_transfusion"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000063) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
angelica.yn106 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$angelica.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "percent"] <- "p_m_i_transport"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_m_i_transport"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "percent"] <- "p_m_i_transport"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000044) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
angelica.yn107 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn107
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$angelica.yn, HerbData_Cleaned$infant_infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "valid_percent"] <- "p_i_infection"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_i_infection"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "valid_percent"] <- "p_i_infection"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.74) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
angelica.yn108 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$angelica.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "percent"] <- "p_i_hospitalized"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_i_hospitalized"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000000027) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
angelica.yn109 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$angelica.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "percent"] <- "p_nicu"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_nicu"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "valid_percent"] <- "p_nicu"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.069) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_nicu, p_nicu, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
angelica.yn110 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$angelica.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(angelica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(angelica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
angelica.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
angelica.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
angelica.yn01 <- angelica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn02 <- angelica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
angelica.yn01
angelica.yn02
#Standardize the column name across tables.
names(angelica.yn01)[names(angelica.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(angelica.yn01)[names(angelica.yn01) == "percent"] <- "p_dead"
names(angelica.yn01)[names(angelica.yn01) == "n"] <- "n_dead"
names(angelica.yn02)[names(angelica.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(angelica.yn02)[names(angelica.yn02) == "percent"] <- "p_dead"
names(angelica.yn02)[names(angelica.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
angelica.yn01 <- angelica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
angelica.yn02 <- angelica.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
angelica.yn01 <- angelica.yn01 %>% select(n_dead, p_dead, pvalue )
angelica.yn02 <- angelica.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
angelica.yn111 <- rbind(angelica.yn01, angelica.yn02)
# Delete unnecessary tables
rm(angelica.yn01, angelica.yn02, HerbYesData01, HerbNoData01)
# Display final table
angelica.yn111

#####
# Join all tables for one herb 
angelica.yn010 <- cbind(angelica.yn101, angelica.yn102)
angelica.yn009 <- cbind(angelica.yn010, angelica.yn103)
angelica.yn008 <- cbind(angelica.yn009, angelica.yn104)
angelica.yn007 <- cbind(angelica.yn008, angelica.yn105)
angelica.yn006 <- cbind(angelica.yn007, angelica.yn106)
angelica.yn005 <- cbind(angelica.yn006, angelica.yn107)
angelica.yn004 <- cbind(angelica.yn005, angelica.yn108)
angelica.yn003 <- cbind(angelica.yn004, angelica.yn109)
angelica.yn002 <- cbind(angelica.yn003, angelica.yn110)
angelica.yn001 <- cbind(angelica.yn002, angelica.yn111)

rm(angelica.yn101, angelica.yn102, angelica.yn103, angelica.yn104, angelica.yn105, angelica.yn106, angelica.yn107, angelica.yn108, angelica.yn109, angelica.yn110, angelica.yn111)
rm(angelica.yn002, angelica.yn003, angelica.yn004, angelica.yn005, angelica.yn006, angelica.yn007, angelica.yn008, angelica.yn009, angelica.yn010)

angelica.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# alfa.yn
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$alfa.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "valid_percent"] <- "p_cesarean"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_cesarean"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "valid_percent"] <- "p_cesarean"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = "Alfalfa") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.26) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = "Alfalfa") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
alfa.yn101 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$alfa.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "valid_percent"] <- "p_m_infection"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_m_infection"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "valid_percent"] <- "p_m_infection"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.85) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
alfa.yn102 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$alfa.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "percent"] <- "p_m_hospital"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_m_hospital"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "valid_percent"] <- "p_m_hospital"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.34) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
alfa.yn103 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$alfa.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "percent"] <- "p_peri_trauma"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_peri_trauma"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "valid_percent"] <- "p_peri_trauma"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.41) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
alfa.yn104 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$alfa.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_blood_loss_1L"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.86) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
alfa.yn105 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$alfa.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_blood_transfusion"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.013) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
alfa.yn106 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$alfa.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "percent"] <- "p_m_i_transport"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_m_i_transport"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "percent"] <- "p_m_i_transport"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.33) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
alfa.yn107 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn107
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$alfa.yn, HerbData_Cleaned$infant_infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "valid_percent"] <- "p_i_infection"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_i_infection"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "valid_percent"] <- "p_i_infection"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.7) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
alfa.yn108 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$alfa.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_i_hospitalized"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.5) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
alfa.yn109 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$alfa.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "valid_percent"] <- "p_nicu"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_nicu"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "valid_percent"] <- "p_nicu"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.78) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_nicu, p_nicu, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
alfa.yn110 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$alfa.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(alfa.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(alfa.yn == 0)
# Calculate n and percentage of outcome by herb exposure
alfa.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
alfa.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
alfa.yn01 <- alfa.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn02 <- alfa.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
alfa.yn01
alfa.yn02
#Standardize the column name across tables.
names(alfa.yn01)[names(alfa.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(alfa.yn01)[names(alfa.yn01) == "percent"] <- "p_dead"
names(alfa.yn01)[names(alfa.yn01) == "n"] <- "n_dead"
names(alfa.yn02)[names(alfa.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(alfa.yn02)[names(alfa.yn02) == "percent"] <- "p_dead"
names(alfa.yn02)[names(alfa.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
alfa.yn01 <- alfa.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.66) %>% filter(Levelb == 0 | Levelb ==1)
alfa.yn02 <- alfa.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
alfa.yn01 <- alfa.yn01 %>% select(n_dead, p_dead, pvalue )
alfa.yn02 <- alfa.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
alfa.yn111 <- rbind(alfa.yn01, alfa.yn02)
# Delete unnecessary tables
rm(alfa.yn01, alfa.yn02, HerbYesData01, HerbNoData01)
# Display final table
alfa.yn111

#####
# Join all tables for one herb 
alfa.yn010 <- cbind(alfa.yn101, alfa.yn102)
alfa.yn009 <- cbind(alfa.yn010, alfa.yn103)
alfa.yn008 <- cbind(alfa.yn009, alfa.yn104)
alfa.yn007 <- cbind(alfa.yn008, alfa.yn105)
alfa.yn006 <- cbind(alfa.yn007, alfa.yn106)
alfa.yn005 <- cbind(alfa.yn006, alfa.yn107)
alfa.yn004 <- cbind(alfa.yn005, alfa.yn108)
alfa.yn003 <- cbind(alfa.yn004, alfa.yn109)
alfa.yn002 <- cbind(alfa.yn003, alfa.yn110)
alfa.yn001 <- cbind(alfa.yn002, alfa.yn111)

rm(alfa.yn101, alfa.yn102, alfa.yn103, alfa.yn104, alfa.yn105, alfa.yn106, alfa.yn107, alfa.yn108, alfa.yn109, alfa.yn110, alfa.yn111)
rm(alfa.yn002, alfa.yn003, alfa.yn004, alfa.yn005, alfa.yn006, alfa.yn007, alfa.yn008, alfa.yn009, alfa.yn010)

alfa.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# nettle.yn
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$nettle.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "percent"] <- "p_cesarean"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_cesarean"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "valid_percent"] <- "p_cesarean"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = "Nettles") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.79) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = "Nettles") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
nettle.yn101 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$nettle.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "valid_percent"] <- "p_m_infection"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_m_infection"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "valid_percent"] <- "p_m_infection"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.55) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
nettle.yn102 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$nettle.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "percent"] <- "p_m_hospital"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_m_hospital"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "valid_percent"] <- "p_m_hospital"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.02) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
nettle.yn103 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$nettle.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "percent"] <- "p_peri_trauma"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_peri_trauma"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "valid_percent"] <- "p_peri_trauma"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000029) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
nettle.yn104 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$nettle.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_blood_loss_1L"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.15) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
nettle.yn105 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$nettle.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_blood_transfusion"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.21) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
nettle.yn106 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$nettle.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "percent"] <- "p_m_i_transport"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_m_i_transport"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "percent"] <- "p_m_i_transport"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.12) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
nettle.yn107 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn107
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$nettle.yn, HerbData_Cleaned$infant_infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "valid_percent"] <- "p_i_infection"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_i_infection"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "valid_percent"] <- "p_i_infection"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.21) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
nettle.yn108 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$nettle.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "percent"] <- "p_i_hospitalized"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_i_hospitalized"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0079) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
nettle.yn109 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$nettle.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "valid_percent"] <- "p_nicu"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_nicu"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "valid_percent"] <- "p_nicu"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0058) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_nicu, p_nicu, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
nettle.yn110 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$nettle.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(nettle.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(nettle.yn == 0)
# Calculate n and percentage of outcome by herb exposure
nettle.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
nettle.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
nettle.yn01 <- nettle.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn02 <- nettle.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
nettle.yn01
nettle.yn02
#Standardize the column name across tables.
names(nettle.yn01)[names(nettle.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(nettle.yn01)[names(nettle.yn01) == "percent"] <- "p_dead"
names(nettle.yn01)[names(nettle.yn01) == "n"] <- "n_dead"
names(nettle.yn02)[names(nettle.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(nettle.yn02)[names(nettle.yn02) == "percent"] <- "p_dead"
names(nettle.yn02)[names(nettle.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
nettle.yn01 <- nettle.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.4) %>% filter(Levelb == 0 | Levelb ==1)
nettle.yn02 <- nettle.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
nettle.yn01 <- nettle.yn01 %>% select(n_dead, p_dead, pvalue )
nettle.yn02 <- nettle.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
nettle.yn111 <- rbind(nettle.yn01, nettle.yn02)
# Delete unnecessary tables
rm(nettle.yn01, nettle.yn02, HerbYesData01, HerbNoData01)
# Display final table
nettle.yn111

#####
# Join all tables for one herb 
nettle.yn010 <- cbind(nettle.yn101, nettle.yn102)
nettle.yn009 <- cbind(nettle.yn010, nettle.yn103)
nettle.yn008 <- cbind(nettle.yn009, nettle.yn104)
nettle.yn007 <- cbind(nettle.yn008, nettle.yn105)
nettle.yn006 <- cbind(nettle.yn007, nettle.yn106)
nettle.yn005 <- cbind(nettle.yn006, nettle.yn107)
nettle.yn004 <- cbind(nettle.yn005, nettle.yn108)
nettle.yn003 <- cbind(nettle.yn004, nettle.yn109)
nettle.yn002 <- cbind(nettle.yn003, nettle.yn110)
nettle.yn001 <- cbind(nettle.yn002, nettle.yn111)

rm(nettle.yn101, nettle.yn102, nettle.yn103, nettle.yn104, nettle.yn105, nettle.yn106, nettle.yn107, nettle.yn108, nettle.yn109, nettle.yn110, nettle.yn111)
rm(nettle.yn002, nettle.yn003, nettle.yn004, nettle.yn005, nettle.yn006, nettle.yn007, nettle.yn008, nettle.yn009, nettle.yn010)

nettle.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# flordx.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$flordx.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "percent"] <- "p_cesarean"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_cesarean"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "valid_percent"] <- "p_cesarean"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = "Floradix") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.1) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = "Floradix") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
flordx.yn101 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$flordx.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "percent"] <- "p_m_infection"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_m_infection"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "valid_percent"] <- "p_m_infection"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.48) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
flordx.yn102 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$flordx.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "percent"] <- "p_m_hospital"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_m_hospital"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "valid_percent"] <- "p_m_hospital"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.4) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
flordx.yn103 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$flordx.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "percent"] <- "p_peri_trauma"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_peri_trauma"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "valid_percent"] <- "p_peri_trauma"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00099) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
flordx.yn104 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$flordx.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_blood_loss_1L"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.021) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
flordx.yn105 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$flordx.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_blood_transfusion"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.25) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
flordx.yn106 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$flordx.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "percent"] <- "p_m_i_transport"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_m_i_transport"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "percent"] <- "p_m_i_transport"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.23) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
flordx.yn107 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$flordx.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "valid_percent"] <- "p_i_infection"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_i_infection"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "valid_percent"] <- "p_i_infection"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.77) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
flordx.yn108 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$flordx.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "percent"] <- "p_i_hospitalized"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_i_hospitalized"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.14) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
flordx.yn109 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$flordx.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "valid_percent"] <- "p_nicu"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_nicu"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "valid_percent"] <- "p_nicu"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.014) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_nicu, p_nicu, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
flordx.yn110 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$flordx.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(flordx.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(flordx.yn == 0)
# Calculate n and percentage of outcome by herb exposure
flordx.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
flordx.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
flordx.yn01 <- flordx.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn02 <- flordx.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
flordx.yn01
flordx.yn02
#Standardize the column name across tables.
names(flordx.yn01)[names(flordx.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(flordx.yn01)[names(flordx.yn01) == "percent"] <- "p_dead"
names(flordx.yn01)[names(flordx.yn01) == "n"] <- "n_dead"
names(flordx.yn02)[names(flordx.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(flordx.yn02)[names(flordx.yn02) == "percent"] <- "p_dead"
names(flordx.yn02)[names(flordx.yn02) == "n"] <- "n_dead"

flordx.yn01 <- add_row(flordx.yn01, Levelb =1, n_dead = 0, p_dead = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
flordx.yn01 <- flordx.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
flordx.yn02 <- flordx.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
flordx.yn01 <- flordx.yn01 %>% select(n_dead, p_dead, pvalue )
flordx.yn02 <- flordx.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
flordx.yn111 <- rbind(flordx.yn01, flordx.yn02)
# Delete unnecessary tables
rm(flordx.yn01, flordx.yn02, HerbYesData01, HerbNoData01)
# Display final table
flordx.yn111

#####
# Join all tables for one herb 
flordx.yn010 <- cbind(flordx.yn101, flordx.yn102)
flordx.yn009 <- cbind(flordx.yn010, flordx.yn103)
flordx.yn008 <- cbind(flordx.yn009, flordx.yn104)
flordx.yn007 <- cbind(flordx.yn008, flordx.yn105)
flordx.yn006 <- cbind(flordx.yn007, flordx.yn106)
flordx.yn005 <- cbind(flordx.yn006, flordx.yn107)
flordx.yn004 <- cbind(flordx.yn005, flordx.yn108)
flordx.yn003 <- cbind(flordx.yn004, flordx.yn109)
flordx.yn002 <- cbind(flordx.yn003, flordx.yn110)
flordx.yn001 <- cbind(flordx.yn002, flordx.yn111)

rm(flordx.yn101, flordx.yn102, flordx.yn103, flordx.yn104, flordx.yn105, flordx.yn106, flordx.yn107, flordx.yn108, flordx.yn109, flordx.yn110, flordx.yn111)
rm(flordx.yn002, flordx.yn003, flordx.yn004, flordx.yn005, flordx.yn006, flordx.yn007, flordx.yn008, flordx.yn009, flordx.yn010)

flordx.yn001
##############################################################################################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# motherwort.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$motherwort.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "percent"] <- "p_cesarean"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_cesarean"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "valid_percent"] <- "p_cesarean"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = "Motherwort") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00014) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = "Motherwort") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
motherwort.yn101 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$motherwort.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "percent"] <- "p_m_infection"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_m_infection"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "valid_percent"] <- "p_m_infection"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0058) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
motherwort.yn102 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$motherwort.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "percent"] <- "p_m_hospital"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_m_hospital"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "valid_percent"] <- "p_m_hospital"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.47) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
motherwort.yn103 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$motherwort.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "percent"] <- "p_peri_trauma"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_peri_trauma"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "valid_percent"] <- "p_peri_trauma"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.36) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
motherwort.yn104 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$motherwort.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_blood_loss_1L"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
motherwort.yn105 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$motherwort.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "percent"] <- "p_blood_transfusion"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_blood_transfusion"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.18) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
motherwort.yn106 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$motherwort.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "percent"] <- "p_m_i_transport"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_m_i_transport"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "percent"] <- "p_m_i_transport"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0012) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
motherwort.yn107 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$motherwort.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "valid_percent"] <- "p_i_infection"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_i_infection"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "valid_percent"] <- "p_i_infection"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.45) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
motherwort.yn108 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$motherwort.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_i_hospitalized"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0013) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
motherwort.yn109 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$motherwort.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "valid_percent"] <- "p_nicu"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_nicu"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "valid_percent"] <- "p_nicu"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.086) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_nicu, p_nicu, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
motherwort.yn110 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$motherwort.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(motherwort.yn == 0)
# Calculate n and percentage of outcome by herb exposure
motherwort.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
motherwort.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
motherwort.yn01 <- motherwort.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
motherwort.yn01
motherwort.yn02
#Standardize the column name across tables.
names(motherwort.yn01)[names(motherwort.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(motherwort.yn01)[names(motherwort.yn01) == "percent"] <- "p_dead"
names(motherwort.yn01)[names(motherwort.yn01) == "n"] <- "n_dead"
names(motherwort.yn02)[names(motherwort.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(motherwort.yn02)[names(motherwort.yn02) == "percent"] <- "p_dead"
names(motherwort.yn02)[names(motherwort.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
motherwort.yn01 <- motherwort.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.5) %>% filter(Levelb == 0 | Levelb ==1)
motherwort.yn02 <- motherwort.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
motherwort.yn01 <- motherwort.yn01 %>% select(n_dead, p_dead, pvalue )
motherwort.yn02 <- motherwort.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
motherwort.yn111 <- rbind(motherwort.yn01, motherwort.yn02)
# Delete unnecessary tables
rm(motherwort.yn01, motherwort.yn02, HerbYesData01, HerbNoData01)
# Display final table
motherwort.yn111

#####
# Join all tables for one herb 
motherwort.yn010 <- cbind(motherwort.yn101, motherwort.yn102)
motherwort.yn009 <- cbind(motherwort.yn010, motherwort.yn103)
motherwort.yn008 <- cbind(motherwort.yn009, motherwort.yn104)
motherwort.yn007 <- cbind(motherwort.yn008, motherwort.yn105)
motherwort.yn006 <- cbind(motherwort.yn007, motherwort.yn106)
motherwort.yn005 <- cbind(motherwort.yn006, motherwort.yn107)
motherwort.yn004 <- cbind(motherwort.yn005, motherwort.yn108)
motherwort.yn003 <- cbind(motherwort.yn004, motherwort.yn109)
motherwort.yn002 <- cbind(motherwort.yn003, motherwort.yn110)
motherwort.yn001 <- cbind(motherwort.yn002, motherwort.yn111)

rm(motherwort.yn101, motherwort.yn102, motherwort.yn103, motherwort.yn104, motherwort.yn105, motherwort.yn106, motherwort.yn107, motherwort.yn108, motherwort.yn109, motherwort.yn110, motherwort.yn111)
rm(motherwort.yn002, motherwort.yn003, motherwort.yn004, motherwort.yn005, motherwort.yn006, motherwort.yn007, motherwort.yn008, motherwort.yn009, motherwort.yn010)

motherwort.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# dande.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$dande.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "valid_percent"] <- "p_cesarean"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_cesarean"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "valid_percent"] <- "p_cesarean"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01 %>% mutate(Herb = "Dandelion") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.42) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02 %>% mutate(Herb = "Dandelion") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
dande.yn02 <- dande.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
dande.yn101 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$dande.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "percent"] <- "p_m_infection"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_m_infection"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "valid_percent"] <- "p_m_infection"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.59) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
dande.yn102 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$dande.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "percent"] <- "p_m_hospital"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_m_hospital"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "valid_percent"] <- "p_m_hospital"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.76) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
dande.yn103 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$dande.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "percent"] <- "p_peri_trauma"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_peri_trauma"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "valid_percent"] <- "p_peri_trauma"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.41) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
dande.yn104 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$dande.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_blood_loss_1L"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.31) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
dande.yn105 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$dande.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_blood_transfusion"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.66) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
dande.yn106 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$dande.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "percent"] <- "p_m_i_transport"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_m_i_transport"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "percent"] <- "p_m_i_transport"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.63) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
dande.yn107 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn107
####

# Fisher's Test
fisher.test(HerbData_Cleaned$dande.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "valid_percent"] <- "p_i_infection"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_i_infection"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "valid_percent"] <- "p_i_infection"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.76) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
dande.yn108 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$dande.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_i_hospitalized"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.56) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
dande.yn109 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$dande.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "valid_percent"] <- "p_nicu"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_nicu"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "valid_percent"] <- "p_nicu"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.68) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_nicu, p_nicu, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
dande.yn110 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$dande.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(dande.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(dande.yn == 0)
# Calculate n and percentage of outcome by herb exposure
dande.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
dande.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
dande.yn01 <- dande.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn02 <- dande.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
dande.yn01
dande.yn02
#Standardize the column name across tables.
names(dande.yn01)[names(dande.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(dande.yn01)[names(dande.yn01) == "percent"] <- "p_dead"
names(dande.yn01)[names(dande.yn01) == "n"] <- "n_dead"
names(dande.yn02)[names(dande.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(dande.yn02)[names(dande.yn02) == "percent"] <- "p_dead"
names(dande.yn02)[names(dande.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
dande.yn01 <- dande.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.5) %>% filter(Levelb == 0 | Levelb ==1)
dande.yn02 <- dande.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
dande.yn01 <- dande.yn01 %>% select(n_dead, p_dead, pvalue )
dande.yn02 <- dande.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
dande.yn111 <- rbind(dande.yn01, dande.yn02)
# Delete unnecessary tables
rm(dande.yn01, dande.yn02, HerbYesData01, HerbNoData01)
# Display final table
dande.yn111

#####
# Join all tables for one herb 
dande.yn010 <- cbind(dande.yn101, dande.yn102)
dande.yn009 <- cbind(dande.yn010, dande.yn103)
dande.yn008 <- cbind(dande.yn009, dande.yn104)
dande.yn007 <- cbind(dande.yn008, dande.yn105)
dande.yn006 <- cbind(dande.yn007, dande.yn106)
dande.yn005 <- cbind(dande.yn006, dande.yn107)
dande.yn004 <- cbind(dande.yn005, dande.yn108)
dande.yn003 <- cbind(dande.yn004, dande.yn109)
dande.yn002 <- cbind(dande.yn003, dande.yn110)
dande.yn001 <- cbind(dande.yn002, dande.yn111)

rm(dande.yn101, dande.yn102, dande.yn103, dande.yn104, dande.yn105, dande.yn106, dande.yn107, dande.yn108, dande.yn109, dande.yn110, dande.yn111)
rm(dande.yn002, dande.yn003, dande.yn004, dande.yn005, dande.yn006, dande.yn007, dande.yn008, dande.yn009, dande.yn010)

dande.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# echina.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$echina.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "valid_percent"] <- "p_cesarean"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_cesarean"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "valid_percent"] <- "p_cesarean"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01 %>% mutate(Herb = "Echinacea") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.57) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02 %>% mutate(Herb = "Echinacea") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
echina.yn02 <- echina.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
echina.yn101 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$echina.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "valid_percent"] <- "p_m_infection"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_m_infection"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "valid_percent"] <- "p_m_infection"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.23) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
echina.yn102 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$echina.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "percent"] <- "p_m_hospital"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_m_hospital"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "valid_percent"] <- "p_m_hospital"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.85) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
echina.yn103 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$echina.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "percent"] <- "p_peri_trauma"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_peri_trauma"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "valid_percent"] <- "p_peri_trauma"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.42) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
echina.yn104 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$echina.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_blood_loss_1L"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.091) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
echina.yn105 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$echina.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_blood_transfusion"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_blood_transfusion"

echina.yn01 <- add_row(echina.yn01, Levelb = 1, n_blood_transfusion = 0, p_blood_transfusion = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.64) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
echina.yn106 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$echina.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "percent"] <- "p_m_i_transport"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_m_i_transport"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "percent"] <- "p_m_i_transport"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.6) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
echina.yn107 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$echina.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "valid_percent"] <- "p_i_infection"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_i_infection"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "valid_percent"] <- "p_i_infection"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.12) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
echina.yn108 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$echina.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_i_hospitalized"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.31) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
echina.yn109 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$echina.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "valid_percent"] <- "p_nicu"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_nicu"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "valid_percent"] <- "p_nicu"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.94) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_nicu, p_nicu, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
echina.yn110 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$echina.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(echina.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(echina.yn == 0)
# Calculate n and percentage of outcome by herb exposure
echina.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
echina.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
echina.yn01 <- echina.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn02 <- echina.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
echina.yn01
echina.yn02
#Standardize the column name across tables.
names(echina.yn01)[names(echina.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(echina.yn01)[names(echina.yn01) == "percent"] <- "p_dead"
names(echina.yn01)[names(echina.yn01) == "n"] <- "n_dead"
names(echina.yn02)[names(echina.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(echina.yn02)[names(echina.yn02) == "percent"] <- "p_dead"
names(echina.yn02)[names(echina.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
echina.yn01 <- echina.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.46) %>% filter(Levelb == 0 | Levelb ==1)
echina.yn02 <- echina.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
echina.yn01 <- echina.yn01 %>% select(n_dead, p_dead, pvalue )
echina.yn02 <- echina.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
echina.yn111 <- rbind(echina.yn01, echina.yn02)
# Delete unnecessary tables
rm(echina.yn01, echina.yn02, HerbYesData01, HerbNoData01)
# Display final table
echina.yn111

#####
# Join all tables for one herb 
echina.yn010 <- cbind(echina.yn101, echina.yn102)
echina.yn009 <- cbind(echina.yn010, echina.yn103)
echina.yn008 <- cbind(echina.yn009, echina.yn104)
echina.yn007 <- cbind(echina.yn008, echina.yn105)
echina.yn006 <- cbind(echina.yn007, echina.yn106)
echina.yn005 <- cbind(echina.yn006, echina.yn107)
echina.yn004 <- cbind(echina.yn005, echina.yn108)
echina.yn003 <- cbind(echina.yn004, echina.yn109)
echina.yn002 <- cbind(echina.yn003, echina.yn110)
echina.yn001 <- cbind(echina.yn002, echina.yn111)

rm(echina.yn101, echina.yn102, echina.yn103, echina.yn104, echina.yn105, echina.yn106, echina.yn107, echina.yn108, echina.yn109, echina.yn110, echina.yn111)
rm(echina.yn002, echina.yn003, echina.yn004, echina.yn005, echina.yn006, echina.yn007, echina.yn008, echina.yn009, echina.yn010)

echina.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# cohosh.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cohosh.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "percent"] <- "p_cesarean"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_cesarean"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "valid_percent"] <- "p_cesarean"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = "Cohosh") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.72) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = "Cohosh") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
cohosh.yn101 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cohosh.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "percent"] <- "p_m_infection"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_m_infection"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "valid_percent"] <- "p_m_infection"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.021) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
cohosh.yn102 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cohosh.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "percent"] <- "p_m_hospital"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_m_hospital"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "valid_percent"] <- "p_m_hospital"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.28) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
cohosh.yn103 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$cohosh.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "percent"] <- "p_peri_trauma"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_peri_trauma"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "valid_percent"] <- "p_peri_trauma"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.49) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
cohosh.yn104 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$cohosh.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_blood_loss_1L"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000000065) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
cohosh.yn105 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$cohosh.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_blood_transfusion"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.27) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
cohosh.yn106 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cohosh.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "percent"] <- "p_m_i_transport"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_m_i_transport"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "percent"] <- "p_m_i_transport"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.41) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
cohosh.yn107 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$cohosh.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "valid_percent"] <- "p_i_infection"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_i_infection"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "valid_percent"] <- "p_i_infection"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.72) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
cohosh.yn108 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cohosh.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "percent"] <- "p_i_hospitalized"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_i_hospitalized"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.88) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
cohosh.yn109 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$cohosh.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "percent"] <- "p_nicu"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_nicu"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "valid_percent"] <- "p_nicu"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.33) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_nicu, p_nicu, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
cohosh.yn110 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$cohosh.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(cohosh.yn == 0)
# Calculate n and percentage of outcome by herb exposure
cohosh.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
cohosh.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
cohosh.yn01 <- cohosh.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
cohosh.yn01
cohosh.yn02
#Standardize the column name across tables.
names(cohosh.yn01)[names(cohosh.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(cohosh.yn01)[names(cohosh.yn01) == "percent"] <- "p_dead"
names(cohosh.yn01)[names(cohosh.yn01) == "n"] <- "n_dead"
names(cohosh.yn02)[names(cohosh.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(cohosh.yn02)[names(cohosh.yn02) == "percent"] <- "p_dead"
names(cohosh.yn02)[names(cohosh.yn02) == "n"] <- "n_dead"

cohosh.yn01 <- add_row(cohosh.yn01, Levelb = 1, n_dead = 0, p_dead = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
cohosh.yn01 <- cohosh.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
cohosh.yn02 <- cohosh.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
cohosh.yn01 <- cohosh.yn01 %>% select(n_dead, p_dead, pvalue )
cohosh.yn02 <- cohosh.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
cohosh.yn111 <- rbind(cohosh.yn01, cohosh.yn02)
# Delete unnecessary tables
rm(cohosh.yn01, cohosh.yn02, HerbYesData01, HerbNoData01)
# Display final table
cohosh.yn111

#####
# Join all tables for one herb 
cohosh.yn010 <- cbind(cohosh.yn101, cohosh.yn102)
cohosh.yn009 <- cbind(cohosh.yn010, cohosh.yn103)
cohosh.yn008 <- cbind(cohosh.yn009, cohosh.yn104)
cohosh.yn007 <- cbind(cohosh.yn008, cohosh.yn105)
cohosh.yn006 <- cbind(cohosh.yn007, cohosh.yn106)
cohosh.yn005 <- cbind(cohosh.yn006, cohosh.yn107)
cohosh.yn004 <- cbind(cohosh.yn005, cohosh.yn108)
cohosh.yn003 <- cbind(cohosh.yn004, cohosh.yn109)
cohosh.yn002 <- cbind(cohosh.yn003, cohosh.yn110)
cohosh.yn001 <- cbind(cohosh.yn002, cohosh.yn111)

rm(cohosh.yn101, cohosh.yn102, cohosh.yn103, cohosh.yn104, cohosh.yn105, cohosh.yn106, cohosh.yn107, cohosh.yn108, cohosh.yn109, cohosh.yn110, cohosh.yn111)
rm(cohosh.yn002, cohosh.yn003, cohosh.yn004, cohosh.yn005, cohosh.yn006, cohosh.yn007, cohosh.yn008, cohosh.yn009, cohosh.yn010)

cohosh.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# chloro.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$chloro.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "percent"] <- "p_cesarean"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_cesarean"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "valid_percent"] <- "p_cesarean"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = "Chlorophyll") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.031) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = "Chlorophyll") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
chloro.yn101 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$chloro.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "percent"] <- "p_m_infection"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_m_infection"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "valid_percent"] <- "p_m_infection"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.53) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
chloro.yn102 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$chloro.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "percent"] <- "p_m_hospital"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_m_hospital"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "valid_percent"] <- "p_m_hospital"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.79) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
chloro.yn103 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$chloro.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "percent"] <- "p_peri_trauma"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_peri_trauma"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "valid_percent"] <- "p_peri_trauma"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.29) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
chloro.yn104 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$chloro.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_blood_loss_1L"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.2) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
chloro.yn105 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$chloro.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_blood_transfusion"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_blood_transfusion"

chloro.yn01 <- add_row(chloro.yn01, Levelb = 1, n_blood_transfusion = 0, p_blood_transfusion = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
chloro.yn106 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$chloro.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "percent"] <- "p_m_i_transport"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_m_i_transport"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "percent"] <- "p_m_i_transport"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.092) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
chloro.yn107 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$chloro.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "valid_percent"] <- "p_i_infection"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_i_infection"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "valid_percent"] <- "p_i_infection"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.64) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
chloro.yn108 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$chloro.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "percent"] <- "p_i_hospitalized"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_i_hospitalized"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.15) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
chloro.yn109 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$chloro.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "valid_percent"] <- "p_nicu"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_nicu"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "valid_percent"] <- "p_nicu"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.41) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_nicu, p_nicu, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
chloro.yn110 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$chloro.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(chloro.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(chloro.yn == 0)
# Calculate n and percentage of outcome by herb exposure
chloro.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
chloro.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
chloro.yn01 <- chloro.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn02 <- chloro.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
chloro.yn01
chloro.yn02
#Standardize the column name across tables.
names(chloro.yn01)[names(chloro.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(chloro.yn01)[names(chloro.yn01) == "percent"] <- "p_dead"
names(chloro.yn01)[names(chloro.yn01) == "n"] <- "n_dead"
names(chloro.yn02)[names(chloro.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(chloro.yn02)[names(chloro.yn02) == "percent"] <- "p_dead"
names(chloro.yn02)[names(chloro.yn02) == "n"] <- "n_dead"

chloro.yn01 <- add_row(chloro.yn01, Levelb = 1, n_dead = 0, p_dead = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
chloro.yn01 <- chloro.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
chloro.yn02 <- chloro.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
chloro.yn01 <- chloro.yn01 %>% select(n_dead, p_dead, pvalue )
chloro.yn02 <- chloro.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
chloro.yn111 <- rbind(chloro.yn01, chloro.yn02)
# Delete unnecessary tables
rm(chloro.yn01, chloro.yn02, HerbYesData01, HerbNoData01)
# Display final table
chloro.yn111

#####
# Join all tables for one herb 
chloro.yn010 <- cbind(chloro.yn101, chloro.yn102)
chloro.yn009 <- cbind(chloro.yn010, chloro.yn103)
chloro.yn008 <- cbind(chloro.yn009, chloro.yn104)
chloro.yn007 <- cbind(chloro.yn008, chloro.yn105)
chloro.yn006 <- cbind(chloro.yn007, chloro.yn106)
chloro.yn005 <- cbind(chloro.yn006, chloro.yn107)
chloro.yn004 <- cbind(chloro.yn005, chloro.yn108)
chloro.yn003 <- cbind(chloro.yn004, chloro.yn109)
chloro.yn002 <- cbind(chloro.yn003, chloro.yn110)
chloro.yn001 <- cbind(chloro.yn002, chloro.yn111)

rm(chloro.yn101, chloro.yn102, chloro.yn103, chloro.yn104, chloro.yn105, chloro.yn106, chloro.yn107, chloro.yn108, chloro.yn109, chloro.yn110, chloro.yn111)
rm(chloro.yn002, chloro.yn003, chloro.yn004, chloro.yn005, chloro.yn006, chloro.yn007, chloro.yn008, chloro.yn009, chloro.yn010)

chloro.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# gbform.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$gbform.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "percent"] <- "p_cesarean"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_cesarean"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "valid_percent"] <- "p_cesarean"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = "Gentle Birth Formula") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.28) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = "Gentle Birth Formula") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
gbform.yn101 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$gbform.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "valid_percent"] <- "p_m_infection"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_m_infection"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "valid_percent"] <- "p_m_infection"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.81) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
gbform.yn102 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$gbform.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "percent"] <- "p_m_hospital"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_m_hospital"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "valid_percent"] <- "p_m_hospital"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0063) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
gbform.yn103 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$gbform.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "valid_percent"] <- "p_peri_trauma"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_peri_trauma"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "valid_percent"] <- "p_peri_trauma"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000053) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
gbform.yn104 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$gbform.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_blood_loss_1L"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.97) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
gbform.yn105 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn105
####
# Fisher's test
fisher.test(HerbData_Cleaned$gbform.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_blood_transfusion"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.057) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
gbform.yn106 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$gbform.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "percent"] <- "p_m_i_transport"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_m_i_transport"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "percent"] <- "p_m_i_transport"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.21) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
gbform.yn107 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$gbform.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "valid_percent"] <- "p_i_infection"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_i_infection"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "valid_percent"] <- "p_i_infection"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.38) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
gbform.yn108 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$gbform.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_i_hospitalized"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.1) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
gbform.yn109 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$gbform.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "valid_percent"] <- "p_nicu"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_nicu"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "valid_percent"] <- "p_nicu"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.34) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_nicu, p_nicu, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
gbform.yn110 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$gbform.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(gbform.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(gbform.yn == 0)
# Calculate n and percentage of outcome by herb exposure
gbform.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
gbform.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
gbform.yn01 <- gbform.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn02 <- gbform.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
gbform.yn01
gbform.yn02
#Standardize the column name across tables.
names(gbform.yn01)[names(gbform.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(gbform.yn01)[names(gbform.yn01) == "percent"] <- "p_dead"
names(gbform.yn01)[names(gbform.yn01) == "n"] <- "n_dead"
names(gbform.yn02)[names(gbform.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(gbform.yn02)[names(gbform.yn02) == "percent"] <- "p_dead"
names(gbform.yn02)[names(gbform.yn02) == "n"] <- "n_dead"

gbform.yn01 <- add_row(gbform.yn01, Levelb = 1, n_dead = 0, p_dead = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
gbform.yn01 <- gbform.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
gbform.yn02 <- gbform.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
gbform.yn01 <- gbform.yn01 %>% select(n_dead, p_dead, pvalue )
gbform.yn02 <- gbform.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
gbform.yn111 <- rbind(gbform.yn01, gbform.yn02)
# Delete unnecessary tables
rm(gbform.yn01, gbform.yn02, HerbYesData01, HerbNoData01)
# Display final table
gbform.yn111

#####
# Join all tables for one herb 
gbform.yn010 <- cbind(gbform.yn101, gbform.yn102)
gbform.yn009 <- cbind(gbform.yn010, gbform.yn103)
gbform.yn008 <- cbind(gbform.yn009, gbform.yn104)
gbform.yn007 <- cbind(gbform.yn008, gbform.yn105)
gbform.yn006 <- cbind(gbform.yn007, gbform.yn106)
gbform.yn005 <- cbind(gbform.yn006, gbform.yn107)
gbform.yn004 <- cbind(gbform.yn005, gbform.yn108)
gbform.yn003 <- cbind(gbform.yn004, gbform.yn109)
gbform.yn002 <- cbind(gbform.yn003, gbform.yn110)
gbform.yn001 <- cbind(gbform.yn002, gbform.yn111)

rm(gbform.yn101, gbform.yn102, gbform.yn103, gbform.yn104, gbform.yn105, gbform.yn106, gbform.yn107, gbform.yn108, gbform.yn109, gbform.yn110, gbform.yn111)
rm(gbform.yn002, gbform.yn003, gbform.yn004, gbform.yn005, gbform.yn006, gbform.yn007, gbform.yn008, gbform.yn009, gbform.yn010)

gbform.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# arnica.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$arnica.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "percent"] <- "p_cesarean"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_cesarean"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "valid_percent"] <- "p_cesarean"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = "Arnica") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.079) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = "Arnica") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
arnica.yn101 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$arnica.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "valid_percent"] <- "p_m_infection"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_m_infection"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "valid_percent"] <- "p_m_infection"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.75) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
arnica.yn102 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$arnica.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "percent"] <- "p_m_hospital"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_m_hospital"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "valid_percent"] <- "p_m_hospital"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.19) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
arnica.yn103 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$arnica.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "percent"] <- "p_peri_trauma"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_peri_trauma"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "valid_percent"] <- "p_peri_trauma"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0039) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
arnica.yn104 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$arnica.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_blood_loss_1L"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.03) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
arnica.yn105 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$arnica.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_blood_transfusion"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.56) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
arnica.yn106 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$arnica.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "percent"] <- "p_m_i_transport"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_m_i_transport"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "percent"] <- "p_m_i_transport"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.78) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
arnica.yn107 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$arnica.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "valid_percent"] <- "p_i_infection"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_i_infection"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "valid_percent"] <- "p_i_infection"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.27) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
arnica.yn108 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$arnica.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "percent"] <- "p_i_hospitalized"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_i_hospitalized"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.5) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
arnica.yn109 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$arnica.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "valid_percent"] <- "p_nicu"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_nicu"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "valid_percent"] <- "p_nicu"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.42) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_nicu, p_nicu, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
arnica.yn110 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$arnica.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(arnica.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(arnica.yn == 0)
# Calculate n and percentage of outcome by herb exposure
arnica.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
arnica.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
arnica.yn01 <- arnica.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn02 <- arnica.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
arnica.yn01
arnica.yn02
#Standardize the column name across tables.
names(arnica.yn01)[names(arnica.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(arnica.yn01)[names(arnica.yn01) == "percent"] <- "p_dead"
names(arnica.yn01)[names(arnica.yn01) == "n"] <- "n_dead"
names(arnica.yn02)[names(arnica.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(arnica.yn02)[names(arnica.yn02) == "percent"] <- "p_dead"
names(arnica.yn02)[names(arnica.yn02) == "n"] <- "n_dead"

arnica.yn01 <- add_row(arnica.yn01, Levelb = 1, n_dead = 0, p_dead = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
arnica.yn01 <- arnica.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
arnica.yn02 <- arnica.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
arnica.yn01 <- arnica.yn01 %>% select(n_dead, p_dead, pvalue )
arnica.yn02 <- arnica.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
arnica.yn111 <- rbind(arnica.yn01, arnica.yn02)
# Delete unnecessary tables
rm(arnica.yn01, arnica.yn02, HerbYesData01, HerbNoData01)
# Display final table
arnica.yn111

#####
# Join all tables for one herb 
arnica.yn010 <- cbind(arnica.yn101, arnica.yn102)
arnica.yn009 <- cbind(arnica.yn010, arnica.yn103)
arnica.yn008 <- cbind(arnica.yn009, arnica.yn104)
arnica.yn007 <- cbind(arnica.yn008, arnica.yn105)
arnica.yn006 <- cbind(arnica.yn007, arnica.yn106)
arnica.yn005 <- cbind(arnica.yn006, arnica.yn107)
arnica.yn004 <- cbind(arnica.yn005, arnica.yn108)
arnica.yn003 <- cbind(arnica.yn004, arnica.yn109)
arnica.yn002 <- cbind(arnica.yn003, arnica.yn110)
arnica.yn001 <- cbind(arnica.yn002, arnica.yn111)

rm(arnica.yn101, arnica.yn102, arnica.yn103, arnica.yn104, arnica.yn105, arnica.yn106, arnica.yn107, arnica.yn108, arnica.yn109, arnica.yn110, arnica.yn111)
rm(arnica.yn002, arnica.yn003, arnica.yn004, arnica.yn005, arnica.yn006, arnica.yn007, arnica.yn008, arnica.yn009, arnica.yn010)

arnica.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# pulsat.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$pulsat.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "percent"] <- "p_cesarean"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_cesarean"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "valid_percent"] <- "p_cesarean"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = "Pulsatilla") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.000029) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = "Pulsatilla") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
pulsat.yn101 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$pulsat.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "valid_percent"] <- "p_m_infection"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_m_infection"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "valid_percent"] <- "p_m_infection"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.27) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
pulsat.yn102 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$pulsat.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "valid_percent"] <- "p_m_hospital"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_m_hospital"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "valid_percent"] <- "p_m_hospital"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000021) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
pulsat.yn103 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$pulsat.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "valid_percent"] <- "p_peri_trauma"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_peri_trauma"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "valid_percent"] <- "p_peri_trauma"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.045) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
pulsat.yn104 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$pulsat.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_blood_loss_1L"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.29) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
pulsat.yn105 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$pulsat.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_blood_transfusion"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0082) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
pulsat.yn106 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$pulsat.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "percent"] <- "p_m_i_transport"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_m_i_transport"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "percent"] <- "p_m_i_transport"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000045) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
pulsat.yn107 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$pulsat.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "valid_percent"] <- "p_i_infection"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_i_infection"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "valid_percent"] <- "p_i_infection"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.35) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
pulsat.yn108 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$pulsat.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "valid_percent"] <- "p_i_hospitalized"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_i_hospitalized"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000017) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
pulsat.yn109 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$pulsat.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "valid_percent"] <- "p_nicu"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_nicu"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "valid_percent"] <- "p_nicu"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0017) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_nicu, p_nicu, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
pulsat.yn110 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$pulsat.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(pulsat.yn == 0)
# Calculate n and percentage of outcome by herb exposure
pulsat.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
pulsat.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
pulsat.yn01 <- pulsat.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
pulsat.yn01
pulsat.yn02
#Standardize the column name across tables.
names(pulsat.yn01)[names(pulsat.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(pulsat.yn01)[names(pulsat.yn01) == "percent"] <- "p_dead"
names(pulsat.yn01)[names(pulsat.yn01) == "n"] <- "n_dead"
names(pulsat.yn02)[names(pulsat.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(pulsat.yn02)[names(pulsat.yn02) == "percent"] <- "p_dead"
names(pulsat.yn02)[names(pulsat.yn02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
pulsat.yn01 <- pulsat.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.32) %>% filter(Levelb == 0 | Levelb ==1)
pulsat.yn02 <- pulsat.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
pulsat.yn01 <- pulsat.yn01 %>% select(n_dead, p_dead, pvalue )
pulsat.yn02 <- pulsat.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
pulsat.yn111 <- rbind(pulsat.yn01, pulsat.yn02)
# Delete unnecessary tables
rm(pulsat.yn01, pulsat.yn02, HerbYesData01, HerbNoData01)
# Display final table
pulsat.yn111

#####
# Join all tables for one herb 
pulsat.yn010 <- cbind(pulsat.yn101, pulsat.yn102)
pulsat.yn009 <- cbind(pulsat.yn010, pulsat.yn103)
pulsat.yn008 <- cbind(pulsat.yn009, pulsat.yn104)
pulsat.yn007 <- cbind(pulsat.yn008, pulsat.yn105)
pulsat.yn006 <- cbind(pulsat.yn007, pulsat.yn106)
pulsat.yn005 <- cbind(pulsat.yn006, pulsat.yn107)
pulsat.yn004 <- cbind(pulsat.yn005, pulsat.yn108)
pulsat.yn003 <- cbind(pulsat.yn004, pulsat.yn109)
pulsat.yn002 <- cbind(pulsat.yn003, pulsat.yn110)
pulsat.yn001 <- cbind(pulsat.yn002, pulsat.yn111)

rm(pulsat.yn101, pulsat.yn102, pulsat.yn103, pulsat.yn104, pulsat.yn105, pulsat.yn106, pulsat.yn107, pulsat.yn108, pulsat.yn109, pulsat.yn110, pulsat.yn111)
rm(pulsat.yn002, pulsat.yn003, pulsat.yn004, pulsat.yn005, pulsat.yn006, pulsat.yn007, pulsat.yn008, pulsat.yn009, pulsat.yn010)

pulsat.yn001
####################################################################################################################################################
####################################################################################################################################################
# HERB FLAG
# garlic.yn
# Chi-Squared test
chisq.test(HerbData_Cleaned$garlic.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "percent"] <- "p_cesarean"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_cesarean"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "valid_percent"] <- "p_cesarean"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = "Garlic") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.73) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = "Garlic") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
garlic.yn101 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$garlic.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "percent"] <- "p_m_infection"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_m_infection"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "valid_percent"] <- "p_m_infection"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.12) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
garlic.yn102 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$garlic.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "percent"] <- "p_m_hospital"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_m_hospital"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "valid_percent"] <- "p_m_hospital"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.39) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
garlic.yn103 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$garlic.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "percent"] <- "p_peri_trauma"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_peri_trauma"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "valid_percent"] <- "p_peri_trauma"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.55) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
garlic.yn104 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$garlic.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_blood_loss_1L"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.085) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
garlic.yn105 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn105
####
# Fisher's test
fisher.test(HerbData_Cleaned$garlic.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "valid_percent"] <- "p_blood_transfusion"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_blood_transfusion"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.54) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
garlic.yn106 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$garlic.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "percent"] <- "p_m_i_transport"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_m_i_transport"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "percent"] <- "p_m_i_transport"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.42) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
garlic.yn107 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$garlic.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "valid_percent"] <- "p_i_infection"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_i_infection"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "valid_percent"] <- "p_i_infection"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
garlic.yn108 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$garlic.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "percent"] <- "p_i_hospitalized"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_i_hospitalized"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.7) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
garlic.yn109 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$garlic.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "percent"] <- "p_nicu"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_nicu"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "valid_percent"] <- "p_nicu"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.82) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_nicu, p_nicu, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
garlic.yn110 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$garlic.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(garlic.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(garlic.yn == 0)
# Calculate n and percentage of outcome by herb exposure
garlic.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
garlic.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
garlic.yn01 <- garlic.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn02 <- garlic.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
garlic.yn01
garlic.yn02
#Standardize the column name across tables.
names(garlic.yn01)[names(garlic.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(garlic.yn01)[names(garlic.yn01) == "percent"] <- "p_dead"
names(garlic.yn01)[names(garlic.yn01) == "n"] <- "n_dead"
names(garlic.yn02)[names(garlic.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(garlic.yn02)[names(garlic.yn02) == "percent"] <- "p_dead"
names(garlic.yn02)[names(garlic.yn02) == "n"] <- "n_dead"

garlic.yn01 <-add_row(garlic.yn01, Levelb = 1, n_dead = 0, p_dead = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
garlic.yn01 <- garlic.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
garlic.yn02 <- garlic.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
garlic.yn01 <- garlic.yn01 %>% select(n_dead, p_dead, pvalue )
garlic.yn02 <- garlic.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
garlic.yn111 <- rbind(garlic.yn01, garlic.yn02)
# Delete unnecessary tables
rm(garlic.yn01, garlic.yn02, HerbYesData01, HerbNoData01)
# Display final table
garlic.yn111

#####
# Join all tables for one herb 
garlic.yn010 <- cbind(garlic.yn101, garlic.yn102)
garlic.yn009 <- cbind(garlic.yn010, garlic.yn103)
garlic.yn008 <- cbind(garlic.yn009, garlic.yn104)
garlic.yn007 <- cbind(garlic.yn008, garlic.yn105)
garlic.yn006 <- cbind(garlic.yn007, garlic.yn106)
garlic.yn005 <- cbind(garlic.yn006, garlic.yn107)
garlic.yn004 <- cbind(garlic.yn005, garlic.yn108)
garlic.yn003 <- cbind(garlic.yn004, garlic.yn109)
garlic.yn002 <- cbind(garlic.yn003, garlic.yn110)
garlic.yn001 <- cbind(garlic.yn002, garlic.yn111)

rm(garlic.yn101, garlic.yn102, garlic.yn103, garlic.yn104, garlic.yn105, garlic.yn106, garlic.yn107, garlic.yn108, garlic.yn109, garlic.yn110, garlic.yn111)
rm(garlic.yn002, garlic.yn003, garlic.yn004, garlic.yn005, garlic.yn006, garlic.yn007, garlic.yn008, garlic.yn009, garlic.yn010)

garlic.yn001


####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# hemhalt.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$hemhalt.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_cesarean"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_cesarean"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "percent"] <- "p_cesarean"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_cesarean"

hemhalt.yn01 <- add_row(hemhalt.yn01, Levelb = 1, n_cesarean = 0, p_cesarean = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = "Hemhalt") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00072) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = "Hemhalt") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
hemhalt.yn101 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$hemhalt.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_m_infection"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_m_infection"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "valid_percent"] <- "p_m_infection"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.51) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
hemhalt.yn102 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$hemhalt.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_m_hospital"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_m_hospital"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "valid_percent"] <- "p_m_hospital"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.17) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
hemhalt.yn103 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$hemhalt.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_peri_trauma"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_peri_trauma"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "valid_percent"] <- "p_peri_trauma"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.58) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
hemhalt.yn104 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$hemhalt.yn, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_blood_loss_1L"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_blood_loss_1L"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
hemhalt.yn105 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$hemhalt.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_blood_transfusion"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_blood_transfusion"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.53) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
hemhalt.yn106 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$hemhalt.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_m_i_transport"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_m_i_transport"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "percent"] <- "p_m_i_transport"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.000095) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
hemhalt.yn107 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$hemhalt.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "valid_percent"] <- "p_i_infection"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_i_infection"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "valid_percent"] <- "p_i_infection"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
hemhalt.yn108 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$hemhalt.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_i_hospitalized"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_i_hospitalized"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00091) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
hemhalt.yn109 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$hemhalt.yn, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_nicu"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_nicu"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "valid_percent"] <- "p_nicu"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.54) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_nicu, p_nicu, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
hemhalt.yn110 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$hemhalt.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(hemhalt.yn == 0)
# Calculate n and percentage of outcome by herb exposure
hemhalt.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
hemhalt.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
hemhalt.yn01
hemhalt.yn02
#Standardize the column name across tables.
names(hemhalt.yn01)[names(hemhalt.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(hemhalt.yn01)[names(hemhalt.yn01) == "percent"] <- "p_dead"
names(hemhalt.yn01)[names(hemhalt.yn01) == "n"] <- "n_dead"
names(hemhalt.yn02)[names(hemhalt.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(hemhalt.yn02)[names(hemhalt.yn02) == "percent"] <- "p_dead"
names(hemhalt.yn02)[names(hemhalt.yn02) == "n"] <- "n_dead"

hemhalt.yn01 <- add_row(hemhalt.yn01, Levelb = 1, n_dead = 0, p_dead = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
hemhalt.yn01 <- hemhalt.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
hemhalt.yn02 <- hemhalt.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
hemhalt.yn01 <- hemhalt.yn01 %>% select(n_dead, p_dead, pvalue )
hemhalt.yn02 <- hemhalt.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
hemhalt.yn111 <- rbind(hemhalt.yn01, hemhalt.yn02)
# Delete unnecessary tables
rm(hemhalt.yn01, hemhalt.yn02, HerbYesData01, HerbNoData01)
# Display final table
hemhalt.yn111

#####
# Join all tables for one herb 
hemhalt.yn010 <- cbind(hemhalt.yn101, hemhalt.yn102)
hemhalt.yn009 <- cbind(hemhalt.yn010, hemhalt.yn103)
hemhalt.yn008 <- cbind(hemhalt.yn009, hemhalt.yn104)
hemhalt.yn007 <- cbind(hemhalt.yn008, hemhalt.yn105)
hemhalt.yn006 <- cbind(hemhalt.yn007, hemhalt.yn106)
hemhalt.yn005 <- cbind(hemhalt.yn006, hemhalt.yn107)
hemhalt.yn004 <- cbind(hemhalt.yn005, hemhalt.yn108)
hemhalt.yn003 <- cbind(hemhalt.yn004, hemhalt.yn109)
hemhalt.yn002 <- cbind(hemhalt.yn003, hemhalt.yn110)
hemhalt.yn001 <- cbind(hemhalt.yn002, hemhalt.yn111)

rm(hemhalt.yn101, hemhalt.yn102, hemhalt.yn103, hemhalt.yn104, hemhalt.yn105, hemhalt.yn106, hemhalt.yn107, hemhalt.yn108, hemhalt.yn109, hemhalt.yn110, hemhalt.yn111)
rm(hemhalt.yn002, hemhalt.yn003, hemhalt.yn004, hemhalt.yn005, hemhalt.yn006, hemhalt.yn007, hemhalt.yn008, hemhalt.yn009, hemhalt.yn010)

hemhalt.yn001
####################################################################################################################################################
####################################################################################################################################################

# HERB FLAG
# mblend.yn
####
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$mblend.yn, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "percent"] <- "p_cesarean"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_cesarean"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "valid_percent"] <- "p_cesarean"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = "Mother's Blend") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.023) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = "Mother's Blend") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
mblend.yn101 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$mblend.yn, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "percent"] <- "p_m_infection"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_m_infection"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "valid_percent"] <- "p_m_infection"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.61) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_m_infection, p_m_infection, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
mblend.yn102 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$mblend.yn, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "percent"] <- "p_m_hospital"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_m_hospital"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "valid_percent"] <- "p_m_hospital"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000000068) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_m_hospital, p_m_hospital, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
mblend.yn103 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$mblend.yn, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "percent"] <- "p_peri_trauma"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_peri_trauma"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "valid_percent"] <- "p_peri_trauma"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000000045) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
mblend.yn104 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn104
####
# Fisher Test
fisher.test(HerbData_Cleaned_NoCsec$mblend.yn, HerbData_Cleaned_NoCsec$blood_loss_cat)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "valid_percent"] <- "p_blood_loss_1L"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_blood_loss_1L"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "valid_percent"] <- "p_blood_loss_1L"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000058) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
mblend.yn105 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn105
####
# Fisher's Test
fisher.test(HerbData_Cleaned$mblend.yn, HerbData_Cleaned$Blood_Actions_BloodTrans)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "percent"] <- "p_blood_transfusion"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_blood_transfusion"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "valid_percent"] <- "p_blood_transfusion"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.43) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
mblend.yn106 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$mblend.yn, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$transport, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$transport"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "percent"] <- "p_m_i_transport"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_m_i_transport"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$transport"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "percent"] <- "p_m_i_transport"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000017) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
mblend.yn107 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn107
####
# Fisher's Test
fisher.test(HerbData_Cleaned$mblend.yn, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "valid_percent"] <- "p_i_infection"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_i_infection"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "valid_percent"] <- "p_i_infection"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_i_infection, p_i_infection, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
mblend.yn108 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$mblend.yn, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "percent"] <- "p_i_hospitalized"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_i_hospitalized"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "valid_percent"] <- "p_i_hospitalized"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000054) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
mblend.yn109 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn109
####
# Fisher's Test
fisher.test(HerbData_Cleaned$mblend.yn, HerbData_Cleaned$NICU.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "percent"] <- "p_nicu"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_nicu"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "valid_percent"] <- "p_nicu"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.6) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_nicu, p_nicu, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
mblend.yn110 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn110
####
# Fisher's test
fisher.test(HerbData_Cleaned$mblend.yn, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(mblend.yn == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(mblend.yn == 0)
# Calculate n and percentage of outcome by herb exposure
mblend.yn01 <- tabyl(HerbYesData01$dead, sort = TRUE)
mblend.yn02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
mblend.yn01 <- mblend.yn01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn02 <- mblend.yn02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
mblend.yn01
mblend.yn02
#Standardize the column name across tables.
names(mblend.yn01)[names(mblend.yn01) == "HerbYesData01$dead"] <- "Levelb"
names(mblend.yn01)[names(mblend.yn01) == "percent"] <- "p_dead"
names(mblend.yn01)[names(mblend.yn01) == "n"] <- "n_dead"
names(mblend.yn02)[names(mblend.yn02) == "HerbNoData01$dead"] <- "Levelb"
names(mblend.yn02)[names(mblend.yn02) == "percent"] <- "p_dead"
names(mblend.yn02)[names(mblend.yn02) == "n"] <- "n_dead"

mblend.yn01 <- add_row(mblend.yn01, Levelb = 1, n_dead = 0, p_dead = 0)
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
mblend.yn01 <- mblend.yn01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
mblend.yn02 <- mblend.yn02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
mblend.yn01 <- mblend.yn01 %>% select(n_dead, p_dead, pvalue )
mblend.yn02 <- mblend.yn02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
mblend.yn111 <- rbind(mblend.yn01, mblend.yn02)
# Delete unnecessary tables
rm(mblend.yn01, mblend.yn02, HerbYesData01, HerbNoData01)
# Display final table
mblend.yn111

#####
# Join all tables for one herb 
mblend.yn010 <- cbind(mblend.yn101, mblend.yn102)
mblend.yn009 <- cbind(mblend.yn010, mblend.yn103)
mblend.yn008 <- cbind(mblend.yn009, mblend.yn104)
mblend.yn007 <- cbind(mblend.yn008, mblend.yn105)
mblend.yn006 <- cbind(mblend.yn007, mblend.yn106)
mblend.yn005 <- cbind(mblend.yn006, mblend.yn107)
mblend.yn004 <- cbind(mblend.yn005, mblend.yn108)
mblend.yn003 <- cbind(mblend.yn004, mblend.yn109)
mblend.yn002 <- cbind(mblend.yn003, mblend.yn110)
mblend.yn001 <- cbind(mblend.yn002, mblend.yn111)

rm(mblend.yn101, mblend.yn102, mblend.yn103, mblend.yn104, mblend.yn105, mblend.yn106, mblend.yn107, mblend.yn108, mblend.yn109, mblend.yn110, mblend.yn111)
rm(mblend.yn002, mblend.yn003, mblend.yn004, mblend.yn005, mblend.yn006, mblend.yn007, mblend.yn008, mblend.yn009, mblend.yn010)

mblend.yn001
####################################################################################################################################################
####################################################################################################################################################
rm(univariate_table)
univariate_table <- rbind(primr.yn001, 
                          rasp.yn001,
                          castor.yn001,
                          cauloph.yn001,
                          shepp.yn001,
                          angelica.yn001,
                          alfa.yn001,
                          nettle.yn001,
                          flordx.yn001,
                          motherwort.yn001,
                          dande.yn001,
                          echina.yn001,
                          cohosh.yn001,
                          chloro.yn001,
                          gbform.yn001,
                          arnica.yn001,
                          pulsat.yn001,
                          garlic.yn001,
                          hemhalt.yn001,
                          mblend.yn001)

univariate_table
write.csv(univariate_table, "univariate_table_paige_V02_ORs.csv")

rm(primr.yn001, 
   rasp.yn001,
   castor.yn001,
   cauloph.yn001,
   shepp.yn001,
   angelica.yn001,
   alfa.yn001,
   nettle.yn001,
   flordx.yn001,
   motherwort.yn001,
   dande.yn001,
   echina.yn001,
   cohosh.yn001,
   chloro.yn001,
   gbform.yn001,
   arnica.yn001,
   pulsat.yn001,
   garlic.yn001,
   hemhalt.yn001,
   mblend.yn001)


####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####
# XXX01 <- add_row(XXX1, Levelb =1, n_cesarean = 0, p_cesarean = 0)
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$XXX, HerbData_Cleaned$Csec_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$Csec_FlgR, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$Csec_FlgR, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$Csec_FlgR"] <- "Levelb"
names(XXX01)[names(XXX01) == "percent"] <- "p_cesarean"
names(XXX01)[names(XXX01) == "n"] <- "n_cesarean"
names(XXX02)[names(XXX02) == "HerbNoData01$Csec_FlgR"] <- "Levelb"
names(XXX02)[names(XXX02) == "percent"] <- "p_cesarean"
names(XXX02)[names(XXX02) == "n"] <- "n_cesarean"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01 %>% mutate(Herb = "YYY") %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02 %>% mutate(Herb = "YYY") %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
XXX02 <- XXX02 %>% select(Herb, Level, Levelb, n_cesarean, p_cesarean, pvalue )
# Stack Y/N info
XXX101 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX101
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$XXX, HerbData_Cleaned$mompost.RT.infect.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$mompost.RT.infect.yn, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$mompost.RT.infect.yn, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$mompost.RT.infect.yn"] <- "Levelb"
names(XXX01)[names(XXX01) == "valid_percent"] <- "p_m_infection"
names(XXX01)[names(XXX01) == "n"] <- "n_m_infection"
names(XXX02)[names(XXX02) == "HerbNoData01$mompost.RT.infect.yn"] <- "Levelb"
names(XXX02)[names(XXX02) == "valid_percent"] <- "p_m_infection"
names(XXX02)[names(XXX02) == "n"] <- "n_m_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.00000089) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_m_infection, p_m_infection, pvalue )
XXX02 <- XXX02 %>% select(n_m_infection, p_m_infection, pvalue )
# Stack Y/N info
XXX102 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX102
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$XXX, HerbData_Cleaned$MomHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$MomHosp_FlgR, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$MomHosp_FlgR, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$MomHosp_FlgR"] <- "Levelb"
names(XXX01)[names(XXX01) == "valid_percent"] <- "p_m_hospital"
names(XXX01)[names(XXX01) == "n"] <- "n_m_hospital"
names(XXX02)[names(XXX02) == "HerbNoData01$MomHosp_FlgR"] <- "Levelb"
names(XXX02)[names(XXX02) == "valid_percent"] <- "p_m_hospital"
names(XXX02)[names(XXX02) == "n"] <- "n_m_hospital"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_m_hospital, p_m_hospital, pvalue )
XXX02 <- XXX02 %>% select(n_m_hospital, p_m_hospital, pvalue )
# Stack Y/N info
XXX103 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX103
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$XXX, HerbData_Cleaned_NoCsec$peri_trauma.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$peri_trauma.yn, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$peri_trauma.yn, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$peri_trauma.yn"] <- "Levelb"
names(XXX01)[names(XXX01) == "valid_percent"] <- "p_peri_trauma"
names(XXX01)[names(XXX01) == "n"] <- "n_peri_trauma"
names(XXX02)[names(XXX02) == "HerbNoData01$peri_trauma.yn"] <- "Levelb"
names(XXX02)[names(XXX02) == "valid_percent"] <- "p_peri_trauma"
names(XXX02)[names(XXX02) == "n"] <- "n_peri_trauma"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.46) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
XXX02 <- XXX02 %>% select(n_peri_trauma, p_peri_trauma, pvalue )
# Stack Y/N info
XXX104 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX104
####
# Chi-Squared test
chisq.test(HerbData_Cleaned_NoCsec$XXX, HerbData_Cleaned_NoCsec$blood_loss_cat, correct=FALSE)
# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) %>% filter(Csec_FlgR == 0)
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0) %>% filter(Csec_FlgR == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$blood_loss_cat, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$blood_loss_cat, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$blood_loss_cat"] <- "Levelb"
names(XXX01)[names(XXX01) == "valid_percent"] <- "p_blood_loss_1L"
names(XXX01)[names(XXX01) == "n"] <- "n_blood_loss_1L"
names(XXX02)[names(XXX02) == "HerbNoData01$blood_loss_cat"] <- "Levelb"
names(XXX02)[names(XXX02) == "valid_percent"] <- "p_blood_loss_1L"
names(XXX02)[names(XXX02) == "n"] <- "n_blood_loss_1L"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.000033) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
XXX02 <- XXX02 %>% select(n_blood_loss_1L, p_blood_loss_1L, pvalue )
# Stack Y/N info
XXX105 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX105
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$XXX, HerbData_Cleaned$Blood_Actions_BloodTrans, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$Blood_Actions_BloodTrans, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$Blood_Actions_BloodTrans, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(XXX01)[names(XXX01) == "valid_percent"] <- "p_blood_transfusion"
names(XXX01)[names(XXX01) == "n"] <- "n_blood_transfusion"
names(XXX02)[names(XXX02) == "HerbNoData01$Blood_Actions_BloodTrans"] <- "Levelb"
names(XXX02)[names(XXX02) == "valid_percent"] <- "p_blood_transfusion"
names(XXX02)[names(XXX02) == "n"] <- "n_blood_transfusion"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.09) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
XXX02 <- XXX02 %>% select(n_blood_transfusion, p_blood_transfusion, pvalue )
# Stack Y/N info
XXX106 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX106
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$XXX, HerbData_Cleaned$transport, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$transport, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$transport, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$transport"] <- "Levelb"
names(XXX01)[names(XXX01) == "percent"] <- "p_m_i_transport"
names(XXX01)[names(XXX01) == "n"] <- "n_m_i_transport"
names(XXX02)[names(XXX02) == "HerbNoData01$transport"] <- "Levelb"
names(XXX02)[names(XXX02) == "percent"] <- "p_m_i_transport"
names(XXX02)[names(XXX02) == "n"] <- "n_m_i_transport"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
XXX02 <- XXX02 %>% select(n_m_i_transport, p_m_i_transport, pvalue )
# Stack Y/N info
XXX107 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX107
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$XXX, HerbData_Cleaned$infant_infect.yn, correct=FALSE)
# Fisher's Test
fisher.test(HerbData_Cleaned$XXX, HerbData_Cleaned$infant_infect.yn)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$infant_infect.yn, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$infant_infect.yn, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$infant_infect.yn"] <- "Levelb"
names(XXX01)[names(XXX01) == "valid_percent"] <- "p_i_infection"
names(XXX01)[names(XXX01) == "n"] <- "n_i_infection"
names(XXX02)[names(XXX02) == "HerbNoData01$infant_infect.yn"] <- "Levelb"
names(XXX02)[names(XXX02) == "valid_percent"] <- "p_i_infection"
names(XXX02)[names(XXX02) == "n"] <- "n_i_infection"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01  %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.33) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02  %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_i_infection, p_i_infection, pvalue )
XXX02 <- XXX02 %>% select(n_i_infection, p_i_infection, pvalue )
# Stack Y/N info
XXX108 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX108
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$XXX, HerbData_Cleaned$InfHosp_FlgR, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$InfHosp_FlgR, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$InfHosp_FlgR, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$InfHosp_FlgR"] <- "Levelb"
names(XXX01)[names(XXX01) == "valid_percent"] <- "p_i_hospitalized"
names(XXX01)[names(XXX01) == "n"] <- "n_i_hospitalized"
names(XXX02)[names(XXX02) == "HerbNoData01$InfHosp_FlgR"] <- "Levelb"
names(XXX02)[names(XXX02) == "valid_percent"] <- "p_i_hospitalized"
names(XXX02)[names(XXX02) == "n"] <- "n_i_hospitalized"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000000000002) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
XXX02 <- XXX02 %>% select(n_i_hospitalized, p_i_hospitalized, pvalue )
# Stack Y/N info
XXX109 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX109
####
# Chi-Squared test
chisq.test(HerbData_Cleaned$XXX, HerbData_Cleaned$NICU.yn, correct=FALSE)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$NICU.yn, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$NICU.yn, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$NICU.yn"] <- "Levelb"
names(XXX01)[names(XXX01) == "valid_percent"] <- "p_nicu"
names(XXX01)[names(XXX01) == "n"] <- "n_nicu"
names(XXX02)[names(XXX02) == "HerbNoData01$NICU.yn"] <- "Levelb"
names(XXX02)[names(XXX02) == "valid_percent"] <- "p_nicu"
names(XXX02)[names(XXX02) == "n"] <- "n_nicu"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 0.0000000012) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_nicu, p_nicu, pvalue )
XXX02 <- XXX02 %>% select(n_nicu, p_nicu, pvalue )
# Stack Y/N info
XXX110 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX110
####
# Fisher's test
fisher.test(HerbData_Cleaned$XXX, HerbData_Cleaned$dead)

# Group observations by herb exposure (Y/N)
HerbYesData01 <- HerbData_Cleaned %>% filter(XXX == 1) 
HerbNoData01 <- HerbData_Cleaned %>% filter(XXX == 0)
# Calculate n and percentage of outcome by herb exposure
XXX01 <- tabyl(HerbYesData01$dead, sort = TRUE)
XXX02 <- tabyl(HerbNoData01$dead, sort = TRUE) 
XXX01 <- XXX01 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX02 <- XXX02 %>% mutate(Herb = NA_character_) %>% mutate(Level = NA_character_) %>%  mutate(pvalue = NA_integer_)
XXX01
XXX02
#Standardize the column name across tables.
names(XXX01)[names(XXX01) == "HerbYesData01$dead"] <- "Levelb"
names(XXX01)[names(XXX01) == "percent"] <- "p_dead"
names(XXX01)[names(XXX01) == "n"] <- "n_dead"
names(XXX02)[names(XXX02) == "HerbNoData01$dead"] <- "Levelb"
names(XXX02)[names(XXX02) == "percent"] <- "p_dead"
names(XXX02)[names(XXX02) == "n"] <- "n_dead"
# Add Herb, Level (Y/N), pvalue, and only take where outcome == 1
XXX01 <- XXX01 %>% mutate(Level = "Yes") %>% 
  mutate(pvalue = 1) %>% filter(Levelb == 0 | Levelb ==1)
XXX02 <- XXX02 %>% mutate(Level = "No") %>% 
  filter(Levelb == 0 | Levelb ==1)
# Select wanted columns in desired order
XXX01 <- XXX01 %>% select(n_dead, p_dead, pvalue )
XXX02 <- XXX02 %>% select(n_dead, p_dead, pvalue )
# Stack Y/N info
XXX111 <- rbind(XXX01, XXX02)
# Delete unnecessary tables
rm(XXX01, XXX02, HerbYesData01, HerbNoData01)
# Display final table
XXX111

#####
# Join all tables for one herb 
XXX010 <- cbind(XXX101, XXX102)
XXX009 <- cbind(XXX010, XXX103)
XXX008 <- cbind(XXX009, XXX104)
XXX007 <- cbind(XXX008, XXX105)
XXX006 <- cbind(XXX007, XXX106)
XXX005 <- cbind(XXX006, XXX107)
XXX004 <- cbind(XXX005, XXX108)
XXX003 <- cbind(XXX004, XXX109)
XXX002 <- cbind(XXX003, XXX110)
XXX001 <- cbind(XXX002, XXX111)

rm(XXX101, XXX102, XXX103, XXX104, XXX105, XXX106, XXX107, XXX108, XXX109, XXX110, XXX111)
rm(XXX002, XXX003, XXX004, XXX005, XXX006, XXX007, XXX008, XXX009, XXX010)

XXX001