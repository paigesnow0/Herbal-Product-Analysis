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

# Import 0409 herb subset V02.sav
library(haven)
X0409_paige_V02 <- read_sav("0409 paige V02.sav")

# Save as a new tibble and set order of columns
HerbData <- X0409_paige_V02 
head(HerbData)

# ###################################################################################################
# Create a variable for observations, then put it in the front. 
HerbData <- HerbData %>% mutate(obs=1:n())
HerbData <- HerbData %>% select(obs, everything())
head(HerbData)

# Clean text
# Put all chracter text in lower case. 
HerbData$Herb_1Text <- str_to_lower(HerbData$Herb_1Text)
HerbData$Herb_1Reason <- str_to_lower(HerbData$Herb_1Reason)
HerbData$Herb_2Text <- str_to_lower(HerbData$Herb_2Text)
HerbData$Herb_2Reason <- str_to_lower(HerbData$Herb_2Reason)
HerbData$Herb_3Text <- str_to_lower(HerbData$Herb_3Text)
HerbData$Herb_3Reason <- str_to_lower(HerbData$Herb_3Reason)
HerbData$Herb_4Text <- str_to_lower(HerbData$Herb_4Text)
HerbData$Herb_4Reason <- str_to_lower(HerbData$Herb_4Reason)
HerbData$Herb_OtherTxt <- str_to_lower(HerbData$Herb_OtherTxt)
head(HerbData)

# Remove backslashes
HerbData$Herb_1Text <- gsub("/", " ", HerbData$Herb_1Text, fixed=TRUE)
HerbData$Herb_1Reason <- gsub("/", " ", HerbData$Herb_1Reason, fixed=TRUE)
HerbData$Herb_2Text <- gsub("/", " ", HerbData$Herb_2Text, fixed=TRUE)
HerbData$Herb_2Reason <- gsub("/", " ", HerbData$Herb_2Reason, fixed=TRUE)
HerbData$Herb_3Text <- gsub("/", " ", HerbData$Herb_3Text, fixed=TRUE)
HerbData$Herb_3Reason <- gsub("/", " ", HerbData$Herb_3Reason, fixed=TRUE)
HerbData$Herb_4Text <- gsub("/", " ", HerbData$Herb_4Text, fixed=TRUE)
HerbData$Herb_4Reason <- gsub("/", " ", HerbData$Herb_4Reason, fixed=TRUE)
HerbData$Herb_OtherTxt <- gsub("/", " ", HerbData$Herb_OtherTxt, fixed=TRUE)
head(HerbData)

# Remove punctuation
HerbData$Herb_1Text <- removePunctuation(HerbData$Herb_1Text)
HerbData$Herb_1Reason <- removePunctuation(HerbData$Herb_1Reason)
HerbData$Herb_2Text <- removePunctuation(HerbData$Herb_2Text)
HerbData$Herb_2Reason <- removePunctuation(HerbData$Herb_2Reason)
HerbData$Herb_3Text <- removePunctuation(HerbData$Herb_3Text)
HerbData$Herb_3Reason <- removePunctuation(HerbData$Herb_3Reason)
HerbData$Herb_4Text <- removePunctuation(HerbData$Herb_4Text)
HerbData$Herb_4Reason <- removePunctuation(HerbData$Herb_4Reason)
HerbData$Herb_OtherTxt <- removePunctuation(HerbData$Herb_OtherTxt)
head(HerbData)

# Strip White Space
HerbData$Herb_1Text <- stripWhitespace(HerbData$Herb_1Text)
HerbData$Herb_1Reason <- stripWhitespace(HerbData$Herb_1Reason)
HerbData$Herb_2Text <- stripWhitespace(HerbData$Herb_2Text)
HerbData$Herb_2Reason <- stripWhitespace(HerbData$Herb_2Reason)
HerbData$Herb_3Text <- stripWhitespace(HerbData$Herb_3Text)
HerbData$Herb_3Reason <- stripWhitespace(HerbData$Herb_3Reason)
HerbData$Herb_4Text <- stripWhitespace(HerbData$Herb_4Text)
HerbData$Herb_4Reason <- stripWhitespace(HerbData$Herb_4Reason)
HerbData$Herb_OtherTxt <- stripWhitespace(HerbData$Herb_OtherTxt)
HerbData

# Make text that are factors into character strings.
HerbData$Herb_1Text <- as.character(HerbData$Herb_1Text)
HerbData$Herb_1Reason <- as.character(HerbData$Herb_1Reason)
HerbData$Herb_2Text <- as.character(HerbData$Herb_2Text)
HerbData$Herb_2Reason <- as.character(HerbData$Herb_2Reason)
HerbData$Herb_3Text <- as.character(HerbData$Herb_3Text)
HerbData$Herb_3Reason <- as.character(HerbData$Herb_3Reason)
HerbData$Herb_4Text <- as.character(HerbData$Herb_4Text)
HerbData$Herb_4Reason <- as.character(HerbData$Herb_4Reason)
HerbData$Herb_OtherTxt <- as.character(HerbData$Herb_OtherTxt)

###################################################################################################

# We want to see which herbs are most prevalent
# So first we'll stack Herb_1Text, Herb_2Text, Herb_3Text
# We'll unnest() the string and find the cumulative frequency of the most common words

# Create new dataframe for each Herb_#Text, plus create a level to distinguish between herb levels when stacked. 
HerbData_1Text <- data_frame(id = HerbData$id, Herb_1Text = HerbData$Herb_1Text) %>% mutate(Herb_Numb=1)
HerbData_2Text <- data_frame(id = HerbData$id, Herb_2Text = HerbData$Herb_2Text) %>% mutate(Herb_Numb=2)
HerbData_3Text <- data_frame(id = HerbData$id, Herb_3Text = HerbData$Herb_3Text) %>% mutate(Herb_Numb=3)
HerbData_4Text <- data_frame(id = HerbData$id, Herb_4Text = HerbData$Herb_4Text) %>% mutate(Herb_Numb=4)

# Tokenize each dataframe for each Herb_#Text
HerbData_1token <- HerbData_1Text %>% unnest_tokens(word, Herb_1Text)
HerbData_2token <- HerbData_2Text %>% unnest_tokens(word, Herb_2Text)
HerbData_3token <- HerbData_3Text %>% unnest_tokens(word, Herb_3Text)
HerbData_4token <- HerbData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
HerbData_AllToken <- rbind(HerbData_1token, HerbData_2token, HerbData_3token, HerbData_4token)

# Check all the dataframes. 
HerbData_1Text
HerbData_2Text
HerbData_3Text
HerbData_4Text
HerbData_1token
HerbData_2token
HerbData_3token
HerbData_4token
HerbData_AllToken

# Remove excess data frames
rm(HerbData_1Text, HerbData_2Text, HerbData_3Text, HerbData_4Text, HerbData_1token, HerbData_2token, HerbData_3token, HerbData_4token)

###################################################################################################
# Code to produce stop words -> words that you don't care to show up in the text mining...
my_stop_words <- bind_rows(stop_words, 
                           data_frame(word = c("red", "leaf", "and",
                                               as.character(1:27)), 
                                      lexicon = rep("custom", 30)))

###################################################################################################
# Frequency of each word, regardless of Herb_Numb level
HerbData_WordCount <- HerbData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Frequency of word pairs, taking into account the observation & Herb_Numb levels
HerbData_WordPairs <- HerbData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, id, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Check data
HerbData_WordCount
HerbData_WordPairs

# Export to .csv
write.csv(HerbData_WordCount, "HerbData_WordCount_paige_v02.csv")
write.csv(HerbData_WordPairs, "HerbData_WordPairs_paige_v02.csv")

# Generate a visual graph to depict the pairs.
HerbData_WordPairs %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Check to make sure WordPairs, above, is also taking into account Herb_Numb (1-4)
#HerbData_AllToken_v2 <- HerbData_AllToken %>% 
#  mutate(obs2 = 0) 
#HerbData_AllToken_v2

#HerbData_AllToken_v2$obs2[HerbData_AllToken_v2$Herb_Numb==1] <- HerbData_AllToken_v2$obs
#HerbData_AllToken_v2$obs2[HerbData_AllToken_v2$Herb_Numb==2] <- (HerbData_AllToken_v2$obs+8400)[HerbData_AllToken_v2$Herb_Numb==2]
#HerbData_AllToken_v2$obs2[HerbData_AllToken_v2$Herb_Numb==3] <- (HerbData_AllToken_v2$obs+16800)[HerbData_AllToken_v2$Herb_Numb==3]
#HerbData_AllToken_v2$obs2[HerbData_AllToken_v2$Herb_Numb==4] <- (HerbData_AllToken_v2$obs+25200)[HerbData_AllToken_v2$Herb_Numb==4]

#HerbData_AllToken_v2 <- HerbData_AllToken %>% 
#  mutate(obs2 = 0) %>%
#  mutate(obs2 = replace(obs2, Herb_Numb == 1, obs))

#HerbData_AllToken_v2 %>% filter(Herb_Numb == 1)
#HerbData_AllToken_v2 %>% filter(Herb_Numb == 2)
#HerbData_AllToken_v2 %>% filter(Herb_Numb == 3)
#HerbData_AllToken_v2 %>% filter(Herb_Numb == 4)

# Frequency of word pairs 
#HerbData_WordPairs2 <- HerbData_AllToken_v2 %>% 
#  anti_join(my_stop_words) %>%
#  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
#  ungroup()

#write.csv(HerbData_WordPairs2, "HerbData_WordPairs2.csv")
#HerbData_WordPairs2
###################################################################################################
HerbData_WordCount <- read_csv("HerbData_WordCount.csv")
View(HerbData_WordCount)

head(HerbData_WordCount, 50)

# Look more closely at "tea"
# Create new dataframe for each Herb_#Text, filtered by "tea", plus create a level to distinguish between herb levels when stacked. 
TeaData_1Text <- data_frame(obs = HerbData$obs, Herb_1Text = HerbData$Herb_1Text) %>% 
  mutate(Herb_Numb=1) %>% 
  mutate(tea.yn = str_detect(Herb_1Text, "tea")) %>% 
  filter(tea.yn == TRUE)
TeaData_2Text <- data_frame(obs = HerbData$obs, Herb_2Text = HerbData$Herb_2Text) %>% 
  mutate(Herb_Numb=2) %>%
  mutate(tea.yn = str_detect(Herb_2Text, "tea")) %>% 
  filter(tea.yn == TRUE)
TeaData_3Text <- data_frame(obs = HerbData$obs, Herb_3Text = HerbData$Herb_3Text) %>% 
  mutate(Herb_Numb=3) %>%
  mutate(tea.yn = str_detect(Herb_3Text, "tea")) %>% 
  filter(tea.yn == TRUE)
TeaData_4Text <- data_frame(obs = HerbData$obs, Herb_4Text = HerbData$Herb_4Text) %>% 
  mutate(Herb_Numb=4) %>%
  mutate(tea.yn = str_detect(Herb_4Text, "tea")) %>% 
  filter(tea.yn == TRUE)

# Tokenize each dataframe for each Herb_#Text
TeaData_1token <- TeaData_1Text %>% unnest_tokens(word, Herb_1Text)
TeaData_2token <- TeaData_2Text %>% unnest_tokens(word, Herb_2Text)
TeaData_3token <- TeaData_3Text %>% unnest_tokens(word, Herb_3Text)
TeaData_4token <- TeaData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
TeaData_AllToken <- rbind(TeaData_1token, TeaData_2token, TeaData_3token, TeaData_4token)
TeaData_AllToken

# Remove excess data frames
rm(TeaData_1Text, TeaData_2Text, TeaData_3Text, TeaData_4Text, TeaData_1token, TeaData_2token, TeaData_3token, TeaData_4token)
###################################################################################################
# TEA: Frequency of each word, regardless of Herb_Numb level
TeaData_WordCount <- TeaData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# TEA: Frequency of word pairs, taking into account the observation & Herb_Numb levels
TeaData_WordPairs <- TeaData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# TEA: Check data
TeaData_WordCount
TeaData_WordPairs

# TEA: Export to .csv
#write.csv(TeaData_WordCount, "TeaData_WordCount.csv")
#write.csv(TeaData_WordPairs, "TeaData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
TeaData_WordPairs %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at "oil"
# Create new dataframe for each Herb_#Text, filtered by "oil", plus create a level to distinguish between herb levels when stacked. 
OilData_1Text <- data_frame(obs = HerbData$obs, Herb_1Text = HerbData$Herb_1Text) %>% 
  mutate(Herb_Numb=1) %>% 
  mutate(Oil.yn = str_detect(Herb_1Text, "oil")) %>% 
  filter(Oil.yn == TRUE)
OilData_2Text <- data_frame(obs = HerbData$obs, Herb_2Text = HerbData$Herb_2Text) %>% 
  mutate(Herb_Numb=2) %>%
  mutate(Oil.yn = str_detect(Herb_2Text, "oil")) %>% 
  filter(Oil.yn == TRUE)
OilData_3Text <- data_frame(obs = HerbData$obs, Herb_3Text = HerbData$Herb_3Text) %>% 
  mutate(Herb_Numb=3) %>%
  mutate(Oil.yn = str_detect(Herb_3Text, "oil")) %>% 
  filter(Oil.yn == TRUE)
OilData_4Text <- data_frame(obs = HerbData$obs, Herb_4Text = HerbData$Herb_4Text) %>% 
  mutate(Herb_Numb=4) %>%
  mutate(Oil.yn = str_detect(Herb_4Text, "oil")) %>% 
  filter(Oil.yn == TRUE)

# Tokenize each dataframe for each Herb_#Text
OilData_1token <- OilData_1Text %>% unnest_tokens(word, Herb_1Text)
OilData_2token <- OilData_2Text %>% unnest_tokens(word, Herb_2Text)
OilData_3token <- OilData_3Text %>% unnest_tokens(word, Herb_3Text)
OilData_4token <- OilData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
OilData_AllToken <- rbind(OilData_1token, OilData_2token, OilData_3token, OilData_4token)
OilData_AllToken

rm(OilData_1Text, OilData_2Text, OilData_3Text, OilData_4Text, OilData_1token, OilData_2token, OilData_3token, OilData_4token)

###################################################################################################
# Oil: Frequency of each word, regardless of Herb_Numb level
OilData_WordCount <- OilData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Oil: Frequency of word pairs, taking into account the observation & Herb_Numb levels
OilData_WordPairs <- OilData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Oil: Check data
OilData_WordCount
OilData_WordPairs

# Oil: Export to .csv
#write.csv(OilData_WordCount, "OilData_WordCount.csv")
#write.csv(OilData_WordPairs, "OilData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
OilData_WordPairs %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at "Tincture"
# Create new dataframe for each Herb_#Text, filtered by "oil", plus create a level to distinguish between herb levels when stacked. 
TinctureData_1Text <- data_frame(obs = HerbData$obs, Herb_1Text = HerbData$Herb_1Text) %>% 
  mutate(Herb_Numb=1) %>% 
  mutate(Tincture.yn = str_detect(Herb_1Text, "tincture")) %>% 
  filter(Tincture.yn == TRUE)
TinctureData_2Text <- data_frame(obs = HerbData$obs, Herb_2Text = HerbData$Herb_2Text) %>% 
  mutate(Herb_Numb=2) %>%
  mutate(Tincture.yn = str_detect(Herb_2Text, "tincture")) %>% 
  filter(Tincture.yn == TRUE)
TinctureData_3Text <- data_frame(obs = HerbData$obs, Herb_3Text = HerbData$Herb_3Text) %>% 
  mutate(Herb_Numb=3) %>%
  mutate(Tincture.yn = str_detect(Herb_3Text, "tincture")) %>% 
  filter(Tincture.yn == TRUE)
TinctureData_4Text <- data_frame(obs = HerbData$obs, Herb_4Text = HerbData$Herb_4Text) %>% 
  mutate(Herb_Numb=4) %>%
  mutate(Tincture.yn = str_detect(Herb_4Text, "tincture")) %>% 
  filter(Tincture.yn == TRUE)

# Tokenize each dataframe for each Herb_#Text
TinctureData_1token <- TinctureData_1Text %>% unnest_tokens(word, Herb_1Text)
TinctureData_2token <- TinctureData_2Text %>% unnest_tokens(word, Herb_2Text)
TinctureData_3token <- TinctureData_3Text %>% unnest_tokens(word, Herb_3Text)
TinctureData_4token <- TinctureData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
TinctureData_AllToken <- rbind(TinctureData_1token, TinctureData_2token, TinctureData_3token, TinctureData_4token)
TinctureData_AllToken

rm(TinctureData_1Text, TinctureData_2Text, TinctureData_3Text, TinctureData_4Text, TinctureData_1token, TinctureData_2token, TinctureData_3token, TinctureData_4token)

###################################################################################################
# Tincture: Frequency of each word, regardless of Herb_Numb level
TinctureData_WordCount <- TinctureData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Tincture: Frequency of word pairs, taking into account the observation & Herb_Numb levels
TinctureData_WordPairs <- TinctureData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Tincture: Check data
TinctureData_WordCount
TinctureData_WordPairs

# Tincture: Export to .csv
#write.csv(TinctureData_WordCount, "TinctureData_WordCount.csv")
#write.csv(TinctureData_WordPairs, "TinctureData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
TinctureData_WordPairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at "Preg"
# Create new dataframe for each Herb_#Text, filtered by "oil", plus create a level to distinguish between herb levels when stacked. 
PregData_1Text <- data_frame(obs = HerbData$obs, Herb_1Text = HerbData$Herb_1Text) %>% 
  mutate(Herb_Numb=1) %>% 
  mutate(Preg.yn = str_detect(Herb_1Text, "pregna")) %>% 
  filter(Preg.yn == TRUE)
PregData_2Text <- data_frame(obs = HerbData$obs, Herb_2Text = HerbData$Herb_2Text) %>% 
  mutate(Herb_Numb=2) %>%
  mutate(Preg.yn = str_detect(Herb_2Text, "pregna")) %>% 
  filter(Preg.yn == TRUE)
PregData_3Text <- data_frame(obs = HerbData$obs, Herb_3Text = HerbData$Herb_3Text) %>% 
  mutate(Herb_Numb=3) %>%
  mutate(Preg.yn = str_detect(Herb_3Text, "pregna")) %>% 
  filter(Preg.yn == TRUE)
PregData_4Text <- data_frame(obs = HerbData$obs, Herb_4Text = HerbData$Herb_4Text) %>% 
  mutate(Herb_Numb=4) %>%
  mutate(Preg.yn = str_detect(Herb_4Text, "pregna")) %>% 
  filter(Preg.yn == TRUE)

# Tokenize each dataframe for each Herb_#Text
PregData_1token <- PregData_1Text %>% unnest_tokens(word, Herb_1Text)
PregData_2token <- PregData_2Text %>% unnest_tokens(word, Herb_2Text)
PregData_3token <- PregData_3Text %>% unnest_tokens(word, Herb_3Text)
PregData_4token <- PregData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
PregData_AllToken <- rbind(PregData_1token, PregData_2token, PregData_3token, PregData_4token)
PregData_AllToken

rm(PregData_1Text, PregData_2Text, PregData_3Text, PregData_4Text, PregData_1token, PregData_2token, PregData_3token, PregData_4token)

###################################################################################################
# Preg: Frequency of each word, regardless of Herb_Numb level
PregData_WordCount <- PregData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Preg: Frequency of word pairs, taking into account the observation & Herb_Numb levels
PregData_WordPairs <- PregData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Preg: Check data
PregData_WordCount
PregData_WordPairs

# Preg: Export to .csv
#write.csv(PregData_WordCount, "PregData_WordCount.csv")
#write.csv(PregData_WordPairs, "PregData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
PregData_WordPairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at "black"
# Create new dataframe for each Herb_#Text, filtered by "oil", plus create a level to distinguish between herb levels when stacked. 
BlackData_1Text <- data_frame(obs = HerbData$obs, Herb_1Text = HerbData$Herb_1Text) %>% 
  mutate(Herb_Numb=1) %>% 
  mutate(Black.yn = str_detect(Herb_1Text, "black")) %>% 
  filter(Black.yn == TRUE)
BlackData_2Text <- data_frame(obs = HerbData$obs, Herb_2Text = HerbData$Herb_2Text) %>% 
  mutate(Herb_Numb=2) %>%
  mutate(Black.yn = str_detect(Herb_2Text, "black")) %>% 
  filter(Black.yn == TRUE)
BlackData_3Text <- data_frame(obs = HerbData$obs, Herb_3Text = HerbData$Herb_3Text) %>% 
  mutate(Herb_Numb=3) %>%
  mutate(Black.yn = str_detect(Herb_3Text, "black")) %>% 
  filter(Black.yn == TRUE)
BlackData_4Text <- data_frame(obs = HerbData$obs, Herb_4Text = HerbData$Herb_4Text) %>% 
  mutate(Herb_Numb=4) %>%
  mutate(Black.yn = str_detect(Herb_4Text, "black")) %>% 
  filter(Black.yn == TRUE)

# Tokenize each dataframe for each Herb_#Text
BlackData_1token <- BlackData_1Text %>% unnest_tokens(word, Herb_1Text)
BlackData_2token <- BlackData_2Text %>% unnest_tokens(word, Herb_2Text)
BlackData_3token <- BlackData_3Text %>% unnest_tokens(word, Herb_3Text)
BlackData_4token <- BlackData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
BlackData_AllToken <- rbind(BlackData_1token, BlackData_2token, BlackData_3token, BlackData_4token)
BlackData_AllToken

# Remove extra data frames
rm(BlackData_1Text, BlackData_2Text, BlackData_3Text, BlackData_4Text, BlackData_1token, BlackData_2token, BlackData_3token, BlackData_4token)

###################################################################################################
# Black: Frequency of each word, regardless of Herb_Numb level
BlackData_WordCount <- BlackData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Black: Frequency of word pairs, taking into account the observation & Herb_Numb levels
BlackData_WordPairs <- BlackData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Black: Check data
BlackData_WordCount
BlackData_WordPairs

# Black: Export to .csv
#write.csv(BlackData_WordCount, "BlackData_WordCount.csv")
#write.csv(BlackData_WordPairs, "BlackData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
BlackData_WordPairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at "Prenatal"
# Create new dataframe for each Herb_#Text, filtered by "prenatal", plus create a level to distinguish between herb levels when stacked. 
PrenatalData_1Text <- data_frame(obs = HerbData$obs, Herb_1Text = HerbData$Herb_1Text) %>% 
  mutate(Herb_Numb=1) %>% 
  mutate(Prenatal.yn = str_detect(Herb_1Text, "prenatal")) %>% 
  filter(Prenatal.yn == TRUE)
PrenatalData_2Text <- data_frame(obs = HerbData$obs, Herb_2Text = HerbData$Herb_2Text) %>% 
  mutate(Herb_Numb=2) %>%
  mutate(Prenatal.yn = str_detect(Herb_2Text, "prenatal")) %>% 
  filter(Prenatal.yn == TRUE)
PrenatalData_3Text <- data_frame(obs = HerbData$obs, Herb_3Text = HerbData$Herb_3Text) %>% 
  mutate(Herb_Numb=3) %>%
  mutate(Prenatal.yn = str_detect(Herb_3Text, "prenatal")) %>% 
  filter(Prenatal.yn == TRUE)
PrenatalData_4Text <- data_frame(obs = HerbData$obs, Herb_4Text = HerbData$Herb_4Text) %>% 
  mutate(Herb_Numb=4) %>%
  mutate(Prenatal.yn = str_detect(Herb_4Text, "prenatal")) %>% 
  filter(Prenatal.yn == TRUE)

# Tokenize each dataframe for each Herb_#Text
PrenatalData_1token <- PrenatalData_1Text %>% unnest_tokens(word, Herb_1Text)
PrenatalData_2token <- PrenatalData_2Text %>% unnest_tokens(word, Herb_2Text)
PrenatalData_3token <- PrenatalData_3Text %>% unnest_tokens(word, Herb_3Text)
PrenatalData_4token <- PrenatalData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
PrenatalData_AllToken <- rbind(PrenatalData_1token, PrenatalData_2token, PrenatalData_3token, PrenatalData_4token)
PrenatalData_AllToken

# Remove extra data frames
rm(PrenatalData_1Text, PrenatalData_2Text, PrenatalData_3Text, PrenatalData_4Text, PrenatalData_1token, PrenatalData_2token, PrenatalData_3token, PrenatalData_4token)

###################################################################################################
# Prenatal: Frequency of each word, regardless of Herb_Numb level
PrenatalData_WordCount <- PrenatalData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Prenatal: Frequency of word pairs, taking into account the observation & Herb_Numb levels
PrenatalData_WordPairs <- PrenatalData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Prenatal: Check data
PrenatalData_WordCount
PrenatalData_WordPairs

# Prenatal: Export to .csv
#write.csv(PrenatalData_WordCount, "PrenatalData_WordCount.csv")
#write.csv(PrenatalData_WordPairs, "PrenatalData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
PrenatalData_WordPairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at "birth"
# Create new dataframe for each Herb_#Text, filtered by "birth", plus create a level to distinguish between herb levels when stacked. 
BirthData_1Text <- data_frame(obs = HerbData$obs, Herb_1Text = HerbData$Herb_1Text) %>% 
  mutate(Herb_Numb=1) %>% 
  mutate(Birth.yn = str_detect(Herb_1Text, "birth")) %>% 
  filter(Birth.yn == TRUE)
BirthData_2Text <- data_frame(obs = HerbData$obs, Herb_2Text = HerbData$Herb_2Text) %>% 
  mutate(Herb_Numb=2) %>%
  mutate(Birth.yn = str_detect(Herb_2Text, "birth")) %>% 
  filter(Birth.yn == TRUE)
BirthData_3Text <- data_frame(obs = HerbData$obs, Herb_3Text = HerbData$Herb_3Text) %>% 
  mutate(Herb_Numb=3) %>%
  mutate(Birth.yn = str_detect(Herb_3Text, "birth")) %>% 
  filter(Birth.yn == TRUE)
BirthData_4Text <- data_frame(obs = HerbData$obs, Herb_4Text = HerbData$Herb_4Text) %>% 
  mutate(Herb_Numb=4) %>%
  mutate(Birth.yn = str_detect(Herb_4Text, "birth")) %>% 
  filter(Birth.yn == TRUE)

# Tokenize each dataframe for each Herb_#Text
BirthData_1token <- BirthData_1Text %>% unnest_tokens(word, Herb_1Text)
BirthData_2token <- BirthData_2Text %>% unnest_tokens(word, Herb_2Text)
BirthData_3token <- BirthData_3Text %>% unnest_tokens(word, Herb_3Text)
BirthData_4token <- BirthData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
BirthData_AllToken <- rbind(BirthData_1token, BirthData_2token, BirthData_3token, BirthData_4token)
BirthData_AllToken

# Remove extra data frames
rm(BirthData_1Text, BirthData_2Text, BirthData_3Text, BirthData_4Text, BirthData_1token, BirthData_2token, BirthData_3token, BirthData_4token)

###################################################################################################
# Birth: Frequency of each word, regardless of Herb_Numb level
BirthData_WordCount <- BirthData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Birth: Frequency of word pairs, taking into account the observation & Herb_Numb levels
BirthData_WordPairs <- BirthData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Birth: Check data
BirthData_WordCount
BirthData_WordPairs

# Birth: Export to .csv
#write.csv(BirthData_WordCount, "BirthData_WordCount.csv")
#write.csv(BirthData_WordPairs, "BirthData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
BirthData_WordPairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at "formula"
# Create new dataframe for each Herb_#Text, filtered by "oil", plus create a level to distinguish between herb levels when stacked. 
FormulaData_1Text <- data_frame(obs = HerbData$obs, Herb_1Text = HerbData$Herb_1Text) %>% 
  mutate(Herb_Numb=1) %>% 
  mutate(Formula.yn = str_detect(Herb_1Text, "formula")) %>% 
  filter(Formula.yn == TRUE)
FormulaData_2Text <- data_frame(obs = HerbData$obs, Herb_2Text = HerbData$Herb_2Text) %>% 
  mutate(Herb_Numb=2) %>%
  mutate(Formula.yn = str_detect(Herb_2Text, "formula")) %>% 
  filter(Formula.yn == TRUE)
FormulaData_3Text <- data_frame(obs = HerbData$obs, Herb_3Text = HerbData$Herb_3Text) %>% 
  mutate(Herb_Numb=3) %>%
  mutate(Formula.yn = str_detect(Herb_3Text, "formula")) %>% 
  filter(Formula.yn == TRUE)
FormulaData_4Text <- data_frame(obs = HerbData$obs, Herb_4Text = HerbData$Herb_4Text) %>% 
  mutate(Herb_Numb=4) %>%
  mutate(Formula.yn = str_detect(Herb_4Text, "formula")) %>% 
  filter(Formula.yn == TRUE)

# Tokenize each dataframe for each Herb_#Text
FormulaData_1token <- FormulaData_1Text %>% unnest_tokens(word, Herb_1Text)
FormulaData_2token <- FormulaData_2Text %>% unnest_tokens(word, Herb_2Text)
FormulaData_3token <- FormulaData_3Text %>% unnest_tokens(word, Herb_3Text)
FormulaData_4token <- FormulaData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
FormulaData_AllToken <- rbind(FormulaData_1token, FormulaData_2token, FormulaData_3token, FormulaData_4token)
FormulaData_AllToken

# Remove extra data frames
rm(FormulaData_1Text, FormulaData_2Text, FormulaData_3Text, FormulaData_4Text, FormulaData_1token, FormulaData_2token, FormulaData_3token, FormulaData_4token)

###################################################################################################
# Formula: Frequency of each word, regardless of Herb_Numb level
FormulaData_WordCount <- FormulaData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Formula: Frequency of word pairs, taking into account the observation & Herb_Numb levels
FormulaData_WordPairs <- FormulaData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Formula: Check data
FormulaData_WordCount
FormulaData_WordPairs

# Formula: Export to .csv
#write.csv(FormulaData_WordCount, "FormulaData_WordCount.csv")
#write.csv(FormulaData_WordPairs, "FormulaData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
FormulaData_WordPairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at "gentle"
# Create new dataframe for each Herb_#Text, filtered by "gentle", plus create a level to distinguish between herb levels when stacked. 
GentleData_1Text <- data_frame(obs = HerbData$obs, Herb_1Text = HerbData$Herb_1Text) %>% 
  mutate(Herb_Numb=1) %>% 
  mutate(Gentle.yn = str_detect(Herb_1Text, "gentle")) %>% 
  filter(Gentle.yn == TRUE)
GentleData_2Text <- data_frame(obs = HerbData$obs, Herb_2Text = HerbData$Herb_2Text) %>% 
  mutate(Herb_Numb=2) %>%
  mutate(Gentle.yn = str_detect(Herb_2Text, "gentle")) %>% 
  filter(Gentle.yn == TRUE)
GentleData_3Text <- data_frame(obs = HerbData$obs, Herb_3Text = HerbData$Herb_3Text) %>% 
  mutate(Herb_Numb=3) %>%
  mutate(Gentle.yn = str_detect(Herb_3Text, "gentle")) %>% 
  filter(Gentle.yn == TRUE)
GentleData_4Text <- data_frame(obs = HerbData$obs, Herb_4Text = HerbData$Herb_4Text) %>% 
  mutate(Herb_Numb=4) %>%
  mutate(Gentle.yn = str_detect(Herb_4Text, "gentle")) %>% 
  filter(Gentle.yn == TRUE)

# Tokenize each dataframe for each Herb_#Text
GentleData_1token <- GentleData_1Text %>% unnest_tokens(word, Herb_1Text)
GentleData_2token <- GentleData_2Text %>% unnest_tokens(word, Herb_2Text)
GentleData_3token <- GentleData_3Text %>% unnest_tokens(word, Herb_3Text)
GentleData_4token <- GentleData_4Text %>% unnest_tokens(word, Herb_4Text)

# Stack all tokenized data frames. 
GentleData_AllToken <- rbind(GentleData_1token, GentleData_2token, GentleData_3token, GentleData_4token)
GentleData_AllToken

# Remove extra data frames
rm(GentleData_1Text, GentleData_2Text, GentleData_3Text, GentleData_4Text, GentleData_1token, GentleData_2token, GentleData_3token, GentleData_4token)

###################################################################################################
# Gentle: Frequency of each word, regardless of Herb_Numb level
GentleData_WordCount <- GentleData_AllToken %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Gentle: Frequency of word pairs, taking into account the observation & Herb_Numb levels
GentleData_WordPairs <- GentleData_AllToken %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Gentle: Check data
GentleData_WordCount
GentleData_WordPairs

# Gentle: Export to .csv
#write.csv(GentleData_WordCount, "GentleData_WordCount.csv")
#write.csv(GentleData_WordPairs, "GentleData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
GentleData_WordPairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at Herb_OtherText
OtherData_Text <- data_frame(obs = HerbData$obs, Herb_OtherTxt = HerbData$Herb_OtherTxt)

# Tokenize each dataframe for each Herb_#Text
OtherData_token <- OtherData_Text %>% unnest_tokens(word, Herb_OtherTxt)

# Remove extra data frames
rm(OtherData_Text)

###################################################################################################
# Other: Frequency of each word, regardless of Herb_Numb level
OtherData_WordCount <- OtherData_token %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Other: Frequency of word pairs, taking into account the observation & Herb_Numb levels
OtherData_WordPairs <- OtherData_token %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Other: Check data
OtherData_WordCount
OtherData_WordPairs

# Other: Export to .csv
#write.csv(OtherData_WordCount, "OtherData_WordCount.csv")
#write.csv(OtherData_WordPairs, "OtherData_WordPairs.csv")

# Generate a visual graph to depict the pairs.
OtherData_WordPairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at Reason
# Create new dataframe for each Herb_#Reason, plus create a level to distinguish between herb levels when stacked. 
HerbData_1Reason <- data_frame(obs = HerbData$obs, Herb_1Reason = HerbData$Herb_1Reason) %>% 
  mutate(Herb_Numb=1)
HerbData_2Reason <- data_frame(obs = HerbData$obs, Herb_2Reason = HerbData$Herb_2Reason) %>% 
  mutate(Herb_Numb=2) 
HerbData_3Reason <- data_frame(obs = HerbData$obs, Herb_3Reason = HerbData$Herb_3Reason) %>% 
  mutate(Herb_Numb=3)
HerbData_4Reason <- data_frame(obs = HerbData$obs, Herb_4Reason = HerbData$Herb_4Reason) %>% 
  mutate(Herb_Numb=4) 

# Tokenize each dataframe for each Herb_#Reason
HerbData_1token_reas <- HerbData_1Reason %>% unnest_tokens(word, Herb_1Reason)
HerbData_2token_reas <- HerbData_2Reason %>% unnest_tokens(word, Herb_2Reason)
HerbData_3token_reas <- HerbData_3Reason %>% unnest_tokens(word, Herb_3Reason)
HerbData_4token_reas <- HerbData_4Reason %>% unnest_tokens(word, Herb_4Reason)

# Stack all tokenized data frames. 
HerbData_AllToken_reas <- rbind(HerbData_1token_reas, HerbData_2token_reas, HerbData_3token_reas, HerbData_4token_reas)
HerbData_AllToken_reas

# Remove extra data frames
rm(HerbData_1Reason, HerbData_2Reason, HerbData_3Reason, HerbData_4Reason, HerbData_1token_reas, HerbData_2token_reas, HerbData_3token_reas, HerbData_4token_reas)
###################################################################################################
# Herb: Frequency of each word, regardless of Herb_Numb level
HerbData_WordCount_reas <- HerbData_AllToken_reas %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

# Herb: Frequency of word pairs, taking into account the observation & Herb_Numb levels
HerbData_WordPairs_reas <- HerbData_AllToken_reas %>% 
  anti_join(my_stop_words) %>%
  pairwise_count(word, obs, Herb_Numb, sort = TRUE, upper = FALSE)%>%
  ungroup()

# Herb: Check data
HerbData_WordCount_reas
HerbData_WordPairs_reas

# Herb: Export to .csv
#write.csv(HerbData_WordCount_reas, "HerbData_WordCount_reas.csv")
#write.csv(HerbData_WordPairs_reas, "HerbData_WordPairs_reas.csv")

# Generate a visual graph to depict the pairs.
HerbData_WordPairs_reas %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
###################################################################################################
# Look more closely at Blood Actions Other Herb
# Clean Text
HerbData$Blood_ActionsHerbs <- str_to_lower(HerbData$Blood_ActionsHerbs)
HerbData$Blood_ActionsHerbs<- gsub("/", " ", HerbData$Blood_ActionsHerbs, fixed=TRUE)
HerbData$Blood_ActionsHerbs <- removePunctuation(HerbData$Blood_ActionsHerbs)
HerbData$Blood_ActionsHerbs <- stripWhitespace(HerbData$Blood_ActionsHerbs)
HerbData$Blood_ActionsHerbs <- as.character(HerbData$Blood_ActionsHerbs)

# Create new dataframe for each Blood Actions Other Herb, plus create a level to distinguish between herb levels when stacked.
HerbData_Blood_ActionsHerbs <- data_frame(id = HerbData$id, Blood_ActionsHerbs = HerbData$Blood_ActionsHerbs)

# Tokenize each dataframe for each Herb_#Reason
HerbData_token_Blood_ActionsHerbs <- HerbData_Blood_ActionsHerbs %>% unnest_tokens(word, Blood_ActionsHerbs)

# Remove extra data frames
rm(HerbData_Blood_ActionsHerbs)
###################################################################################################
# Blood Actions Other Herbs: Frequency of each word, regardless of Herb_Numb level
BloodActOther_WordCount <- HerbData_token_Blood_ActionsHerbs %>%
 anti_join(my_stop_words) %>%
 count(word, sort = TRUE) %>%
 ungroup()

 # Blood Actions Other Herbs: Frequency of word pairs, taking into account the observation & Herb_Numb levels
BloodActOther_WordPairs <- HerbData_token_Blood_ActionsHerbs %>%
 anti_join(my_stop_words) %>%
 pairwise_count(word, id, sort = TRUE, upper = FALSE)%>%
 ungroup()

 # Blood Actions Other Herbs: Check data
BloodActOther_WordCount
BloodActOther_WordPairs

 # Induct/Aug Other Herbs: Export to .csv
 write.csv(BloodActOther_WordCount, "BloodActOther_WordCount_paige_v02.csv")
 write.csv(BloodActOther_WordPairs, "BloodActOther_WordPairs_paige_v02.csv")

 # Generate a visual graph to depict the pairs.
BloodActOther_WordPairs %>%
  filter(n >= 50) %>%
 graph_from_data_frame() %>%
 ggraph(layout = "fr") +
 geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
 geom_node_point(size = 5) +
 geom_node_text(aes(label = name), repel = TRUE,
                point.padding = unit(0.2, "lines")) +
 theme_void()
##################################################################################################
# Remove excess data frames
rm(BloodActOther_WordCount, BloodActOther_WordPairs, HerbData_WordCount, HerbData_WordPairs)