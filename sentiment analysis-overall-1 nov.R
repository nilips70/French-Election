library(graphics)
library(purrr)
library(stringr) 
library(tm)
library(syuzhet)
library(textclean)
library(readxl)
library(tidyverse)
library(dplyr)
rm(list=ls())

#reading emotions scores datasets
emotions_Arthaud_16_0 <- read_excel("emotions/emotions_Arthaud_16_0.xlsx")
emotions_Asselineau_14_3 <- read_excel("emotions/emotions_Asselineau_14_3.xlsx")
emotions_Cheminade_14_3 <- read_excel("emotions/emotions_Cheminade_14_3.xlsx")
emotions_Dupont_14_3 <- read_excel("emotions/emotions_Dupont_14_3.xlsx")
emotions_Fillon_14_3 <- read_excel("emotions/emotions_Fillon_14_3.xlsx")
emotions_Hamon_14_3 <- read_excel("emotions/emotions_Hamon_14_3.xlsx")
emotions_Lassalle_14_3 <- read_excel("emotions/emotions_Lassalle_14_3.xlsx")
emotions_lepen_14_3 <- read_excel("emotions/emotions_lepen_14_3.xlsx")
emotions_macron_14_3 <- read_excel("emotions/emotions_macron_14_3.xlsx")
emotions_Melenchon_14_3 <- read_excel("emotions/emotions_Melenchon_14_3.xlsx")
emotions_Poutou_14_3 <- read_excel("emotions/emotions_Poutou_14_3.xlsx")

########################### emotional analysi for 11 candidates #############################

#emotional visualization Arthaud
barplot(colSums(emotions_Arthaud_16_0),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Arthaud")

df_arthaud <- emotions_Arthaud_16_0 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Arthaud")


#emotional visualization Asselineau
barplot(colSums(emotions_Asselineau_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Asselineau")

df_Asselineau <- emotions_Asselineau_14_3 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Asselineau")

#emotional visualization Cheminade
barplot(colSums(emotions_Cheminade_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Cheminade")

df_Cheminade <- emotions_Cheminade_14_3 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Cheminade")


#emotional visualization Dupont
barplot(colSums(emotions_Dupont_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Dupont")

df_Dupont <- emotions_Dupont_14_3 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Dupont")


#emotional visualization Fillon
barplot(colSums(emotions_Fillon_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Fillon")

df_Fillon <- emotions_Fillon_14_3 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Fillon")


#emotional visualization Hamon
barplot(colSums(emotions_Hamon_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Hamon")

df_Hamon <- emotions_Hamon_14_3 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Hamon")


#emotional visualization Lassalle
barplot(colSums(emotions_Lassalle_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Lassalle")

df_Lassalle <- emotions_Lassalle_14_3 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Lassalle")


#emotional visualization lepen
barplot(colSums(emotions_lepen_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for LePen")

df_lepen <- emotions_lepen_14_3 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Lepen")


#emotional visualization macron
barplot(colSums(emotions_macron_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Macron")

df_macron <- emotions_macron_14_3 %>% 
  mutate(total = sum(.) 
      ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Macron")


#emotional visualization Melenchon
barplot(colSums(emotions_Melenchon_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Melenchon")

df_Melenchon <- emotions_Melenchon_14_3 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Melenchon")


#emotional visualization Poutou
barplot(colSums(emotions_Poutou_14_3),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Poutou")

df_Poutou <- emotions_Poutou_14_3 %>% 
  mutate(total = sum(.) 
  ) %>% 
  summarise_all(~ mean(sum(.) / total)) %>% 
  select(-total) %>%  mutate(   id = "Poutou")
################################################################
df_emotions <- list(
  df_arthaud,
  df_Asselineau,
  df_Cheminade,
  df_Poutou,
  df_Melenchon,
  df_macron,
  df_lepen,
  df_Lassalle,
  df_Hamon,
  df_Fillon,
  df_Dupont
) %>% reduce(full_join)

#write_xlsx(df_emotions,"emotions_all_percentages.xlsx")
