library(graphics)
library(purrr)
library(stringr) 
library(tm)
library(syuzhet)
library(textclean)
library(tidyverse)
library(dplyr)
library(corrplot)
library(readr)
library(lubridate)
library(plotly)
library(readxl)
library(sf)
library(rgdal)
library(spdplyr)
library(tigris)
library(likert)
library(grid)
library(corrplot)

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

########################### VISUALIZATION - emotional analysis for 11 candidates #############################

#START VISUALIZATION - BAR CHART FOR 10 EMOTIONS

#emotional visualization Arthaud
barplot(colSums(emotions_Arthaud_16_0),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets for Arthaud")

df_arthaud <- emotions_Arthaud_16_0 %>% 
  mutate(total = sum(.)) %>% 
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

#END VISUALIZATION - BAR CHART FOR 10 EMOTIONS
################################################################

#creating a table of emotions for 11 candidates

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



#################################################################
#START - GENERAL - interest over time - GOOGLE TRENDS
#################################################################
#reading data
interests_over_time <- read_csv("interests_over_time.csv")

#data preparation 
df_interest <- interests_over_time %>% 
  pivot_longer(names_to = "candidate", values_to = "cnt", -c(index, day)) %>% 
  mutate(day = dmy(day))

#visualization
plot1 <- ggplot(data= df_interest, aes(day, cnt, color = candidate)) + geom_line() +theme_minimal()

ggplotly(plot1)       
################### END - GENERAL - interest over time - GOOGLE TRENDS ######################


######################## all emotions for all candidates in one plot ################

##########  1 ###########
emotions_all_percentages <- read_excel("emotions_all_percentages.xlsx")

df_em <- emotions_all_percentages %>% 
  pivot_longer(names_to = "emotions", values_to = "em_percent", -id)

ggplot(data=df_em, mapping= aes(x=id, y=em_percent, fill=emotions)) + 
  geom_col() + xlab("Candidate") +ylab("Emotions %") + ggtitle("Emtions % for each Candidate")


##########  2 ###########
#df_em2 <- df_em %>% 
#  mutate(emotions2 = ifelse(emotions == "anticipation" , "positive",
#                            ifelse(emotions == "joy", "positive",
#                                   ifelse(emotions == "positive",  "positive",
#                                          ifelse(emotions == "trust", "positive", 
#                                                 ifelse(emotions == "anger", "negative",
#                                                        ifelse(emotions == "disgust", "negative",
#                                                               ifelse(emotions == "fear", "negative",
#                                                                      ifelse(emotions == "negative", "negative",
#                                                                             ifelse(emotions == "sadness", "negative", "neutral")))))))))) 
#                           
#ggplot(data=df_em2, mapping= aes(x=id, y=em_percent, fill=emotions2)) +
#  geom_col() + xlab("Candidate") +ylab("Emotions %") + ggtitle("Emtions % for each Candidate")

###################
arthaud <- emotions_Arthaud_16_0 %>% mutate(id = "Arthaud")
Asselineau <- emotions_Asselineau_14_3 %>% mutate(id = "Asselineau")
Cheminade <- emotions_Cheminade_14_3 %>% mutate(id = "Cheminade")
Dupont <- emotions_Dupont_14_3 %>% mutate(id = "Dupont")
Fillon <- emotions_Fillon_14_3 %>% mutate(id = "Fillon")
Hamon <- emotions_Hamon_14_3 %>% mutate(id = "Hamon")
Lassalle <- emotions_Lassalle_14_3 %>% mutate(id = "Lassalle")
lepen <- emotions_lepen_14_3 %>% mutate(id = "Lepen")
macron <- emotions_macron_14_3 %>% mutate(id = "Macron")
Melenchon <- emotions_Melenchon_14_3 %>% mutate(id = "Melenchon")
Poutou <- emotions_Poutou_14_3 %>% mutate(id = "Poutou")

#binding all the above datasets
test <- rbind(arthaud,
              Asselineau,
              Cheminade,
              Dupont,
              Fillon,
              Hamon,
              Lassalle,
              lepen,
              macron,
              Melenchon,
              Poutou)

#correlation of the emotions
corrplot(cor(test[,1:10]), order = 'AOE', type = 'lower')

#aggregating the emotions and puting them into only two categories: positive and negative
test1 <- test %>% mutate(manfi = anger + sadness + negative + fear + disgust) %>% 
  mutate(mosbat = positive + joy + trust + anticipation + surprise)

test1 <- test1 %>% select(id, manfi, mosbat) %>% rename(negative = manfi, positive = mosbat)

test2 <- test1 %>% filter(negative !=0 | positive != 0) %>% filter(negative != positive)

test2 <- test2  %>%  mutate(status = ifelse (negative > positive, "negative", "positive" ))


#df_whole_withoutzero <- df_whole_withoutzero %>% 
#                              mutate(emotion = factor(emotion, 
#                                                      ordered = T), 
#                                     id= as.factor(id)) %>% as.data.frame()
#
#likert_test = likert(df_whole_withoutzero[,2, drop = FALSE], grouping = df_whole_withoutzero$id)
#plot(likert_test, include.histogram = F)

####################### glm  ############
test2$id <- as.factor(test2$id)

glm.fit = glm(status ~ id, data = test2, family=binomial)
summary(glm.fit)

coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

glm.probs= predict(glm.fit, type = "response")
contrasts(Direction)

glm.pred = rep("Down", 1250)
glm.pred[glm.probs >0.5]="Up"

table(glm.pred, Direction)

####################### glm  ############
####################### chi squared test ############

test2 %>% select(id, status) %>%  mutate(status= as.factor(status)) %>% 
  ggplot(aes(status, ..count..)) + 
  geom_bar(aes(fill = id), position = "dodge") + theme_bw()

test2<- test2 %>%  mutate(status= as.factor(status)) 

chisq.test(test2$id,test2$status)
