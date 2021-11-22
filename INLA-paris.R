library(tidyverse)
library(leaflet)
library(corrplot)
library(purrr)
library(sf)
library(INLA)
library(spdep)  #adjacency matrix
rm(list = ls())

# ========================================================================================================
# ========================================= Using updated dataset ========================================
# ========================================================================================================
#reading the dataset
df_merged <- readRDS("parisarea.rds")

#data preparation
#Louan-Villegruis-Fontaine, 481, lepen
df_merged$LePenVotes[which(df_merged$COM_NAME == "Louan-Villegruis-Fontaine")] <- df_merged$LePenVotes[which(df_merged$COM_NAME == "Louan-Villegruis-Fontaine")] + 1  

#Chauffour-lès-Etréchy, 47, fillon
df_merged$FillonVotes[which(df_merged$COM_NAME == "Chauffour-lès-Etréchy")] <- df_merged$FillonVotes[which(df_merged$COM_NAME == "Chauffour-lès-Etréchy")] + 1

#Puiselet-le-Marais, 136, fillon
df_merged$FillonVotes[which(df_merged$COM_NAME == "Puiselet-le-Marais")] <- df_merged$FillonVotes[which(df_merged$COM_NAME == "Puiselet-le-Marais")] + 1

#Souzy-la-Briche, 167, fillon
df_merged$LePenVotes[which(df_merged$COM_NAME == "Souzy-la-Briche")] <- df_merged$LePenVotes[which(df_merged$COM_NAME == "Souzy-la-Briche")] + 1

#Congerville-Thionville, 168, macron
df_merged$MacronVotes[which(df_merged$COM_NAME == "Congerville-Thionville")] <- df_merged$MacronVotes[which(df_merged$COM_NAME == "Congerville-Thionville")] + 1

#La Chapelle-Iger, 314, macron
df_merged$LePenVotes[which(df_merged$COM_NAME == "La Chapelle-Iger")] <- df_merged$LePenVotes[which(df_merged$COM_NAME == "La Chapelle-Iger")] + 1

#Montenils, 522, lepen
df_merged$LePenVotes[which(df_merged$COM_NAME == "Montenils")] <- df_merged$LePenVotes[which(df_merged$COM_NAME == "Montenils")] + 1

#Mours, 898, lepen
df_merged$LePenVotes[which(df_merged$COM_NAME == "Mours")] <- df_merged$LePenVotes[which(df_merged$COM_NAME == "Mours")] + 1

#Paray-Douaville ,1195, lepen
df_merged$LePenVotes[which(df_merged$COM_NAME == "Paray-Douaville")] <- df_merged$LePenVotes[which(df_merged$COM_NAME == "Paray-Douaville")] + 1

#Richebourg ,1211, fillon
df_merged$FillonVotes[which(df_merged$COM_NAME == "Richebourg")] <- df_merged$FillonVotes[which(df_merged$COM_NAME == "Richebourg")] + 1



#Extracting adjacency matrix
g <- poly2nb(df_merged) 

#Visualising Adjacency matrix based on the row's number
image(inla.graph2matrix(g),xlab="",ylab="")  

#adding a column for row's number
df_merged <- df_merged %>% mutate(code = seq(1,nrow(.),1)) %>% select(-c(DEP_NAME, COM_NAME, Voted, Registered, population, 
                                                                         turnout_rate, MacronPct, LePenPct, FillonPct, MelenchonPct,
                                                                         temprange, Paris, EU_circo, agriworkers, distance, immigrants, working, unemployed,
                                                                         whitecollar, oversixty)) 

#data preparation for converting multinomial into poisson dist.
df_final <- df_merged %>%       
  st_drop_geometry() %>% 
  pivot_longer(names_to = "candidate", values_to = "vote",
               -c(X, Y, elderly_rate:code))

#preparing a dictionary for recognizing the candidates
cand_code <- unique(df_final$candidate)
candidate_code <- data.frame(candidate = cand_code, id = seq(1,4,1))

df_final <- left_join(df_final, candidate_code)

#adding spatial effects to the dataset
df_final$sp_effect <- NA        #Lepen
df_final$sp_effect2 <- NA       #macron
df_final$sp_effect3 <- NA       #fillon
df_final$sp_effect4 <- NA       #melenchon

df_final <- df_final %>%
  mutate(sp_effect = ifelse(id == 2, code,sp_effect),
         sp_effect2 = ifelse(id == 1, code,sp_effect2),
         sp_effect3 = ifelse(id == 3, code, sp_effect3),
         sp_effect4 = ifelse(id == 4, code, sp_effect4))

#Checking correlations
corrr <- df_final %>% 
  select(elderly_rate, whitecol_rate, agri_rate, unemployment_rate, poverty_rate, life_exp, education_level, immig_rate)
   

corrplot(cor(corrr), order = "hclust", type = "upper")

#creating id for random effects(bcz inla works this way) 
df_final <- df_final %>% mutate(
  id_im  = id,
  id_po  = id,
  id_ex  = id,
  id_ed  = id,
  id_un = id,
  id_el  = id,
  id_wh = id,
  id_ag = id
)


#har kudum az in f ha ye random effecte age dakhele f chizi nanvisi fek mikone iid hast
#fixed=t yani estimate nashe un hyperparameter, constr=t yani sum coef ha 0 she (sum to zero constraint for this model)
#formula.spatial = vote ~ -1 +
#  f(code, initial = -10, fixed = T) +     
#  f(id_im, percent_imm_dep, fixed = T, constr = T) +
#  f(id_po, poverty_rate, fixed = T, constr = T) +
#  f(id_ex, avg_life, fixed = T, constr = T) +
#  f(id_ed, people_education, fixed = T, constr = T)  +
#  f(id_wa, SNHM14, fixed = T, constr = T) +
#  f(id_un, unemployment_rate, fixed = T, constr = T) +
#  f(id_el, elderly_rate, fixed = T, constr = T) +
#  f(id_wh, whitecol_rate, fixed = T, constr = T) +
#  f(id_ag, agri_rate, fixed = T, constr = T) +
#  f(sp_effect, model = 'besag', graph = g) + #spatial random effect 
#  f(sp_effect2, model = 'besag', graph = g) #+
#f(sp_effect3, model = 'besag', graph = g) #+
# f(sp_effect4, model = 'besag', graph = g)
#f(id_ed, people_education, fixed = T, constr = T)  


formula.spatial = vote ~ -1 +
  f(code, initial = -10, fixed = T) +
  f(id_ed, education_level, fixed = T, constr = T)  +
  f(id_im, immig_rate, fixed = T, constr = T) +
  f(id_el, elderly_rate, fixed = T, constr = T) +
  f(id_un, unemployment_rate, fixed = T, constr = T) +
  f(id_ag, agri_rate, fixed = T, constr = T) +
  f(sp_effect2, model = 'besag', graph = g) + #spatial random effect 
  f(sp_effect3, model = 'besag', graph = g) 
 



######################## Training fit ##################

# Fit the model
model = inla(formula.spatial, 
             family = "poisson", 
             data = df_final,
             control.predictor = list(link = 1 , compute = TRUE))

# Checking model fit on training data, plausibility=unility func
df_accuracy <- df_final %>% 
  mutate(plausibility = model$summary.fitted.values[1:nrow(.), "mode"]) %>% 
  group_by(code) %>% 
  mutate(total_votes = sum(vote),
         total_plausibility = sum(plausibility),
         real_probs = vote/total_votes,
         pred_probs = plausibility/total_plausibility) %>% 
  ungroup()

summary(df_accuracy$real_probs - df_accuracy$pred_probs)


# Confusion matrix on training data
df_conf_train <- df_accuracy %>% 
  group_by(code) %>% 
  summarise(winner_real = .$candidate[which(vote == max(vote))],
            winner_pred = .$candidate[which(pred_probs == max(pred_probs))])


train_conf_tabl <- table(df_conf_train$winner_real, df_conf_train$winner_pred)
train_conf_tabl
sum(diag(train_conf_tabl))/sum(train_conf_tabl) # Accuracy, sume ddiagonal / kol


# =============================================TEST SET=======================================================

# Evaluating model performance on test data
set.seed(1)
samp <- sample(1:length(unique(df_final$code)), 195) # Selecting 30 departments at random

df_test <- df_final %>%   #NA bezar tuye departemanayi ke tu sample hastan
  mutate(com_name = code,
         vote = ifelse(code %in% samp, NA, vote),
         code = ifelse(code %in% samp, NA, code))


model.test = inla(formula.spatial, 
                  family = "poisson", 
                  data = df_test,
                  control.predictor = list(link = 1 , compute = TRUE))


df_pred <- df_test %>% 
  mutate(plausibility = model.test$summary.fitted.values[1:nrow(.), "mode"]) %>% 
  group_by(com_name) %>% 
  mutate(total_plausibility = sum(plausibility),
         pred_probs = plausibility/total_plausibility) %>% 
  ungroup() %>% 
  select(com_name, id, pred_probs) %>% 
  filter(com_name %in% samp)


df_real <- df_final %>% 
  group_by(code) %>% 
  mutate(total_votes = sum(vote),
         true_probs = vote/total_votes,
         com_name = code) %>% 
  ungroup() %>% 
  select(com_name, id, true_probs) %>% 
  filter(com_name %in% samp)


test2 <- left_join(df_real, df_pred) %>% mutate(diff = (true_probs - pred_probs)^2)
summary(test2$diff) 
sqrt(mean(test2$diff))

# Accuracy by confusion matrix
real_winners <- df_final %>% 
  filter(code %in% samp) %>% 
  select(code, candidate, vote) %>% 
  group_by(code) %>% 
  summarise(winner = .$candidate[which(vote == max(vote))]) %>% 
  rename(com_name = code) %>% 
  ungroup()


test <- left_join(df_pred, candidate_code) %>% 
  group_by(com_name) %>% 
  summarise(winner_pred = as.character(.$candidate[which(pred_probs == max(pred_probs))])) %>% 
  ungroup()

t <- full_join(real_winners, test)

sum(diag(table(t$winner, t$winner_pred)))/sum(table(t$winner, t$winner_pred))
t1 <- table(t$winner, t$winner_pred)
t1


#############################################################################
#interpretation of results
#############################################################################
#immigration
E_LF_im = model$summary.random$id_im$`0.5quant`[2] -  model$summary.random$id_im$`0.5quant`[3]   #lepen - fillon
lower_LF_im = E_LF_im - 2*sqrt(model$summary.random$id_im$sd[2]^2 + model$summary.random$id_im$sd[3]^2)
upper_LF_im = E_LF_im + 2*sqrt(model$summary.random$id_im$sd[2]^2 + model$summary.random$id_im$sd[3]^2)

E_MF_im = model$summary.random$id_im$`0.5quant`[1] -  model$summary.random$id_im$`0.5quant`[3]   #macron - fillon
lower_MF_im = E_MF_im - 2*sqrt(model$summary.random$id_im$sd[1]^2 + model$summary.random$id_im$sd[3]^2)
upper_MF_im = E_MF_im + 2*sqrt(model$summary.random$id_im$sd[1]^2 + model$summary.random$id_im$sd[3]^2)

E_MeF_im = model$summary.random$id_im$`0.5quant`[4] - model$summary.random$id_im$`0.5quant`[3]   #melenchon - fillon
lower_MeF_im = E_MeF_im - 2*sqrt(model$summary.random$id_im$sd[4]^2 + model$summary.random$id_im$sd[3]^2)
upper_MeF_im = E_MeF_im + 2*sqrt(model$summary.random$id_im$sd[4]^2 + model$summary.random$id_im$sd[3]^2)

E_ML_im = model$summary.random$id_im$`0.5quant`[1] -  model$summary.random$id_im$`0.5quant`[2]   #macron - lepen
lower_ML_im = E_ML_im - 2*sqrt(model$summary.random$id_im$sd[1]^2 + model$summary.random$id_im$sd[2]^2)
upper_ML_im = E_ML_im + 2*sqrt(model$summary.random$id_im$sd[1]^2 + model$summary.random$id_im$sd[2]^2)

E_MeL_im = model$summary.random$id_im$`0.5quant`[4] - model$summary.random$id_im$`0.5quant`[2]   #melenchon - lepen
lower_MeL_im = E_MeL_im - 2*sqrt(model$summary.random$id_im$sd[4]^2 + model$summary.random$id_im$sd[2]^2)
upper_MeL_im = E_MeL_im + 2*sqrt(model$summary.random$id_im$sd[4]^2 + model$summary.random$id_im$sd[2]^2)

E_MeM_im = model$summary.random$id_im$`0.5quant`[4] - model$summary.random$id_im$`0.5quant`[1]   #melenchon - macron
lower_MeM_im = E_MeM_im - 2*sqrt(model$summary.random$id_im$sd[4]^2 + model$summary.random$id_im$sd[1]^2)
upper_MeM_im = E_MeM_im + 2*sqrt(model$summary.random$id_im$sd[4]^2 + model$summary.random$id_im$sd[1]^2)

##############################################################
#life expectancy
E_LF_ex = model$summary.random$id_ex$`0.5quant`[8] -  model$summary.random$id_ex$`0.5quant`[5]   #lepen - fillon
lower_LF_ex = E_LF_ex - 2*sqrt(model$summary.random$id_ex$sd[8]^2 + model$summary.random$id_ex$sd[5]^2)
upper_LF_ex = E_LF_ex + 2*sqrt(model$summary.random$id_ex$sd[8]^2 + model$summary.random$id_ex$sd[5]^2)

E_MF_ex = model$summary.random$id_ex$`0.5quant`[9] -  model$summary.random$id_ex$`0.5quant`[5]   #macron - fillon
lower_MF_ex = E_MF_ex - 2*sqrt(model$summary.random$id_ex$sd[9]^2 + model$summary.random$id_ex$sd[5]^2)
upper_MF_ex = E_MF_ex + 2*sqrt(model$summary.random$id_ex$sd[9]^2 + model$summary.random$id_ex$sd[5]^2)

E_MeF_ex = model$summary.random$id_ex$`0.5quant`[10] - model$summary.random$id_ex$`0.5quant`[5]   #melenchon - fillon
lower_MeF_ex = E_MeF_ex - 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[5]^2)
upper_MeF_ex = E_MeF_ex + 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[5]^2)

E_ML_ex = model$summary.random$id_ex$`0.5quant`[9] -  model$summary.random$id_ex$`0.5quant`[8]   #macron - lepen
lower_ML_ex = E_ML_ex - 2*sqrt(model$summary.random$id_ex$sd[9]^2 + model$summary.random$id_ex$sd[8]^2)
upper_ML_ex = E_ML_ex + 2*sqrt(model$summary.random$id_ex$sd[9]^2 + model$summary.random$id_ex$sd[8]^2)

E_MeL_ex = model$summary.random$id_ex$`0.5quant`[10] - model$summary.random$id_ex$`0.5quant`[8]   #melenchon - lepen
lower_MeL_ex = E_MeL_ex - 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[8]^2)
upper_MeL_ex = E_MeL_ex + 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[8]^2)

E_MeM_ex = model$summary.random$id_ex$`0.5quant`[10] - model$summary.random$id_ex$`0.5quant`[9]   #melenchon - macron
lower_MeM_ex = E_MeM_ex - 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[9]^2)
upper_MeM_ex = E_MeM_ex + 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[9]^2)
##############################################################
#poverty rate
E_LF_po = model$summary.random$id_po$`0.5quant`[8] -  model$summary.random$id_po$`0.5quant`[5]   #lepen - fillon
lower_LF_po = E_LF_po - 2*sqrt(model$summary.random$id_po$sd[8]^2 + model$summary.random$id_po$sd[5]^2)
upper_LF_po = E_LF_po + 2*sqrt(model$summary.random$id_po$sd[8]^2 + model$summary.random$id_po$sd[5]^2)

E_MF_po = model$summary.random$id_po$`0.5quant`[9] -  model$summary.random$id_po$`0.5quant`[5]   #macron - fillon
lower_MF_po = E_MF_po - 2*sqrt(model$summary.random$id_po$sd[9]^2 + model$summary.random$id_po$sd[5]^2)
upper_MF_po = E_MF_po + 2*sqrt(model$summary.random$id_po$sd[9]^2 + model$summary.random$id_po$sd[5]^2)

E_MeF_po = model$summary.random$id_po$`0.5quant`[10] - model$summary.random$id_po$`0.5quant`[5]   #melenchon - fillon
lower_MeF_po = E_MeF_po - 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[5]^2)
upper_MeF_po = E_MeF_po + 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[5]^2)

E_ML_po = model$summary.random$id_po$`0.5quant`[9] -  model$summary.random$id_po$`0.5quant`[8]   #macron - lepen
lower_ML_po = E_ML_po - 2*sqrt(model$summary.random$id_po$sd[9]^2 + model$summary.random$id_po$sd[8]^2)
upper_ML_po = E_ML_po + 2*sqrt(model$summary.random$id_po$sd[9]^2 + model$summary.random$id_po$sd[8]^2)

E_MeL_po = model$summary.random$id_po$`0.5quant`[10] - model$summary.random$id_po$`0.5quant`[8]   #melenchon - lepen
lower_MeL_po = E_MeL_po - 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[8]^2)
upper_MeL_po = E_MeL_po + 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[8]^2)

E_MeM_po = model$summary.random$id_po$`0.5quant`[10] - model$summary.random$id_po$`0.5quant`[9]   #melenchon - macron
lower_MeM_po = E_MeM_po - 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[9]^2)
upper_MeM_po = E_MeM_po + 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[9]^2)

##############################################################
#education
E_LF_ed = model$summary.random$id_ed$`0.5quant`[2] -  model$summary.random$id_ed$`0.5quant`[3]   #lepen - fillon
lower_LF_ed = E_LF_ed - 2*sqrt(model$summary.random$id_ed$sd[2]^2 + model$summary.random$id_ed$sd[3]^2)
upper_LF_ed = E_LF_ed + 2*sqrt(model$summary.random$id_ed$sd[2]^2 + model$summary.random$id_ed$sd[3]^2)

E_MF_ed = model$summary.random$id_ed$`0.5quant`[1] -  model$summary.random$id_ed$`0.5quant`[3]   #macron - fillon
lower_MF_ed = E_MF_ed - 2*sqrt(model$summary.random$id_ed$sd[1]^2 + model$summary.random$id_ed$sd[3]^2)
upper_MF_ed = E_MF_ed + 2*sqrt(model$summary.random$id_ed$sd[1]^2 + model$summary.random$id_ed$sd[3]^2)

E_MeF_ed = model$summary.random$id_ed$`0.5quant`[4] - model$summary.random$id_ed$`0.5quant`[3]   #melenchon - fillon
lower_MeF_ed = E_MeF_ed - 2*sqrt(model$summary.random$id_ed$sd[4]^2 + model$summary.random$id_ed$sd[3]^2)
upper_MeF_ed = E_MeF_ed + 2*sqrt(model$summary.random$id_ed$sd[4]^2 + model$summary.random$id_ed$sd[3]^2)

E_ML_ed = model$summary.random$id_ed$`0.5quant`[1] -  model$summary.random$id_ed$`0.5quant`[2]   #macron - lepen
lower_ML_ed = E_ML_ed - 2*sqrt(model$summary.random$id_ed$sd[1]^2 + model$summary.random$id_ed$sd[2]^2)
upper_ML_ed = E_ML_ed + 2*sqrt(model$summary.random$id_ed$sd[1]^2 + model$summary.random$id_ed$sd[2]^2)

E_MeL_ed = model$summary.random$id_ed$`0.5quant`[4] - model$summary.random$id_ed$`0.5quant`[2]   #melenchon - lepen
lower_MeL_ed = E_MeL_ed - 2*sqrt(model$summary.random$id_ed$sd[4]^2 + model$summary.random$id_ed$sd[2]^2)
upper_MeL_ed = E_MeL_ed + 2*sqrt(model$summary.random$id_ed$sd[4]^2 + model$summary.random$id_ed$sd[2]^2)

E_MeM_ed = model$summary.random$id_ed$`0.5quant`[4] - model$summary.random$id_ed$`0.5quant`[1]   #melenchon - macron
lower_MeM_ed = E_MeM_ed - 2*sqrt(model$summary.random$id_ed$sd[4]^2 + model$summary.random$id_ed$sd[1]^2)
upper_MeM_ed = E_MeM_ed + 2*sqrt(model$summary.random$id_ed$sd[4]^2 + model$summary.random$id_ed$sd[1]^2)
##############################################################
#average salary
E_LF_wa = model$summary.random$id_wa$`0.5quant`[8] -  model$summary.random$id_wa$`0.5quant`[5]   #lepen - fillon
lower_LF_wa = E_LF_wa - 2*sqrt(model$summary.random$id_wa$sd[8]^2 + model$summary.random$id_wa$sd[5]^2)
upper_LF_wa = E_LF_wa + 2*sqrt(model$summary.random$id_wa$sd[8]^2 + model$summary.random$id_wa$sd[5]^2)

E_MF_wa = model$summary.random$id_wa$`0.5quant`[9] -  model$summary.random$id_wa$`0.5quant`[5]   #macron - fillon
lower_MF_wa = E_MF_wa - 2*sqrt(model$summary.random$id_wa$sd[9]^2 + model$summary.random$id_wa$sd[5]^2)
upper_MF_wa = E_MF_wa + 2*sqrt(model$summary.random$id_wa$sd[9]^2 + model$summary.random$id_wa$sd[5]^2)

E_MeF_wa = model$summary.random$id_wa$`0.5quant`[10] - model$summary.random$id_wa$`0.5quant`[5]   #melenchon - fillon
lower_MeF_wa = E_MeF_wa - 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[5]^2)
upper_MeF_wa = E_MeF_wa + 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[5]^2)

E_ML_wa = model$summary.random$id_wa$`0.5quant`[9] -  model$summary.random$id_wa$`0.5quant`[8]   #macron - lepen
lower_ML_wa = E_ML_wa - 2*sqrt(model$summary.random$id_wa$sd[9]^2 + model$summary.random$id_wa$sd[8]^2)
upper_ML_wa = E_ML_wa + 2*sqrt(model$summary.random$id_wa$sd[9]^2 + model$summary.random$id_wa$sd[8]^2)

E_MeL_wa = model$summary.random$id_wa$`0.5quant`[10] - model$summary.random$id_wa$`0.5quant`[8]   #melenchon - lepen
lower_MeL_wa = E_MeL_wa - 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[8]^2)
upper_MeL_wa = E_MeL_wa + 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[8]^2)

E_MeM_wa = model$summary.random$id_wa$`0.5quant`[10] - model$summary.random$id_wa$`0.5quant`[9]   #melenchon - macron
lower_MeM_wa = E_MeM_wa - 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[9]^2)
upper_MeM_wa = E_MeM_wa + 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[9]^2)

##############################################################
#unemployment rate
E_LF_un = model$summary.random$id_un$`0.5quant`[2] -  model$summary.random$id_un$`0.5quant`[3]   #lepen - fillon
lower_LF_un = E_LF_un - 2*sqrt(model$summary.random$id_un$sd[2]^2 +  model$summary.random$id_un$sd[3]^2) # x,y are assumed to be gaussian and independent, std(X-Y) = sqrt(std(X)^2 + std(Y)^2)
upper_LF_un = E_LF_un + 2*sqrt(model$summary.random$id_un$sd[2]^2 +  model$summary.random$id_un$sd[3]^2) 

E_MF_un = model$summary.random$id_un$`0.5quant`[1] -  model$summary.random$id_un$`0.5quant`[3]   #macron - fillon
lower_MF_un = E_MF_un - 2*sqrt(model$summary.random$id_un$sd[1]^2 +  model$summary.random$id_un$sd[3]^2)
upper_MF_un = E_MF_un + 2*sqrt(model$summary.random$id_un$sd[1]^2 +  model$summary.random$id_un$sd[3]^2)

E_MeF_un = model$summary.random$id_un$`0.5quant`[4] - model$summary.random$id_un$`0.5quant`[3]   #melenchon - fillon
lower_MeF_un = E_MeF_un - 2*sqrt(model$summary.random$id_un$sd[4]^2 +  model$summary.random$id_un$sd[3]^2)
upper_MeF_un = E_MeF_un + 2*sqrt(model$summary.random$id_un$sd[4]^2 +  model$summary.random$id_un$sd[3]^2)

E_ML_un = model$summary.random$id_un$`0.5quant`[1] -  model$summary.random$id_un$`0.5quant`[2]   #macron - lepen
lower_ML_un = E_ML_un - 2*sqrt(model$summary.random$id_un$sd[1]^2 +  model$summary.random$id_un$sd[2]^2)
upper_ML_un = E_ML_un + 2*sqrt(model$summary.random$id_un$sd[1]^2 +  model$summary.random$id_un$sd[2]^2)


E_MeL_un = model$summary.random$id_un$`0.5quant`[4] - model$summary.random$id_un$`0.5quant`[2]   #melenchon - lepen
lower_MeL_un = E_MeL_un - 2*sqrt(model$summary.random$id_un$sd[4]^2 +  model$summary.random$id_un$sd[2]^2)
upper_MeL_un = E_MeL_un + 2*sqrt(model$summary.random$id_un$sd[4]^2 +  model$summary.random$id_un$sd[2]^2)


E_MeM_un = model$summary.random$id_un$`0.5quant`[4] - model$summary.random$id_un$`0.5quant`[1]   #melenchon - macron
lower_MeM_un = E_MeM_un - 2*sqrt(model$summary.random$id_un$sd[4]^2 +  model$summary.random$id_un$sd[1]^2)
upper_MeM_un = E_MeM_un + 2*sqrt(model$summary.random$id_un$sd[4]^2 +  model$summary.random$id_un$sd[1]^2)

######################################################################
#elderly rate
E_LF_el = model$summary.random$id_el$`0.5quant`[2] -  model$summary.random$id_el$`0.5quant`[3]   #lepen - fillon
lower_LF_el = E_LF_el - 2*sqrt(model$summary.random$id_el$sd[2]^2 + model$summary.random$id_el$sd[3]^2)
upper_LF_el = E_LF_el + 2*sqrt(model$summary.random$id_el$sd[2]^2 + model$summary.random$id_el$sd[3]^2)

E_MF_el = model$summary.random$id_el$`0.5quant`[1] -  model$summary.random$id_el$`0.5quant`[3]   #macron - fillon
lower_MF_el = E_MF_el - 2*sqrt(model$summary.random$id_el$sd[1]^2 + model$summary.random$id_el$sd[3]^2)
upper_MF_el = E_MF_el + 2*sqrt(model$summary.random$id_el$sd[1]^2 + model$summary.random$id_el$sd[3]^2)

E_MeF_el = model$summary.random$id_el$`0.5quant`[4] - model$summary.random$id_el$`0.5quant`[3]   #melenchon - fillon
lower_MeF_el = E_MeF_el - 2*sqrt(model$summary.random$id_el$sd[4]^2 + model$summary.random$id_el$sd[3]^2)
upper_MeF_el = E_MeF_el + 2*sqrt(model$summary.random$id_el$sd[4]^2 + model$summary.random$id_el$sd[3]^2)

E_ML_el = model$summary.random$id_el$`0.5quant`[1] -  model$summary.random$id_el$`0.5quant`[2]   #macron - lepen
lower_ML_el = E_ML_el - 2*sqrt(model$summary.random$id_el$sd[1]^2 + model$summary.random$id_el$sd[2]^2)
upper_ML_el = E_ML_el + 2*sqrt(model$summary.random$id_el$sd[1]^2 + model$summary.random$id_el$sd[2]^2)

E_MeL_el = model$summary.random$id_el$`0.5quant`[4] - model$summary.random$id_el$`0.5quant`[2]   #melenchon - lepen
lower_MeL_el = E_MeL_el - 2*sqrt(model$summary.random$id_el$sd[4]^2 + model$summary.random$id_el$sd[2]^2)
upper_MeL_el = E_MeL_el + 2*sqrt(model$summary.random$id_el$sd[4]^2 + model$summary.random$id_el$sd[2]^2)

E_MeM_el = model$summary.random$id_el$`0.5quant`[4] - model$summary.random$id_el$`0.5quant`[1]   #melenchon - macron
lower_MeM_el = E_MeM_el - 2*sqrt(model$summary.random$id_el$sd[4]^2 + model$summary.random$id_el$sd[1]^2)
upper_MeM_el = E_MeM_el + 2*sqrt(model$summary.random$id_el$sd[4]^2 + model$summary.random$id_el$sd[1]^2)
######################################################################
#white collar
E_LF_wh = model$summary.random$id_wh$`0.5quant`[8] -  model$summary.random$id_wh$`0.5quant`[5]   #lepen - fillon
lower_LF_wh = E_LF_wh - 2*sqrt(model$summary.random$id_wh$sd[8]^2 + model$summary.random$id_wh$sd[5]^2)
upper_LF_wh = E_LF_wh + 2*sqrt(model$summary.random$id_wh$sd[8]^2 + model$summary.random$id_wh$sd[5]^2)

E_MF_wh = model$summary.random$id_wh$`0.5quant`[9] -  model$summary.random$id_wh$`0.5quant`[5]   #macron - fillon
lower_MF_wh = E_MF_wh - 2*sqrt(model$summary.random$id_wh$sd[9]^2 + model$summary.random$id_wh$sd[5]^2)
upper_MF_wh = E_MF_wh + 2*sqrt(model$summary.random$id_wh$sd[9]^2 + model$summary.random$id_wh$sd[5]^2)

E_MeF_wh = model$summary.random$id_wh$`0.5quant`[10] - model$summary.random$id_wh$`0.5quant`[5]   #melenchon - fillon
lower_MeF_wh = E_MeF_wh - 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[5]^2)
upper_MeF_wh = E_MeF_wh + 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[5]^2)

E_ML_wh = model$summary.random$id_wh$`0.5quant`[9] -  model$summary.random$id_wh$`0.5quant`[8]   #macron - lepen
lower_ML_wh = E_ML_wh - 2*sqrt(model$summary.random$id_wh$sd[9]^2 + model$summary.random$id_wh$sd[8]^2)
upper_ML_wh = E_ML_wh + 2*sqrt(model$summary.random$id_wh$sd[9]^2 + model$summary.random$id_wh$sd[8]^2)

E_MeL_wh = model$summary.random$id_wh$`0.5quant`[10] - model$summary.random$id_wh$`0.5quant`[8]   #melenchon - lepen
lower_MeL_wh = E_MeL_wh - 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[8]^2)
upper_MeL_wh = E_MeL_wh + 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[8]^2)

E_MeM_wh = model$summary.random$id_wh$`0.5quant`[10] - model$summary.random$id_wh$`0.5quant`[9]   #melenchon - macron
lower_MeM_wh = E_MeM_wh - 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[9]^2)
upper_MeM_wh = E_MeM_wh + 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[9]^2)

######################################################################
#agriculture rate
E_LF_ag = model$summary.random$id_ag$`0.5quant`[2] -  model$summary.random$id_ag$`0.5quant`[3]   #lepen - fillon
lower_LF_ag = E_LF_ag - 2*sqrt(model$summary.random$id_ag$sd[2]^2 + model$summary.random$id_ag$sd[3]^2)
upper_LF_ag = E_LF_ag + 2*sqrt(model$summary.random$id_ag$sd[2]^2 + model$summary.random$id_ag$sd[3]^2)

E_MF_ag =model$summary.random$id_ag$`0.5quant`[1] -  model$summary.random$id_ag$`0.5quant`[3]   #macron - fillon
lower_MF_ag = E_MF_ag - 2*sqrt(model$summary.random$id_ag$sd[1]^2 + model$summary.random$id_ag$sd[3]^2)
upper_MF_ag = E_MF_ag + 2*sqrt(model$summary.random$id_ag$sd[1]^2 + model$summary.random$id_ag$sd[3]^2)

E_MeF_ag =model$summary.random$id_ag$`0.5quant`[4] - model$summary.random$id_ag$`0.5quant`[3]   #melenchon - fillon
lower_MeF_ag = E_MeF_ag - 2*sqrt(model$summary.random$id_ag$sd[4]^2 + model$summary.random$id_ag$sd[3]^2)
upper_MeF_ag = E_MeF_ag + 2*sqrt(model$summary.random$id_ag$sd[4]^2 + model$summary.random$id_ag$sd[3]^2)

E_ML_ag =model$summary.random$id_ag$`0.5quant`[1] -  model$summary.random$id_ag$`0.5quant`[2]   #macron - lepen
lower_ML_ag = E_ML_ag - 2*sqrt(model$summary.random$id_ag$sd[1]^2 + model$summary.random$id_ag$sd[2]^2)
upper_ML_ag = E_ML_ag + 2*sqrt(model$summary.random$id_ag$sd[1]^2 + model$summary.random$id_ag$sd[2]^2)

E_MeL_ag =model$summary.random$id_ag$`0.5quant`[4] - model$summary.random$id_ag$`0.5quant`[2]   #melenchon - lepen
lower_MeL_ag = E_MeL_ag - 2*sqrt(model$summary.random$id_ag$sd[4]^2 + model$summary.random$id_ag$sd[2]^2)
upper_MeL_ag = E_MeL_ag + 2*sqrt(model$summary.random$id_ag$sd[4]^2 + model$summary.random$id_ag$sd[2]^2)

E_MeM_ag =model$summary.random$id_ag$`0.5quant`[4] - model$summary.random$id_ag$`0.5quant`[1]   #melenchon - macron
lower_MeM_ag = E_MeM_ag - 2*sqrt(model$summary.random$id_ag$sd[4]^2 + model$summary.random$id_ag$sd[1]^2)
upper_MeM_ag = E_MeM_ag + 2*sqrt(model$summary.random$id_ag$sd[4]^2 + model$summary.random$id_ag$sd[1]^2)

##########################################################################
#visualization
##########################################################################
## Mapping Spatial Effects for lepen
#sp1 <- model$summary.random$sp_effect$`0.5quant`
#df_merged$sp1 <- sp1
#
#
#scale_range <- c(-0.7, 0.7)
#
#pal <- colorNumeric("RdBu", domain = scale_range)
#
#
#leaflet() %>%
#  addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
#  addProviderTiles("Esri.WorldGrayCanvas") %>%
#  addPolygons(data = df_merged, fillColor = ~pal(df_merged$sp1), 
#              fillOpacity = 0.8,
#              group = "Votes",
#              weight = 0.2,
#              smoothFactor = 0.2,
#              highlight = highlightOptions(
#                weight = 5,
#                color = "#666",
#                fillOpacity = 0.2,
#                bringToFront = TRUE),
#              popup = ~paste0(sp1),
#              labelOptions = labelOptions(
#                style = list("font-weight" = "normal", padding = "3px 8px"),
#                textsize = "15px",
#                direction = "auto")) %>%
#  addLegend(pal = pal, values = df_merged$sp1, opacity = 0.7) 


# Mapping first candidate predictions
df_merged <- left_join(df_merged, df_conf_train)
df_merged <- df_merged %>% 
  mutate(pred_status = ifelse(winner_real == winner_pred, 1, 0))

scale_range <- c(-0.5, 1.5)

pal <- colorNumeric("RdBu", domain = scale_range)


leaflet() %>%
  addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = df_merged, fillColor = ~pal(df_merged$pred_status), 
              fillOpacity = 0.8,
              group = "Votes",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              popup = ~paste0(code),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))


# Mapping Spatial Effects for macron
sp2 <- model$summary.random$sp_effect2$`0.5quant`
df_merged$sp2 <- sp2


scale_range <- c(-1.79, 1.79)

pal <- colorNumeric("RdBu", domain = scale_range)


leaflet() %>%
  addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = df_merged, fillColor = ~pal(df_merged$sp2), 
              fillOpacity = 0.8,
              group = "Votes",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              popup = ~paste0(sp2),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = df_merged$sp2, opacity = 0.7) 




# Mapping Spatial Effects for fillon
sp3 <- model$summary.random$sp_effect3$`0.5quant`
df_merged$sp3 <- sp3


scale_range <- c(-3, 3)

pal <- colorNumeric("RdBu", domain = scale_range)


leaflet() %>%
  addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = df_merged, fillColor = ~pal(df_merged$sp3), 
              fillOpacity = 0.8,
              group = "Votes",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              popup = ~paste0(sp2),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = df_merged$sp3, opacity = 0.7) 






# Mapping first candidate predictions
df_merged <- left_join(df_merged, df_conf_train)
df_merged <- df_merged %>% 
  mutate(pred_status = ifelse(winner_real == winner_pred, 1, 0))

scale_range <- c(-0.5, 1.5)

pal <- colorNumeric("RdBu", domain = scale_range)


leaflet() %>%
  addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = df_merged, fillColor = ~pal(df_merged$pred_status), 
              fillOpacity = 0.8,
              group = "Votes",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              popup = ~paste0(code),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

df_missed <- df_merged %>% filter(pred_status == 0)
df_missed2 <- df_accuracy %>% filter(code == 13 | code == 28 | code == 31 | code == 64 | code == 93)

