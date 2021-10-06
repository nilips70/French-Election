library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(spdplyr)
library(tigris)
rm(list=ls())


#preparing election result dataset
Presidentielle_2017_Resultats_Communes_Tour_1_c <- read_excel("Presidentielle_2017_Resultats_Communes_Tour_1_c.xls")
results <- Presidentielle_2017_Resultats_Communes_Tour_1_c %>%
  rename(DEP_CODE = `Code du département`,
         DEP_NAME = `Libellé du département`,
         COM_CODE = `Code de la commune`,
         COM_NAME = `Libellé de la commune`,
         Registered = Inscrits,
         Abstained = Abstentions,
         `%Abstd/Regd` = `% Abs/Ins`,
         Voted = Votants,
         `%Voted/Regd`= `% Vot/Ins`,
         Blank = Blancs,
         `%Blank/Regd` = `% Blancs/Ins`,
         `%Blank/Voted` = `% Blancs/Vot`,
         Spoiled = Nuls,
         `%Spld/Regd` = `% Nuls/Ins`,
         `%Spld/Voted` = `% Nuls/Vot`,
         Valid = `Exprimés`,
         `%Valid/Regd` = `% Exp/Ins`,
         `%Valid/Voted` = `% Exp/Vot`,
         Cand1_Code = `N°Panneau...19`,
         Cand1_Sex = `Sexe...20`,
         Cand1_LastName = `Nom...21`,
         Cand1_Firstname = `Prénom...22`,
         Cand1_Votes = `Voix...23`,
         `Cand1_%Votes/Regd` = `% Voix/Ins...24`,
         `Cand1_%Votes/Valid` = `% Voix/Exp...25`,
         Cand2_Code = `N°Panneau...26`,
         Cand2_Sex = `Sexe...27`,
         Cand2_LastName = `Nom...28`,
         Cand2_Firstname = `Prénom...29`,
         Cand2_Votes = `Voix...30`,
         `Cand2_%Votes/Regd` = `% Voix/Ins...31`,
         `Cand2_%Votes/Valid` = `% Voix/Exp...32`,
         Cand3_Code = `N°Panneau...33`,
         Cand3_Sex = `Sexe...34`,
         Cand3_LastName = `Nom...35`,
         Cand3_Firstname = `Prénom...36`,
         Cand3_Votes = `Voix...37`,
         `Cand3_%Votes/Regd` = `% Voix/Ins...38`,
         `Cand3_%Votes/Valid` = `% Voix/Exp...39`,
         Cand4_Code = `N°Panneau...40`,
         Cand4_Sex = `Sexe...41`,
         Cand4_LastName = `Nom...42`,
         Cand4_Firstname = `Prénom...43`,
         Cand4_Votes = `Voix...44`,
         `Cand4_%Votes/Regd` = `% Voix/Ins...45`,
         `Cand4_%Votes/Valid` = `% Voix/Exp...46`,
         Cand5_Code = `N°Panneau...47`,
         Cand5_Sex = `Sexe...48`,
         Cand5_LastName = `Nom...49`,
         Cand5_Firstname = `Prénom...50`,
         Cand5_Votes = `Voix...51`,
         `Cand5_%Votes/Regd` = `% Voix/Ins...52`,
         `Cand5_%Votes/Valid` = `% Voix/Exp...53`,
         Cand6_Code = `N°Panneau...54`,
         Cand6_Sex = `Sexe...55`,
         Cand6_LastName = `Nom...56`,
         Cand6_Firstname = `Prénom...57`,
         Cand6_Votes = `Voix...58`,
         `Cand6_%Votes/Regd` = `% Voix/Ins...59`,
         `Cand6_%Votes/Valid` = `% Voix/Exp...60`,
         Cand7_Code = `N°Panneau...61`,
         Cand7_Sex = `Sexe...62`,
         Cand7_LastName = `Nom...63`,
         Cand7_Firstname = `Prénom...64`,
         Cand7_Votes = `Voix...65`,
         `Cand7_%Votes/Regd` = `% Voix/Ins...66`,
         `Cand7_%Votes/Valid` = `% Voix/Exp...67`,
         Cand8_Code = `N°Panneau...68`,
         Cand8_Sex = `Sexe...69`,
         Cand8_LastName = `Nom...70`,
         Cand8_Firstname = `Prénom...71`,
         Cand8_Votes = `Voix...72`,
         `Cand8_%Votes/Regd` = `% Voix/Ins...73`,
         `Cand8_%Votes/Valid` = `% Voix/Exp...74`,
         Cand9_Code = `N°Panneau...75`,
         Cand9_Sex = `Sexe...76`,
         Cand9_LastName = `Nom...77`,
         Cand9_Firstname = `Prénom...78`,
         Cand9_Votes = `Voix...79`,
         `Cand9_%Votes/Regd` = `% Voix/Ins...80`,
         `Cand9_%Votes/Valid` = `% Voix/Exp...81`,
         Cand10_Code = `N°Panneau...82`,
         Cand10_Sex = `Sexe...83`,
         Cand10_LastName = `Nom...84`,
         Cand10_Firstname = `Prénom...85`,
         Cand10_Votes = `Voix...86`,
         `Cand10_%Votes/Regd` = `% Voix/Ins...87`,
         `Cand10_%Votes/Valid` = `% Voix/Exp...88`,
         Cand11_Code = `N°Panneau...89`,
         Cand11_Sex = `Sexe...90`,
         Cand11_LastName = `Nom...91`,
         Cand11_Firstname = `Prénom...92`,
         Cand11_Votes = `Voix...93`,
         `Cand11_%Votes/Regd` = `% Voix/Ins...94`,
         `Cand11_%Votes/Valid` = `% Voix/Exp...95`)

# results <- saveRDS(results, file = "results.rds")
results <- readRDS("results.rds")


#preparing coordination dataset
name_geographic_information <- read_csv("name_geographic_information.csv")
coords <- name_geographic_information %>% 
  dplyr::select(numéro_département, nom_département, latitude, longitude) %>% 
  rename(code = numéro_département, DEP_NAME = nom_département)
#coords <- saveRDS(coords, file= "coords.rds")
coords <- readRDS("coords.rds")


#preparing salary per department dataset
DEPS_base_cc_dads_2014 <- read_excel("DEPS-base-cc-dads-2014.xlsx")
df_dep_sal <- DEPS_base_cc_dads_2014 %>% 
  dplyr::select(CODGEO, LIBGEO, SNHM14)

#df_dep_sal <- saveRDS(df_dep_sal, file= "df_dep_sal.rds")
df_dep_sal <- readRDS("df_dep_sal.rds")


#preparing population per department dataset
correspondance_code_insee_code_postal <- read_csv("correspondance-code-insee-code-postal.csv")
df_dep_pop <- correspondance_code_insee_code_postal %>% 
  dplyr::select(Département, Population) %>% 
  rename(nom = Département, pop = Population) %>% 
  mutate(nom = str_to_title(nom)) %>% 
  group_by(nom) %>% summarise(pop = sum(pop))

#df_dep_pop <- saveRDS(df_dep_pop, file= "df_dep_pop.rds")
df_dep_pop <- readRDS("df_dep_pop.rds")


#creating a dataset which includes both commune and department names and the geometery
# I used a dataset from stanford:https://geodata.lib.berkeley.edu/catalog/stanford-fq136pf8502
# I used this one because in our dataset some of the commune names are in 2 or 3 departments at the same time
# so the map looks like it is torned apart. but this dataset is more consistent
BTX_TD_MEN3_2014 <- read_excel("BTX_TD_MEN3_2014.xls") #Employement data

df_unemps <- BTX_TD_MEN3_2014 %>% #Note: Paris is not in the dataset
  rowwise() %>% 
  mutate(total_unemployed = round(sum(across(starts_with("TACTR12")), na.rm = T), 0),
         total_employed = round(sum(across(starts_with("TACTR11")))),
         labor_force = total_employed + total_unemployed,
         unemployment_rate = (total_unemployed / labor_force)) %>%
  dplyr::select(LIBGEO, unemployment_rate )


# Extracting paired columns of dep names and com names
depcom <- rgdal::readOGR("stanford-fq136pf8502-geojson.json")
#df_depcom <- st_as_sf(depcom)
df_depcom <- as.data.frame(depcom)

df_depcom <- df_depcom %>% 
  dplyr::select(name_2, name_5) %>% 
  rename(nom = name_2, LIBGEO = name_5) 

#saveRDS(df_depcom, "df_depcom.rds")

#Adding dep names to unemplyement rate dataset
df_unemps <- list( 
  df_unemps,
  df_depcom
) %>% 
  reduce(left_join) # 538 NA's introduced possibly because of communes name change from 2014 to 2015

departments_unempolyment_rate <- df_unemps %>% 
                                 group_by(nom) %>% 
                                 summarise(unemployment_rate = sum(unemployment_rate, na.rm = T))
  

#saveRDS(departments_unempolyment_rate, "departments_unempolyment_rate.rds")

# calculating the labor force in each department

# de_employ <- df_comdep_pop %>% 
#   rowwise() %>% 
#   mutate(total_unemployed = round(sum(across(starts_with("TACTR12")), na.rm = T), 0),
#          female_unemployed = round(
#            sum(
#              across(
#                intersect(
#                  starts_with("TACTR12"), ends_with("SEXE2")
#                ), 
#                na.rm = T), 
#              0)
#          ),
#          male_unemployed = round(total_unemployed - female_unemployed),
#          tactr11 = round(sum(across(starts_with("TACTR11")))),
#          tactr21 = round(sum(across(starts_with("TACTR21")))),
#          tactr22 = round(sum(across(starts_with("TACTR22")))),
#          tactr24 = round(sum(across(starts_with("TACTR24")))),
#          tactr26 = round(sum(across(starts_with("TACTR26")))),
#          otherthanunemployed= sum(tactr11, tactr21, tactr22, tactr24, tactr26),
#          unemployment_rate = (total_unemployed / otherthanunemployed)) %>%
#   dplyr::select(CODGEO, LIBGEO, nom, total_unemployed, female_unemployed, male_unemployed, otherthanunemployed,unemployment_rate )
# 
# df_employ <- de_employ %>% mutate(nom = as.factor(nom)) %>% group_by(nom) %>% 
#   summarise(total_unemployed = sum(total_unemployed, na.rm = T),
#             female_unemployed = sum(female_unemployed, na.rm = T),
#             male_unemployed = sum(male_unemployed, na.rm = T),
#             otherthanunemployed = sum(otherthanunemployed, na.rm = T),
#             unemployment_rate = sum(unemployment_rate, na.rm = T)
#             )
#saveRDS(df_employ, "df_employ.rds")     


         