library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(spdplyr)
library(tigris)
library(lubridate)
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
coords <- saveRDS(coords, file= "coords.rds")
coords <- readRDS("coords.rds")


#preparing salary per department dataset
DEPS_base_cc_dads_2014 <- read_excel("DEPS-base-cc-dads-2014.xlsx")
df_dep_sal <- DEPS_base_cc_dads_2014 %>% 
  dplyr::select(CODGEO, LIBGEO, SNHM14)

#df_dep_sal <- saveRDS(df_dep_sal, file= "df_dep_sal.rds")
df_dep_sal <- readRDS("df_dep_sal.rds")


#preparing population per department dataset
correspondance_code_insee_code_postal <- read_csv("~/Desktop/Datasets/French Election/Data/correspondance-code-insee-code-postal.csv")df_dep_pop <- correspondance_code_insee_code_postal %>% 
  dplyr::select(Département, Population) %>% 
  rename(nom = Département, pop = Population) %>% 
  mutate(nom = str_to_title(nom)) %>% #for making capital letters to small letters
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
  

#preparing unemployment data from https://losd-data.staging.derilinx.com/dataset/ilo-unemployment-rate-for-ages-15-74-by-nuts3-regions-france-quarterly
unemployement <- read_csv("france-unemployement_rate-nuts3-quarterly.csv")

unemployement <-unemployement %>% 
  rename( code = Code, dep_name = nom) %>% 
  mutate(dep_name = str_to_title(dep_name))  #for making capital letters to small letters

#saveRDS(unemployement, file= "unemployement.rds")


#preparing age data


BTX_TD_MEN4_2014 <- read_excel("~/Desktop/Datasets/French Election/Data/BTX_TD_MEN4_2014.xls")

df_ages <- BTX_TD_MEN4_2014 %>% #Note: Paris is not in the dataset
  rowwise() %>% 
  mutate('20_24' = round(sum(across(starts_with("AGEQ20_80020")), na.rm = T), 0),
         '25_29' = round(sum(across(starts_with("AGEQ20_80025")))),
         '30_34' = round(sum(across(starts_with("AGEQ20_80030")))),
         '35_39' = round(sum(across(starts_with("AGEQ20_80035")))),
         '40_44' = round(sum(across(starts_with("AGEQ20_80040")))),
         '45_49' = round(sum(across(starts_with("AGEQ20_80045")))),
         '50_54' = round(sum(across(starts_with("AGEQ20_80050")))),
         '55_59' = round(sum(across(starts_with("AGEQ20_80055")))),
         '60_64' = round(sum(across(starts_with("AGEQ20_80060")))),
         '65_69' = round(sum(across(starts_with("AGEQ20_80065")))),
         '70_74' = round(sum(across(starts_with("AGEQ20_80070")))),
         '75_79' = round(sum(across(starts_with("AGEQ20_80075")))),
         '80' = round(sum(across(starts_with("AGEQ20_80080"))))) %>%
  dplyr::select(LIBGEO, '20_24',
                '25_29',
                '30_34',
                '35_39',
                '40_44',
                '45_49',
                '50_54',
                '55_59',
                '60_64',
                '65_69',
                '70_74',
                '75_79',
                '80')


df_ages <- list( 
  df_depcom,
  df_ages
  
  ) %>% 
  reduce(left_join) 


departments_age <- df_ages %>% 
  group_by(nom) %>% 
  summarise(`20_24` = sum(`20_24`, na.rm = T),
            `25_29` = sum(`25_29`, na.rm = T),
            `30_34` = sum(`30_34`, na.rm = T),
            `35_39` = sum(`35_39`, na.rm = T),
            `40_44` = sum(`40_44`, na.rm = T),
            `45_49` = sum(`45_49`, na.rm = T),
            `50_54` = sum(`50_54`, na.rm = T),
            `55_59` = sum(`55_59`, na.rm = T),
            `60_64` = sum(`60_64`, na.rm = T),
            `65_69` = sum(`65_69`, na.rm = T),
            `70_74` = sum(`70_74`, na.rm = T),
            `75_79` = sum(`75_79`, na.rm = T),
            `80` = sum(`80`, na.rm = T)
            )
departments_age <- departments_age %>% 
  pivot_longer(names_to = "age", values_to ="people" , -nom)


new_age =  data.frame(age = unique(departments_age$age), 
                      average_age = c(22,27,32,37,42,47,52,57,62,67,72,77,82))

departments_age <- left_join(departments_age, new_age) 

#saveRDS(departments_age, "departments_age.rds") 


#preparing departments shapes dataset 
deps <- rgdal::readOGR("departements.geojson")

#saveRDS(deps, "departments.rds")


#preparing immigration dataset for departments
BTX_TD_IMG1A_2017 <- read_excel("~/Desktop/Datasets/French Election/Data/BTX_TD_IMG1A_2017.xlsx")
df_imm_dep <- BTX_TD_IMG1A_2017 %>% rowwise() %>% 
  mutate(female_immigrants = round(sum(across(ends_with("IMMI1_SEXE2")), na.rm = T), 0),
         male_immigrants = round(sum(across(starts_with("IMMI1_SEXE1")))),
         total_imm = female_immigrants + male_immigrants,
         code = str_sub(CODGEO, 1, 2)
  ) %>%
  dplyr::select(code, total_imm )

df_imm_dep <- df_imm_dep %>% 
  group_by(code) %>% summarise(total_imm = sum(total_imm)) %>% 
  mutate(total_immigrants_france = sum(total_imm))

departments <- readRDS("~/Desktop/Datasets/French Election/French_election_app/departments.rds")
df_imm_dep <- left_join(df_imm_dep, as.data.frame(departments))

correspondance_code_insee_code_postal <- read_csv("~/Desktop/Datasets/French Election/Data/correspondance-code-insee-code-postal.csv")
chiz <- correspondance_code_insee_code_postal %>% 
  dplyr::select(`Code INSEE`, `Département`, Population) %>% 
  rename(code = `Code INSEE`) %>% 
  mutate(code = str_sub(code, 1,2)) #only selecting the first two numbers of the code variable 

df_imm_dep <- left_join(df_imm_dep, chiz)
df_imm_dep <- df_imm_dep %>% group_by(code) %>% summarise(Population = sum(Population),
                                                          total_imm = mean(total_imm)) %>% 
  mutate(percent_imm_dep = round(total_imm*100 /Population))  

#saveRDS(df_imm_dep, "df_imm_dep.rds")

#preparing temperature dataset for departments
temperature_quotidienne_departementale <- read_delim("~/Desktop/Datasets/French Election/Data/temperature-quotidienne-departementale.csv", 
                                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

temp <- temperature_quotidienne_departementale %>% filter(year(date_obs) == 2018 ) %>% #choosing only 2018 data
  group_by(departement) %>% summarise(tmoy = mean(tmoy),
                                      code_insee_departement = min(code_insee_departement)) %>% 
  rename(code = code_insee_departement)

temp <- temp %>% mutate(temp_interval = ifelse(tmoy > 15 , "> 15", 
                                            ifelse(tmoy > 13.5 & tmoy <= 15, "13.5 - 15", 
                                                ifelse(tmoy > 13 & tmoy <= 13.5, "13 - 13.5",
                                                       ifelse(tmoy > 12.5 & tmoy <= 13, "12.5 - 13",
                                                              ifelse(tmoy > 12.25 & tmoy <= 12.5, "12.25 - 12.5",
                                                                     ifelse(tmoy > 12.125 & tmoy <=12.25, "12.125 - 12.25",
                                                                            ifelse(tmoy > 12 & tmoy <= 12.125, "12 - 12.125",
                                                                                   ifelse(tmoy > 11.75 & tmoy<= 12, "11.75 - 12",
                                                                                          ifelse(tmoy > 11.5 & tmoy <=11.75, "11.5 - 11.75", 
                                                                                                 ifelse(tmoy >11 & tmoy <= 11.5, "11 - 11.5", "< 11"
                                                                                                       ))))))))))
                         
)

#saveRDS(temp, "temp.rds")

#preparing poverty dataset for departments
povert_rte_departments_2017 <- read_excel("~/Desktop/Datasets/French Election/Data/povert rte departments 2017.xlsx")
poverty <- povert_rte_departments_2017 %>% 
  dplyr::select( CODGEO, LIBGEO, TP6017) %>% 
  rename(code = CODGEO, poverty_rate= TP6017)

poverty <- poverty %>% mutate(pov_interval = ifelse(poverty_rate > 21 , ">21",
                                                 ifelse(poverty_rate <= 21 & poverty_rate > 15, "14.5 - 21", "14.5>")))
#saveRDS(poverty, "poverty.rds")
