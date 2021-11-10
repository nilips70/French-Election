library(gtrendsR)
library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(spdplyr)
library(tigris)
library(lubridate)
library(writexl)
library(leaflet)
library(lubridate)
library(corrplot)
library(corrr)
rm(list=ls())

#reading datasets
reg_regcode <- read_csv("reg_regcode.csv")
region <- rgdal::readOGR("stanford-region.json")
communes <- rgdal::readOGR("communes.geojson")
name_geographic_information <- read_csv("~/Desktop/Datasets/French Election/Data/name_geographic_information.csv")

#extracting data in France
#flood
inondation <- gtrends("inondation", geo = "FR", time= "2016-04-23 2017-04-23")
ino_reg <- inondation$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_inondation = hits, Region = location)

flood <- gtrends("flood", geo = "FR", time= "2016-04-23 2017-04-23")
flood_reg <- flood$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_flood = hits, Region = location)

ino_overtime <- inondation$interest_over_time %>% select(date, hits) %>% rename(hit_ino = hits)

#climate change 
climatique <- gtrends("changement climatique", geo = "FR", time= "2016-04-23 2017-04-23")
climatique_reg <- climatique$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_climatique = hits, Region = location)

climate <- gtrends("climate change", geo = "FR", time= "2016-04-23 2017-04-23")
climate_reg <- climate$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_climate = hits, Region = location)

climatique_overtime <- climatique$interest_over_time %>% select(date, hits) %>% rename(hit_clim = hits)

#paris agreement
accord <- gtrends("accord de paris", geo = "FR", time= "2016-04-23 2017-04-23")
accord_reg <- accord$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_accordparis = hits, Region = location)

agreement <- gtrends("paris agreement", geo = "FR", time= "2016-04-23 2017-04-23")
agreement_reg <- agreement$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_agreement = hits, Region = location)

accord_overtime <- accord$interest_over_time %>% select(date, hits) %>% rename(hit_accord = hits)

#oil refinery
petrol <- gtrends("raffinerie de pétrole", geo = "FR", time= "2016-04-23 2017-04-23")
petrol_reg <- petrol$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_petrol = hits, Region = location)

oil <- gtrends("oil refinery", geo = "FR", time= "2016-04-23 2017-04-23")
oil_reg <- oil$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_oil = hits, Region = location)

petrol_overtime <- petrol$interest_over_time %>% select(date, hits) %>% rename(hit_petrol = hits)

#air pollution
pollutionf <- gtrends("pollution de l'air", geo = "FR", time= "2016-04-23 2017-04-23")
pollutionf_reg <- pollutionf$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_pollution = hits, Region = location)

pollutione <- gtrends("air pollution", geo = "FR", time= "2016-04-23 2017-04-23")
pollutione_reg <- pollutione$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_pollutione = hits, Region = location)

pollutionf_overtime <- pollutionf$interest_over_time %>% select(date, hits) %>% rename(hit_poll = hits)


#renewable
ren <- gtrends("renouvelables", geo = "FR", time= "2016-04-23 2017-04-23")
ren_reg <- ren$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_renewable = hits, Region = location)

renew <- gtrends("renewable energy", geo = "FR", time= "2016-04-23 2017-04-23")
renew_reg <- renew$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_renewable_energy = hits, Region = location)

ren_overtime <- ren$interest_over_time %>% select(date, hits) %>% rename(hit_ren = hits)


df_clim <- list(ren_reg,renew_reg,
                pollutionf_reg,pollutione_reg,
                petrol_reg,oil_reg,
                agreement_reg,accord_reg,
                climate_reg,climatique_reg,
                flood_reg, ino_reg) %>% reduce(left_join)

df_clim2 <- df_clim %>% 
  rowwise() %>% 
mutate(hits_renew_energy = sum(c(hits_renewable,hits_renewable_energy), na.rm = T),
                              hits_polluti0n = sum(c(hits_pollution ,hits_pollutione), na.rm = T),
                              hits_petr0l = sum(c(hits_petrol , hits_oil), na.rm = T),
                              hits_parisagreement = sum(c(hits_agreement , hits_accordparis), na.rm = T),
                              hits_climate_change = sum(c(hits_climate , hits_climatique), na.rm = T),
                              hits_flo0d = sum(c(hits_flood , hits_inondation), na.rm = T)) %>% 
  select(Region, hits_renew_energy, hits_polluti0n, hits_parisagreement, hits_climate_change, hits_flo0d)


#Debout la France   - Dupont
Debout_la_France <- gtrends("Debout la France", geo = "FR", time= "2016-04-23 2017-04-23")
Debout_la_France_reg <- Debout_la_France$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_Debout_la_France = hits, Region = location)

Debout_la_France_overtime <- Debout_la_France$interest_over_time %>% select(date, hits) %>% rename(hits_Debout_la_France = hits)


#National Front      - lepen
National_Front <- gtrends("National Front", geo = "FR", time= "2016-04-23 2017-04-23")
National_Front_reg <- National_Front$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_National_Front = hits, Region = location)

National_Front_overtime <- National_Front$interest_over_time %>% select(date, hits) %>% rename(hits_National_Front = hits)


#En Marche         - macron
En_Marche <- gtrends("En Marche", geo = "FR", time= "2016-04-23 2017-04-23")
En_Marche_reg <- En_Marche$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_En_Marche = hits, Region = location)

En_Marche_overtime <- En_Marche$interest_over_time %>% select(date, hits) %>% rename(hits_En_Marche = hits)


#Socialist Party      -hamon
Socialist_Party <- gtrends("Socialist Party", geo = "FR", time= "2016-04-23 2017-04-23")
Socialist_Party_reg <- Socialist_Party$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_Socialist_Party = hits, Region = location)

Socialist_Party_overtime <- Socialist_Party$interest_over_time %>% select(date, hits) %>% rename(hits_Socialist_Party = hits)


#Lutte Ouvrière         -arthaud
Lutte_Ouvriere <- gtrends("Lutte Ouvrière", geo = "FR", time= "2016-04-23 2017-04-23")
Lutte_Ouvriere_reg <- Lutte_Ouvriere$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_Lutte_Ouvriere = hits, Region = location)

Lutte_Ouvriere_overtime <- Lutte_Ouvriere$interest_over_time %>% select(date, hits) %>% rename(hits_Lutte_Ouvriere = hits)


#New Anticapitalist Party     - poutou
Anticapitalist <- gtrends("Nouveau Parti anticapitaliste", geo = "FR", time= "2016-04-23 2017-04-23")
Anticapitalist_reg <- Anticapitalist$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_Anticapitalist = hits, Region = location)

Anticapitalist_overtime <- Anticapitalist$interest_over_time %>% select(date, hits) %>% rename(hits_Anticapitalist = hits)


#Solidarité et progrès      -cheminade
Solidarit <- gtrends("Solidarité et progrès", geo = "FR", time= "2016-04-23 2017-04-23")
Solidarit_reg <- Solidarit$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_Solidarit = hits, Region = location)

Solidarit_overtime <- Solidarit$interest_over_time %>% select(date, hits) %>% rename(hits_Solidarit = hits)



#Résistons         -lassalle
Resistons <- gtrends("Résistons", geo = "FR", time= "2016-04-23 2017-04-23")
Resistons_reg <- Resistons$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_Resistons = hits, Region = location)

Resistons_overtime <- Resistons$interest_over_time %>% select(date, hits) %>% rename(hits_Resistons = hits)

#La France Insoumise         - melenchon
Insoumise <- gtrends("La France Insoumise", geo = "FR", time= "2016-04-23 2017-04-23")
Insoumise_reg <- Insoumise$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_Insoumise = hits, Region = location)

Insoumise_overtime <- Insoumise$interest_over_time %>% select(date, hits) %>% rename(hits_Insoumise = hits)


#Popular Republican Union            - asselin
Republican <- gtrends("Union Populaire Républicaine", geo = "FR", time= "2016-04-23 2017-04-23")
Republican_reg <- Republican$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_Republican = hits, Region = location)

Republican_overtime <- Republican$interest_over_time %>% select(date, hits) %>% rename(hits_Republican = hits)


#The Republicans         -fillon
the_Republican <- gtrends("Union Populaire Républicaine", geo = "FR", time= "2016-04-23 2017-04-23")
the_Republican_reg <- the_Republican$interest_by_region %>% 
  select(-c(geo, gprop, keyword)) %>% rename(hits_the_Republican = hits, Region = location)

the_Republican_overtime <- the_Republican$interest_over_time %>% select(date, hits) %>% rename(hits_the_Republican = hits)




#visualization (time plot)
df_overtime1 <- list(climatique_overtime,
                    accord_overtime,
                   ren_overtime
                   ) %>% 
  reduce(left_join)

df_overtime2 <- list(climatique_overtime,
                     accord_overtime,
                     petrol_overtime,
                     pollutionf_overtime,
                     ren_overtime) %>% 
  reduce(left_join)

df_overtime_long<- df_overtime1 %>% #mutate(hit_ino = as.integer(hit_ino)) %>% 
  pivot_longer(names_to = "hits", values_to = "cnt", -date)
                                                
ggplot(data=df_overtime_long, aes(x=yday(date), y=cnt, color=hits)) + 
  geom_line()

df_overtime <- df_overtime %>% mutate(hit_ino = as.integer(hit_ino))

df_overtime[51, 2] <- 0    #0 instead of NA

corrplot(cor(df_overtime2[,2:6]), order = 'AOE', type = 'lower')



#data preparation for regions shapefiles
reg_regcode <- reg_regcode %>% mutate(regcode = as.character(regcode))

df_region <- st_as_sf(region)

df_region <- df_region %>% 
  dplyr::select(name_1, cca_1, geometry) %>% 
  rename(regname = name_1, regcode = cca_1)

#merging datasets together
df_merged <- list(reg_regcode,
                 df_clim2,
                 df_region,
                 the_Republican_reg,
                 Republican_reg,
                 Insoumise_reg,
                 Resistons_reg,
                 Solidarit_reg,
                 Anticapitalist_reg,
                 Lutte_Ouvriere_reg,
                 Socialist_Party_reg,
                 En_Marche_reg,
                 National_Front_reg,
                 Debout_la_France_reg
                 ) %>% 
  reduce(left_join) %>% st_as_sf()

df_merged_withoutgeo <- df_merged %>% st_drop_geometry() %>% select(-c(regname, Region, regcode,hits_Socialist_Party, hits_the_Republican,hits_Republican,hits_Resistons:hits_Anticapitalist))


c <- cor(df_merged_withoutgeo, use = "pairwise.complete.obs")
corrplot(c, order = 'hclust', addrect = 2)


saveRDS(df_merged, "df_hits.RDS")
#visualization (map)
pal1 <- colorBin("Purples", df_merged$hits_flo0d, bins= c(0,40,50,100, 150,200))
pal2 <- colorBin("Purples", df_merged$hits_climate_change, bins= c(0,40,50,100, 150,200))
pal3 <- colorBin("Purples", df_merged$hits_parisagreement, bins= c(0,40,50,100, 150,200))
#pal4 <- colorBin("Purples", df_merged$hits_petrol, 8, pretty = FALSE)
#pal5 <- colorBin("Purples", df_merged$hits_pollution, 8, pretty = FALSE)
pal6 <- colorBin("Purples", df_merged$hits_renew_energy, bins= c(0,100, 120, 140,150,180,200))
pal7 <- colorBin("Purples", df_merged$hits_Insoumise, bins= c(0, 40,60, 80,100))
pal8 <- colorBin("Purples", df_merged$hits_Lutte_Ouvriere, bins= c(0, 40,60,100))
pal9 <- colorBin("Purples", df_merged$hits_En_Marche, bins= c(0, 60,70, 85,100))
pal10 <- colorBin("Purples", df_merged$hits_Debout_la_France, bins= c(0, 40,60,100))
pal11 <- colorBin("Purples", df_merged$hits_National_Front, bins= c(0, 20,50, 80,100))


df_popup <- df_merged %>% 
  mutate(pop1 = paste0(df_merged$hits_flo0d," hits at ", df_merged$regname),
         pop2 = paste0(df_merged$hits_climate_change," hits at ", df_merged$regname),
         pop3 = paste0(df_merged$hits_parisagreement," hits at ", df_merged$regname),
         #pop4 = paste0(df_merged$hits_petrol," hits at ", df_merged$regname),
         #pop5 = paste0(df_merged$hits_pollution," hits at ", df_merged$regname),
         pop6 = paste0(df_merged$hits_renew_energy," hits at ", df_merged$regname),
         pop7 = paste0(df_merged$hits_Insoumise," hits at ", df_merged$regname),
         pop8 = paste0(df_merged$hits_Lutte_Ouvriere," hits at ", df_merged$regname),
         pop9 = paste0(df_merged$hits_En_Marche," hits at ", df_merged$regname),
         pop10 = paste0(df_merged$hits_Debout_la_France," hits at ", df_merged$regname),
         pop11 = paste0(df_merged$hits_National_Front," hits at ", df_merged$regname))
  
leaflet() %>%
  addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
  addPolygons(data = df_merged, fillColor = ~pal1(df_merged$hits_flo0d), #layerId= ~nom,
              fillOpacity = 0.7,
              group = "Flood",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=df_popup$pop1,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal1, group = "Flood" , values = df_merged$hits_flo0d, title = "Flood hits", opacity = 0.7) %>% 
  addPolygons(data = df_merged, fillColor = ~pal2(df_merged$hits_climate_change), #layerId= ~nom,
              fillOpacity = 0.7,
              group = "Climate Change",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=df_popup$pop2,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal2, group = "Climate Change" , values = df_merged$hits_climate_change, title = "Climate Change hits", opacity = 0.7) %>% 
  addPolygons(data = df_merged, fillColor = ~pal3(df_merged$hits_parisagreement), #layerId= ~nom,
              fillOpacity = 0.7,
              group = "Paris Agreement",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=df_popup$pop3,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal3, group = "Paris Agreement" , values = df_merged$hits_parisagreement, title = "Paris Agreement hits", opacity = 0.7) %>%
  addPolygons(data = df_merged, fillColor = ~pal6(df_merged$hits_renew_energy), #layerId= ~nom,
              fillOpacity = 0.7,
              group = "Renewable Energy",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=df_popup$pop6,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal6, group = "Renewable Energy" , values = df_merged$hits_renew_energy, title = "Renewable Energy hits", opacity = 0.7) %>%
  addPolygons(data = df_merged, fillColor = ~pal7(df_merged$hits_Insoumise), #layerId= ~nom,
              fillOpacity = 0.7,
              group = "Melenchon's Party",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=df_popup$pop7,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal7, group = "Melenchon's Party" , values = df_merged$hits_Insoumise, title = "Melenchon's Party hits", opacity = 0.7) %>%
  addPolygons(data = df_merged, fillColor = ~pal8(df_merged$hits_Lutte_Ouvriere), #layerId= ~nom,
              fillOpacity = 0.7,
              group = "Arthaud's Party",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=df_popup$pop8,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal8, group = "Arthaud's Party" , values = df_merged$hits_Lutte_Ouvriere, title = "Arthaud's Party hits", opacity = 0.7) %>%
  addPolygons(data = df_merged, fillColor = ~pal9(df_merged$hits_En_Marche), #layerId= ~nom,
              fillOpacity = 0.7,
              group = "Macron's Party",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=df_popup$pop9,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal9, group = "Macron's Party" , values = df_merged$hits_En_Marche, title = "Macron's Party hits", opacity = 0.7) %>%
  addPolygons(data = df_merged, fillColor = ~pal10(df_merged$hits_Debout_la_France), #layerId= ~nom,
              fillOpacity = 0.7,
              group = "Dupont's Party",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=df_popup$pop10,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal10, group = "Dupont's Party" , values = df_merged$hits_Debout_la_France, title = "Dupont's Party hits", opacity = 0.7) %>%
  addPolygons(data = df_merged, fillColor = ~pal11(df_merged$hits_National_Front), #layerId= ~nom,
              fillOpacity = 0.7,
              group = "Le Pen's Party",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=df_popup$pop11,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal11, group = "Le Pen's Party" , values = df_merged$hits_National_Front, title = "Le Pen's Party hits", opacity = 0.7) %>%
   addLayersControl(
    overlayGroups = c( "Flood", "Climate Change", "Paris Agreement","Renewable Energy","Melenchon's Party", "Arthaud's Party" , "Macron's Party", "Dupont's Party", "Le Pen's Party"),
    options = layersControlOptions(collapsed = T),
    position = "bottomleft"
  ) %>% hideGroup(c("Climate Change","Paris Agreement", "Renewable Energy", "Melenchon's Party", "Arthaud's Party","Macron's Party", "Dupont's Party", "Le Pen's Party"
                   ))


##################################################################################
#data preparation to connect dep names to gtrends datasets
geog <- name_geographic_information %>% 
  select(code_région, nom_région, numéro_département, nom_département, nom_commune, code_insee) %>% 
  rename(regcode = code_région) %>% mutate(regcode = as.numeric(regcode))

#check whether my code and regions are the same with the name_geographic_info dataset
#mine doesnt include regions with code less than 11
test <- geog %>% select(nom_région, code_région) %>% unique()   

df_merged<- df_merged %>% mutate(regcode = as.numeric(regcode))

df_merged_w <- left_join(df_merged, geog) %>% as.data.frame()

#saveRDS(df_merged_w, "hits.RDS")
