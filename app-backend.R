library(leaflet)
library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(spdplyr)
library(tigris)
library(plotly)
library(shinyBS)
library(stringr)
library(shinythemes)
library(purrr)
rm(list=ls())

########################reading datasets #######################
# the datasets format have changed from csv to rds, 
#because rds is a compressed file, so the size is reduced

results <- readRDS("results.rds")
coords <- readRDS("geographic_information.rds")
df_sal <- readRDS("df_dep_sal.rds")
df_pop <- readRDS("df_dep_pop.rds")
deps <- rgdal::readOGR("departements.geojson")
df_depcom <- readRDS("df_depcom.rds")
#departments_unemployement_rate <- readRDS("departments_unempolyment_rate.rds")
unemployement <- readRDS("unemployement.rds")
departments_age <- readRDS("departments_age.rds")

########################## Data preparation ######################
coords <- coords %>% 
  mutate(code = as.factor(code),
         DEP_NAME = as.factor(DEP_NAME)) %>% 
  rename(nom = DEP_NAME) %>% select(-code) #bcz in the deps dataset it was nom

df_sal <- df_sal %>% 
  rename(nom = LIBGEO) %>% 
  mutate(nom = as.factor(nom))

deps <-  deps %>% 
  mutate(nom = as.factor(nom))

#selecting whatever is needed from result dataset
df <- results %>% 
  select(DEP_NAME, Cand1_LastName, Cand1_Votes, 
         Cand2_LastName, Cand2_Votes, 
         Cand3_LastName, Cand3_Votes, 
         Cand4_LastName, Cand4_Votes,
         Cand5_LastName, Cand5_Votes, 
         Cand6_LastName, Cand6_Votes, 
         Cand7_LastName, Cand7_Votes, 
         Cand8_LastName, Cand8_Votes, 
         Cand9_LastName, Cand9_Votes,
         Cand10_LastName, Cand10_Votes, 
         Cand11_LastName, Cand11_Votes)


#calculating each candidate's vote in a department
df2 <- df %>%
  pivot_longer(cols = -DEP_NAME, names_to = c("set",".value"), 
               names_pattern = "^[A-Za-z]+(\\d+)_([A-Za-z]+)")


#calculating each candidate's vote percent in a department & calculating the first candidate's percent in each department
df3 <- df2 %>% group_by(DEP_NAME) %>% 
  mutate(total_votes = sum(Votes))%>% 
  group_by(DEP_NAME, LastName) %>% 
  mutate(votep = sum(Votes),
         percent_vote = votep / total_votes) %>% 
  summarise(percent_vote = min(percent_vote)) %>% 
  mutate(first_cand = LastName[which.max(percent_vote)]) %>% 
  mutate(firstcand_dep = max(percent_vote)) %>% 
  mutate(firstcand_dep = round(firstcand_dep*100))




######################### starting the server section #########################


    
    #merging and mapping
    df_color <- df3 %>% mutate(percent_vote = floor(percent_vote * 100)) #fixing the color palette for each candidate
    df_map <- df3 %>% filter(LastName == "MACRON") %>%  #user's input for example: "MACRON" instead of input$candidate
      rename(nom = DEP_NAME) %>% 
      mutate(nom = as.factor(nom))
    
    
    df_map$percent_vote <- floor(df_map$percent_vote * 100) #rounding the percents to be shown on map
    
    #merging all datasets to deps and the library is purrr
    df_merged <- list(
      deps,
      df_map,
      coords,
      df_sal,
      df_pop,
      departments_age
    ) %>% 
      reduce(left_join)
  
    df_merged <- df_merged %>% mutate(code = str_remove(code, "^0+")) %>% 
      left_join(., unemployement, by= "code")
    
    #map
    pal <- colorBin("magma", df_color$percent_vote, 8, pretty = FALSE)
    #pal <- colorNumeric("Purples", domain = df_color$percent_vote) #choosing the color palette for the main plot
    pal2 <- colorFactor(palette =  c("#AA0000", "#057C85", "#808080" ,"#0087cd", "#0066CC",
                                     "#ed1651", "#ADC1FD", "#004A77", "#FFD600", "#C9462C", "Snow3"), 
                        levels = c("ARTHAUD", "ASSELINEAU", "CHEMINADE", "DUPONT-AIGNAN", "FILLON", 
                                   "HAMON", "LASSALLE", "LE PEN", "MACRON", "MÉLENCHON", "#C0081F"))
    pal3 <- colorBin("viridis", df_merged$unemployment_rate, 5, pretty = FALSE)
    
    #making popups on the map (paste0 can get both variables and words)
    df4 <- df_merged %>% 
      mutate(pop1 = paste0(df_merged$percent_vote,"% at ", df_merged$nom),
             pop2 = paste0(df_merged$nom," population: ", df_merged$pop, " (x1000)"),
             pop3= paste0(df_merged$first_cand, "is the first selected candidate at ", df_merged$nom),
             pop4 = paste0(round(df_merged$firstcand_dep * 100, 1), "% at ", df_merged$nom),
             pop5 = paste0("unemployment rate: ", round(df_merged$unemployment_rate, 1), "% at ", df_merged$nom))
    

    #mitubi az popup_sb estefade nakonio be jash hamun df4$pop1 ro bzari
    
    leaflet() %>%
      addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
      addCircleMarkers(data = df_merged, lng = ~longitude, 
                       lat = ~latitude, ~ pop/70, color = "red" , popup = ~df4$pop2,
                       stroke = F, group = "Population") %>% #group is for putting these in different layers
      addCircleMarkers(data = df_merged, lng = ~longitude, lat = ~latitude,
                       ~ SNHM14, fillOpacity = 0.7 ,color = "orange", 
                       popup = ~paste0("Average Salary: " ,round(SNHM14, 1), " €/hr at ", df_merged$nom),
                       stroke = F, group = "Mean Wage") %>%
      addPolygons(data = df_merged, fillColor = ~pal(df_merged$percent_vote), layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Votes",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))  %>%
      addPolygons(data = df_merged, fillColor = ~pal2(df_merged$first_cand), #layerId= ~nom2,
                  fillOpacity = 0.7,
                  group = "First Candidate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop4,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))  %>%
      addPolygons(data = df_merged, fillColor = ~pal3(df_merged$unemployment_rate), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Unemployment Rate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop5,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      addLegend(pal = pal, group = "Votes" , values = df_merged$percent_vote, title = "Vote %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      addLegend(pal = pal3, group = "Unemployment Rate" , values = df_merged$unemployment_rate, title = "Unemployment Rate %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      addLegend(pal = pal2, group = "First Candidate" , values = df_merged$first_cand, opacity = 0.7) %>%
      addLayersControl(
        overlayGroups = c("Population","Mean Wage", "Votes", "First Candidate", "Unemployment Rate"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = T)
      ) %>% hideGroup(c("Population","Mean Wage", "First Candidate", "Unemployment Rate"))
    
    

  ##################end of reactive part 1
  
  
  ################starting reactive part 2  
  
  #bar chart for dep 1


    
    
    plot1 <- df3 %>% filter(DEP_NAME == "Ain") %>% 
      mutate(percent_vote =round(percent_vote * 100, 1)) %>% 
      ggplot(aes(x = reorder(LastName, -percent_vote), y = percent_vote, fill=LastName, 
                 text = percent_vote)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(values = c("#AA0000", "#057C85", "#808080" ,"#0087cd", "#0066CC",
                                   "#ed1651", "#ADC1FD", "#004A77", "#FFD600", "#C9462C", "Snow3"))+
      theme_minimal() +
      ggtitle(paste0("Candidate Preference in ", "Ain") )+
      theme(axis.text.x = element_text(size = 7, face = "bold", angle = 45, margin = margin(t = 6)),
            legend.position = "none") +
      labs(x="Candidate", y="Vote %")
    
    ggplotly(plot1, tooltip = "text")

 #######################################scatter plots aleki###################################################   
    #scatter plot percent_vote vs average salary
  df_merged %>%  
    as.data.frame() %>% 
      ggplot(aes(x=SNHM14, y=percent_vote)) + geom_point() + geom_smooth()+
    theme_minimal() +
    ggtitle(paste0("Vote% VS. Average Salary in ", "Ain ") ) +
    labs(x="Average Salary", y="Vote %")
  
  #scatter plot percent_vote vs population
    df_merged %>% 
      as.data.frame() %>% 
      ggplot(aes(x=pop, y=percent_vote)) + geom_point() + geom_smooth()+
      theme_minimal() +
      ggtitle(paste0("Vote% VS. Population in ", "Ain ") ) +
      labs(x="Population", y="Vote %")
    
    
    #scatter plot percent_vote vs unemployment_rate
    df_merged %>% 
      as.data.frame() %>% 
      ggplot(aes(x=unemployment_rate, y=percent_vote)) + geom_point() + geom_smooth()+
      theme_minimal() +
      ggtitle(paste0("Vote% VS. Unemployment_Rate in ", "Ain ") ) +
      labs(x="Unemployment_Rate", y="Vote %")

    #######################################################################################    
   
     #scatter plot percent_vote vs age from: https://academic.oup.com/gerontologist/article/42/1/92/641498
    
    test <- df_merged %>% 
      as.data.frame()  %>% 
      mutate(age_category = ifelse(age== "20_24" | age== "25_29"|age== "30_34", "young adults",
                                   ifelse(age== "35_39" | age== "40_44"|age== "45_49"| age== "50_54", "middle_aged adults",
                                          "older adults"))) %>% 
      group_by(nom, age_category) %>% 
      summarise(ppl_in_age_cat = sum(people),
                percent_vote = percent_vote,
                average_age = average_age,
                LastName= LastName) %>% 
      group_by(nom) %>% 
      mutate(perc_ppl_age_cat = ppl_in_age_cat / sum(ppl_in_age_cat),
             mean1 = average_age * perc_ppl_age_cat)
      
    test1 <- test %>% 
      group_by(nom) %>% 
      summarise(mean_age = sum(mean1), 
                percent_vote = mean(percent_vote)
                ) %>% 
      dplyr::select(nom, mean_age, percent_vote) 
    
    ggplot(data = test1, aes(x= mean_age, y= percent_vote))+geom_point()+
      geom_smooth(method = "lm")+ theme_minimal() +
      ggtitle(paste0("MACRON's" , " Vote% VS. Average Age ", "at ") )
    
    
    
    
      
  #  ggplot(data=test, aes(x=perc_ppl_age_cat, y=percent_vote, color=age_category)) + 
   #   geom_point()+
    #  geom_smooth(method= "lm", se=FALSE)
   #it <- lm(percent_vote ~ perc_ppl_age_cat+age_category, data=test)
   #est %>% 
   # mutate(fit = fitted(fit)) %>%
   # ggplot(aes(x=perc_ppl_age_cat, y=percent_vote, color=age_category)) + 
   # geom_point(size=1)+  # points smalle
   # geom_smooth(method=lm, se=F)+
   # geom_line(aes(y=fit), linetype="twodash", size=1.5)
    
    
    test %>% 
     filter(age_category == "middle_aged adults") %>% 
     ggplot(aes(x=perc_ppl_age_cat, y=percent_vote)) + geom_point() +
     geom_smooth(method = "lm") 
    #  labs(x= "percent of people between 20-24", y="percent of LE PEN's vote") +
    #  theme_minimal() 
      #ggtitle(paste0("Vote% VS. Unemployment_Rate in ", "Ain ") ) +
      #labs(x="Age", y="Vote %")

    