library(leaflet)
library(shinydashboard)
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
library(shinycssloaders)
library(purrr)
rm(list=ls())

########################reading datasets #######################
#communes datasets
#df_com_sal <- readRDS("df_com_sal.rds")
#df_depcom_pop <- readRDS("df_depcom_pop.rds")
communes_age <- readRDS("communes_age.rds")
df_test <- readRDS("merged_votes_map.rds")
com_vote <- readRDS("com_vote.rds")

#departments datasets
df_imm_dep <- readRDS("~/Desktop/Datasets/French Election/French_election_app/df_imm_dep.rds")
dep_vote <- readRDS("dep_vote.rds")
results <- readRDS("results.rds")
coords <- readRDS("geographic_information.rds")
df_sal <- readRDS("df_dep_sal.rds")
df_pop <- readRDS("df_dep_pop.rds")
deps <- readRDS("departments.rds")
temp <- readRDS("~/Desktop/Datasets/French Election/French_election_app/temp.rds")
unemployement <- readRDS("unemployement.rds")
departments_age <- readRDS("departments_age.rds")
poverty <- readRDS("poverty.rds")
life <- readRDS("life.rds")
education <- readRDS("~/Desktop/Datasets/French Election/French_election_app/education.rds")
########################## Data preparation ######################

coords <- coords %>% 
  mutate(code = as.factor(code),
         DEP_NAME = as.factor(DEP_NAME)) %>% 
  rename(nom = DEP_NAME) %>% select(-code) #bcz in the deps dataset it was nom


df_sal <- df_sal %>% 
  rename(nom = LIBGEO) %>% 
  mutate(nom = as.factor(nom))

#selecting whatever is needed from result dataset
df <- results %>% 
  select(DEP_NAME,COM_NAME, Cand1_LastName, Cand1_Votes, 
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
df2 <- df %>% select(-COM_NAME) %>% 
  pivot_longer(cols = -DEP_NAME, names_to = c("set",".value"), 
               names_pattern = "^[A-Za-z]+(\\d+)_([A-Za-z]+)")

df2_c <- df %>% select(-DEP_NAME) %>%
  pivot_longer(cols = -COM_NAME, names_to = c("set",".value"), 
               names_pattern = "^[A-Za-z]+(\\d+)_([A-Za-z]+)")

#calculating each candidate's vote percent in a department & calculating the first candidate's percent in each department
df3 <- df2 %>% group_by(DEP_NAME) %>% 
  mutate(total_votes = sum(Votes))%>% 
  group_by(DEP_NAME, LastName) %>% 
  mutate(votep = sum(Votes),
         percent_vote = votep / total_votes) %>% 
  summarise(percent_vote = min(percent_vote)) %>% 
  mutate(first_cand = LastName[which.max(percent_vote)]) %>% 
  mutate(firstcand_dep = max(percent_vote))

df_name <- unique(df2$DEP_NAME)

###########################starting the ui section ######################
ui <- fluidPage(
  
  #aesthetic 
  theme = shinytheme("flatly"),
  
  navbarPage("Exploring French Election Data", theme = shinytheme("flatly"),
             tabPanel("Departments",
                      sidebarLayout(
                        column(width = 4,
                               fluidRow(
                                 #h4(div(HTML("<em> Select a candidate: </em>"))),
                                 selectInput(
                                   "candidate", "Choose a candidate:",
                                   c("Marine LE PEN"  = "LE PEN" ,
                                     "Jean-Luc MÉLENCHON" = "MÉLENCHON"   ,
                                     "Emmanuel MACRON"   = "MACRON"   ,
                                     "François FILLON"   = "FILLON"   ,
                                     "Nicolas DUPONT-AIGNAN" = "DUPONT-AIGNAN",
                                     "Jean LASSALLE" = "LASSALLE"     ,
                                     "Benoît HAMON" = "HAMON"        ,
                                     "François ASSELINEAU" = "ASSELINEAU" ,
                                     "Philippe POUTOU" =  "POUTOU")
                                 ),
                                 
                                 plotlyOutput("view2"),
                               ),
                        ),
                        mainPanel(
                          leafletOutput("view", height = 500) %>% withSpinner(color = "#1E90FF")
                        )
                      )
             ),
             tabPanel("Communes",
                      sidebarLayout(
                        column(width = 4,
                               fluidRow(
                                 #h4(div(HTML("<em> Select a candidate: </em>"))),
                                 selectInput(
                                   "candidate_c", "Choose a candidate:",
                                   c("Marine LE PEN"  = "LE PEN" ,
                                     "Jean-Luc MÉLENCHON" = "MÉLENCHON"   ,
                                     "Emmanuel MACRON"   = "MACRON"   ,
                                     "François FILLON"   = "FILLON"   ,
                                     "Nicolas DUPONT-AIGNAN" = "DUPONT-AIGNAN",
                                     "Jean LASSALLE" = "LASSALLE"     ,
                                     "Benoît HAMON" = "HAMON"        ,
                                     "François ASSELINEAU" = "ASSELINEAU" ,
                                     "Philippe POUTOU" =  "POUTOU",
                                     "Nathalie ARTHAUD" = "ARTHAUD",
                                     "Jacques CHEMINADE" = "CHEMINADE")
                                 ),
                                 
                                 selectInput("department", "Select a department", df_name, selected = "Aisne"),
                               ),
                        ),
                        mainPanel(
                          leafletOutput("view_c", height = 500) %>% withSpinner(color = "#1E90FF")
                        )
                      )
             ),
             tabPanel("Scatter Plots",
                      sidebarLayout(
                        column(width = 2,
                               fluidRow(
                                 #h4(div(HTML("<em> Select a candidate: </em>"))),
                                 selectInput(
                                   "candidate_scatter_plot", "Choose a candidate:",
                                   c("Marine LE PEN"  = "LE PEN" ,
                                     "Jean-Luc MÉLENCHON" = "MÉLENCHON"   ,
                                     "Emmanuel MACRON"   = "MACRON"   ,
                                     "François FILLON"   = "FILLON"   ,
                                     "Nicolas DUPONT-AIGNAN" = "DUPONT-AIGNAN",
                                     "Jean LASSALLE" = "LASSALLE"     ,
                                     "Benoît HAMON" = "HAMON"        ,
                                     "François ASSELINEAU" = "ASSELINEAU" ,
                                     "Philippe POUTOU" =  "POUTOU",
                                     "Nathalie ARTHAUD" = "ARTHAUD",
                                     "Jacques CHEMINADE" = "CHEMINADE")
                                 ),
                                 
                               ),
                        ),
                        mainPanel(
                          column(width = 6,
                                 plotlyOutput("view_scatter") %>% withSpinner(color = "#1E90FF"),
                                 hr()
                          ),
                          column(width = 6,
                                 plotlyOutput("view_scatter_age") %>% withSpinner(color = "#1E90FF"),
                                 hr()
                          ),
                          column(width = 6,
                                 plotlyOutput("view_scatter_unemployment") %>% withSpinner(color = "#1E90FF")
                          ),
                          column(width = 6,
                                 plotlyOutput("view_scatter_salary") %>% withSpinner(color = "#1E90FF")
                          ),
                          column(width = 6,
                                 plotlyOutput("view_scatter_immigrants") %>% withSpinner(color = "#1E90FF")
                          ),
                          
                          column(width = 6,
                                 plotlyOutput("view_scatter_temp") %>% withSpinner(color = "#1E90FF")
                          ),
                          column(width = 6,
                                 plotlyOutput("view_scatter_poverty") %>% withSpinner(color = "#1E90FF")
                          ),
                          column(width = 6,
                                 plotlyOutput("view_scatter_life") %>% withSpinner(color = "#1E90FF")
                          ),
                        )
                      )
             )
  )
  
  
)


######################### starting the server section #########################

server <- function(input, output) {
  
  ################starting reactive part department map ################
  output$view <- renderLeaflet({ #pay attention to what is the type of plot (main plot)
    
    
    
    #merging and mapping
    df_color <- df3 %>% mutate(percent_vote = floor(percent_vote * 100)) #fixing the color palette for each candidate
    df_map <- df3 %>% filter(LastName == input$candidate) %>%  #user's input for example: "MACRON" instead of input$candidate
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
      df_imm_dep,
      temp,
      poverty,
      life,
      education
    ) %>% 
      reduce(left_join)
    
    df_merged <- df_merged %>% mutate(code = str_remove(code, "^0+")) %>% 
      left_join(., unemployement, by= "code")
    
    #map
    pal <- colorBin("magma", df_color$percent_vote, 8, pretty = FALSE)
    #pal <- colorNumeric("Purples", domain = df_color$percent_vote) #choosing the color palette for the main plot
    pal2 <- colorFactor(palette = c("#AA0000", "#057C85", "#808080" ,"#0087cd", "#0066CC",
                                    "#ed1651", "#ADC1FD", "#004A77", "#FFD600", "#C9462C", "Snow3"), 
                        levels = c("ARTHAUD", "ASSELINEAU", "CHEMINADE", "DUPONT-AIGNAN", "FILLON", 
                                   "HAMON", "LASSALLE", "LE PEN", "MACRON", "MÉLENCHON", "POUTOU"))

    pal3 <- colorBin("inferno", df_merged$unemployment_rate, 4, pretty = T)   
    pal4 <- colorBin("inferno", df_merged$percent_imm_dep, 8, pretty = T)
    pal5 <- colorFactor(palette = c("#8B0000", "#FF0000", "#FF4500" ,"#FF8C00", "#FFFFCC",
                                    "#99CCFF", "#00CCFF", "#0099FF", "#0066FF", "#0033CC", "#0000CC"), 
                        levels = c("15 or above", "13.5 - 15", "13 - 13.5", "12.5 - 13", "12.25 - 12.5", 
                                   "12.125 - 12.25", "12 - 12.125", "11.75 - 12", "11.5 - 11.75", "11- 11.5", "11 or below"))
    
    pal6 <- colorFactor(palette = c("#FF6600", "#FF9900","#FFCC66" ), levels = c(">21" , "14.5 - 21", "14.5>"))
    pal7 <- colorBin("inferno", df_merged$avg_life, 3, pretty = T)
    pal8 <- colorFactor(palette = c("#FF6600", "#FF9900","#FFCC66" ), levels = c(">=18.8" , "17.2 - 18.8", "17.2=<"))

    
    
    #making popups on the map (paste0 can get both variables and words)
    df4 <- df_merged %>% 
      mutate(pop1 = paste0(df_merged$percent_vote,"% at ", df_merged$nom),
             pop2 = paste0(df_merged$nom," population: ", df_merged$pop, " (x1000)"),
             pop3= paste0(df_merged$first_cand, "is the first selected candidate at ", df_merged$nom),
             pop4 = paste0(round(df_merged$firstcand_dep * 100, 1), "% at ", df_merged$nom),
             pop5 = paste0("Unemployment Rate: ", round(df_merged$unemployment_rate, 1), "% at ", df_merged$nom),
             pop6 = paste0("Immigrants at " ,df_merged$nom, ": ", df_merged$percent_imm_dep),
             pop7= paste0("Average Temp. at ", df_merged$nom, ": ", df_merged$tmoy),
             pop8 = paste0("Poverty Rate at ", df_merged$nom, ": ", df_merged$poverty_rate),
             pop9= paste0("Avg. Life Expectancy at ", df_merged$nom, ": ", df_merged$avg_life),
             pop10 = paste0("People with the Highest Education % at ", df_merged$nom, ": ", df_merged$edu_int)
             )
    
    popup_sb <- df4$pop1
    #df_merged$nom2 <- df_merged$nom
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
                  label=popup_sb,
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
      addPolygons(data = df_merged, fillColor = ~pal4(df_merged$percent_imm_dep), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Immigrants",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop6,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addPolygons(data = df_merged, fillColor = pal5(df_merged$temp_interval), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Average Temprature",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addPolygons(data = df_merged, fillColor = ~pal6(df_merged$pov_interval), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Poverty Rate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop8,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addPolygons(data = df_merged, fillColor = ~pal7(df_merged$avg_life), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Avg. Life Expectancy",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop9,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addPolygons(data = df_merged, fillColor = ~pal8(df_merged$edu_int), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Share of People with the Highest Education",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop10,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, group = "Votes" , values = df_merged$percent_vote, title = "Vote %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      addLegend(pal = pal3, group = "Unemployment Rate" , values = df_merged$unemployment_rate, title = "Unemployment Rate %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      addLegend(pal = pal2, group = "First Candidate" , values = df_merged$first_cand, title = "Candidate Name" ,opacity = 0.7) %>%
      addLegend(pal = pal4, group = "Immigrants" , values = df_merged$percent_imm_dep, title = "Immigrants (per 100K people)" ,opacity = 0.7) %>%
      addLegend(pal = pal5, group = "Average Temprature" , values = df_merged$temp_interval, title = "Average Temprature (°C)" ,opacity = 0.7) %>%
      addLegend(pal = pal6, group = "Poverty Rate" , values = df_merged$pov_interval, title = "Poverty Rate %" ,opacity = 0.7, labFormat = labelFormat(suffix = " %")) %>%
      addLegend(pal = pal7, group = "Avg. Life Expectancy" , values = df_merged$avg_life, title = "Avg. Life Expectancy" ,opacity = 0.7) %>%
      addLegend(pal = pal8, group = "Share of People with the Highest Education" , values = df_merged$edu_int, title = "Education Rate %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      addLayersControl(
        overlayGroups = c( "Votes", "First Candidate", "Population","Immigrants", "Mean Wage","Unemployment Rate", 
                           "Average Temprature", "Poverty Rate", "Avg. Life Expectancy",
                           "Share of People with the Highest Education"),
        options = layersControlOptions(collapsed = T),
        position = "bottomleft"
      ) %>% hideGroup(c("Population","Mean Wage", "First Candidate", "Unemployment Rate", "Immigrants",
                        "Average Temprature", "Poverty Rate", "Avg. Life Expectancy",
                        "Share of People with the Highest Education"))
    
    
  }) 
  
  ##################end of reactive part 1
  
  
  ################starting reactive part 2  
  
  #bar chart for dep 1
  output$view2 <- renderPlotly({
    
    #location <- ifelse(is.null(input$view_marker_click$lng),
    #                  input$view_shape_click$id, 
    #                  as.character(coords$nom[which(coords$longitude == input$view_marker_click$lng)])
    #                  )
    #
    
    req(!is.null(input$view_shape_click$id))
    location <- input$view_shape_click$id #yani jayi ke karbar rush click mikone tu naghshe
    
    
    plot1 <- df3 %>% filter(DEP_NAME == location) %>% 
      mutate(percent_vote =round(percent_vote * 100, 1), 
             firstcand_dep =round(firstcand_dep * 100, 1)) %>% 
      ggplot(aes(x = reorder(LastName, -percent_vote), y = percent_vote, fill=LastName, 
                 text = percent_vote)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(values = c("#AA0000", "#057C85", "#808080" ,"#0087cd", "#0066CC",
                                   "#ed1651", "#ADC1FD", "#004A77", "#FFD600", "#C9462C", "Snow3"))+
      theme_minimal() +
      ggtitle(paste0("Candidate Preference in ", location))+
      theme(axis.text.x = element_text(size = 7, face = "bold", angle = 45, margin = margin(t = 6)),
            legend.position = "none") +
      labs(x="Candidate", y="Vote %")
    
    ggplotly(plot1, tooltip = "text")
    
  }) %>% bindEvent(input$view_shape_click)
  
  
  
  
  #################### Communes ##############
  output$view_c <- renderLeaflet({ #pay attention to what is the type of plot (main plot)
    
    
    
    #merging and mapping
    df_color <- com_vote %>% mutate(percent_vote = floor(percent_vote * 100)) #fixing the color palette for each candidate
    
    
    df_map <- com_vote %>% filter(DEP_NAME == input$department, LastName == input$candidate_c) 
    
    df_map$percent_vote <- floor(df_map$percent_vote * 100) #rounding the percents to be shown on map
    
    
    pal <- colorBin("magma", df_color$percent_vote, 8, pretty = FALSE)
    pal2 <- colorFactor(palette = c("#AA0000", "#057C85", "#808080" ,"#0087cd", "#0066CC",
                                    "#ed1651", "#ADC1FD", "#004A77", "#FFD600", "#C9462C", "Snow3"), 
                        levels = c("ARTHAUD", "ASSELINEAU", "CHEMINADE", "DUPONT-AIGNAN", "FILLON", 
                                   "HAMON", "LASSALLE", "LE PEN", "MACRON", "MÉLENCHON", "POUTOU"))
    pal3 <- colorBin("viridis", domain = df_map$pop,10, pretty = T)
    # pal4 <- colorBin("viridis", domain = df_map$mean_salary_com,5, pretty = T)
    pal5 <- colorBin("magma", domain = df_map$imm_percent,8, pretty = T)
    pal6 <- colorBin("magma", domain = df_map$unemp_rate,8, pretty = T)
    
    df4 <- df_map %>% 
      mutate(pop1 = paste0(df_map$percent_vote,"% at ", df_map$COM_NAME),
             pop2 = paste0("Population at ", df_map$COM_NAME, " : ",df_map$pop, " (x1000)"),
             pop3= paste0(df_map$COM_NAME , " : Won by ", df_map$first_cand),
             pop5= paste0("Immigrants at " ,df_map$COM_NAME, ": ", df_map$imm_percent),
             #pop4 = paste0("Average Salary: " ,round(df_map$mean_salary_com, 1), " €/hr at ", df_map$COM_NAME),
             pop6= paste0("Unemployment Rate: ", round(df_map$unemp_rate, 1), "% at ", df_map$COM_NAME))
    
    
    leaflet() %>%
      addTiles() %>% setView(min(df_map$longitude), min(df_map$latitude),zoom = 8) %>% #mape kolie kore zamin
      addPolygons(data = df_map, fillColor = ~pal(percent_vote), layerId= ~percent_vote,
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
      addPolygons(data = df_map, fillColor = ~pal2(df_map$first_cand), #layerId= ~nom2,
                  fillOpacity = 0.7,
                  group = "First Candidate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))  %>%
      addPolygons(data = df_map, fillColor = ~pal3(df_map$pop), #layerId= ~nom2,
                  fillOpacity = 0.7,
                  group = "Population",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))  %>%
      # addPolygons(data = df_map, fillColor = ~pal3(df_map$mean_salary_com), #layerId= ~nom2,
      #             fillOpacity = 0.7,
      #             group = "Average Salary",
      #             weight = 0.2,
      #             smoothFactor = 0.2,
      #             highlight = highlightOptions(
      #               weight = 5,
      #               color = "#666",
      #               fillOpacity = 0.2,
      #               bringToFront = TRUE),
      #             label=df4$pop4,
    #             labelOptions = labelOptions(
    #               style = list("font-weight" = "normal", padding = "3px 8px"),
    #               textsize = "15px",
    #               direction = "auto"))  %>%
    addPolygons(data = df_map, fillColor = ~pal5(imm_percent), #layerId= ~percent_vote,
                fillOpacity = 0.7,
                group = "Immigrants",
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
                  direction = "auto"))  %>%
      addPolygons(data = df_map, fillColor = ~pal6(unemp_rate), #layerId= ~percent_vote,
                  fillOpacity = 0.7,
                  group = "Unemployment Rate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop6,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))  %>%
      addLegend(pal = pal, group = "Votes" , values = df_map$percent_vote, title = "Vote %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      addLegend(pal = pal2, group = "First Candidate" , values = df_map$first_cand, title = "Candidate Name", opacity = 0.7) %>%
      addLegend(pal = pal3, group = "Population" , values = df_map$pop, title = "Population (x 1000)",opacity = 0.7) %>%
      # addLegend(pal = pal3, group = "Average Salary" , values = df_map$mean_salary_com, title = "€/hr",opacity = 0.7) %>%
      addLegend(pal = pal5, group = "Immigrants" , values = df_map$imm_percent, title = "Immigrants (per 100K people)",opacity = 0.7) %>%
      addLegend(pal = pal6, group = "Unemployment Rate" , values = df_map$unemp_rate, title = "Unemployment Rate %",opacity = 0.7, labFormat = labelFormat(suffix = " %")) %>%
      addLayersControl(
        overlayGroups = c("Votes", "First Candidate",
                          "Population", "Immigrants","Unemployment Rate" ), # "Average Salary"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = T)
      ) %>% hideGroup(c( "First Candidate"
                         , "Population", "Immigrants", "Unemployment Rate")) #, "Average Salary"))
    
    
  }) 
  #####################end communes
  
  df_merged <- reactive({ 
    
    df_map <- df3 %>% filter(LastName == input$candidate_scatter_plot) %>%  #user's input for example: "MACRON" instead of input$candidate
      rename(nom = DEP_NAME) %>% 
      mutate(nom = as.factor(nom))
    
    
    df_merged <- list(
      as.data.frame(deps),
      df_map,
      coords,
      df_sal,
      df_pop,
      departments_age,
      df_imm_dep,
      temp,
      poverty,
      life
    ) %>% 
      reduce(left_join)
    
    df_merged <- df_merged %>% mutate(code = str_remove(code, "^0+")) %>% 
      left_join(., unemployement, by= "code")
    
    return(df_merged)
    
  })
  
  #Scatter plots
  output$view_scatter <- renderPlotly({ # Vote vs Pop
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=pop, y=percent_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Population (in deparments)") ) +
      labs(x="Population (x 1000)", y="Vote %")
    
    ggplotly(plot1, tooltip = "text")
    
  }) 
  
  
  output$view_scatter_age <- renderPlotly({ # Vote vs age
    
    test <- df_merged() %>% 
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
    
    p <- ggplot(data = test1, aes(x= mean_age, y= percent_vote))+geom_point(aes(text = nom))+
      geom_smooth(method = "lm")+ theme_minimal() +
      ggtitle(paste0("Vote - mean age (in deparments)") ) +
      labs(x = "Average age" , y = "Vote %")
    
    
    ggplotly(p, tooltip = "text")
    
  })
  
  
  output$view_scatter_unemployment <- renderPlotly({ #Vote vs unemployment
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=unemployment_rate, y=percent_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Unemployment (in departments)") ) +
      labs(x="Unemployment Rate %", y="Vote %")
    
    ggplotly(plot1, tooltip = "text")
    
  }) 
  
  
  
  output$view_scatter_salary <- renderPlotly({ #Vote vs salary
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=SNHM14, y=percent_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Mean Salary (in departments)") ) +
      labs(x="Average Salary", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
    
  }) 
  
  
  output$view_scatter_immigrants <- renderPlotly({ #Vote vs immigrants
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=percent_imm_dep, y=percent_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Immigrants (in departments)") ) +
      labs(x="Immigrants (per 100k)", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
    
  }) 
  
  output$view_scatter_temp <- renderPlotly({ #Vote vs temp
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=tmoy, y=percent_vote)) + geom_point(aes(text = nom)) + geom_smooth()+
      theme_minimal() +
      ggtitle(paste0("Vote - Temprature (in departments)") ) +
      labs(x="Average Temprature (°C)", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
    
    
    
  })  
  output$view_scatter_poverty <- renderPlotly({ #Vote vs poverty
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=poverty_rate, y=percent_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Poverty % (in departments)") ) +
      labs(x="Poverty Rate %", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
    
    
    
  })  
  
  output$view_scatter_life <- renderPlotly({ #Vote vs life expectancy
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=avg_life, y=percent_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Life Expectancy (in departments)") ) +
      labs(x="Life Expectancy", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
    
    
    
  })  
}#end of server

# Run the application
shinyApp(ui = ui, server = server)
