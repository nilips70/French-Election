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
library(purrr)
rm(list=ls())

########################reading datasets #######################
#communes datasets
#df_com_sal <- readRDS("df_com_sal.rds")
#df_depcom_pop <- readRDS("df_depcom_pop.rds")
communes_age <- readRDS("communes_age.rds")
df_test <- readRDS("merged_votes_map.rds")

#departments datasets
results <- readRDS("results.rds")
coords <- readRDS("geographic_information.rds")
df_sal <- readRDS("df_dep_sal.rds")
df_pop <- readRDS("df_dep_pop.rds")
deps <- readRDS("departments.rds")
#df_depcom <- readRDS("df_depcom.rds")
unemployement <- readRDS("unemployement.rds")
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
                          leafletOutput("view", height = 500)
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
                                 
                                 selectInput("department", "Select a department", df_name),
                               ),
                        ),
                        mainPanel(
                          leafletOutput("view_c")
                        )
                      )
             )
  )
  
  
)
# ui <- fluidPage(
# 
#   #aesthetic
#   theme = shinytheme("united"),
#   titlePanel("Exploring 2017 French Election Data"),
#   hr(),
#   sidebarLayout(
#     sidebarPanel(
#       fluidRow(
#         #h4(div(HTML("<em> Select a candidate: </em>"))),
#         selectInput(
#           "candidate", "Choose a candidate:",
#           c("Marine LE PEN"  = "LE PEN" ,
#             "Jean-Luc MÉLENCHON" = "MÉLENCHON"   ,
#             "Emmanuel MACRON"   = "MACRON"   ,
#             "François FILLON"   = "FILLON"   ,
#             "Nicolas DUPONT-AIGNAN" = "DUPONT-AIGNAN",
#             "Jean LASSALLE" = "LASSALLE"     ,
#             "Benoît HAMON" = "HAMON"        ,
#             "François ASSELINEAU" = "ASSELINEAU" ,
#             "Philippe POUTOU" =  "POUTOU",
#             "Nathalie ARTHAUD" = "ARTHAUD",
#             "Jacques CHEMINADE" = "CHEMINADE")
#         ),
#         plotlyOutput("view2"),
#       ),
#     ),
# 
# 
# 
#     # Main panel for displaying outputs ----
#     mainPanel(
#       navbarPage(
#         "",
#         tabPanel(
#           "Departments",
#           leafletOutput("view", height = 400),
#           hr(),
# 
#           #action button and bsModal are working together      ########
#           #actionButton("go", "Plot"),
#           # textOutput("temp"),
#           # tableOutput("view"),
#           # Output: HTML table with requested number of observations ---
#           #bsModal("modalExample", "Something", "go", size = "large", plotlyOutput("plot")),
#           # plotOutput("plot")
#           #br(), #space between the texts and maps
# 
# 
#         ), # End of main panel, unja bade parantez virgul bud vaghti about ro dashtim
# 
#         tabPanel(
#          "Communes",
#          fluidPage(
#            fluidRow(
#              leafletOutput("view_c", height = 400),
#              hr(),
#             #p("This app is developed for comparing traffic patterns in Ireland between different time periods and uses data from", span("https://www.nratrafficdata.ie/.", style = "color:blue")),
#              #p("Note that there could be some issues with the data such as missing values and etc. Feel free to use the 'Plot' button and observe the data in more detail.")
#            )
#           )
#         )
#       )
#     )
#   )
# )
# 

######################### starting the server section #########################

server <- function(input, output) {
  
  ################starting reactive part 1
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
      df_pop
      #departments_unemployement_rate,
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
    #making popups on the map (paste0 can get both variables and words)
    df4 <- df_merged %>% 
      mutate(pop1 = paste0(df_merged$percent_vote,"% at ", df_merged$nom),
             pop2 = paste0(df_merged$nom," population: ", df_merged$pop, " (x1000)"),
             pop3= paste0(df_merged$first_cand, "is the first selected candidate at ", df_merged$nom),
             pop4 = paste0(round(df_merged$firstcand_dep * 100, 1), "% at ", df_merged$nom),
             pop5 = paste0("Unemployment Rate: ", round(df_merged$unemployment_rate, 1), "% at ", df_merged$nom))
    
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
      addLegend(pal = pal, group = "Votes" , values = df_merged$percent_vote, title = "Vote %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      addLegend(pal = pal3, group = "Unemployment Rate" , values = df_merged$unemployment_rate, title = "Unemployment Rate %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      addLegend(pal = pal2, group = "First Candidate" , values = df_merged$first_cand, opacity = 0.7) %>%
      addLayersControl(
        overlayGroups = c("Population","Mean Wage", "Votes", "First Candidate", "Unemployment Rate"),
        options = layersControlOptions(collapsed = T),
        position = "bottomleft"
      ) %>% hideGroup(c("Population","Mean Wage", "First Candidate", "Unemployment Rate"))
    
    
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
  
 
  
  
  ####################Communes##############
  output$view_c <- renderLeaflet({ #pay attention to what is the type of plot (main plot)
    
    
    
    #merging and mapping
    df_color <- df_test %>% mutate(percent_vote = floor(percent_vote * 100)) #fixing the color palette for each candidate
    # df_depcom <- df_depcom %>% rename(COM_NAME = LIBGEO)
    # 
    # df_test <- list(
    #   df_depcom,
    #   df3_c
    # ) %>% 
    #   reduce(left_join)
    
    df_map <- df_test %>% filter(nom == input$department, LastName == input$candidate_c) 
    
    df_map$percent_vote <- floor(df_map$percent_vote * 100) #rounding the percents to be shown on map

    
    pal <- colorBin("magma", df_color$percent_vote, 8, pretty = FALSE)
    #pal <- colorNumeric("Purples", domain = df_color$percent_vote) #choosing the color palette for the main plot
    pal2 <- colorFactor(palette = c("#AA0000", "#057C85", "#808080" ,"#0087cd", "#0066CC",
                                    "#ed1651", "#ADC1FD", "#004A77", "#FFD600", "#C9462C", "Snow3"), 
                        levels = c("ARTHAUD", "ASSELINEAU", "CHEMINADE", "DUPONT-AIGNAN", "FILLON", 
                                   "HAMON", "LASSALLE", "LE PEN", "MACRON", "MÉLENCHON", "POUTOU"))
    #pal2 <- colorFactor(topo.colors(4), domain = df_color$first_cand)
   
    
    df4 <- df_map %>% 
      mutate(pop1 = paste0(df_map$percent_vote,"% at ", df_map$nom),
             #pop2 = paste0(df_merged$nom," population: ", df_merged$pop, " (x1000)"),
             pop3= paste0(df_map$first_cand, " is the first selected candidate at ", df_map$nom))
   
  
    leaflet() %>%
      addTiles() %>% setView(min(df_map$longitude), min(df_map$latitude) ,zoom = 8) %>% #mape kolie kore zamin
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
      addLegend(pal = pal, group = "Votes" , values = df_map$percent_vote, title = "Vote %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      addLegend(pal = pal2, group = "First Candidate" , values = df_map$first_cand, opacity = 0.7) %>%
      addLayersControl(
        overlayGroups = c("Votes", "First Candidate"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = T)
      ) %>% hideGroup(c( "First Candidate"))
    
    
  }) 
   #####################end communes
}#end of server

# Run the application
shinyApp(ui = ui, server = server)

