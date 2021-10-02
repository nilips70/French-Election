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

#reading datasets
results <- readRDS("results.rds")
coords <- readRDS("geographic_information.rds")
df_sal <- readRDS("df_dep_sal.rds")
df_pop <- readRDS("df_dep_pop.rds")
deps <- rgdal::readOGR("departements.geojson")


########################## Data preparation ######################
coords <- coords %>% 
  mutate(code = as.factor(code),
         DEP_NAME = as.factor(DEP_NAME)) %>% 
  rename(nom = DEP_NAME) %>% select(-code)

df_sal <- df_sal %>% 
  rename(nom = LIBGEO) %>% 
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




#calculating each candidate's vote percent in a department
df3 <- df2 %>% group_by(DEP_NAME) %>% 
  mutate(total_votes = sum(Votes))%>% 
  group_by(DEP_NAME, LastName) %>% 
  mutate(votep = sum(Votes),
         percent_vote = votep / total_votes) %>% 
  summarise(percent_vote = min(percent_vote))

###########################starting the ui section ######################

ui <- fluidPage(
  
  #aesthetic 
  theme = shinytheme("flatly"),
  titlePanel("Exploring French Election Data"),
  hr(),
  sidebarLayout(
    sidebarPanel(
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
        plotlyOutput("view2"), #barchart candidates
      ),
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      navbarPage(
        "",
        tabPanel(
          "Mapping",
          tags$style(
            type = "text/css", "html, body {width:100%;height:100%}",
            ".leaflet .legend i{
      border-radius: 50%;
      width: 10px;
      height: 10px;
      margin-top: 4px;
      }
    "
          ),
          leafletOutput("view"), #main map 
          hr(),

          #action button and bsModal are working together      ########
          #actionButton("go", "Plot"),
          # textOutput("temp"),
          # tableOutput("view"),
          # Output: HTML table with requested number of observations ---
          bsModal("modalExample", "Something", "go", size = "large", plotlyOutput("plot")),
          # plotOutput("plot")
          #br(), #space between the texts and maps
        
        
          ), # End of main panel
        
        tabPanel(
          "About",
          fluidPage(
            fluidRow(
              p("This app is developed for comparing traffic patterns in Ireland between different time periods and uses data from", span("https://www.nratrafficdata.ie/.", style = "color:blue")),
              p("Note that there could be some issues with the data such as missing values and etc. Feel free to use the 'Plot' button and observe the data in more detail.")
            )
          )
        )
      )
    )
  )
)

#starting the server section
server <- function(input, output) {
 
  #starting reactive part 1
   output$view <- renderLeaflet({ #pay attention to what is the type of plot
    
 
  
  #merging and mapping
     df_color <- df3 %>% mutate(percent_vote = floor(percent_vote * 100)) #fixing the color palette for each candidate
     df_map <- df3 %>% filter(LastName == input$candidate) %>%  #user's input MACRON input$candidate
       rename(nom = DEP_NAME) %>% 
       mutate(nom = as.factor(nom))
     
     
     df_map$percent_vote <- floor(df_map$percent_vote * 100) #rounding the percents to be shown on map

     #merging all datasets to deps
     df_merged <- list(
       deps,
       df_map,
       coords,
       df_sal,
       df_pop
     ) %>% 
       reduce(left_join)
     
     #map
     pal <- colorNumeric("Purples", domain = df_color$percent_vote)
     
     #making popups on the map (paste0 can get both variables and words)
     df4 <- df_merged %>% 
       mutate(pop1 = paste0(df_merged$percent_vote,"% at ", df_merged$nom),
              pop2 = paste0(df_merged$nom," population: ", df_merged$pop, " (x1000)"))
     popup_sb <- df4$pop1
     
     
     leaflet() %>%
       addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
       addCircleMarkers(data = df_merged, lng = ~longitude, 
                        lat = ~latitude, ~ pop/70, color = "red" , popup = ~df4$pop2,
                        stroke = F, group = "Population") %>% #group is for putting these in different layers
       addCircleMarkers(data = df_merged, lng = ~longitude, lat = ~latitude,
                        ~ SNHM14, fillOpacity = 0.7 ,color = "orange", 
                        popup = ~paste0("Average Salary: " ,round(SNHM14, 1), " €/hr"),
                        stroke = F, group = "Mean Wage") %>%
       addPolygons(data = df_merged, fillColor = ~pal(df_merged$percent_vote), layerId= ~nom,
                   fillOpacity = 0.8,
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
       addLegend(pal = pal, values = df_merged$percent_vote, title = "Vote %", opacity = 0.7,
                 labFormat = labelFormat(suffix = " %")) %>%
       addLayersControl(
         overlayGroups = c("Population","Mean Wage", "Votes"),
         options = layersControlOptions(collapsed = T)
       ) %>% hideGroup(c("Population","Mean Wage"))
  
  
   })  #end of reactive part 1
 
   
#starting reactive part 2  
   
   #bar chart for dep 1
   output$view2 <- renderPlotly({
     
     #location <- ifelse(is.null(input$view_marker_click$lng),
     #                  input$view_shape_click$id, 
     #                  as.character(coords$nom[which(coords$longitude == input$view_marker_click$lng)])
     #                  )
     #
     location <- input$view_shape_click$id
     
     
     
   plot1 <- df3 %>% filter(DEP_NAME == location) %>% 
     mutate(percent_vote =round(percent_vote * 100, 1)) %>% 
     ggplot(aes(x = reorder(LastName, -percent_vote), y = percent_vote, fill=LastName, 
                text = percent_vote)) + 
     geom_bar(stat = "identity") + theme_minimal() +
     ggtitle(paste0("Candidate Preference in ", location) )+
     theme(axis.text.x = element_text(size = 7, face = "bold", angle = 45, margin = margin(t = 6)),
           legend.position = "none") +
     labs(x="Candidate", y="Vote %")
   
   ggplotly(plot1, tooltip = "text")
   
   }) %>% bindEvent(input$view_shape_click)
   
   
}#end of server

# Run the application
shinyApp(ui = ui, server = server)
