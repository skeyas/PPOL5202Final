list.of.packages <- c("shiny","tidyverse","shinythemes","rstudioapi","stringr","ggplot2")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# load necessary packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(rstudioapi)
library(stringr)
library(tigris)
library(mapview)
library(leaflet)
library(sf)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

# load necessary data/do necessary data manipulations (if applicable)
#current_path = rstudioapi::getActiveDocumentContext()$path 
#setwd(dirname(current_path ))

options(tigris_class = "sf")

states <- tigris::states(cb=T) %>% mutate(State=NAME) %>%  st_as_sf()
states = st_transform(states, crs="+proj=longlat +datum=WGS84")

counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv', show_col_types = FALSE) %>% 
  filter(state_abbreviation=="NJ")
county_shapes = tigris::counties(cb=T) %>% mutate(county_name=NAMELSAD)
county_shapes = st_transform(county_shapes, crs="+proj=longlat +datum=WGS84")

drug_stats <- readr::read_csv("NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv", show_col_types = FALSE) %>% 
  separate_wider_delim(cols="County", delim=", ", names=c("county_name", "state_abbreviation")) %>% rename_all(~str_replace_all(., "\\s+|-|/", "_")) %>%
  mutate(death_count_raw=round(Model_based_Death_Rate*Population / 100000, digits=0), year=Year, death_rate=Model_based_Death_Rate / 100000)

drug_stats_state_level <- drug_stats %>% group_by(state_abbreviation, State, year) %>% 
  summarise(population = sum(Population), deaths=sum(death_count_raw)) %>%
  mutate(death_rate=deaths/population)

drug_stats_county_nj <- drug_stats %>% filter(state_abbreviation == "NJ")

nj_county_ods <- readr::read_csv("county-ods-16.csv", show_col_types = FALSE) %>% drop_na() %>% mutate(county_name=paste(COUNTY, "County"))

counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv', show_col_types = FALSE)

nj_county_ods <- nj_county_ods %>% left_join(counties, by="county_name") %>% 
  pivot_longer(cols = matches("\\d\\s.+"), names_to="year", values_to = "count") %>% 
  extract(year, c("year", "drug_type"), "(\\d+)\\s(.+)") %>% mutate(year=as.numeric(year), across("drug_type", str_replace, "\\*", "")) %>%
  filter(year > 2000, county_name != "Total County", state_name == "New Jersey") %>%
  mutate(across('county_name', str_replace, ' County', ''))


drug_stats_county_nj <- drug_stats_county_nj %>% mutate(Year=year) %>% left_join(nj_county_ods, by=c("year", "county_name"))

ui <- shinyUI(navbarPage("The Opioid Crisis in Visuals",
          tabPanel("Overview",
            fluidPage(
              tags$h3("According to the CDC, the number of people who died from a drug overdose 
                 in 2021 was over six times the number in 1999, and over 75% of 2021 drug
                 overdose deaths involved an opioid.", tags$a(
                   tags$sup("CDC"),
                   target = "_blank",
                   href = "https://www.cdc.gov/opioids/basics/epidemic.html"
                 )),
              p("This dashboard displays New Jersey's drug overdose 
                  statistics, contextualized by other states, using information
                from the CDC and NJ Advance Media."),
              p("Overall, drug overdose deaths have been on the rise in the United States."),
              fluidRow(
                column(12, sliderInput("DateField", "Select Date Range", 2003, 2021, c(2003, 2021), step=1, sep="")),
                fluidRow(
                  column(12, h3("Average Drug Overdose Death Rate Per 100, 000 People by U.S. State"), strong("Drug overdose deaths on the rise in U.S."), plotOutput("plotOne"))
                ),
                fluidRow(
                  column(12, h3("Drug Overdose Death Rate Per 100, 000 People by U.S. State, Averaged Over Years"), 
                         strong("West Virginia far ahead in average drug overdose death per year"), leafletOutput("plotTwo"))
                )
              )
            )
                   
          ),
          tabPanel("New Jersey High Level Statistics",
            fluidPage(
              fluidRow(
                column(6, sliderInput("DateField2", "Select Date Range", 2003, 2021, c(2003, 2021), step=1, sep="")),
                column(6, uiOutput("stateAbbreviations"))
              ),
              column(12, h3("Statewide Drug Overdose Death Rate in New Jersey Over Time"), 
                     strong("Drug overdoses on the rise"), 
                     plotOutput("plotFour")),
              fluidRow(
                column(12, h3("Comparative County Level Drug Overdose Death, Averaged Over Years"), leafletOutput("plotFive"))
              )
            )         
          ),
          tabPanel("Drug Level Breakdown",
            fluidPage(
              mainPanel(
                h3("A dataset put out by New Jersey Advance Media includes heroin overdose statistics since 2004 and fentanyl overdose statistics since 2012."),
                p("The average fentanyl death rate per county more than doubled between 2014 and 2015."),
                fluidRow(
                  column(12, checkboxGroupInput(inputId = "drugType",
                                                label = "Drug Type",
                                                choiceNames = list("Heroin", "Fentanyl", "Overall"),
                                                choiceValues = list("Heroin", "Fentanyl", "Total"),
                                                inline = T), align="center"),
                  column(12, h3("Average Drug Overdose Death Rate Per 100, 000 People by New Jersey County"), 
                         strong("Drug overdoses on the rise"), 
                         plotOutput("plotThree"))
                  
                )
              )
            )         
          )
))

# Define server logic
server <- function(input, output) {
  output$plotOne <- renderPlot({
    drug_stats_state_level %>% filter(year > input$DateField[1] & year < input$DateField[2]) %>% 
      group_by(year) %>% summarize(mean_death_rate=mean(death_rate*100000)) %>% 
      ggplot(aes(x = as.factor(year), y = mean_death_rate, group=1)) + geom_line() + geom_point() +
      labs(y = "Mean drug overdoses across states", x="Year", caption = "CDC") + 
      theme_minimal() + theme(text=element_text(family="Times"))
  })
  output$plotTwo <- renderLeaflet({
    df <- drug_stats_state_level %>% filter(year > input$DateField[1] & year < input$DateField[2]) %>%
      group_by(state_abbreviation, State) %>% summarize(mean_death_rate=mean(death_rate))
    df <- geo_join(spatial_data = states, df, by = "State") %>% drop_na()
    pal <- colorNumeric("Greens", domain=df$mean_death_rate)
    leaflet() %>%
      setView(-98.5795, 39.8282, zoom=4) %>%  # center of US 
      addPolygons(data = df,stroke = FALSE,
                  fillColor = ~pal(df$mean_death_rate), 
                  fillOpacity = 1, 
                  label = paste0(df$state_abbreviation, ": ", paste0(round(df$mean_death_rate*100000, digits=2), "/ 100, 000")))
  })
  
  output$plotThree <- renderPlot({
    
    nj_county_ods %>% filter(drug_type %in% input$drugType) %>% 
      group_by(year, drug_type) %>% summarize(mean_overdoses=mean(count)) %>% 
      ggplot(aes(x = as.factor(year), y = mean_overdoses, group=drug_type, colour=drug_type)) + geom_point() + geom_line() +
      labs(y = "Mean drug overdoses across counties", x="Year", caption = "NJ Advance Media", color="Drug type") + 
      theme_minimal() + theme(text=element_text(family="Times"))
  })
  output$plotFour <- renderPlot({
    if(is.null(input$StateField)) {
      tmp <- c("NJ")
    }
    else {
      tmp <- c("NJ", input$StateField)
    }
    drug_stats_state_level %>% filter(year > input$DateField2[1] & 
                                        year < input$DateField2[2] & state_abbreviation %in% tmp) %>% 
      group_by(year, state_abbreviation) %>% summarize(death_rate=mean(death_rate*100000)) %>% 
      ggplot(aes(x = as.factor(year), y = death_rate, group=state_abbreviation, colour=state_abbreviation)) + geom_point() + geom_line() +
      labs(y = "Drug overdose death rate per 100, 000 people", x="Year", caption = "CDC") + 
      theme_minimal() + theme(text=element_text(family="Times"))
  })
  
  output$plotFive <- renderLeaflet({
    df <- drug_stats %>% filter(year > input$DateField2[1] & year < input$DateField2[2] & state_abbreviation %in% c("NJ", input$StateField)) %>%
      group_by(state_abbreviation, State, county_name) %>% summarize(mean_death_rate=mean(death_rate*100000))
    county_shapes <- county_shapes %>% filter(STATE_NAME %in% c("New Jersey", state.name[match(input$StateField,state.abb)]))
    df <- geo_join(spatial_data = county_shapes, df, by = "county_name")
    pal <- colorNumeric("Greens", domain=df$mean_death_rate)
    leaflet() %>%
      setView(-74.871826, 39.833851, zoom=5) %>%
      addPolygons(data = df,stroke = FALSE,
                  fillColor = ~pal(df$mean_death_rate), 
                  fillOpacity = 1, 
                  label = paste0(df$county_name, ": ", paste0(round(df$mean_death_rate*100000, digits=2), "/ 100, 000")))
    
  })
  
  output$stateAbbreviations <- renderUI({
    selectInput(
      "StateField", 
      "Select State for Comparison",
      unique(drug_stats_state_level$state_abbreviation), 
      selected = NULL)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)