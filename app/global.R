library(shiny)
library(dplyr)
library(shinydashboard)
library(plotly)
library(rjson)
library(shinyWidgets)
library(tidyverse)
source('../ui_components/boro_tab.R')

precincts <- fromJSON(file='../geo_jsons/precincts-2.json' )
staten_island_precincts <- fromJSON(file = '../geo_jsons/staten_island_precincts.json')
queens_precincts <- fromJSON(file = '../geo_jsons/queens_precincts.json')
manhattan_precincts <- fromJSON(file = '../geo_jsons/manhattan_precincts.json')
bronx_precincts <- fromJSON(file = '../geo_jsons/bronx_precincts.json')
brooklyn_precincts <- fromJSON(file = '../geo_jsons/brooklyn_precincts.json')

# queens <- read.csv("../data/queens.csv")
# nyc <- read.csv("../data/nypd_complaints.csv")


# most_prevelant_crimes = (nyc %>% group_by(OFNS_DESC) %>% summarise(n = n()) %>% arrange(-n))$OFNS_DESC
