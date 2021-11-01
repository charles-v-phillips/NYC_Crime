library(shiny)
library(dplyr)
library(shinydashboard)
library(plotly)
library(rjson)
library(shinyWidgets)
library(tidyverse)
library(ggalluvial)
library(RColorBrewer)

# nyc <- read.csv('../data/clean_v6.csv')
source('../ui_components/boro_tab.R')

precincts <- fromJSON(file='../geo_jsons/precincts-2.json' )
staten_island_precincts <- fromJSON(file = '../geo_jsons/staten_island_precincts.json')
queens_precincts <- fromJSON(file = '../geo_jsons/queens_precincts.json')
manhattan_precincts <- fromJSON(file = '../geo_jsons/manhattan_precincts.json')
bronx_precincts <- fromJSON(file = '../geo_jsons/bronx_precincts.json')
brooklyn_precincts <- fromJSON(file = '../geo_jsons/brooklyn_precincts.json')





