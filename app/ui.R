library(shinydashboard)
#Shiny Dashboard initialized with a header, sidebar, and dashboard body
#side_bar and dishboard_body can be found in boro_tab.R
dashboardPage(
  dashboardHeader(title = "Crime in NYC Precincts"),
  side_bar,
  dashboard_body
)



