# case_when(input$boro == "MANHATTAN" ~ manhattan_precincts,
#           input$boro == "STATEN ISLAND" ~ staten_island_precincts,
#           input$boro == "QUEENS", ~ queens_precincts,
#           input$boro == "BROOKLYN" ~ brooklyn_precint,
#           input$boro == "BRONX" ~ bronx_precincts
# )
function(input, output){
  output$p <- renderPlotly({
    if(input$boro == "MANHATTAN") precinct = manhattan_precincts
    if(input$boro == "STATEN ISLAND") precinct =  staten_island_precincts
    if(input$boro == "QUEENS") precinct = queens_precincts
    if(input$boro == "BRONX") precinct = bronx_precincts
    if(input$boro == "BROOKLYN") precinct = brooklyn_precincts
    if(input$boro == "ALL") precinct = precincts
    
    if(input$boro != "ALL") q = nyc%>%filter(input$boro == BORO_NM) %>% group_by(ADDR_PCT_CD) %>% summarise(n = n())
    if(input$boro == "ALL") q = nyc %>% group_by(ADDR_PCT_CD) %>% summarise(n = n())
    plot_ly(q) %>% add_trace(
      type = "choropleth",
      geojson = precinct,
      locations = unique(q$ADDR_PCT_CD),
      z = ~n,
      colors = "Purples",
      featureidkey="properties.Precinct",
      marker=list(line=list(
        width=0)
      )
    ) %>% layout(geo = list(
      fitbounds = "locations",
      visible = FALSE
    ), margin = list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 4
    ))
    
  })
}