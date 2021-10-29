function(input, output){
  output$p <- renderPlotly({
    if(input$boro == "MANHATTAN") precinct = manhattan_precincts
    if(input$boro == "STATEN ISLAND") precinct =  staten_island_precincts
    if(input$boro == "QUEENS") precinct = queens_precincts
    if(input$boro == "BRONX") precinct = bronx_precincts
    if(input$boro == "BROOKLYN") precinct = brooklyn_precincts
    if(input$boro == "ALL") precinct = precincts
    
    
    raw = ifelse(input$radio == T,T,F)
    all_boro = input$boro == "ALL"
    
    if(raw & all_boro) q =  nyc %>% 
      filter(year %in% input$yearRange, OFNS_DESC %in% input$crime)%>% 
      group_by(ADDR_PCT_CD) %>% 
      summarise(n = n())
    if(raw & !all_boro) q = nyc %>% 
      filter(year %in% input$yearRange, BORO_NM == input$boro, OFNS_DESC %in% input$crime )%>% 
      group_by(ADDR_PCT_CD) %>% 
      summarise(n = n())
    
    if(!raw & all_boro) q = 
      nyc%>%
      filter(year %in% input$yearRange, OFNS_DESC %in%  input$crime ) %>%
      group_by(ADDR_PCT_CD,OFNS_DESC) %>%
      summarise(num_crime_occurences = n(), population = mean(pop)) %>%
      mutate(total = sum(num_crime_occurences))%>% 
      pivot_wider(names_from =OFNS_DESC, values_from = num_crime_occurences) %>%
      ungroup()%>%
      mutate(n = total/population)
    
    if(!raw & !all_boro) q =
    nyc%>%
      filter( BORO_NM == input$boro,year %in% input$yearRange, OFNS_DESC %in%  input$crime ) %>%
      group_by(ADDR_PCT_CD,OFNS_DESC) %>%
      summarise(num_crime_occurences = n(), population = mean(pop)) %>%
      mutate(total = sum(num_crime_occurences))%>% 
      pivot_wider(names_from =OFNS_DESC, values_from = num_crime_occurences) %>%
      ungroup()%>%
      mutate(n = total/population)
    
    
    
    
    
    
    
    
    
    
    
      
    # nyc %>% filter(year %in% input$yearRange) %>% group_by(ADDR_PCT_CD) %>% 
    
    # if(!all_boro) q = nyc%>%filter(input$boro == BORO_NM ) %>%  filter(year %in% input$yearRange) %>% filter(OFNS_DESC %in% input$crime) %>%group_by(ADDR_PCT_CD) %>% summarise(n = n())
    # if(all_boro) q = nyc %>% filter(year %in% input$yearRange)%>% group_by(ADDR_PCT_CD) %>% summarise(n = n())
    
    
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