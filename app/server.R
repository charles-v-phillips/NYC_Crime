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
      filter(year %in% input$yearRange[1] : input$yearRange[2], OFNS_DESC %in% input$crime)%>% 
      group_by(ADDR_PCT_CD) %>% 
      summarise(n = n())
    if(raw & !all_boro) q = nyc %>% 
      filter(year %in% input$yearRange[1] : input$yearRange[2], BORO_NM == input$boro, OFNS_DESC %in% input$crime )%>% 
      group_by(ADDR_PCT_CD) %>% 
      summarise(n = n())
    
    if(!raw & all_boro) q = 
      nyc%>%
      filter(year %in% input$yearRange[1] : input$yearRange[2], OFNS_DESC %in%  input$crime ) %>%
      group_by(ADDR_PCT_CD,OFNS_DESC) %>%
      summarise(num_crime_occurences = n(), population = mean(pop)) %>%
      mutate(total = sum(num_crime_occurences))%>% 
      pivot_wider(names_from =OFNS_DESC, values_from = num_crime_occurences) %>%
      ungroup()%>%
      mutate(n = total/population)
    
    if(!raw & !all_boro) q =
      nyc%>%
        filter( BORO_NM == input$boro,year %in% input$yearRange[1] : input$yearRange[2], OFNS_DESC %in%  input$crime ) %>%
        group_by(ADDR_PCT_CD,OFNS_DESC) %>%
        summarise(num_crime_occurences = n(), population = mean(pop)) %>%
        mutate(total = mean(num_crime_occurences))%>%
        pivot_wider(names_from =OFNS_DESC, values_from = num_crime_occurences) %>%
        ungroup()%>%
        mutate(n = total/population)
      
      
      # nyc%>%
      # filter( BORO_NM == "MANHATTAN",year %in% 2012 : 2012, OFNS_DESC %in%  c("ARSON", "RAPE") ) %>%
      # group_by(ADDR_PCT_CD,OFNS_DESC) %>%
      # summarise(num_crime_occurences = n(), population = mean(pop))%>%
      # mutate(total = mean(num_crime_occurences))%>% 
      # pivot_wider(names_from =OFNS_DESC, values_from = num_crime_occurences) %>%
      # ungroup()%>%
      # mutate(n = total/population)
    
    

    
    
    
    
    
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
  
  output$g2 <- renderPlot({
    if(!(input$boro == "ALL")) pl = 
      nyc %>% filter(BORO_NM == input$boro, year %in% input$yearRange[1] : input$yearRange[2], OFNS_DESC %in% input$crime) %>% 
      group_by(year, OFNS_DESC) %>% summarise(n = n()) %>% ungroup() %>%
      ggplot() + geom_bar(aes(x = year,y = n, fill = OFNS_DESC),stat = 'identity',position = 'dodge')
    
    if(input$boro == "ALL") pl = 
      nyc %>% filter(year %in% input$yearRange[1] : input$yearRange[2], OFNS_DESC %in% input$crime) %>% 
      group_by(year, OFNS_DESC) %>% summarise(n = n()) %>% ungroup() %>%
      ggplot() + geom_bar(aes(x = year,y = n, fill = OFNS_DESC),stat = 'identity',position = 'dodge')
    
    pl
    
    
  })
  
  output$suspectGenderPieCharts <- renderPlot({
    
    nyc %>% filter(year == input$yearRange[2] , OFNS_DESC %in% input$crime, SUSP_SEX %in% c("M","F")) %>%
      group_by(year,OFNS_DESC,SUSP_SEX) %>% 
      summarise(n = n()) %>% mutate(total = sum(n), ratio = n/total) %>% ungroup()%>%
      ggplot(aes(x = "", y = ratio, fill = SUSP_SEX)) +geom_col(color = "black") + 
      geom_text(aes(label = round(ratio*100,2)),position = position_stack(vjust = 0.5)) + coord_polar(theta = "y") + 
      facet_grid(~OFNS_DESC)
  })
  
  output$victimGenderPieCharts <- renderPlot({
    nyc %>% filter(year == input$yearRange[2] , OFNS_DESC %in% input$crime, VIC_SEX %in% c("M","F")) %>%
      group_by(year,OFNS_DESC,VIC_SEX) %>% 
      summarise(n = n()) %>% mutate(total = sum(n), ratio = n/total) %>% ungroup()%>%
      ggplot(aes(x = "", y = ratio, fill = VIC_SEX)) +geom_col(color = "black") + 
      geom_text(aes(label = round(ratio*100,2)),position = position_stack(vjust = 0.5)) + coord_polar(theta = "y") + 
      facet_grid(~OFNS_DESC)
    
  })
  
  output$suspectAgePieCharts <- renderPlot({
    
    nyc %>% 
      filter(year == input$yearRange[2], OFNS_DESC %in% input$crime, SUSP_AGE_GROUP %in% c("25-44", "18-24","45-64","<18","65+")) %>%   group_by(OFNS_DESC,SUSP_AGE_GROUP) %>%
      summarise(n = n()) %>%
      mutate(total = sum(n), ratio = n/total) %>%
      ggplot(aes(x = "", y = ratio, fill = SUSP_AGE_GROUP)) +geom_col(color = "black") +
      coord_polar(theta = "y") +
      facet_grid(~OFNS_DESC)
    
  })
  output$victimAgePieCharts <- renderPlot({
    nyc %>% 
      filter(year == input$yearRange[2], OFNS_DESC %in% input$crime, VIC_AGE_GROUP %in% c("25-44", "18-24","45-64","<18","65+")) %>%   group_by(OFNS_DESC,VIC_AGE_GROUP) %>%
      summarise(n = n()) %>%
      mutate(total = sum(n), ratio = n/total) %>%
      ggplot(aes(x = "", y = ratio, fill = VIC_AGE_GROUP)) +geom_col(color = "black") +
      coord_polar(theta = "y") +
      facet_grid(~OFNS_DESC)
    
  })
  
  output$suspectRacePieCharts <- renderPlot({
    nyc %>%
      filter(year == input$yearRange[2], OFNS_DESC %in% input$crime, SUSP_RACE %in% c("BLACK", "BLACK HISPANIC", "WHITE HISPANIC", "WHITE", "ASIAN / PACIFIC ISLANDER", "AMERICAN INDIAN/ALASKAN NATIVE" , "AMERICAN INDIAN/ALASKAN NATIVE", "OTHER")) %>% 
      group_by(OFNS_DESC, SUSP_RACE) %>%
      summarise(n = n()) %>%
      mutate(total = sum(n), ratio = n/total) %>%
      ggplot(aes(x = "", y = ratio, fill = SUSP_RACE)) +geom_col(color = "black") +
      coord_polar(theta = "y") +
      facet_grid(~OFNS_DESC)
  })
  
  output$victimRacePieCharts <- renderPlot({
    nyc %>%
        filter(year == input$yearRange[2], OFNS_DESC %in% input$crime, VIC_RACE %in% c("BLACK", "BLACK HISPANIC", "WHITE HISPANIC", "WHITE", "ASIAN / PACIFIC ISLANDER", "AMERICAN INDIAN/ALASKAN NATIVE" , "AMERICAN INDIAN/ALASKAN NATIVE", "OTHER")) %>% 
        group_by(OFNS_DESC, VIC_RACE) %>%
        summarise(n = n()) %>%
        mutate(total = sum(n), ratio = n/total) %>%
        ggplot(aes(x = "", y = ratio, fill = VIC_RACE)) +geom_col(color = "black") +
        coord_polar(theta = "y") +
        facet_grid(~OFNS_DESC)
  })
  
  output$crimeByRaceAlluvial <- renderPlot({
    
    wh = nyc %>% filter(OFNS_DESC %in% input$crimesToLookAt, SUSP_RACE %in% input$suspect, VIC_RACE %in% input$suspect) %>%
      group_by(SUSP_RACE,VIC_RACE,OFNS_DESC) %>% summarise(n = n())
    
    ggplot(data = wh,
           aes(axis1 = SUSP_RACE, axis2 = VIC_RACE,y = n)) + 
      scale_x_discrete(limits = c("Suspect Race", "Victim Race")) + 
      geom_alluvium(aes(fill = OFNS_DESC)) + geom_stratum() + 
      geom_text(stat = "stratum", aes(label = after_stat(stratum))) + 
      theme_minimal()
  })
  
  
  
  
  
}