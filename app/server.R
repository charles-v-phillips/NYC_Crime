function(input, output){
  output$p <- renderPlotly(
    plot_ly(nyc) %>% add_trace(
      type = "choropleth",
      geojson = bronx_precincts,
      locations = unique(nyc$ADDR_PCT_CD),
      z = ~4,
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
  )
}