side_bar  <- dashboardSidebar(selectizeInput(inputId='boro',label='Borough',
                                          choices=append(unique(nyc$BORO_NM),"ALL")),
                           selectizeInput(inputId = 'crime', label = 'Crime',
                                          choices = unique(nyc$OFNS_DESC)),
                           selectizeInput(inputId = "year", label = "Year",
                                           choices = 2006:2020))


dashboard_body <- dashboardBody(tabsetPanel(tabPanel("Component 2",
                                         plotlyOutput("p")
                                       )))


  
