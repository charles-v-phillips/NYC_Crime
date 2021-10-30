side_bar  <- dashboardSidebar(
  radioButtons("radio", label = h3("Radio buttons"),
               choices = list("Raw" = T, "Density" = F), 
               selected = T),
  
                           selectizeInput(inputId='boro',
                                             label='Borough',
                                          choices=append(unique(nyc$BORO_NM),"ALL")),
                           pickerInput(inputId = 'crime', 
                                          label = 'Crime',
                                          choices = unique(most_prevelant_crimes),
                                          multiple = T
                                       ),
                                          
                           sliderInput(inputId = "yearRange", 
                                       label = "Year",
                                       min = 2006, max = 2020,
                                       value = c(2017,2019),
                                       step = 1,
                                       animate = animationOptions(interval = 3000, loop = FALSE, playButton = NULL,
                                                                   pauseButton = FALSE) ))



dashboard_body <- dashboardBody(
  tabsetPanel(
    tabPanel(
      "Component 2",plotlyOutput("p"),
            fluidRow(column(12,plotOutput("g2")))
                                             )
          )
        )


  
