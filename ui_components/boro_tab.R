#SideBar 
side_bar  <- dashboardSidebar(
  radioButtons("radio", label = h3("Data Mode"),
               choices = list("Frequency" = T, "Per Capita" = F), 
               selected = T),
  
                           selectizeInput(inputId='boro',
                                             label='Borough',
                                          choices=append(unique(nyc$BORO_NM),"ALL")),
                           pickerInput(inputId = 'crime', 
                                          label = 'Crime',
                                          choices = unique((nyc %>% group_by(OFNS_DESC) %>% summarise(n = n()) %>%arrange(-n))$OFNS_DESC),
                                          multiple = T
                                       ),
                                          
                           sliderInput(inputId = "yearRange", 
                                       label = "Year",
                                       min = 2006, max = 2020,
                                       value = c(2017,2019),
                                       step = 1,
                                       animate = animationOptions(interval = 3000, loop = FALSE, playButton = NULL,
                                                                   pauseButton = FALSE) ))



#
dashboard_body <- dashboardBody(
  tabsetPanel(
    
    #Here we will display the chloropleth map and also some histograms showing crime rates for selected crimes
    tabPanel(
      "Map",fluidRow(column(6,plotlyOutput("p")),column(6,plotOutput("precinctOrder"))),
            fluidRow(plotOutput("g2"))),
    
    #Analyze the Gender Breakdown of victims and suspects for selected crimes
    tabPanel("Gender Breakdown",
             fluidRow(plotOutput("suspectGenderPieCharts")),
             fluidRow( plotOutput("victimGenderPieCharts"))),
    
    #Analyze the Age Breakdown of victims and suspects for selected crimes
    tabPanel("Age Breakdown",
             fluidRow(plotOutput("suspectAgePieCharts")),
             fluidRow(plotOutput("victimAgePieCharts"))),
    
    #Analyze the Race Breakdown for certain crimes
    tabPanel("Race Breakdown",
             fluidRow(plotOutput("suspectRacePieCharts")),
             fluidRow(plotOutput("victimRacePieCharts"))),
    
    #Here we can see what race is committing what crimes
    tabPanel("Alluvial Plot",
             fluidRow(column(2,
               pickerInput(
                inputId = "suspect",
                label = "Suspect Race",
                choices = unique(nyc$SUSP_RACE),
                multiple = T
             )),
                column(2,pickerInput(
                  inputId = "crimesToLookAt",
                  label = "Crimes",
                  choices = unique((nyc %>% group_by(OFNS_DESC) %>% summarise(n = n()) %>%arrange(-n))$OFNS_DESC),
                  multiple = T
                )),
                column(2,pickerInput("victim",
                                     label = "Victim Race",
                                     choices = unique(nyc$SUSP_RACE),
                                     multiple = T)),
              column(2,sliderInput(inputId = "years", 
                                   label = "Year",
                                   min = 2006, max = 2020,
                                   value = c(2017,2019),
                                   step = 1)),
             column(1,checkboxInput("log", label = "Log", value = F)),
             column(1,checkboxInput("removeYearCol", label = "Years",value = F)),
             column(2,actionButton("go", "Plot"))),
             
             fluidRow(plotOutput("AlluvialPlot")))
          )
        )



  
