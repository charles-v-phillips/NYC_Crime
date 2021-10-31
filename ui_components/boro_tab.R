side_bar  <- dashboardSidebar(
  radioButtons("radio", label = h3("Radio buttons"),
               choices = list("Raw" = T, "Density" = F), 
               selected = T),
  
                           selectizeInput(inputId='boro',
                                             label='Borough',
                                          choices=append(unique(nyc$BORO_NM),"ALL")),
                           pickerInput(inputId = 'crime', 
                                          label = 'Crime',
                                          choices = unique(nyc$OFNS_DESC),
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
                                             ),
    tabPanel("Gender",
             fluidRow(plotOutput("suspectGenderPieCharts")),
             fluidRow( plotOutput("victimGenderPieCharts"))),
    
    tabPanel("Age",
             fluidRow(plotOutput("suspectAgePieCharts")),
             fluidRow(plotOutput("victimAgePieCharts"))),
    tabPanel("Race",
             fluidRow(plotOutput("suspectRacePieCharts")),
             fluidRow(plotOutput("victimRacePieCharts"))),
    tabPanel("Something a little spicy",
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
             column(1,checkboxInput("allYears", label = "ALl Years",value = F))),
             
             fluidRow(plotOutput("AlluvialPlot")))
          )
        )



  
