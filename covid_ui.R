ui <- fluidPage(
  
  # Application title
  titlePanel(h1("COVID-19 Cases",
                style='background-color:#517c96;
                       color: white;
                       padding-left: 15px;')),
  tabsetPanel(
  tabPanel("Explore", fluid=TRUE,
  fluidRow(
    column(3,style = "background-color:#e0e0e0",
           "sidebar",
           wellPanel(
             shinyWidgets::setSliderColor(c("#517c96"), c(1)),
             sliderInput("DateSelect",
                         "Date:",
                         min = as.Date("2020-03-01","%Y-%m-%d"),
                         max = as.Date("2020-04-20","%Y-%m-%d"),
                         value=as.Date("2020-04-20"),timeFormat="%Y-%m-%d"),
             selectInput('state','State',state_names),
             selectInput('county','County',county_names),
             #selectInput('county','County',c('1','2')),
#             uiOutput("countyUI"),
             selectInput('y_scale','Scale Y-Axis',c('Continuous','Log 10')),
             selectInput('plot_type','Time Series Chart', c('Compare','Fit'))
           ),
           wellPanel(style = "background:#ff8200",
                     HTML('<center><img src="UTK_long_logo.png" width="120px"></center>')
           )
           ),
    column(9,style = "background-color:white;",
           fluidRow(
             mainPanel(
               leafletOutput("usPlot"),
#               plotOutput("usPlot"),
               width=11
             )),
           fluidRow(
             column(6,
                    mainPanel(
                      plotOutput("tsPlot", 
                                 click = "tsClick", 
                                 hover = hoverOpts(id = "tsHover", delayType = "throttle")),
                      width=11
                    )),
             column(6,
                    "Growth Rate over previous week by Number of Cases",
                    tableOutput("table"))
           ),
    verbatimTextOutput("info")
  ) )
  ),
  tabPanel("About",fluid=TRUE,
  p("This app is intended to allow useful comparison and monitoring of small counties during the COVID outbreak. Small counties are challenging to visualize becuase they are influenced by noise and uncertainty inherit in small counts. This uncertainty may mask underlying trends. This project uses Bayesian methods that borrow information across counties and dates to estimate smooth trends."),
  h3("Data"),
  p("The raw data data are derived from the New York Times Data ",
    a("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv"),
    " with some processing to impute county where necessary. The most significant such case is Kansas City, MO, which is entered separately from the counties that contain it."),
  h3("Model")
  )
  )
)