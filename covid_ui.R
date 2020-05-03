require(leaflet)
require(shinyWidgets)

ui <- fluidPage(
  
  # Application title
  titlePanel(h1("COVID-19 Cases",
                style='background-color:#517c96;
                       color: white;
                       padding-left: 15px;'),
              windowTitle = 'COVID-19 Cases'),
  tabsetPanel(
  tabPanel("Explore", fluid=TRUE,
  fluidRow(
    column(3,style = "background-color:#e0e0e0",
           "sidebar",
           wellPanel(
             selectInput('layer', 'Map Layer', c('Modeled Count', 'Raw Count', 'Smoothed Count' )),
             shinyWidgets::setSliderColor(c("#517c96"), c(1)),
             sliderInput("DateSelect",
                         "Date:",
                         min = as.Date("2020-03-01","%Y-%m-%d"),
                         max = last_date,
                         value=last_date,timeFormat="%Y-%m-%d"),
             selectInput('state','State',state_names),
             selectInput('county','County',county_names),
             #selectInput('county','County',c('1','2')),
#             uiOutput("countyUI"),
             selectInput('y_scale','Scale Y-Axis',c('Continuous','Log 10')),
             selectInput('plot_type','Time Series Chart', tsPlot_type_label)
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
             column(12,
                    "These data are New Cases reported per day. Most cases of Covid-19 may go unreported. This is provided in the hope that the number of reported cases may be a useful indicator of disease severity.")
           ),
           fluidRow(
             column(6,
                    mainPanel(
                      #svgPanZoomOutput(outputId="tsPlot",
                      plotOutput("tsPlot", 
                                 click = "tsClick", 
                                 hover = hoverOpts(id = "tsHover", delayType = "throttle"),
                                 height='200px', width='100%')
                    )),
             column(6,
                    "Growth Rate over previous week by Number of Cases",
                    tableOutput("table"))
           ),
    #verbatimTextOutput("info")
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
