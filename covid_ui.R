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
  h3("Purpose"),
  HTML("<p> This app is intended to allow useful comparison and monitoring of small counties during the COVID outbreak. 
    Small counties are challenging to visualize becuase they are influenced by noise and uncertainty inherit in small counts. This uncertainty may mask underlying trends.</p>"),
  h3("What's the problem with small counties?"),
  HTML('<p>When places are small, it is hard to estimate the rate of disease. Suppose a place has 5,000 people. 
    There may be 5 days in a row with no reported case (rate = 0 cases per 10,000), and then one suddenly one day with a reported case (rate = 2 cases per 10,000).
    Is the rate 0 in 10,000 or 2 in 10,000.  Or something in between? It is probaby something in between. 
    Moreover, what happens in one rural county is likely to be similar to what happens in other rural counties.
    We use models that "borrow information" across counties in a state to provide more stable, and hopefully more accurate, esimates.</p>'),
  h3("Data"),
  p("The map provides the following types of Covid-19 rate estimates:"),
  HTML("<p>The <strong>modeled</strong> estimate uses our model that attempt to address the small county problem. In instances, you may find counties where the model does not appear to fit the data. This is most likeky because the county is very different than the rest of the state, and the model is choosing to follow state-level trends.</p>"),
  HTML('<p>The <strong>raw</strong> estimate are derived directly from the daily counts of cases as reported by the New York Times at a("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv"). The counts have been modified sligthly by scientists at Oak Ridge National Laboratory when reports do not clealy follow county boundaries.</p>'),
  HTML('<p>The <strong>smoothed</strong> estimate are created by averaging the daily counts over the previous 7 days.'),
  h3("An explanation of uncertainty in the modeled estimates"),
  HTML('<p>The single county time series chart shows the model as well as confidence intervals. A good analogy for the estimate and confidence intervals are estimates of hurricanes.  During a hurricane, the path of the eye of the storm is predicted.  A "cone of uncertainty" shows our uncertainty about the path that the eye of the storm will follow.  The huuricane, however is much bigger than the eye of the storm.  Similarly, the actualy number of cases may be quite larger than the cone of uncertainty in the time series chart.')
  )
  )
)
