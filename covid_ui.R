require(leaflet)
require(shinyWidgets)

ui <- fluidPage(
  
  # Application title
  titlePanel(h1("COVID-19 NOWcast",
                style='background-color:#517c96;
                       color: white;
                       padding-left: 15px;'),
              windowTitle = 'COVID-19 Nowcast'),
  tabsetPanel(
  tabPanel("Explore", fluid=TRUE,
  fluidRow(
    column(3,style = "background-color:#e0e0e0",
           wellPanel(
             selectInput('layer', 'Map Layer', c('Trend Line', 'Raw Count', 'Smoothed Count', 'Growth Rate' )),
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
               width=12
             )),
           fluidRow(
             column(12,
                    wellPanel(
                    "The Nowcast is an estimate of the new cases of COVID-19 that are reported each day. Most cases of Covid-19 may go unreported. This is provided in the hope that changes in the number of reported cases may be a useful indicator of changes in disease severity. Owing to differences in how counties and states report cases, the estimates are more valid as time series trends than as comparisons of rates between counties."
                    ))
           ),
           fluidRow(
             column(12, 
               mainPanel(
                        plotlyOutput("tsPlot2", 
                        height='400px', width='100%')
           ))
           ),
#           fluidRow(
#             column(6,
#                    mainPanel(
#                      #svgPanZoomOutput(outputId="tsPlot",
#                      plotOutput("tsPlot", 
#                                 click = "tsClick", 
#                                 hover = hoverOpts(id = "tsHover", delayType = "throttle"),
#                                 height='200px', width='100%')
#                    )),
#             column(6,
#                    textOutput("tableText"),
#                    tableOutput("table"))
#           ),
    #verbatimTextOutput("info")
  ) )
  ),
  tabPanel("About",fluid=TRUE,
  h3("Purpose"),
  HTML("<p> This app is intended to allow useful comparison and monitoring of small counties during the COVID outbreak. 
    Small counties are challenging to visualize because they are influenced by noise and uncertainty inherit in small counts. This uncertainty may mask underlying trends.</p>"),
  h3("What's the problem with small counties?"),
  HTML('<p>When places are small, it is hard to estimate the rate of disease. Suppose a place has 5,000 people. 
    There may be 5 days in a row with no reported case (rate = 0 cases per 10,000), and then one suddenly one day with a reported case (rate = 2 cases per 10,000).
    Is the rate 0 in 10,000 or 2 in 10,000.  Or something in between? It is probaby something in between. 
    Moreover, what happens in one rural county is likely to be similar to what happens in other rural counties.
    We use models that "borrow information" across counties in a state to provide more stable, and hopefully more accurate, esimates.</p>'),
  h3("Data"),
  p("The map provides the following types of Covid-19 rate estimates:"),
  HTML("<p>The <strong>modeled</strong> estimate uses our model that attempt to address the small county problem. In instances, you may find counties where the model does not appear to fit the data. This is most likeky because the county is very different than the rest of the state, and the model is choosing to follow state-level trends.</p>"),
  HTML('<p>The <strong>raw</strong> estimate are derived directly from the daily counts of cases as collected and reported by the New York Times at a("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv"). Where available, the New York Times has attempted to collect data from local health districts, which may differ from the counts reported by state departments of health, and as reported by the Johns Hopkins dataset at a("https://github.com/CSSEGISandData/COVID-19").  The counts have been modified sligthly by scientists at Oak Ridge National Laboratory when reports do not clealy follow county boundaries.</p>'),
  HTML('<p>The <strong>smoothed</strong> estimate are created by averaging the daily counts over the previous 7 days.'),
  h3("The counts here are different that what I've seen from my state?"),
  HTML('<p>The source data used here for modeling the trend are collected by the New York Times from county health departments whereever possible.  When a patient in a county receives a test, the results are sent by the lab to the county department of health. When the county receives the lab report, this number is then passed on the State Department of Health, which will investigate the case further and verify the address of the patient. This verification can lead to a lag in reporting at the state level. Furthermore, most states will remove the cases of out-of-state patients, even if the patient contracted the disease locally. The purpose of state recording and verification process is to create an authoritative, historical record, not to create a current indicator of daily trends. Most counties will make public health decision on their own counts, not on the state counts. For the purpose of this nowcasting application, county-derived reports may be considered to the most timely, accurate, and suitable for characterizing the present situation</p>'),
  h3("An explanation of uncertainty in the modeled estimates"),
  HTML('<p>The single county time series chart shows the model as well as confidence intervals. A good analogy for the estimate and confidence intervals are estimates of hurricanes.  During a hurricane, the path of the eye of the storm is predicted.  A "cone of uncertainty" shows our uncertainty about the path that the eye of the storm will follow.  The huuricane, however is much bigger than the eye of the storm.  Similarly, the actualy number of cases may be quite larger than the cone of uncertainty in the time series chart.'),
  h3("About the authors"),
  HTML('<p>The app and modeled data were created by Nicholas Nagle and Jesse Piburn. Nicholas Nagle is an Associate Professor in the Department of Geography at University of Tennessee with research expertise in Geographic Data Science and Demography.  Jesse Piburn is a Research Scientist in Geographic Data Sciences at Oak Ridge National Laboratory.</p>'),
  HTML('<p>For further information or comment, contact Dr. Nicholas Nagle at <a href = "mailto: nnagle@utk.edu">nnagle@utk.edu</a> ')
  )
  )
)
