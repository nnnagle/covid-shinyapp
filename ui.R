ui <- fluidPage(
  
  # Application title
  titlePanel("COVID Cases Dashboard"),
  
  fluidRow(
    column(3,
           "sidebar",
           wellPanel(
             sliderInput("DateSelect",
                         "Date:",
                         min = as.Date("2020-03-01","%Y-%m-%d"),
                         max = as.Date("2020-04-20","%Y-%m-%d"),
                         value=as.Date("2020-04-20"),timeFormat="%Y-%m-%d"),
             selectInput('state','State',state_names),
             uiOutput("countyUI"),
             selectInput('y_scale','Scale Y-Axis',c('Continuous','Log 10')),
             selectInput('plot_type','Time Series Chart', c('Compare','Fit'))
           )),
    column(9,
           fluidRow(
             mainPanel(
               plotOutput("usPlot"),
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
             column(6,verbatimTextOutput("info") )
           ),
  ) )
)