# Define server logic required to draw a histogram
server <- function(input, output) {
  library(tidyverse)
  library(ggplot2)
  library(sf)
  
  load('results.RData')
  rm(geodf)
  load('counties_sf.RData')
  geodf <- counties_sf
  out_df <- out_df %>%
    mutate(rate = lambda_q50*10000) %>%
    mutate(rate_c = cut(rate, 
                        breaks=c(-Inf, .1, .3, 1, 3, 10, Inf),
                        labels = c('< .1', '.1-.3', '.3 - 1', '1-3', '3-10', '>10' )))
  
  slope_df <- out_df %>%
    group_by(state_name, county_name) %>%
    arrange(date) %>%
    mutate(growth = (lambda_q50-lag(lambda_q50,7))/lag(lambda_q50,7)) %>%
    mutate(growth_c = cut(growth,
                          breaks=c(-Inf,-.5, -.1,.1,.5,1,Inf),
                          labels=c('More than Halved', "-50% to -10%", "-10% - 10%", "10-50", "50% - 100%", "More than Doubled")))
  
  
  ##############################################
  # Create a state_boundary layer for reference
  state_geo <- geodf %>%
    left_join(out_df %>% filter(date==date[[1]]) %>% select(geoid, state_fips, state_name)) %>%
    group_by(state_fips, state_name) %>%
    summarize()
  
  state_names <- out_df %>% select(state_name) %>% unique() %>% arrange(state_name) %>% pull(state_name)
  state_list <- as.list(1:length(state_names))
  names(state_list) <- state_names
  
  #Dynamic County Selector
  output$countyUI <- renderUI({
    county_names <- out_df %>%
      select(state_name, county_name) %>%
      filter(state_name == input$state) %>%
      unique() %>%
      arrange(county_name) %>%
      pull(county_name)
    selectInput('county',
                label='County',
                choices=county_names,
                multiple = FALSE)
  })
  
  output$usPlot <- renderPlot({
    ggobj <- ggplot(data = geodf %>%
                      left_join(out_df %>%
                                  filter(date == input$DateSelect)),
           aes = aes()) +
      geom_sf(aes(fill=rate_c), color=NA) + 
      scale_fill_brewer('Rate', palette = 'YlOrRd', na.value='grey80') +
      #scale_fill_manual('Rate', values = scales::brewer_pal(palette='YlOrRd')(6), na.value='grey') +
      geom_sf(data=state_geo, fill=NA, size=.2,inherit.aes = TRUE) +
      theme( 
        panel.background = element_rect(fill = "#ffffff", colour = "#ffffff",
                                        size = 2, linetype = "solid"),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank())
   ggobj 
    
  })
  
  tsPlotData <- reactive({
    data = out_df %>%
      filter(state_name== input$state)})
  
  tsHoverData <- reactive({
    data = nearPoints(tsPlotData(), input$tsHover, maxpoints = 1)
    if(is.null(nrow(data))) return(NULL) else return(data)
  })
  
  tsHighlightData <- reactive({
    data = tsPlotData() %>%
      filter(county_name %in% input$county)
  })
  
  output$tsPlot <- renderPlot({
    if(input$plot_type=='Compare'){
      plt <- ggplot(data = tsPlotData(),
                    mapping = aes(x=date, y=rate,group=county_name))+
        geom_line(aes(group=county_name), alpha=.2) + 
        geom_vline(xintercept=input$DateSelect)
      if(nrow(tsHighlightData()>0)){
        plt <- plt + geom_line(data=tsHighlightData(), 
                               aes(group='county_name'))
      }
      if(input$y_scale == 'Log 10'){ plt <- plt + scale_y_log10()}
    } else{
      plt <- ggplot(data=tsHighlightData(),
                    mapping = aes(x=date)) +
        geom_line(mapping=aes(y=fudge*lambda_q50*acs_total_pop_e)) +
        geom_ribbon(mapping=aes(ymax=fudge*lambda_q85*acs_total_pop_e,
                                ymin=fudge*lambda_q15*acs_total_pop_e),
                    alpha=.25, color=NA) +
        geom_point(mapping = aes(y=new_cases_mdl+.01), color='red')+
        labs(y='New Cases')
      if(input$y_scale == 'Log 10'){ plt <- plt + scale_y_log10()}
    }
    plt 
  },
  height = 200)
  
  output$info <- renderPrint({
    df <- slope_df %>%
      filter(date== input$DateSelect)
    table(`growth in last week`=df$growth_c,`rate (per 10,000 persons)`=df$rate_c)
    
   # tsHighlightData()
    #print(input$mapClick)
    #nearPoints(tsPlotData(), input$tsClick, maxpoints = 1)
  })
}