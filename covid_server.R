
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  library(tidyverse)
  library(ggplot2)
  library(leaflet)
  library(sf)
  library(shinyWidgets)
  library(lubridate)
  library(plotly)
  #browser()
  staticData <- geodf %>% left_join(out_df %>%
                                      filter(date == date[1]))
  
  data <- reactiveValues(state='Tennessee',
                         county='Anderson',
                         county_list = out_df %>%
                                       filter(state_name == 'Tennessee') %>%
                                       select(county_name) %>% 
                                       unique() %>%
                                       arrange(county_name) %>%
                                       pull(county_name) 
                           )
  
  tsPlotData <- reactive({
    data = out_df %>%
      filter(state_name== data$state)})
  
  tsHighlightData <- reactive({
    data = tsPlotData() %>%
      filter(county_name %in% data$county) %>%
      arrange(date)
  })
  
  ###################################3
  # Update map data for date and input_layer
  mapData <- reactive({
    df <- geodf %>%
      left_join(out_df %>%
                  filter(date == input$DateSelect))
    df <- switch(input$layer,
                 "Modeled Count" = {
                   mutate(df, 
                          fill_layer=rate_c, 
                          label_layer=paste0("<strong>", state_name, "</strong><br>", county_name, " <br><strong>rate</strong>: ", round(rate,3)))
                 },
                 "Raw Count" = {
                   mutate(df, 
                          fill_layer=count_c, 
                          label_layer=paste0("<strong>", state_name, "</strong><br>", county_name, " <br><strong>count</strong>: ", round(count_n,3))) 
                 },
                 "Smoothed Count" = {
                   mutate(df, 
                          fill_layer=smooth_c, 
                          label_layer=paste0("<strong>", state_name, "</strong><br>", county_name, " <br><strong>moving average</strong>: ", round(smooth_n,3))) 
                 },
                 "Growth Rate" = {
                   mutate(df,
                          fill_layer = growth_c,
                          label_layer=paste0("<strong>", state_name, "</strong><br>", county_name, " <br><strong>growth rate</strong>: ", round(100*(growth),1)))
                 })
    return(df)
  })
  
#  observeEvent(data$state, {
#    # Update County names
#    county_names <- out_df %>%
#      filter(state_name == data$state) %>%
#      select(county_name) %>% 
#      unique() %>%
#      arrange(county_name) %>%
#      pull(county_name) 
#    data$county_list <- county_names
#    data$county = county_names[1]
#  })
  
  observeEvent(data$county, {
  })
  
  observeEvent(input$state, {
    data$state = input$state
  })
  
  observeEvent(input$county, {
    data$county = input$county
  })
  
  observeEvent(event_data('plotly_click'), {
    d <- event_data('plotly_click')
    updateSelectInput(session,
                      inputId='county', 
                      #choices = data$county_list,
                      selected=d$key[1])
  })
  
  # Change county when state is changed
  # observeEvent(input$state,{
  #              data$state <- input$state
  #              # Update County names
  #              county_names <- out_df %>%
  #                filter(state_name == input$state) %>%
  #                unique() %>%
  #                arrange(county_name) %>%
  #                pull(county_name)
  #              # Check if mouseclick is same as state (don't update if so)
  #              x <- input$usPlot_shape_click
  #              if(is.null(x)){
  #                updateSelectInput(session,
  #                                  inputId='county', 
  #                                  choices = county_names )
  #              } else{
  #                pt <- st_as_sf(
  #                  tibble(LONG = x$lng,
  #                         LAT = x$lat),
  #                  coords = c("LONG", "LAT"),
  #                  crs = 4326)
  #                selection <- suppressMessages(st_join(pt, geodf))
  #                if(selection$state_name[1]!= input$state){
  #                  updateSelectInput(session,
  #                                    inputId='county', 
  #                                    choices = county_names )
  #                }
  #              }
  #              })
  # 
  # Change state and county on map click
  observeEvent(input$usPlot_shape_click, {
    x <- input$usPlot_shape_click
    if(!is.null(x)){
       pt <- st_as_sf(
         tibble(LONG = x$lng,
                LAT = x$lat),
         coords = c("LONG", "LAT"),
         crs = 4326)
      # overlay pt on spatial data to select region
      selection <- suppressMessages(st_join(pt, geodf))
      #st_name <- selection$state_name[1]
      #shinyjs::runjs("Shiny.setInputValue('state','Tennessee')")
      updateSelectInput(session,
                        inputId='state', 
                        selected = selection$state_name[1] )
      county_names <- out_df %>%
        select(state_name, county_name) %>%
        filter(state_name == selection$state_name[1]) %>%
        unique() %>%
        arrange(county_name) %>%
        pull(county_name)
      updateSelectInput(session,
                        inputId='county', 
                        choices = county_names,
                        selected = selection$county_name[1] )
    }
    
  })
  
  epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2^(14:8))
  
  
  
  output$usPlot <- renderLeaflet({
    #pal <- colorNumeric("Blues", domain = mapData()$acs_total_pop_e)
    pal <- colorFactor(
      palette = scales::brewer_pal(palette='YlOrRd')(nlevels(out_df$rate_c)),
      levels=levels(out_df$rate_c))
    pal2 <- colorFactor(
      palette = scales::brewer_pal(palette='RdYlBu', direction=-1)(nlevels(out_df$growth_c)),
      levels=levels(out_df$growth_c)
    )
    leaflet(
      staticData,  
      options = leafletOptions(crs = epsg2163)
      ) %>%
    addPolygons(
      data = state_geo,
      weight=1, color = "#444444", opacity = 1,
      fill=FALSE)
  })
  

  
  #####################################################
  # observe (mostly mapData) to update map
  observe({
    if(input$layer=="Growth Rate"){ 
      pal <- colorFactor(
        palette = scales::brewer_pal(palette='RdYlBu', direction=-1)(nlevels(out_df$growth_c)+1)[-1],
        levels=levels(out_df$growth_c))
      leg_title <- "Growth Rate (%)" 
      } else{
    pal <- colorFactor(
      palette = scales::brewer_pal(palette='YlOrRd')(nlevels(out_df$rate_c)),
      levels=levels(out_df$rate_c))
    leg_title = "Count (per 10,000 people)"
    }
    leafletProxy("usPlot", data=mapData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data=mapData(),
        weight = 0.2, color = "#c0c0c0", opacity = 1,
        fillColor = ~pal(fill_layer), fillOpacity = 1.0, smoothFactor = 0.5,
        label = lapply(mapData()$label_layer, HTML),
        labelOptions = labelOptions(direction = "auto")) %>%
      addPolygons(
        data = state_geo,
        weight=1, color = "#444444", opacity = 1,
        fill=FALSE) %>%
      addLegend("bottomright", pal = pal, values = ~fill_layer,
                title = leg_title,
                opacity = 1
      )
  })
  
  output$usPlot2 <- renderPlot({
    ggobj <- ggplot(data = geodf %>%
                      left_join(
                        out_df %>%
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
  
  
  #output$tsPlot <- renderSvgPanZoom({
  output$tsPlot <- renderPlot({
    #browser()
    if(input$plot_type==tsPlot_type_label[[1]]){ # comparison plot
      ylims = c(1e-5, min(100, max(tsPlotData()$rate,na.rm=TRUE)))
      log_ylims = c(1e-5, min(100, max(tsPlotData()$rate,na.rm=TRUE)))
      plt <- ggplot(data = tsPlotData(),
                    mapping = aes(x=date, y=rate, group=county_name))+
        geom_line(aes(group=county_name), alpha=10/length(unique(tsPlotData()$county_name))) + 
        geom_vline(xintercept=input$DateSelect) +
        labs(title=paste0('All counties in ', input$state),
             subtitle=paste0('Highlighted county: ', input$county))
      if(nrow(tsHighlightData()>0)){
        plt <- plt + geom_line(data=tsHighlightData(), 
                               aes(group='county_name'), color='red') 
      }
      if(input$y_scale == 'Log 10'){ 
        plt <- plt + scale_y_log10('Cases (per 10,0000 persons)', limits=ylims)
        } else {
        plt <- plt + scale_y_continuous('Cases (per 10,000 persons)', limits=ylims)
      }
    } else{ # single county plot
      #browser()
      toppred <- tsHighlightData()$pop[1] * max(tsHighlightData()$lambda_q85/1e8, na.rm=TRUE)
      topdata <- max(tsHighlightData()$new_cases_mdl, na.rm=TRUE)
      if(toppred > 1.5*topdata){
        ylims = c(1e-5, 1.5*topdata)
        } else ylims = c(1e-5, max(1.5*topdata,1.1*toppred))
      plt <- ggplot(data=tsHighlightData(),
                    mapping = aes(x=date)) +
        geom_line(mapping=aes(y=(lambda_q50/1e8)*pop)) +
        geom_ribbon(mapping=aes(ymax=(lambda_q85/1e8)*pop,
                                ymin=(lambda_q15/1e8)*pop),
                    alpha=.25, color=NA) +
        geom_point(mapping = aes(y=new_cases_mdl+.01), color='red')+
        geom_vline(xintercept=input$DateSelect) +
        labs(y='Cases', title=paste0('New Cases in ', input$county, ' County, ', input$state))
      if(input$y_scale == 'Log 10'){ plt <- plt + scale_y_log10('New Cases', limits=ylims)
      } else{
        plt <- plt + scale_y_continuous('New Cases', limits=ylims)
      }
    }
    plt <- plt + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y/%m/%d")
    plt
    #svgPanZoom(plt, controlIconsEnabled = T)
  }, height=300)
  
  plot2 <- reactive({
    if(input$plot_type==tsPlot_type_label[[1]]){ # comparison plot
      ylims = c(1e-5, min(100, max(tsPlotData()$rate,na.rm=TRUE)))
      log_ylims = c(1e-5, min(100, max(tsPlotData()$rate,na.rm=TRUE)))
      plt <- ggplot(data = tsPlotData(),
                    mapping = aes(x=date, y=rate, group=county_name,
                                  text = paste('County: ', county_name),
                                  key=county_name))+
        geom_line(aes(), alpha=10/length(unique(tsPlotData()$county_name))) + 
        geom_vline(xintercept=input$DateSelect) +
        labs(title=paste0('All counties in ', input$state),
             subtitle=paste0('Highlighted county: ', input$county))
      if(nrow(tsHighlightData()>0)){
        plt <- plt + geom_line(data=tsHighlightData(), 
                               aes(), color='red') 
      }
      if(input$y_scale == 'Log 10'){ 
        plt <- plt + scale_y_log10('Rate (10^y cases per 10,000 persons)', limits=ylims)
      } else {
        plt <- plt + scale_y_continuous('Rate (Cases per 10,000 persons)', limits=ylims)
      }
    } else {# Single county plot
      # Determine reasonable plot limits
      # Calculate top data point and prediction point
      toppred <- tsHighlightData()$pop[1] * max(tsHighlightData()$lambda_q85/1e8, na.rm=TRUE)
      topdata <- max(tsHighlightData()$new_cases_mdl, na.rm=TRUE)
      if(toppred > 1.5*topdata){
        ylims = c(1e-4, 1.5*topdata)
      } else ylims = c(1e-5, max(1.5*topdata,1.1*toppred))
      plt <- ggplot(data=tsHighlightData() %>% 
                      mutate(y=(lambda_q50/1e8)*pop,
                             ymin = (lambda_q15/1e8)*pop,
                             ymax = (lambda_q85/1e8)*pop) %>%
                      mutate(ymin = pmin(ymin, ylims[2]),
                             ymax = pmin(ymax, ylims[2])),
                    mapping = aes(x=date,
                                  y = y,
                                  ymin=ymin, ymax=ymax,
                                  group=county_name,
                                  text = paste('New Cases: ', new_cases_mdl))) +
        geom_point(mapping = aes(y=new_cases_mdl+.01), color='red')+
        geom_line() +
        geom_ribbon(aes(group=1),alpha=.25, color=NA) +
        geom_vline(xintercept=input$DateSelect) +
        labs(y='Cases', title=paste0('New Cases in ', input$county, ' County, ', input$state))
      if(input$y_scale == 'Log 10'){ 
        plt <- plt + scale_y_log10('New Cases (units = 10^y)', limits=ylims) + 
          annotate('text', x=as_date('2020-04-15'), y=.001, 
                   label='Zeros have been set to .001')
      } else{
        plt <- plt + scale_y_continuous('New Cases', limits=ylims)
      }
    }
    return(plt)
  })
  
  output$tsPlot2 <- renderPlotly({
    #browser()
    plt <- plot2() + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d")
    plt
    ggplotly(plt, tooltip=c('text'), dynamicTicks=TRUE) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c(
               'sendDataToCloud',
               'toImage',
               'autoScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'toggleSpikelines'
             ))
#      config(displayModeBar = FALSE)
    #svgPanZoom(plt, controlIconsEnabled = T)
  })
  
  
  output$table <- renderTable({
    df <- out_df %>%
      filter(date== input$DateSelect)
    tab <- table(`growth in last week`=df$growth_c,`rate (per 10,000 persons)`=df$rate_c)
    as.data.frame.matrix(tab)
  }, rownames = TRUE, colnames = TRUE)
  
  output$tableText <- renderText({
#    sprintf("Growth Rate over week prior to %s %s by Number of Cases on %s %s",
#            month(input$DateSelect, label=TRUE),
#            day(input$DateSelect),
#            month(input$DateSelect, label=TRUE),
#            day(input$DateSelect))
    paste("Growth Rate over week prior to" , month(input$DateSelect, label=TRUE), day(input$DateSelect), 
    "by Number of Cases on", month(input$DateSelect, label=TRUE), day(input$DateSelect))
  })
  
  output$info <- renderPrint({
    #df <- slope_df %>%
    #  filter(date== input$DateSelect)
    #table(`growth in last week`=df$growth_c,`rate (per 10,000 persons)`=df$rate_c)
    print(input$usPlot_shape_click)
    print(input$usPlot_shape_click$lng)
    print(is.null(input$usPlot_shape_click))
    print(input$state)
    print(sprintf('data:State %s', data$state))
    print(sprintf('data:County %s', data$county))
    print(event_data('plotly_click'))
    print(data$county_list)
    if(!is.null(input$usPlot_shape_click)){
      pt <- st_as_sf(
        tibble(LONG = input$usPlot_shape_click$lng,
               LAT = input$usPlot_shape_click$lat),
        coords = c("LONG", "LAT"),
        crs = 4326)
      selection <- suppressMessages(st_join(pt, geodf))
      print(selection)
    }
    #print(selection$state_name[1])
   # tsHighlightData()
    #print(input$mapClick)
    #nearPoints(tsPlotData(), input$tsClick, maxpoints = 1)
  })
  

}