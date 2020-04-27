
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  library(tidyverse)
  library(ggplot2)
  library(leaflet)
  library(sf)
  library(shinyWidgets)
  
  
  # Change county when state is changed
  observeEvent(input$state,{
               county_names <- out_df %>%
                 select(state_name, county_name) %>%
                 filter(state_name == input$state) %>%
                 unique() %>%
                 arrange(county_name) %>%
                 pull(county_name)
               # Check if mouseclick is same as state (don't update if so)
               x <- input$usPlot_shape_click
               if(is.null(x)){
                 updateSelectInput(session,
                                   inputId='county', 
                                   choices = county_names )
               } else{
                 pt <- st_as_sf(
                   tibble(LONG = x$lng,
                          LAT = x$lat),
                   coords = c("LONG", "LAT"),
                   crs = 4326)
                 selection <- suppressMessages(st_join(pt, geodf))
                 if(selection$state_name[1]!= input$state){
                   updateSelectInput(session,
                                     inputId='county', 
                                     choices = county_names )
                 }
               }
               })
  
  # Change state and county on map click
  observeEvent(input$usPlot_shape_click, {
    x <- input$usPlot_shape_click
    if(!is.null(x)){
       pt <- st_as_sf(
         tibble(LONG = x$lng,
                LAT = x$lat),
         coords = c("LONG", "LAT"),
         crs = 4326)
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
    resolutions = 2^(14:10))
  
  staticData <- geodf %>% left_join(out_df %>%
                                      filter(date == date[1]))
  
  output$usPlot <- renderLeaflet({
    #pal <- colorNumeric("Blues", domain = mapData()$acs_total_pop_e)
    pal <- colorFactor(
      palette = scales::brewer_pal(palette='YlOrRd')(nlevels(out_df$rate_c)),
      levels=levels(out_df$rate_c))
    leaflet(
      staticData,  
      options = leafletOptions(crs = epsg2163)
      ) %>%
    # %>%
    #   addPolygons(
    #     data=mapData(),
    #     weight = 0, color = "#444444", opacity = 1,
    #     fillColor = ~pal(rate_c), fillOpacity = 0.7, smoothFactor = 0.5,
    #     label = ~paste(rate_c),
    #     labelOptions = labelOptions(direction = "auto"),
    #     highlightOptions = highlightOptions(
    #       weight = 1,
    #       fillOpacity = 0.7,
    #       bringToFront = FALSE)) %>%
    addPolygons(
      data = state_geo,
      weight=1, color = "#444444", opacity = 1,
      fill=FALSE)
  })
  
  ###################################3
  # Update map data for date
  mapData <- reactive({
    df <- geodf %>%
      left_join(out_df %>%
                  filter(date == input$DateSelect))
    switch(input$layer,
           "Modeled Count" = {df <- mutate(df, map_layer=rate_c)},
           "Raw Count" = {df <- mutate(df, map_layer=count_c)},
           "Smoothed Count" = {df <- mutate(df, map_layer=smooth_c)})
    return(df)
  })
  
  observe({
    pal <- colorFactor(
      palette = scales::brewer_pal(palette='YlOrRd')(nlevels(out_df$rate_c)),
      levels=levels(out_df$rate_c))
    leafletProxy("usPlot", data=mapData()) %>%
      clearShapes() %>%
      addPolygons(
        data=mapData(),
        weight = 0, color = "#444444", opacity = 1,
        fillColor = ~pal(map_layer), fillOpacity = 0.7, smoothFactor = 0.5,
        label = ~paste(map_layer),
        labelOptions = labelOptions(direction = "auto")) %>%
      addPolygons(
        data = state_geo,
        weight=1, color = "#444444", opacity = 1,
        fill=FALSE)
  })
  
  output$usPlot2 <- renderPlot({
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
  
  output$table <- renderTable({
    df <- slope_df %>%
      filter(date== input$DateSelect)
    tab <- table(`growth in last week`=df$growth_c,`rate (per 10,000 persons)`=df$rate_c)
    as.data.frame.matrix(tab)
  }, rownames = TRUE, colnames = TRUE)
  
  output$info <- renderPrint({
    #df <- slope_df %>%
    #  filter(date== input$DateSelect)
    #table(`growth in last week`=df$growth_c,`rate (per 10,000 persons)`=df$rate_c)
    print(input$usPlot_shape_click)
    print(input$usPlot_shape_click$lng)
    print(is.null(input$usPlot_shape_click))
    print(input$state)
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