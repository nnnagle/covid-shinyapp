library(shiny)

load('results.RData')
rm(geodf)
load('counties_sf_geo.RData')
geodf <- counties_sf_geo %>%
  left_join(out_df %>% 
              select(geoid, state_name, county_name) %>% 
              unique(),
            by='geoid')
state_names <- out_df %>% select(state_name) %>% unique() %>% arrange(state_name) %>% pull(state_name)
county_names <- out_df %>%
  select(state_name, county_name) %>%
  filter(state_name == 'Alabama') %>%
  unique() %>%
  arrange(county_name) %>%
  pull(county_name)



out_df <- out_df %>%
  mutate(rate = lambda_q50*10000) %>%
  mutate(rate_c = cut(rate, 
                      breaks=c(-Inf, .1, .3, 1, 3, 10, Inf),
                      labels = c('< .1', '.1-.3', '.3-1', '1-3', '3-10', '>10' )))

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


source('covid_ui.r')
source('covid_server.r')

shinyApp(ui, server)
