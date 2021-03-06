cat(file=stderr(), paste(Sys.info()), "\n")

.libPaths(c("/home/nnagle/R/x86_64-pc-linux-gnu-library/3.6", .libPaths()))
cat(file=stderr(), Cstack_info(), "\n")
cat(file=stderr(), R.Version()$version.string, "\n")
cat(file=stderr(), paste(packageVersion('sf')), "\n")
cat(file=stderr(), paste(packageVersion('rlang')), "\n")
cat(file=stderr(), paste(.libPaths()), "\n")


library(tidyverse)
library(sf)


#load('results.RData')
out_df <- readRDS('results.RDS')
#rm(geodf)
load('counties_sf_geo.RData')
geodf <- counties_sf_geo %>%
  mutate_at(c('geoid'), as.factor) %>%
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
  group_by(state_name, county_name) %>%
  arrange(date) %>%
  mutate(new_cases_mdl = ifelse(is.na(new_cases_mdl), 0, new_cases_mdl)) %>%
  mutate(cum_cases = cumsum(new_cases_mdl)) %>%
  mutate(cases_smooth = (cum_cases - lag(cum_cases,7, default = 0)) / 7) %>%
  ungroup()
           
out_df <- out_df %>%
  mutate(fudge = fudge/1e8) %>%
  mutate(rate = fudge*lambda_q50*(10000/1e8)) %>%
  mutate(rate_c = cut(rate, 
                      breaks=c(-Inf, .1, .3, 1, 3, 10, Inf),
                      labels = c('< .1', '.1-.3', '.3-1', '1-3', '3-10', '>10' ))) %>%
  mutate(count_n = new_cases_mdl/acs_total_pop_e*10000,
         count_c = cut(count_n,
                       breaks=c(-Inf, .1, .3, 1, 3, 10, Inf),
                       labels = c('< .1', '.1-.3', '.3-1', '1-3', '3-10', '>10' ))) %>%
  mutate(smooth_n = cases_smooth/acs_total_pop_e*10000,
         smooth_c = cut(smooth_n,
                        breaks=c(-Inf, .1, .3, 1, 3, 10, Inf),
                        labels = c('< .1', '.1-.3', '.3-1', '1-3', '3-10', '>10' )))

slope_df <- out_df %>%
  group_by(state_name, county_name) %>%
  arrange(date) %>%
  mutate(growth = (lambda_q50-lag(lambda_q50,7))/lag(lambda_q50,7)) %>%
  mutate(growth_c = cut(growth,
                        breaks=c(-Inf,-.5, -.1,.1,.5,1,Inf),
                        labels=c('More than Halved', "-50% to -10%", "-10% - 10%", "10-50", "50% - 100%", "More than Doubled"))) %>%
  ungroup()


##############################################
# Create a state_boundary layer for reference
state_geo <- geodf %>%
  left_join(out_df %>% filter(date==date[[1]]) %>% select(geoid, state_fips, state_name)) %>%
  group_by(state_fips, state_name) %>%
  summarise()


# Define the UI
source('covid_ui.R', local=TRUE)
source('covid_server.R', local=TRUE)

# Return a Shiny app object
shinyApp(ui = ui, server = server)
