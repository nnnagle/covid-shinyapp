# Compress Results object to be web friendly:
library(tidyverse)
file.copy(from = '~/Dropbox/students/Piburn/covid-model/results/results_2020-04-29.RData',
          to = 'results.RData')
load('~/Dropbox/students/Piburn/covid-model/results/results_2020-05-02.RData')

out_df2 <- data_out %>% 
  select(
    geoid,
    state_name,
    county_name,
    pop,
    date,
    starts_with('total'),
    starts_with('new'),
    starts_with('lambda'),
    starts_with('Ysim')
 
  )

out_df2$state_name= as.factor(out_df2$state_name)
#out_df2$state_fips = as.factor(out_df2$state_fips)
out_df2$county_name = as.factor(out_df2$county_name)
out_df2$geoid = as.factor(out_df2$geoid)

out_df3 <- out_df2 %>% 
  mutate_at(
    vars(starts_with('lambda')), 
    function(x){ as.integer(x*1e8)}
    ) %>%
  mutate_if(
    is.numeric, as.integer) 
  
saveRDS(out_df3, 'results.RDS')

