# Compress Results object to be web friendly:
library(tidyverse)
load('results.RData')

out_df2 <- out_df %>% 
  select(
    geoid,
    state_fips,
    state_name,
    county_name,
    acs_total_pop_e,
    date,
    starts_with('total'),
    starts_with('new'),
    starts_with('lambda'),
    fudge
  )
out_df2$state_name= as.factor(out_df2$state_name)
out_df2$state_fips = as.factor(out_df2$state_fips)
out_df2$county_name = as.factor(out_df2$county_name)
out_df2$geoid = as.factor(out_df2$geoid)
out_df3 <- out_df2 %>% 
  mutate_at(
    vars(starts_with('lambda'),'fudge'), 
    function(x){ as.integer(x*1e8)}
    ) %>%
  mutate_if(
    is.numeric, as.integer)
  
saveRDS(out_df3, 'results.RDS')

