# Compress Results object to be web friendly:
library(tidyverse)
library(googlesheets4)
DATE <- '2020-06-04'
file.copy(from = '~/Dropbox/covid-model/results/results_2020-06-04.RData',
          to = '~/Dropbox/covid-shinyapp/results.RData',
          overwrite=TRUE)

load('~/Dropbox/covid-shinyapp/results.RData')

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
 
  ) %>%
  mutate(new_cases_mdl = new_cases)

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
  
saveRDS(out_df3, file.path('~/Dropbox/covid-shinyapp','results.RDS'))


gsheet_data <- data_out %>%
  select(
    geoid, 
    pop,
    state_name,
    county_name,
    date,
    lambda_q50,
    lambda_q05,
    lambda_q95,
    total_cases,
    Y,
    new_cases) %>%
  mutate(lambda_q50 = round(lambda_q50*pop,digits=4),
         lambda_q05 = round(lambda_q05*pop, digits=4),
         lambda_q95 = round(lambda_q95*pop, digits=4)
  ) %>%
  select(-state_name, -pop, -county_name)
         
# Calculate 5-day Re
gsheet_data <- gsheet_data %>%
  group_by(geoid) %>%
  arrange(date) %>%
  mutate(Re4 = round(lambda_q50 / lag(lambda_q50,n=4L), digits=3)) %>%
  ungroup()



gsheet_nest <- 
  gsheet_data %>%
  mutate(nest_id = floor(row_number()/10000)) %>%
  group_by(nest_id) %>%
  nest()

#sheet_name <- 'gs-covid-data'
#ss <- gs4_create(sheet_name, sheets=gsheet_nest$data[[1]])
#for(i in 2:length(gsheet_nest$data)) {
#  print(i)
#  sheet_append('1Apx4abCnkbYrx5cywnf_uyo6KDp3zzUYhYWViEeDz44',gsheet_nest$data[[i]])
#}


# Write the following googlesheets
# OID : OID, geoid, date
# lambda: OID, lambda
# count
# 

geoids <- covidmodeldata::acs_data %>%
  sf::st_drop_geometry()  %>%
  arrange(geoid) %>%
  pull(geoid) %>%
  as.character()

gsheet_nest <- crossing(
  geoid = geoids,
  date = unique(gsheet_data$date)
) %>%
  arrange(geoid, date) %>%
  mutate(oid=row_number()) %>%
  left_join(gsheet_data, by=c('geoid','date')) %>%
  mutate(nest_id = ceiling(row_number()/10000)) %>%
  group_by(nest_id) %>%
  nest()

#gs4_create(name='nowcast-data-oid', sheets=list('sheet1'))

ss <- list(oid = '1ZL96eyXOIttUFYoqXDoLKv7QWOU0U5v_pDUC4WLYmDU',
           lambda = '1Ka98U_UbBdammjSX4rDEi1rtA_4cSk-alWbxoQugb1Y',
           ribbon = '1StGxcqUGaX9C1KnwBVTZYiiRauXU5iHCfxxi3YRjG_s',
           cases =  '18r9Iuxwdvc1vSAgfEBhTxPxiAVBz1qLn1lwGQxWfnGs')

#######
# OID
sheet_write(data = gsheet_nest$data[[1]] %>%
              select(geoid, date, oid),
              ss= ss$oid,
            sheet = 'oid')

for(i in 2:length(gsheet_nest$data)) {
  print(i)
  sheet_append(data = gsheet_nest$data[[i]] %>% select(geoid, date, oid),
               ss = ss$oid,
               sheet='oid')
  Sys.sleep(.005)
}

#######
# lambda
sheet_write(data = gsheet_nest$data[[1]] %>%
              filter(!(date < lubridate::as_date(DATE) & !is.numeric(total_cases))) %>%
              select(oid, lambda=lambda_q50, Re4),
            ss= ss$lambda,
            sheet = 'lambda')

for(i in 2:length(gsheet_nest$data)) {
  print(i)
  sheet_append(data =  gsheet_nest$data[[i]] %>%
                 filter(!(date < lubridate::as_date(DATE) & !is.numeric(total_cases))) %>%
                 select(oid, lambda=lambda_q50, Re4),
               ss = ss$lambda,
               sheet='lambda')
  Sys.sleep(.005)
}

#######
# ribbon
date_keep <- seq.Date(from=min(gsheet_data$date),
                      to=max(gsheet_data$date),
                      by=3) 
date_keep <- date_keep + (max(gsheet_data$date)-max(date_keep))
  
sheet_write(data = gsheet_nest$data[[1]] %>%
              filter(date %in% date_keep) %>%
              select(oid, lambda_min = lambda_q05, lambda_max = lambda_q95) %>%
              drop_na() ,
            ss= ss$ribbon,
            sheet = 'ribbon')

for(i in 2:length(gsheet_nest$data)) {
  print(i)
  sheet_append(data = gsheet_nest$data[[i]] %>% 
                 filter(date %in% date_keep) %>%
                 select(oid, lambda_min = lambda_q05, lambda_max = lambda_q95) %>% 
                 drop_na() ,
               ss = ss$ribbon,
               sheet='ribbon')
  Sys.sleep(.005)
}

#######
# cases
sheet_write(data = gsheet_nest$data[[1]] %>%
              select(oid, new_cases) %>% drop_na(),
            ss= ss$cases,
            sheet = 'cases')

for(i in 2:length(gsheet_nest$data)) {
  print(i)
  sheet_append(data = gsheet_nest$data[[i]] %>% select(oid, new_cases) %>% drop_na(),
               ss = ss$cases,
               sheet='cases')
  Sys.sleep(.005)
}

