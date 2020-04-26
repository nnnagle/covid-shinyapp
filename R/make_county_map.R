# Use the helper functions from urbnmapr
source('R/urbnmapr_utils.R')

counties_sf <- get_shapefile(2016, 'county', '20m') %>%
  st_as_sf() %>%
# set prejection to US National Atlas Equal Area
  sf::st_transform(crs = 2163) %>%
  mutate(county_fips = paste0(STATEFP, COUNTYFP)) %>%
  select(county_fips) %>%
  left_join(get_county_fips(), by = "county_fips") %>%
  filter(!state_fips %in% c("60", "66", "69", "72", "78"))


nyc_counties <- c('36061','36081','36005','36085','36047')

counties_sf <- counties_sf %>%
  mutate(geoid = county_fips) %>%
  mutate(geoid = ifelse(county_fips %in% nyc_counties,
                        '36NYC', county_fips)) %>%
  select(geoid) %>%
  group_by(geoid) %>%
  summarize()

save(counties_sf, file='counties_sf.RData')
