# Use the helper functions from urbnmapr
library(sf)
source('R/urbnmapr_utils.R')

# counties_sf will have Albers projection
# counties_sf_geo will have geographic coordinates

counties_sf <- get_shapefile(2016, 'county', '20m') %>%
  st_as_sf() %>%
# set prejection to US National Atlas Equal Area
  sf::st_transform(crs = 2163) %>%
  mutate(county_fips = paste0(STATEFP, COUNTYFP)) %>%
  select(county_fips) %>%
  left_join(get_county_fips(), by = "county_fips") %>%
  filter(!state_fips %in% c("60", "66", "69", "72", "78"))


counties_sf_geo <- get_shapefile(2016, 'county', '20m') %>%
  st_as_sf() %>%
  # set prejection to US National Atlas Equal Area
  mutate(county_fips = paste0(STATEFP, COUNTYFP)) %>%
  select(county_fips) %>%
  left_join(get_county_fips(), by = "county_fips") %>%
  filter(!state_fips %in% c("60", "66", "69", "72", "78"))


nyc_counties <- c('36061','36081','36005','36085','36047')

counties_sf <- counties_sf %>%
  mutate(geoid = county_fips) %>%
  mutate(geoid = ifelse(county_fips %in% nyc_counties,
                        '36NYC', county_fips)) %>%
  mutate(county_name = ifelse(geoid=='36NYC', 'New York City', county_name)) %>%
  select(geoid, state_name, county_name) %>%
  group_by(geoid, state_name,county_name) %>%
  summarize() %>%
  ungroup()

counties_sf_geo <- counties_sf_geo %>%
  mutate(geoid = county_fips) %>%
  mutate(geoid = ifelse(county_fips %in% nyc_counties,
                        '36NYC', county_fips)) %>%
  mutate(county_name = ifelse(geoid=='36NYC', 'New York City', county_name)) %>%
  select(geoid, state_name, county_name) %>%
  group_by(geoid, state_name,county_name) %>%
  summarize() %>%
  ungroup()

#save(counties_sf, file='counties_sf.RData')
#save(counties_sf_geo, file='counties_sf_geo.RData')

# Load county-level data for tableau
acs_data <- covidmodeldata::acs_data %>% st_drop_geometry()
counties_sf <- counties_sf %>%
  select(-county_name) %>%
  left_join(acs_data %>% 
              select(geoid, 
                     county_name,
                     pop = acs_total_pop_e))

# Rescale the coordinates to be on the earth
usg <- st_geometry(counties_sf)
usg <- usg / 100000
st_geometry(counties_sf) <- usg

#overwrite coordinates of albers to geographic to fool tableau
st_crs(counties_sf) <- st_crs(counties_sf_geo)

# Make state map
state_sf <- counties_sf %>%
  group_by(state_name) %>%
  summarize()

cast_all <- function(xg) {
  lapply(c("MULTILINESTRING"), 
         function(x) st_cast(xg, x))
}

st_geoms_poly <- st_geometry(state_sf)
st_geoms_line <- st_geoms_poly
for(i in 1:length(st_geoms_poly)) {
  if( any(class(st_geoms_poly[[i]])=='MULTIPOLYGON')) {
    st_geoms_line[[i]] <- st_cast(st_geoms_poly[[i]],to = 'MULTILINESTRING')
  } else st_geoms_line[[i]] <- st_cast(st_geoms_poly[[i]],to = 'LINESTRING')
}
st_geometry(state_sf) <- st_geoms_line
st_geometry(state_sf) <- st_geoms_poly

state_lines <- as(as(state_sf,"Spatial"), "SpatialLinesDataFrame")
st_write(counties_sf, '/home/nnagle/Dropbox/covid-shinyapp/counties.shp', delete_layer=TRUE)
writeOGR(state_lines, dsn='.', layer='stbound',driver="ESRI Shapefile",overwrite_layer=TRUE)
#st_write(state_sf, '/home/nnagle/Dropbox/covid-shinyapp/stbound.shp', delete_layer=TRUE)
st_write(counties_sf_geo, '/home/nnagle/Dropbox/covid-shinyapp/counties_geo.shp',delete_layer=TRUE)
