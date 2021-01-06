source(here::here("R_marcus", "setup.R"))
library("scales")

schools_df <- read_csv(here::here("outputs", "schools_too_close.csv"))


schools_df <- schools_df %>%
  left_join(munis_df, by = c("code_muni")) %>%
  mutate(new_address = sprintf("%s, %s, Brazil", no_entidade, name_muni))

# geocode schools by name and city
geo_coordinates <- geocode(schools_df$new_address, output = "all")

geo_coordinates <- geo_coordinates %>%
  rename(new_lon = lon, new_lat = lat)

schools_df <- read_csv(here::here("outputs", "schools_too_close.csv"))


distances <- geodist(schools_df %>% select(lon, lat),
                     geo_coordinates, paired = TRUE, measure = "geodesic")

schools_df <- cbind(schools_df, geo_coordinates)
schools_df$displacement <- distances

schools_df %>%
  ggplot() + geom_histogram(aes(displacement)) +
  facet_wrap(~n)
  scale_x_log10(labels = comma_format())



schools_df %>%
  filter(code_muni == "3550308", displacement < 90000) %>%
  # mapview(xcol="lon", ycol="lat", crs=4326)
  # mapview(xcol="new_lon", ycol="new_lat", zcol="displacement", crs=4326)
  ggplot() +
  geom_segment(aes(x=lon, xend=new_lon, y=lat, yend=new_lat))

mapviewGetOption("basemaps")


