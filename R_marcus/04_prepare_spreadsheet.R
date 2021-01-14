library("tidyverse")

source("R_marcus/setup.R")

hospitals_df <- read_csv(here::here("outputs", "hospitals_too_close.csv"))

hospitals_to_check <- hospitals_df %>%
  filter(n >= 3) %>%
  mutate(google_link = map2_chr(lon, lat, function(lon, lat) {
    return(paste0("https://www.google.com/maps/@", lat, ",", lon, ',19z'))
  })) %>%
  select(google_link, lon, lat, cnes, estabelecimento, municipio, code_muni,
         SearchedAddress, MatchedAddress, PrecisionDepth, geocode_engine,
         n_vizinhos = n) %>%
  arrange(lon, lat) %>%
  distinct()


schools_df <- read_csv(here::here("outputs", "schools_too_close.csv")) %>%
  left_join(munis_df, by=c("code_muni"))

schools_to_check <- schools_df %>%
  filter(n >= 3) %>%
  mutate(google_link = map2_chr(lon, lat, function(lon, lat) {
    return(paste0("https://www.google.com/maps/@", lat, ",", lon, ',19z'))
  })) %>%
  select(google_link, lon, lat, co_entidade, no_entidade, code_muni, name_muni,
         SearchedAddress, MatchedAddress, PrecisionDepth, geocode_engine,
         n_vizinhos = n) %>%
  arrange(lon, lat) %>%
  distinct()

write_csv(hospitals_to_check, here::here("outputs", "hospitals_to_check.csv"))
write_csv(schools_to_check, here::here("outputs", "schools_to_check.csv"))


