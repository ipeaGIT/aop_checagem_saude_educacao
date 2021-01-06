source(here::here("R_marcus", "setup.R"))

hospitals_df <- rbind(
  read_rds(here::here("data/acesso_oport/hospitais", "2017",
                      "hospitais_geocoded_pmaq_2017.rds")) %>% mutate(year = 2017),
  read_rds(here::here("data/acesso_oport/hospitais", "2018",
                      "hospitais_geocoded_pmaq_2018.rds")) %>% mutate(year = 2018),
  read_rds(here::here("data/acesso_oport/hospitais", "2019",
                      "hospitais_geocoded_pmaq_2019.rds")) %>% mutate(year = 2019),
  fill = TRUE
) %>%
  filter(PrecisionDepth %in% c("PMAQ", "street_number", "route"))

city_short <- "spo"
city_code <- "3550308"
city_name <- "SAO PAULO"
y = 2017



find_hospitals_near <- function(city_name, y = 2017, hospitals, distance_threshold = 10) {
  city_name <- str_to_upper(city_name)
  hospitals_filtered <- hospitals_df %>%
    filter(municipio == city_name, year == y)

  hospital_distances <- geodist(hospitals_filtered %>% select(lon, lat)) %>% as_tibble()
  colnames(hospital_distances) <- hospitals_filtered$cnes
  hospital_distances$hospital_a <- hospitals_filtered$cnes

  hospital_distances_df <- hospital_distances %>%
    pivot_longer(-hospital_a, names_to = "hospital_b", values_to = "distance") %>%
    filter(hospital_a != hospital_b, distance < 15)


  return(hospital_distances_df %>%
           count(hospital_a, sort = TRUE) %>%
           mutate(year = y))
}

find_hospitals_near("SAO PAULO", 2017, hospitals_df)

hospitals_too_close <- map2_df(munis_df$name_muni, munis_df$abrev_muni, function(muni_name, muni_sigla){
  s1 <- find_hospitals_near(muni_name, 2017, hospitals_df)
  s2 <- find_hospitals_near(muni_name, 2018, hospitals_df)
  s3 <- find_hospitals_near(muni_name, 2019, hospitals_df)
  return(rbind(s1, s2, s3))
})

hospitals_too_close_df <- inner_join(hospitals_df, hospitals_too_close,
                                     by = c("cnes"="hospital_a", "year"))

write_csv(hospitals_too_close_df, here::here("outputs", "hospitals_too_close.csv"))


hospitals_too_close_df %>%
  filter(year == 2017) %>%
  count(n) %>%
  filter(n >= 3)

Total: 123

hospitals_too_close_df %>%
  filter(n == 8, year == 2018) %>% View()
  filter(municipio == "PORTO ALEGRE") %>%
  mapview(xcol="lon", ycol="lat", crs=4326)

hospitals_too_close_df %>%
  filter(n >= 3) %>%
  View()

schools_df %>%
  count(n)


schools_df %>%
  filter(str_detect(no_entidade, "BUENOS AIRES")) %>% View()








