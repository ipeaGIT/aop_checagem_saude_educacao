source(here::here("R_marcus", "setup.R"))

schools_df <- rbind(
  read_rds(here::here("data/acesso_oport/censo_escolar", "2017",
                      "educacao_inep_final_2017.rds")) %>% mutate(year = 2017),
  read_rds(here::here("data/acesso_oport/censo_escolar", "2018",
                      "educacao_inep_final_2018.rds")) %>% mutate(year = 2018),
  read_rds(here::here("data/acesso_oport/censo_escolar", "2019",
                      "educacao_inep_final_2019.rds")) %>% mutate(year = 2019),
  fill = TRUE
) %>%
  filter(PrecisionDepth %in% c("inep", "street_number", "route"))

# school_distances <- geodist(schools_df %>% select(lon, lat)) %>% as.data.frame()

city_short <- "spo"
city_code <- "3550308"
y = 2017

find_schools_near <- function(city_short, city_code, y = 2017, schools, distance_threshold = 10) {
  schools_filtered <- schools_df %>%
    filter(code_muni == city_code, year == y)

  school_distances <- geodist(schools_filtered %>% select(lon, lat)) %>% as_tibble()
  colnames(school_distances) <- schools_filtered$co_entidade
  school_distances$school_a <- schools_filtered$co_entidade

  school_distances_df <- school_distances %>%
    pivot_longer(-school_a, names_to = "school_b", values_to = "distance") %>%
    filter(school_a != school_b, distance < 15)


  return(school_distances_df %>%
           count(school_a, sort = TRUE) %>%
           mutate(year = y))
}


schools_too_close <- map2_df(munis_df$code_muni, munis_df$abrev_muni, function(muni_code, muni_sigla){
  s1 <- find_schools_near(muni_sigla, muni_code, 2017, schools_df)
  s2 <- find_schools_near(muni_sigla, muni_code, 2018, schools_df)
  s3 <- find_schools_near(muni_sigla, muni_code, 2019, schools_df)
  return(rbind(s1, s2, s3))
})


schools_too_close_df <- inner_join(schools_df, schools_too_close,
           by = c("co_entidade"="school_a", "year"))

write_csv(schools_too_close_df, here::here("outputs", "schools_too_close.csv"))

schools_too_close_df %>%
  filter(year == 2017) %>%
  count(n) %>%
  filter(n >= 3)

Total: 51

