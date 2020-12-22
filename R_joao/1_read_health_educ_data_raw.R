# load setup

source('R_joao/setup_jpparga.R')

cities <- munis_df$abrev_muni

# 1 read data-raw and save data -------------------------------------------

# * 1.1 education ---------------------------------------------------------

read_education_geocode <- function(city, year){

  city_code <- munis_df[abrev_muni == city,.(code_muni)]$code_muni

  educ <- readr::read_rds(here::here('data-raw','censo_escolar', paste0('educacao_inep_final_', year,'.rds')))

  educ_city <- data.table::setDT(educ)[
    as.numeric(code_muni) == city_code,
    .(co_entidade, code_muni, no_entidade, mat_infantil, mat_fundamental, mat_medio,
      endereco, SearchedAddress, MatchedAddress, PrecisionDepth, geocode_engine, lon, lat)
  ]
  educ_na <- educ_city[is.na(lat)]

  educ_city_sf <- sf::st_as_sf(educ_city[!is.na(educ_city$lat), ], coords = c('lon','lat'), crs = 4326, remove = F)

  hex <- readr::read_rds(here::here('data-raw', 'hex_municipio', paste0('hex_', city,'_09_', year, '.rds')))

  hex_educ <- hex %>% sf::st_join(educ_city_sf)

  educ_city_sf <- educ_city_sf %>%
    mutate(
      # ver se intercepta com os limites dos hexagonos da cidade
      limite = lengths(sf::st_intersects(educ_city_sf, hex)),
      # adicionar ano
      ano = year
    ) %>%
    # incluir o hexagono que a oportunidade pertence no ano em questao
    sf::st_join(hex %>% select(-c(h3_resolution, sigla_muni)))

  hex_educ_oport <- hex_educ %>%
    filter(!is.na(co_entidade))

  hex_educ_agregado <- data.table::setDT(hex_educ)[, .(mat_infantil = sum(mat_infantil, na.rm=TRUE),
                                                       mat_fundamental = sum(mat_fundamental, na.rm=TRUE),
                                                       mat_medio = sum(mat_medio, na.rm=TRUE),
                                                       total_censo_escolar = mat_infantil+mat_fundamental+mat_medio),
                                                   by = id_hex]

  hex_educ_agregado <- dplyr::left_join(hex, hex_educ_agregado)


  educ_fora_limite <- educ_city_sf %>%
    filter(limite == 0)


  if (!dir.exists(here('data', city))) {
    dir.create(here('data', city))
  }


  readr::write_rds(educ_na, here::here('data', city, paste0('educ_NAs_',city,'_',year,'.rds')))
  readr::write_rds(educ_city_sf, here::here('data', city, paste0('educ_',city,'_',year,'.rds')))
  readr::write_rds(educ_fora_limite, here::here('data', city, paste0('educ_outside_',city,'_',year,'.rds')))
  readr::write_rds(hex_educ_oport, here::here('data',city, paste0('hex_educ_oport_',city,'_',year,'.rds')))
  readr::write_rds(hex_educ_agregado, here::here('data',city, paste0('hex_educ_agr_',city,'_',year,'.rds')))

}

for (i in 2017:2019){
  purrr::walk2(.x = cities, .y = i, .f = read_education_geocode)
}


# * 1.2 health ------------------------------------------------------------

read_health_geocode <- function(city, year){

  city_code <- munis_df[abrev_muni == city,.(code_muni)]$code_muni

  health <- readr::read_rds(here::here('data-raw','hospitais',paste0('hospitais_geocoded_pmaq_',year,'.rds')))

  health_city <- data.table::setDT(health)[
    code_muni == stringr::str_sub(as.character(city_code),1,6),
    .(cnes, code_muni, estabelecimento, tipo_unidade,
      logradouro, numero, bairro, cep,
      health_low, health_med, health_high,
      MatchedAddress, SearchedAddress, PrecisionDepth, geocode_engine, lon,lat)
  ]

  health_na <- health_city[is.na(lat)]

  health_city_sf <- sf::st_as_sf(health_city[!is.na(health_city$lat), ], coords = c('lon','lat'), crs = 4326, remove = F)

  hex <- readr::read_rds(here::here('data-raw', 'hex_municipio', paste0('hex_', city,'_09_', year, '.rds')))

  hex_health <- hex %>% sf::st_join(health_city_sf)

  health_city_sf <- health_city_sf %>%
    mutate(
      # ver se intercepta com os limites dos hexagonos da cidade
      limite = lengths(sf::st_intersects(health_city_sf, hex)),
      # adicionar ano
      ano = year
    ) %>%
    # incluir o hexagono que a oportunidade pertence no ano em questao
    sf::st_join(hex %>% select(-c(h3_resolution, sigla_muni)))

  hex_health_oport <- hex_health %>%
    filter(!is.na(cnes))


  hex_health_agregado <- data.table::setDT(hex_health)[,.(health_low = sum(health_low, na.rm=TRUE),
                                                          health_med = sum(health_med, na.rm=TRUE),
                                                          health_high = sum(health_high, na.rm=TRUE),
                                                          total_health = health_low+health_med+health_high),
                                                       by = id_hex]

  hex_health_agregado <- dplyr::left_join(hex, hex_health_agregado)

  health_fora_limite <- health_city_sf %>%
    filter(limite == 0)


  if (!dir.exists(here('data', city))) {
    dir.create(here('data', city))
  }

  readr::write_rds(health_na, here::here('data', city, paste0('health_NAs_',city,'_',year,'.rds')))
  readr::write_rds(health_city_sf, here::here('data', city, paste0('health_',city,'_',year,'.rds')))
  readr::write_rds(health_fora_limite, here::here('data', city, paste0('health_outside_',city,'_',year,'.rds')))
  readr::write_rds(hex_health_oport, here::here('data',city, paste0('hex_health_oport_',city,'_',year,'.rds')))
  readr::write_rds(hex_health_agregado, here::here('data',city, paste0('hex_health_agr_',city,'_',year,'.rds')))

}


for (i in 2017:2019){
  purrr::walk2(.x = cities, .y = i, .f = read_health_geocode)
}

