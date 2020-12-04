### Read and save geocode ###

# Reads geocoded schools and hospitals
# Joins geocoded data with city hex grids (all cities, all years)
# Saves 3 datasets by year for each city:
# # i. hex_agr: total nummber of shools/hospitals by hex
# # ii. hex_total: every school/hospital address, coordinate and id_hex
# # iii. NAs: locations with missing lat/lon data


source('R_lucas/setup.R')

# Education -----
#
# Load geocoded schools and filter city

read_education_geocode <- function(city){

  year <- '2019'
  city_code <- munis_df[abrev_muni == city,.(code_muni)]$code_muni

  educ <- readr::read_rds(here::here('R_lucas','data-raw','escolas',paste0('educacao_inep_final_',year,'.rds')))

  educ_city <- data.table::setDT(educ)[as.numeric(code_muni) == city_code, .(co_entidade,code_muni,no_entidade,
                                                                            mat_infantil,mat_fundamental,mat_medio,
                                                                            endereco,MatchedAddress,
                                                                            lon,lat)]
  educ_na <- educ_city[is.na(lat)]

  educ_city_sf <- sf::st_as_sf(educ_city %>% na.omit(), coords = c('lon','lat'), crs = 4326)

  hex <- readr::read_rds(here::here('R_lucas','data-raw','hex', paste0('hex_',city,'_09_2019.rds')))

  hex_educ <- hex %>% sf::st_join(educ_city_sf)

  hex_educ_total <- hex_educ %>%
    na.omit() %>%
    dplyr::select(co_entidade,no_entidade,code_muni,sigla_muni,
                  dplyr::matches('mat_'),
                  endereco, MatchedAddress, id_hex) %>%
    dplyr::left_join(educ_city %>% dplyr::select(co_entidade,lon,lat),by='co_entidade')

  hex_educ <- data.table::setDT(hex_educ)[,.(mat_infantil = sum(mat_infantil, na.rm=TRUE),
                                             mat_fundamental = sum(mat_fundamental, na.rm=TRUE),
                                             mat_medio = sum(mat_medio, na.rm=TRUE),
                                             total_escolas = mat_infantil+mat_fundamental+mat_medio),
                                             by = id_hex]

  hex_educ_agregado <- dplyr::left_join(hex,hex_educ)

  #dir.create(paste0('R_lucas/data/',city))

  readr::write_csv(educ_na,  here::here('R_lucas','data',city,paste0('educ_NAs_',city,'_',year,'.rds')))
  readr::write_rds(hex_educ_total, here::here('R_lucas','data',city,paste0('educ_total_',city,'_',year,'.rds')))
  readr::write_rds(hex_educ_agregado, here::here('R_lucas','data',city,paste0('educ_agr_',city,'_',year,'.rds')))

}

cities <- munis_df$abrev_muni

purrr::walk(.x =  cities,.f = read_education_geocode)

# Health -----
#
# Load geocoded hospitals and filter city

read_health_geocode <- function(city){

  year <- '2017'
  city_code <- munis_df[abrev_muni == city,.(code_muni)]$code_muni

  health <- readr::read_rds(here::here('R_lucas','data-raw','hospitais',paste0('hospitais_geocoded_pmaq_',year,'.rds')))

  health_city <- data.table::setDT(health)[code_muni == stringr::str_sub(as.character(city_code),1,6),
                                           .(cnes,code_muni,estabelecimento,
                                             tipo_unidade,health_low,health_med,health_high,
                                             MatchedAddress,lon,lat)]
  health_na <- health_city[is.na(lat)]

  health_city_sf <- sf::st_as_sf(health_city %>% na.omit(), coords = c('lon','lat'), crs = 4326)

  hex <- readr::read_rds(here::here('R_lucas','data-raw','hex', paste0('hex_',city,'_09_2019.rds')))

  hex_health <- hex %>% sf::st_join(health_city_sf)

  hex_health_total <- hex_health %>%
    na.omit() %>%
    dplyr::select(cnes,code_muni,sigla_muni,estabelecimento,
                  tipo_unidade,health_low,health_med,health_high,
                  MatchedAddress, id_hex) %>%
    dplyr::left_join(health_city %>% dplyr::select(cnes,lon,lat),by='cnes')

  hex_health <- data.table::setDT(hex_health)[,.(health_low = sum(health_low, na.rm=TRUE),
                                                 health_med = sum(health_med, na.rm=TRUE),
                                                 health_high = sum(health_high, na.rm=TRUE),
                                             total_health = health_low+health_med+health_high),
                                          by = id_hex]

  hex_health_agregado <- dplyr::left_join(hex,hex_health)

  #dir.create(paste0('R_lucas/data/',city))

  readr::write_csv(health_na,  here::here('R_lucas','data',city,paste0('health_NAs_',city,'_',year,'.rds')))
  readr::write_rds(hex_health_total, here::here('R_lucas','data',city,paste0('health_total_',city,'_',year,'.rds')))
  readr::write_rds(hex_health_agregado, here::here('R_lucas','data',city,paste0('health_agr_',city,'_',year,'.rds')))

}

purrr::walk(.x =  cities,.f = read_health_geocode)
