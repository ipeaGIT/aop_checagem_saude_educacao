# load setup

source('R_joao/setup_jpparga.R')

cities <- munis_df$abrev_muni

# 1 create variation database ---------------------------------------------

# * 1.1 education ---------------------------------------------------------

check_diff_educ <- function(city){

  # read dfs
  bases <- dir(here::here(), pattern = paste0('educ_', city), recursive = T)

  # bind them into list
  l_bases <- bases %>%
    purrr::map(read_rds) %>%
    data.table::rbindlist()

  # change them into wide format
  l_bases_wider <- l_bases %>%
    tidyr::pivot_wider(
      id_cols = co_entidade,
      names_from = ano,
      values_from = id_hex,
      names_prefix = 'ano_'
    )

  # setDT
  l_bases_wider <- data.table::setDT(l_bases_wider)

  # compare hex between same oport (educ) between the years
  l_bases_wider <- l_bases_wider %>%
    mutate(
      comparacao = dplyr::case_when(
        ano_2017 == ano_2018 & ano_2017 == ano_2019 ~ 'OK',
        is.na(ano_2017) & is.na(ano_2018) & is.na(ano_2019) ~ 'All NA',
        (is.na(ano_2017) & is.na(ano_2018)) | (is.na(ano_2017) & is.na(ano_2019)) |
          (is.na(ano_2018) & is.na(ano_2019)) ~ 'Two NA',
        is.na(ano_2017) ~ '2017 NA',
        is.na(ano_2018) ~ '2018 NA',
        is.na(ano_2019) ~ '2019 NA',
        ano_2017 == ano_2018 & ano_2017 != ano_2019 ~ '2019 different',
        ano_2017 == ano_2019 & ano_2017 != ano_2018 ~ '2018 different',
        ano_2018 == ano_2019 & ano_2017 != ano_2019 ~ '2017 different',
        ano_2018 != ano_2019 & ano_2017 != ano_2019 ~ 'All different',
        T ~ 'Left out; check variable'
      )
    )

  # filter results
  results <- l_bases_wider[comparacao!='OK']

  # filter original df with the same oportunities (educ)
  l_bases <- l_bases[co_entidade %in% results$co_entidade]

  # add column with problem-type
  l_bases <- left_join(l_bases, results %>% select(co_entidade, comparacao))

  # arrange
  l_bases <- l_bases %>% arrange(co_entidade, ano)

  # transform to sf
  #l_bases <- l_bases %>%
  #  sf::st_as_sf(coords = c('lon','lat'), crs = 4326, remove = F)

  # check directory
  #if (!dir.exists(here('data', city))) {
  #  dir.create(here('data', city))
  #}

  # save df
  readr::write_rds(l_bases, here::here('data', city, paste0('difference_years_education_',city, '.rds')))

}

walk(.x = cities, .f = check_diff_educ)


# * 1.2 health ------------------------------------------------------------

check_diff_health <- function(city){

  # read dfs
  bases <- dir(here::here(), pattern = paste0('health_', city), recursive = T)

  #return(bases)

  # bind them into list
  l_bases <- bases %>%
    purrr::map(read_rds) %>%
    data.table::rbindlist()

  # change them into wide format
  l_bases_wider <- l_bases %>%
    tidyr::pivot_wider(
      id_cols = cnes,
      names_from = ano,
      values_from = id_hex,
      names_prefix = 'ano_'
    )

  # setDT
  l_bases_wider <- data.table::setDT(l_bases_wider)

  # compare hex between same oport (educ) between the years
  l_bases_wider <- l_bases_wider %>%
    mutate(
      comparacao = dplyr::case_when(
        ano_2017 == ano_2018 & ano_2017 == ano_2019 ~ 'OK',
        is.na(ano_2017) & is.na(ano_2018) & is.na(ano_2019) ~ 'All NA',
        (is.na(ano_2017) & is.na(ano_2018)) | (is.na(ano_2017) & is.na(ano_2019)) |
          (is.na(ano_2018) & is.na(ano_2019)) ~ 'Two NA',
        is.na(ano_2017) ~ '2017 NA',
        is.na(ano_2018) ~ '2018 NA',
        is.na(ano_2019) ~ '2019 NA',
        ano_2017 == ano_2018 & ano_2017 != ano_2019 ~ '2019 different',
        ano_2017 == ano_2019 & ano_2017 != ano_2018 ~ '2018 different',
        ano_2018 == ano_2019 & ano_2017 != ano_2019 ~ '2017 different',
        ano_2018 != ano_2019 & ano_2017 != ano_2019 ~ 'All different',
        T ~ 'Left out; check variable'
      )
    )

  # filter results
  results <- l_bases_wider[comparacao!='OK']

  # filter original df with the same oportunities (educ)
  l_bases <- l_bases[cnes %in% results$cnes]

  # add column with problem-type
  l_bases <- left_join(l_bases, results %>% select(cnes, comparacao))

  # arrange
  l_bases <- l_bases %>% arrange(cnes, ano)

  # save df
  readr::write_rds(l_bases, here::here('data', city, paste0('difference_years_health_',city, '.rds')))

}


walk(.x = cities, .f = check_diff_health)

