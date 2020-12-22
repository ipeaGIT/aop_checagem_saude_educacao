# load setup

source('R_joao/setup_jpparga.R')

cities <- munis_df$abrev_muni

# 1 read data -------------------------------------------------------------


# * 1.1 read complete health & educ data ----------------------------------

filefolder_health <- here::here('data-raw', 'hospitais')
health_complete_files <- dir(filefolder_health, full.names = T)
health_complete <- health_complete_files %>%
  purrr::map(read_rds)
names(health_complete) <- c('health_complete_2017','health_complete_2018','health_complete_2019')
purrr::walk(health_complete, data.table::setDT)

filefolder_educ <- here::here('data-raw', 'censo_escolar')
educ_complete_files <- dir(filefolder_educ, full.names = T)
educ_complete <- educ_complete_files %>%
  purrr::map(read_rds)
names(educ_complete) <- c('educ_complete_2017','educ_complete_2018','educ_complete_2019')
purrr::walk(educ_complete, data.table::setDT)

# * 1.2 read variation data between years ---------------------------------

bind_all_educ <- dir(here::here(), pattern = paste0('difference_years_education_'), recursive = T)
bind_all_health <- dir(here::here(), pattern = paste0('difference_years_health_'), recursive = T)

bind_all_educ <- bind_all_educ %>%
  purrr::map(read_rds) %>%
  data.table::rbindlist()

bind_all_health <- bind_all_health %>%
  purrr::map(read_rds) %>%
  data.table::rbindlist()


# 2 cleanse data ----------------------------------------------------------


# * 2.1 exclude low precision depth (already being treated) ----------------

# generate db with all hospitals/schools already being treated by having faulty geolocation
# db: low_precision_depth.xlsx; cols: code_local, tipo_local
# use db https://docs.google.com/spreadsheets/d/107zDdkyTGUGJMzZrDHjD2vm61inEqDefQTnGXLpTdOA/edit#gid=328994671

low_pd <- rio::import(file = 'data/low_precision_depth.xlsx')
low_pd <- data.table::setDT(low_pd)

low_pd_educ <- low_pd[tipo_local=='escola']
low_pd_health <- low_pd[!tipo_local=='escola']

# rename code variable
names(low_pd_educ)[names(low_pd_educ) == 'code_local'] <- 'co_entidade'
names(low_pd_health)[names(low_pd_health) == 'code_local'] <- 'cnes'

# fix number of characters (correct cnes' leading zeros)
low_pd_health$cnes <- sprintf("%07d", low_pd_health$cnes)

# filter health/educ already treated manually (low_pd)
check_educ <- bind_all_educ[!co_entidade %in% low_pd_educ$co_entidade]
check_health <- bind_all_health[!cnes %in% low_pd_health$cnes]

# remove low_pd
#rm(low_pd, low_pd_health, low_pd_educ)

# * 2.2 filter good precision depth ---------------------------------------

#
check_educ <- check_educ[
  PrecisionDepth %in% c('inep', '4 estrelas', '3 estrelas', 'street_number', 'route')]
check_health <- check_health[
  PrecisionDepth %in% c('inep', '4 estrelas', '3 estrelas', 'street_number', 'route')]

# number of individual opportunities to check
#length(unique(check_educ$co_entidade))
#length(unique(check_health$cnes))



# 3 check data ------------------------------------------------------------


# * 3.1 check health data -------------------------------------------------

check_health %>% count(comparacao)

#### INCORRETO/CORRIGIR GEOLOCALIZACAO
# all NA
#### INCORRETO/CORRIGIR GEOLOCALIZACAO
check_health[comparacao=='All NA'] %>% View()
# Apenas hospitais de 2019 e sem id_hex
unique(check_health[comparacao=='All NA']$cnes)
# conferir se cnes existem nos outros anos (2017 ou 2018) [should be F for 2017-18 & T for 2019]
any(unique(check_health[comparacao=='All NA']$cnes) %in%
      health_complete$health_complete_2017$cnes)
any(unique(check_health[comparacao=='All NA']$cnes) %in%
      health_complete$health_complete_2018$cnes)
any(unique(check_health[comparacao=='All NA']$cnes) %in%
      health_complete$health_complete_2019$cnes)


# 2019 NA
# check differente between id_hex_2017 & id_hex_2018
health_2019_na <- check_health[comparacao=='2019 NA'] %>%
  tidyr::pivot_wider(
    id_cols = cnes,
    names_from = ano,
    values_from = id_hex,
    names_prefix = 'ano_'
  )
health_2019_na$check <- health_2019_na$ano_2017 == health_2019_na$ano_2018
all(health_2019_na$check==T)
# use complete health data to check 2019 [should be F]
any(check_health[comparacao == '2019 NA']$cnes %in% health_complete$health_complete_2019$cnes)
# Hospitais corretamente sem observacao em 2019

# 2018 NA
health_2018_na <- check_health[comparacao == '2018 NA']
# use complete health data to check 2018 [should be F]
any(check_health[comparacao == '2018 NA']$cnes %in% health_complete$health_complete_2018$cnes)

# 2017 NA
# check differente between id_hex_2018 & id_hex_2019
health_2017_na <- check_health[comparacao=='2017 NA'] %>%
  tidyr::pivot_wider(
    id_cols = cnes,
    names_from = ano,
    values_from = id_hex,
    names_prefix = 'ano_'
  )
health_2017_na$check <- health_2017_na$ano_2018 == health_2017_na$ano_2019
all(health_2017_na$check==T)
# use complete health data to check 2017 [should be F]
any(check_health[comparacao == '2017 NA']$cnes %in% health_complete$health_complete_2017$cnes)


#### INCORRETO/CORRIGIR GEOLOCALIZACAO
# Two NA
#### INCORRETO/CORRIGIR GEOLOCALIZACAO
# what to check: if hosp with two NA are legitimate (i.e., the hosp aren't present in two CNES df)
# or if there are more than 1 occurence in these dfs and, therefore, NA is a result of a failure..
#..in the geolocalization process

# bind health_complete
bind_health_complete <- health_complete %>%
  purrr::map(~dplyr::select(., cnes, code_muni, estabelecimento, tipo_unidade,
                            logradouro, numero, bairro, cep,
                            health_low, health_med, health_high,
                            MatchedAddress, SearchedAddress, PrecisionDepth, geocode_engine, lon,lat)) %>%
  data.table::rbindlist()

# check number of occurrences of each hosp
number_occurrences_health <- bind_health_complete[ , .N, by = cnes]
# filter which are > 1 and are present in the variation df
health_problem_two_na <-
  check_health[comparacao == 'Two NA' & cnes %in% number_occurrences_health[N > 1]$cnes]


# patterns
check_health <- check_health %>%
  mutate(
    padrao = dplyr::case_when(
      # count
      comparacao == 'All NA' ~ 'INCORRETO/CORRIGIR: Apenas hospitais de 2019 e sem id_hex', # 71
      comparacao == '2019 NA' ~ 'Correto: Hospitais sem observacao em 2019', # 288
      comparacao == '2018 NA' ~ 'Correto: Hospitais sem observacao em 2018 (cnes 2006472)', #2
      comparacao == '2017 NA' ~ 'Correto: Hospitais sem observacao em 2017', # 404
      comparacao == 'Two NA' ~ 'INCORRETO/CORRIGIR: 6 hospitais com Two NA incorreto', # 215
      T ~ 'Ainda não verificado'
    )
  )


#### INCORRETO/CORRIGIR GEOLOCALIZACAO
check_health <- check_health %>%
  dplyr::mutate(
    corrigir = dplyr::case_when(
      comparacao == "All NA" | cnes %in% health_problem_two_na$cnes ~ 'Corrigir',
      T ~ 'Não corrigir/Sem problemas'
    )
  )

# * 3.2 check educ data ---------------------------------------------------


check_educ %>% count(comparacao)

#### INCORRETO/CORRIGIR GEOLOCALIZACAO
# 2019 different
#### INCORRETO/CORRIGIR GEOLOCALIZACAO
# check which munis the observations belong
dplyr::left_join(
  check_educ[comparacao=='2019 different'],
  munis_df %>% select(code_muni, name_muni),
  by = c('code_muni' = 'code_muni')
) %>%
  count(name_muni)
# all 2019 different' have observations on all years (2017-18-19)
unique(check_educ[comparacao=='2019 different', .N, by = co_entidade][]$N)
# same SearchedAddress; difference between MatchedAddress between the years;
# correct which lat-lon (2017-18 or 2019) is correct



# 2019 NA
# check differente between id_hex_2017 & id_hex_2018
educ_2019_na <- check_educ[comparacao=='2019 NA'] %>%
  tidyr::pivot_wider(
    id_cols = co_entidade,
    names_from = ano,
    values_from = id_hex,
    names_prefix = 'ano_'
  )
educ_2019_na$check <- educ_2019_na$ano_2017 == educ_2019_na$ano_2018
all(educ_2019_na$check==T)
# use complete health data to check 2019 [should be F]
any(check_educ[comparacao == '2019 NA']$co_entidade %in% educ_complete$educ_complete_2019$co_entidade)
# RESULTADO: Escolas corretamente sem observacao em 2019

# 2018 NA
#check_educ[comparacao=='2018 NA'] %>% View()
# use complete health data to check 2018 [should be F]
any(check_educ[comparacao == '2018 NA']$co_entidade %in% educ_complete$educ_complete_2018$co_entidade)
# RESULTADO: Escolas corretamente sem observacao em 2018

# 2017 NA
# check differente between id_hex_2018 & id_hex_2019
educ_2017_na <- check_educ[comparacao=='2017 NA'] %>%
  tidyr::pivot_wider(
    id_cols = co_entidade,
    names_from = ano,
    values_from = id_hex,
    names_prefix = 'ano_'
  )
educ_2017_na$check <- educ_2017_na$ano_2018 == educ_2017_na$ano_2019
all(educ_2017_na$check==T)
# use complete health data to check 2017 [should be F]
any(check_educ[comparacao == '2017 NA']$co_entidade %in% educ_complete$educ_complete_2017$co_entidade)
# RESULTADO: Escolas corretamente sem observacao em 2017


# Two NA

# what to check: if educ with two NA are legitimate (i.e., the educ aren't present in two EDUC df)
# or if there are more than 1 occurence in these dfs and, therefore, NA is a result of a failure..
#..in the geolocalization process

# bind educ_complete
bind_educ_complete <- educ_complete %>%
  purrr::map(~dplyr::select(., co_entidade, code_muni, mat_infantil, mat_fundamental,mat_medio,
                            endereco, SearchedAddress, MatchedAddress, PrecisionDepth,
                            geocode_engine, lon,lat)) %>%
  data.table::rbindlist()

# check number of occurrences of each educ
number_occurrences_educ <- bind_educ_complete[ , .N, by = co_entidade]
# filter which are > 1 and are present in the variation df
educ_problem_two_na <-
  check_educ[comparacao == 'Two NA' & co_entidade %in% number_occurrences_educ[N > 1]$co_entidade]

# RESULTADO: no problematic educ with 'Two NA'

# patterns
check_educ <- check_educ %>%
  mutate(
    padrao = dplyr::case_when(
      # count
      comparacao == '2019 different' ~ 'INCORRETO/CORRIGIR: correct manually; check which lat-lon (2017-18 or 2019) is correct', # 114
      comparacao == '2019 NA' ~ 'Correto: Escolas sem observacao em 2019', # 96
      comparacao == '2018 NA' ~ 'Correto: Escolas sem observacao em 2018', # 20
      comparacao == '2017 NA' ~ 'Correto: Escolas sem observacao em 2017', # 206
      comparacao == 'Two NA' ~ 'Correto: Nenhuma escola com problema nos Two NA', # 92
      T ~ 'Ainda não verificado'
    )
  )

##### CORRIGIR
check_educ <- check_educ %>%
  dplyr::mutate(
    corrigir = dplyr::case_when(
      comparacao == "2019 different" ~ 'Corrigir',
      T ~ 'Não corrigir/Sem problemas'
    )
  )


# 3.3 save data to be corrected -------------------------------------------

if (!dir.exists(here('data', 'CORRIGIR'))) {
  dir.create(here('data', 'CORRIGIR'))
}

readr::write_rds(check_health[corrigir=='Corrigir'], here::here('data', 'CORRIGIR', 'corrigir_health.rds'))
#data.table::fwrite(check_health[corrigir=='Corrigir'], here::here('data', 'CORRIGIR', 'corrigir_health.csv'))
readr::write_rds(check_educ[corrigir=='Corrigir'], here::here('data', 'CORRIGIR', 'corrigir_educ.rds'))
#data.table::fwrite(check_educ[corrigir=='Corrigir'], here::here('data', 'CORRIGIR', 'corrigir_educ.csv'))

