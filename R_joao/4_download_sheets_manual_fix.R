
# 0 load setup ------------------------------------------------------------

source('R_joao/setup_jpparga.R')

# 1 education -------------------------------------------------------------

# * 1.1 download data -----------------------------------------------------

educ <- googlesheets4::read_sheet(
  ss = 'https://docs.google.com/spreadsheets/d/1pDSZb3T9A9nGQ2vcBlS6d_voJ8sqLm44P8T_I3ctdlQ/edit#gid=643478619',
  sheet = 'corrigir_educ',
  range = "corrigir_educ!C:J"
)

# * 1.2 clean data --------------------------------------------------------

# setDT
data.table::setDT(educ)

# criar colunas tipo de correcao (correcao) e o que fazer (acao)
educ[
  ,
  `:=`(correcao = "mudanca_hex_anos",
       acao = data.table::fifelse(is.na(lat), "remover", "atualizar"))
]

# criar coluna `motivo` (da acao que sera tomada)
educ[
  ,
  motivo := data.table::fifelse(acao == "remover", "nao_encontrado", "endereco_OK")
]

# mudar nome das colunas
data.table::setnames(educ, c("co_entidade"), c("id"))

# remover colunas indesejadas
educ[, c("comparacao") := NULL]

# reordenar colunas
data.table::setcolorder(
  educ,
  c("id","lon","lat","ano","no_entidade","code_muni","name_muni","correcao","acao","motivo")
  )


# * 1.3 save data ---------------------------------------------------------

# criar diretorio (se necessario)
if (!dir.exists(here('data', 'geocode_manual_fix'))) {
  dir.create(here('data', 'geocode_manual_fix'))
}

# salvar dados
data.table::fwrite(educ, here::here('data','geocode_manual_fix', "schools_good_PD.csv"))


# 2 health ----------------------------------------------------------------

# * 2.1 download data -----------------------------------------------------

health <- googlesheets4::read_sheet(
  ss = 'https://docs.google.com/spreadsheets/d/1pDSZb3T9A9nGQ2vcBlS6d_voJ8sqLm44P8T_I3ctdlQ/edit#gid=643478619',
  sheet = 'corrigir_health',
  range = "corrigir_health!C:Q"
)

# * 2.2 clean data --------------------------------------------------------

# setDT
data.table::setDT(health)

# remover colunas desnecessarias
health[
  ,
  c('cep','comparacao','SearchedAddress', 'MatchedAddress', 'limite', 'PrecisionDepth','geocode_engine') := NULL
]

# criar colunas tipo de correcao (correcao) e o que fazer (acao)
health[
  ,
  `:=`(correcao = "mudanca_hex_anos",
       acao = data.table::fifelse(is.na(lat) | !is.na(obs), "remover", "atualizar"))
]

# criar coluna `motivo` (da acao que sera tomada)
health[
  ,
  motivo := dplyr::case_when(
    is.na(lat) ~ "nao_encontrado",
    !is.na(obs) ~ "tipo_estab_incorreto",
    T ~ "endereco_OK"
  )
]

# mudar nome das colunas
data.table::setnames(health, c("cnes"), c("id"))

# reordenar colunas
data.table::setcolorder(
  health,
  c("id","lon","lat","ano","estabelecimento","code_muni","name_muni","correcao","acao","motivo","obs")
)


# * 2.3 save data ---------------------------------------------------------

# criar diretorio (se necessario)
if (!dir.exists(here('data', 'geocode_manual_fix'))) {
  dir.create(here('data', 'geocode_manual_fix'))
}

# salvar dados
data.table::fwrite(health, here::here('data','geocode_manual_fix', "hospitals_good_PD.csv"))

