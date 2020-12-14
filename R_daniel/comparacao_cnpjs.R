library(ggplot2)
library(data.table)
library(dplyr)


# setup -------------------------------------------------------------------


if (!exists("rais_2017_basica")){

  rais_2017_basica <- readr::read_rds(
    "../../data/acesso_oport/rais/2017/rais_2017_corrigido_geocoded_censoEscolar.rds"
  )
  data.table::setDT(rais_2017_basica)

}

if (!exists("rais_2018_basica")){

  rais_2018_basica <- readr::read_rds(
    "../../data/acesso_oport/rais/2018/rais_2018_corrigido_geocoded_censoEscolar.rds"
  )
  data.table::setDT(rais_2018_basica)

}

if (!exists("rais_2019_basica")){

  rais_2019_basica <- readr::read_rds(
    "../../data/acesso_oport/rais/2019/rais_2019_corrigido_geocoded_censoEscolar.rds"
  )
  data.table::setDT(rais_2019_basica)

}

if (!exists("rais_2017_gmaps")){

  rais_2017_gmaps <- readr::read_rds(
    "../../data/acesso_oport/rais/2017/check/rais_2017_check3.rds"
  )
  data.table::setDT(rais_2017_gmaps)

}

if (!exists("rais_2018_gmaps")){

  rais_2018_gmaps <- readr::read_rds(
    "../../data/acesso_oport/rais/2018/check/rais_2018_check2.rds"
  )
  data.table::setDT(rais_2018_gmaps)

}

if (!exists("rais_2019_gmaps")){

  rais_2019_gmaps <- readr::read_rds(
    "../../data/acesso_oport/rais/2019/check/rais_2019_check1.rds"
  )
  data.table::setDT(rais_2019_gmaps)

}

if (!exists("munis_df")) {

  munis_df <- data.table::setDT(tibble::tribble(
    ~code_muni, ~abrev_muni, ~name_muni,        ~abrev_estado, ~modo_2017, ~modo_2018, ~modo_2019, ~modo_2020,
    2304400,    "for",       "Fortaleza",       "CE",          "todos",    "todos",    "todos",    "todos",
    3550308,    "spo",       "Sao Paulo",       "SP",          "todos",    "todos",    "todos",    "todos",
    3304557,    "rio",       "Rio de Janeiro",  "RJ",          "ativo",    "todos",    "todos",    "todos",
    4106902,    "cur",       "Curitiba",        "PR",          "todos",    "todos",    "todos",    "todos",
    4314902,    "poa",       "Porto Alegre",    "RS",          "todos",    "todos",    "todos",    "todos",
    3106200,    "bho",       "Belo Horizonte",  "MG",          "todos",    "todos",    "todos",    "todos",
    5300108,    "bsb",       "Brasilia",        "DF",          "ativo",    "ativo",    "ativo",    "ativo",
    2927408,    "sal",       "Salvador",        "BA",          "ativo",    "ativo",    "ativo",    "ativo",
    1302603,    "man",       "Manaus",          "AM",          "ativo",    "ativo",    "ativo",    "ativo",
    2611606,    "rec",       "Recife",          "PE",          "ativo",    "ativo",    "todos",    "todos",
    5208707,    "goi",       "Goiania",         "GO",          "ativo",    "ativo",    "todos",    "ativo",
    1501402,    "bel",       "Belem",           "PA",          "ativo",    "ativo",    "ativo",    "ativo",
    3518800,    "gua",       "Guarulhos",       "SP",          "ativo",    "ativo",    "ativo",    "ativo",
    3509502,    "cam",       "Campinas",        "SP",          "todos",    "todos",    "todos",    "ativo",
    2111300,    "slz",       "Sao Luis",        "MA",          "ativo",    "ativo",    "ativo",    "ativo",
    3304904,    "sgo",       "Sao Goncalo",     "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
    2704302,    "mac",       "Maceio",          "AL",          "ativo",    "ativo",    "ativo",    "ativo",
    3301702,    "duq",       "Duque de Caxias", "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
    5002704,    "cgr",       "Campo Grande",    "MS",          "ativo",    "ativo",    "ativo",    "ativo",
    2408102,    "nat",       "Natal",           "RN",          "ativo",    "ativo",    "ativo",    "ativo"
  ))

}


# funções auxiliares ------------------------------------------------------


format_num <- function(x) {

  checkmate::assert_number(x)

  format(x, big.mark = ".", decimal.mark = ",")

}

format_pct <- function(x) {

  checkmate::assert_number(x)

  if (x == 1) return(paste0(x * 100, "%"))

  sub(" ", "", paste0(format(x * 100, decimal.mark = ",", digits = 1, nsmall = 1), "%"))

}

format_rel <- function(x, tot) {

  checkmate::assert_number(x)
  checkmate::assert_number(tot)

  paste0(format_num(x), " (", format_pct(x / tot), ")")

}


# análise -----------------------------------------------------------------


gerar_relatorio <- function(ano, dts_empregos, nomes_etapas) {

  # checa inputs

  checkmate::assert_number(ano)
  checkmate::assert_list(dts_empregos)
  checkmate::assert_character(nomes_etapas)

  # rais inteira

  total_cnpjs_orig <- nrow(dts_empregos[[1]])
  total_emp_orig   <- sum(dts_empregos[[1]]$total, na.rm = TRUE)

  rais_inteira <- lapply(1:length(dts_empregos), function(i) {

    total_cnpjs <- nrow(dts_empregos[[i]])
    empregos_total <- sum(dts_empregos[[i]]$total, na.rm = TRUE)
    empregos_corri <- sum(dts_empregos[[i]]$total_corrigido, na.rm = TRUE)

    data.table::data.table(
      etapa = nomes_etapas[i],
      cnpjs_total = format_rel(total_cnpjs, total_cnpjs_orig),
      emp_total = format_rel(empregos_total, total_emp_orig),
      emp_corri = format_rel(empregos_corri, total_emp_orig)
    )

  })
  rais_inteira <- data.table::rbindlist(rais_inteira)

  # rais por município

  rais_munis <- lapply(1:nrow(munis_df), function(i) {

    code_muni <- substr(munis_df[i]$code_muni, 1, 6)

    rais_orig_muni     <- dts_empregos[[1]][codemun == code_muni]
    total_cnpjs_orig_m <- nrow(rais_orig_muni)
    total_emp_orig_m   <- sum(rais_orig_muni$total, na.rm = TRUE)

    estats_rais <- lapply(1:length(dts_empregos), function(j) {

      rais_muni        <- dts_empregos[[j]][codemun == code_muni]
      total_cnpjs_m    <- nrow(rais_muni)
      empregos_total_m <- sum(rais_muni$total, na.rm = TRUE)
      empregos_corri_m <- sum(rais_muni$total_corrigido, na.rm = TRUE)

      data.table::data.table(
        etapa = nomes_etapas[j],
        cnpjs_total = format_rel(total_cnpjs_m, total_cnpjs_orig_m),
        emp_total = format_rel(empregos_total_m, total_emp_orig_m),
        emp_corri = format_rel(empregos_corri_m, total_emp_orig_m)
      )

    })
    estats_rais <- data.table::rbindlist(estats_rais)

  })
  rais_munis <- stats::setNames(rais_munis, munis_df$name_muni)

  # gera relatório

  diretorio <- paste0("./reports/cnpjs_por_etapa")
  if (!dir.exists(diretorio)) dir.create(diretorio)

  arquivo_relatorio <- normalizePath(paste0(diretorio, "/analise_", ano, ".html"))

  rmarkdown::render(
    "./R_daniel/esqueleto_rais.Rmd",
    output_file = arquivo_relatorio,
    params = list(
      ano = ano,
      rais_inteira = rais_inteira,
      rais_munis = rais_munis
    ),
    quiet = TRUE
  )

}

atualizar_relatorios <- function() {

  boa_precisao <- c("4 Estrelas", "3 Estrelas", "street_number", "route")
  nome_etapas <- paste0("Fase ", 1:4)

  gerar_relatorio(
    2017,
    list(
      rais_2017_basica,
      rais_2017_basica[PrecisionDepth %chin% boa_precisao],
      rais_2017_gmaps[PrecisionDepth %chin% boa_precisao],
      rais_2017_gmaps[PrecisionDepth %chin% boa_precisao][! cnae.setor %chin% c("78", "80")]
    ),
    nome_etapas
  )

  gerar_relatorio(
    2018,
    list(
      rais_2018_basica,
      rais_2018_basica[PrecisionDepth %chin% boa_precisao],
      rais_2018_gmaps[PrecisionDepth %chin% boa_precisao],
      rais_2018_gmaps[PrecisionDepth %chin% boa_precisao][! cnae.setor %chin% c("78", "80")]
    ),
    nome_etapas
  )

  gerar_relatorio(
    2019,
    list(
      rais_2019_basica,
      rais_2019_basica[PrecisionDepth %chin% boa_precisao],
      rais_2019_gmaps[PrecisionDepth %chin% boa_precisao],
      rais_2019_gmaps[PrecisionDepth %chin% boa_precisao][! cnae.setor %chin% c("78", "80")]
    ),
    nome_etapas
  )

}
