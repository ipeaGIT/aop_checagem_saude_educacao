library(ggplot2)
library(data.table)
library(dplyr)


# setup -------------------------------------------------------------------


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

if (!exists("limite_empregos")) {

  limite_empregos <- data.table::setDT(tibble::tribble(
    ~abrev_muni, ~limite,
    "for",       1000,
    "spo",       3000,
    "rio",       2000,
    "cur",       1000,
    "poa",       1000,
    "bho",       1500,
    "bsb",       1000,
    "sal",       1500,
    "man",       1000,
    "rec",       1000,
    "goi",       1000,
    "bel",       1000,
    "gua",       1000,
    "cam",       1000,
    "slz",       1000,
    "sgo",       500,
    "mac",       500,
    "duq",       500,
    "cgr",       750,
    "nat",       750
  ))

}


# funções auxiliares ------------------------------------------------------


filtra_rais <- function(ano, setores_problematicos, boa_precisao, codigo_muni) {

  rais_filtrada <- get(paste0("rais_", ano, "_gmaps"))
  rais_filtrada <- rais_filtrada[! cnae.setor %chin% setores_problematicos]
  rais_filtrada <- rais_filtrada[PrecisionDepth %chin% boa_precisao]
  rais_filtrada <- rais_filtrada[codemun == codigo_muni]
  rais_filtrada[, link := paste0("[Link](https://www.google.com/maps/search/", MatchedAddress, ",/@554m/data=!3m1!1e3)")]

}


# análise -----------------------------------------------------------------


analisar_cnpjs_probs <- function() {

  # setores problematicos - ver propostas_outliers.R pra mais detalhes sobre cada setor

  setores_problematicos <- c(
    "35","36","49","51","82","38", "78", "80", "41", "42", "43", "64", "81", "84"
  )

  # boa precisão

  boa_precisao <- c("4 Estrelas", "3 Estrelas", "street_number", "route")

  # percorre os empregos de cada cidade do projeto e gera relatório com outliers

  for (i in seq.int(nrow(munis_df))) {

    codigo_muni <- substr(munis_df$code_muni[i], 1, 6)
    sigla_muni  <- munis_df$abrev_muni[i]
    nome_muni   <- munis_df$name_muni[i]
    limite_muni <- limite_empregos$limite[i]

    # filtra rais para conter apenas cnpjs do município

    rais_muni_2017 <- filtra_rais(2017, setores_problematicos, boa_precisao, codigo_muni)
    rais_muni_2018 <- filtra_rais(2018, setores_problematicos, boa_precisao, codigo_muni)
    rais_muni_2019 <- filtra_rais(2019, setores_problematicos, boa_precisao, codigo_muni)

    # gera relatório

    diretorio <- paste0("./reports/cnpjs_problematicos")
    if (!dir.exists(diretorio)) dir.create(diretorio)

    arquivo_relatorio <- normalizePath(paste0(diretorio, "/cnpjs_", sigla_muni, ".html"))

    rmarkdown::render(
      "./R_daniel/esqueleto_cnpjs_probs.Rmd",
      output_file = arquivo_relatorio,
      params = list(
        muni = nome_muni,
        rais_2017 = rais_muni_2017,
        rais_2018 = rais_muni_2018,
        rais_2019 = rais_muni_2019,
        limite = limite_muni
      ),
      quiet = TRUE
    )

  }

}
