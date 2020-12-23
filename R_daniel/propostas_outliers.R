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


gerar_outliers <- function(rais, setores) {

  # possíveis outliers segundo valor dos quantis

  outs <- rais[
    cnae.setor %chin% setores,
    .(
      p70 = quantile(total, 0.70),
      p75 = quantile(total, 0.75),
      p80 = quantile(total, 0.80),
      p85 = quantile(total, 0.85),
      p90 = quantile(total, 0.90),
      p95 = quantile(total, 0.95)
    ),
    keyby = .(cnae.setor)
  ]

  # calcula valor do intervalo interquartílico da dist entre o p90 e p100

  iqr_decil <- rais[outs[, .(cnae.setor, p90)], on = "cnae.setor", nomatch = NULL]
  iqr_decil <- iqr_decil[total >= p90, .(iqr_90_100 = IQR(total)), keyby = .(cnae.setor)]

  # possíveis outliers segundo valor do p90 e iqr

  outs[iqr_decil, on = "cnae.setor", iqr_90_100 := i.iqr_90_100]
  outs[
    ,
    `:=`(
      iqr05 = p90 + 0.5 * iqr_90_100,
      iqr10 = p90 + 1.0 * iqr_90_100,
      iqr15 = p90 + 1.5 * iqr_90_100,
      iqr20 = p90 + 2.0 * iqr_90_100,
      iqr25 = p90 + 2.5 * iqr_90_100,
      iqr30 = p90 + 3.0 * iqr_90_100
    )
  ]

}

analisar_outliers <- function(ano) {

  # setores problematicos

  # 35 eletricidade, gas e agua quente OK
  # 36 captacao, tratamento e distribuicao de agua OK
  # 49 transporte terrestre OK
  # 51 transporte aereo OK
  # 82 servicoes prestados principalmente a empresas
  # 38 limpeza urbana, esgoto e atividades relacionadas (COLETA, TRATAMENTO E DISPOSIÇÃO DE RESÍDUOS; RECUPERAÇÃO DE MATERIAIS 2.0)
  # 78 SELEÇÃO, AGENCIAMENTO E LOCAÇÃO DE MÃO-DE-OBRA
  # 80 ATIVIDADES DE VIGILÂNCIA, SEGURANÇA E INVESTIGAÇÃO
  # 41 CONSTRUÇÃO DE EDIFÍCIOS
  # 42 OBRAS DE INFRA-ESTRUTURA
  # 43 SERVIÇOS ESPECIALIZADOS PARA CONSTRUÇÃO
  # 64 ATIVIDADES DE SERVIÇOS FINANCEIROS
  # 81 SERVIÇOS PARA EDIFÍCIOS E ATIVIDADES PAISAGÍSTICAS

  setores_problematicos <- c(
    "35","36","49","51","82","38", "78", "80", "41", "42", "43", "64", "81", "84"
  )

  # processar bases

  rais     <- data.table::copy(get(paste0("rais_", ano, "_gmaps")))
  outliers <- gerar_outliers(rais, setores_problematicos)
  rais     <- outliers[rais, on = "cnae.setor"]

  # formata os numeros em "outliers" pra ficarem bonitinhos

  outliers <- outliers[
    ,
    lapply(.SD, format, digits = 1, nsmall = 1, big.mark = ".", decimal.mark = ","),
    .SDcols = names(outliers)
  ]

  # calcula o total corrigido pra cada caso

  nomes <- c(
    paste0("p", seq(70, 95, 5)),
    paste0("iqr", formatC(seq(5, 30, 5), width = 2, flag = 0))
  )

  for (nome in nomes) {

    coluna <- paste0("tc_", nome)
    rais[
      cnae.setor %chin% setores_problematicos,
      (coluna) := data.table::fifelse(total > get(nome), get(nome), total)
    ]
    rais[
      ! cnae.setor %chin% setores_problematicos,
      (coluna) := total_corrigido
    ]

  }

  # corrige a coluna 'total' para que ela passe a considerar tb empregos do inep

  rais[is.na(total), total := total_corrigido]

  # soma total de empregos pra calcular porcentagens de empregos considerados

  soma_total <- sum(rais$total)

  # calcula percentagem de empregos considerados na rais em cada caso

  boa_precisao <- c("4 Estrelas", "3 Estrelas", "street_number", "route")

  outliers_inteira <- data.table::data.table(
    muni = "RAIS (gmaps)",
    tot = format_rel(soma_total, soma_total)
  )

  nomes <- c(paste0("tc_", nomes), "total_corrigido")

  for (coluna in nomes) {
    soma_out <- rais[PrecisionDepth %chin% boa_precisao, sum(get(coluna))]
    outliers_inteira[, (coluna) := format_rel(soma_out, soma_total)]
  }

  # calcula percentagem de empregos considerados na rais em cada caso em cada cidade

  outliers_munis <- lapply(1:nrow(munis_df), function(i) {

    codigo <- substr(munis_df$code_muni[i], 1, 6)
    sigla  <- munis_df$abrev_muni[i]
    soma_total_muni <- sum(rais[codemun == codigo]$total)

    muni <- data.table::data.table(
      muni = sigla,
      tot = format_rel(soma_total_muni, soma_total_muni)
    )

    for (coluna in nomes) {
      soma_out_muni <- rais[
        PrecisionDepth %chin% boa_precisao & codemun == codigo,
        sum(get(coluna))
      ]
      muni[, (coluna) := format_rel(soma_out_muni, soma_total_muni)]
    }

    return(muni)

  })
  outliers_munis <- data.table::rbindlist(outliers_munis)

  # gera relatório

  diretorio <- paste0("./reports/proposta_outliers")
  if (!dir.exists(diretorio)) dir.create(diretorio)

  arquivo_relatorio <- normalizePath(paste0(diretorio, "/outliers_", ano, ".html"))

  rmarkdown::render(
    "./R_daniel/esqueleto_outliers.Rmd",
    output_file = arquivo_relatorio,
    params = list(
      ano = ano,
      outliers = outliers,
      outliers_inteira = outliers_inteira,
      outliers_munis = outliers_munis
    ),
    quiet = TRUE
  )

}

atualizar_outliers <- function() {

  analisar_outliers(2017)
  analisar_outliers(2018)
  analisar_outliers(2019)

}



