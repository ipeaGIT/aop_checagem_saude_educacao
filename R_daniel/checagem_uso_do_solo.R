library(ggplot2)
library(data.table)
library(dplyr)


# setup -------------------------------------------------------------------


if (!exists("empregos_2017")) {

  empregos_2017 <- readr::read_rds(
    "../../data/acesso_oport/rais/2017/rais_2017_etapa8.rds"
  )
  data.table::setDT(empregos_2017)

}

if (!exists("empregos_2018")) {

  empregos_2018 <- readr::read_rds(
    "../../data/acesso_oport/rais/2018/rais_2018_etapa10.rds"
  )
  data.table::setDT(empregos_2018)

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


# análises e visualizações ------------------------------------------------


comparacao_uso_do_solo <- function(sigla_muni,
                                   oport = c("empregos", "saude", "edu"),
                                   mapas = FALSE,
                                   relatorio = FALSE) {

  nome_muni <- munis_df[abrev_muni == sigla_muni]$name_muni
  message(paste0(nome_muni, ":"))

  # lê e prepara dados de uso do solo

  grid_2017 <- readr::read_rds(sprintf("../../data/acesso_oport/hex_agregados/2017/hex_agregado_%s_09_2017.rds", sigla_muni))
  data.table::setDT(grid_2017)
  grid_2018 <- readr::read_rds(sprintf("../../data/acesso_oport/hex_agregados/2018/hex_agregado_%s_09_2018.rds", sigla_muni))
  data.table::setDT(grid_2018)
  grid_2019 <- readr::read_rds(sprintf("../../data/acesso_oport/hex_agregados/2019/hex_agregado_%s_09_2019.rds", sigla_muni))
  data.table::setDT(grid_2019)

  uso_do_solo <- rbind(grid_2017, grid_2018, grid_2019, idcol = "ano")
  uso_do_solo[, ano := ano + 2016]

  # prepara dados segundo tipo de oportunidade desejada

  oport_desejada <- oport[1]

  variavel_desejada <- paste0(oport_desejada, "_total")
  uso_do_solo <- uso_do_solo[, .(ano, id_hex, oport = get(variavel_desejada), geometry)]

  # calcula diferença entre cada combinação de 2 anos distintos

  comb_anos <- data.table::as.data.table(utils::combn(2017:2019, 2))

  dif_uso_do_solo <- lapply(
    comb_anos,
    function(anos) {

      ano_max <- as.character(max(anos))
      ano_min <- as.character(min(anos))

      diferenca <- uso_do_solo[ano %in% anos]
      diferenca <- data.table::dcast(diferenca, id_hex ~ ano, value.var = "oport")
      diferenca[, diferenca := get(ano_max) - get(ano_min)]
      diferenca[, id_anos := paste0(ano_min, " -> ", ano_max)]
      diferenca[, (ano_max) := NULL]
      diferenca[, (ano_min) := NULL]

    }
  )
  dif_uso_do_solo <- data.table::rbindlist(dif_uso_do_solo)
  dif_uso_do_solo <- dif_uso_do_solo[!is.na(diferenca)]

  # criar objeto com geometria pra usar mais a frente

  geometria <- grid_2017[, .(id_hex, geometry)]

  # cria diretorios para salvar resultados

  diretorio_raiz <- paste0("./reports/distribuicao_uso_do_solo")
  if (!dir.exists(diretorio_raiz)) dir.create(diretorio_raiz)

  diretorio_muni <- paste0(diretorio_raiz, "/", sigla_muni)
  if (!dir.exists(diretorio_muni)) dir.create(diretorio_muni)


  # * mapas comparativos ----------------------------------------------------


  if (mapas) {

    borda_cidade <- geobr::read_municipality(
      munis_df[abrev_muni == sigla_muni]$code_muni,
      year = 2010
    )

    bbox <- sf::st_bbox(sf::st_as_sf(geometria))
    asp_ratio <- (bbox$xmax - bbox$xmin) / (bbox$ymax - bbox$ymin)

    espacamento <- 6.25 - 5 * asp_ratio

    # grafico 1: mapa da distribuição das atividades por ano

    p1 <- ggplot() +
      geom_sf(data = borda_cidade, fill = "gray90") +
      geom_sf(data = sf::st_as_sf(uso_do_solo), aes(fill = oport), color = NA) +
      facet_wrap(~ ano, nrow = 1) +
      scale_fill_viridis_c(name = oport_desejada, option = "inferno") +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.spacing.x = unit(ifelse(espacamento <= 0.5, 0.5, espacamento), "inches")
      )

    # grafico 2: boxplot da distribuição da diferença entre cada ano

    p2 <- ggplot() +
      geom_boxplot(data = dif_uso_do_solo, aes(diferenca)) +
      facet_wrap(~ id_anos, nrow = 1) +
      scale_x_continuous(name = "diferença") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), panel.spacing.x = unit(0.5, "inches"))

    # grafico 3: mapa da distribuição da diferença entre cada ano

    dif_uso_do_solo[geometria, on = "id_hex", geometry := i.geometry]

    p3 <- ggplot() +
      geom_sf(data = borda_cidade, fill = "white") +
      geom_sf(data = sf::st_as_sf(dif_uso_do_solo), aes(fill = diferenca), color = NA) +
      geom_sf(data = borda_cidade, fill = NA) +
      scale_fill_gradient2(name = "diferença", low = "firebrick4", high = "dodgerblue4") +
      facet_wrap(~ id_anos, nrow = 1) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.spacing.x = unit(ifelse(espacamento <= 0.5, 0.5, espacamento), "inches")
      )

    # junta os gráficos 1-3 em uma mesma visualização

    nome_oport <- data.table::fifelse(oport_desejada == "edu", "educação", oport_desejada)

    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        paste0("Distribuição de oportunidades de ", nome_oport, " - ", nome_muni),
        fontface = "bold",
        x = 0.03,
        hjust = 0,
        vjust = 1
      )

    p2w <- cowplot::plot_grid(p2, ncol = 2, rel_widths = c(1, 0.08))

    p <- cowplot::plot_grid(title, p1, p3, p2w, rel_heights = c(0.06, 1, 1, 0.5), ncol = 1)

    # salva visualizações

    arquivo_distribuicao <- paste0(diretorio_muni, "/", oport_desejada, ".png")

    ggsave(
      arquivo_distribuicao,
      plot = p,
      units = "in",
      width = 12.5,
      height = 5.8,
      dpi = 150
    )

    return(0)

  }


  # * relatório -------------------------------------------------------------


  if (relatorio & oport_desejada == "empregos") {

    message("    Gerando relatório")

    # como os dados de 2019 são iguais aos de 2018, por enquanto, filtra o df de
    # diferença pra que ele tenha apenas as diferenças entre 2017 e 2018

    dif_uso_do_solo <- dif_uso_do_solo[id_anos == "2017 -> 2018"]

    # definição de outlier (arbitrária): hexágonos cuja diferença supere 1/3 do
    # máximo de empregos em apenas um hexágono na cidade

    maximo_empregos <- max(uso_do_solo$oport)

    outliers <- dif_uso_do_solo[diferenca > maximo_empregos / 3]

    # procura CNPJs que estejam nesses hexágonos em 2017 e em 2018

    cnpjs_outliers_2017 <- acha_cnpjs_hexagonos(
      sigla_muni,
      outliers$id_hex,
      geometria,
      empregos_2017,
      2017
    )

    cnpjs_outliers_2018 <- acha_cnpjs_hexagonos(
      sigla_muni,
      outliers$id_hex,
      geometria,
      empregos_2018,
      2018
    )

    return(cnpjs_outliers_2017)

    # une as duas bases a fins de comparação

    if (nrow(outliers) > 0) {

      cnpjs_outliers <- merge(
        cnpjs_outliers_2017,
        cnpjs_outliers_2018,
        by = c("id_hex", "id_estab"),
        all = TRUE,
        suffixes = c("_2017", "_2018")
      )

    } else {

      cnpjs_outliers <- data.table::data.table(NULL)

    }

    # gera e salva relatório

    arquivo_outliers <- tempfile(pattern = "outliers", fileext = ".csv")
    data.table::fwrite(outliers, arquivo_outliers)

    arquivo_cnpjs <- tempfile(pattern = "cnpjs", fileext = ".csv")
    suppressWarnings(data.table::fwrite(cnpjs_outliers, arquivo_cnpjs))

    if (nrow(outliers) == 0) message("    Nenhum outlier foi encontrado")

    arquivo_relatorio <- normalizePath(file.path(diretorio_muni, "empregos_outliers.html"))

    rmarkdown::render(
      "./R_daniel/esqueleto_relatorio.Rmd",
      output_file = arquivo_relatorio,
      params = list(
        cidade = nome_muni,
        maximo_empregos = maximo_empregos,
        arquivo_outliers = arquivo_outliers,
        arquivo_cnpjs = arquivo_cnpjs
      ),
      quiet = TRUE
    )

  } else {

    # no caso de saúde e educação, hexágonos podem ser considerados outliers por
    # duas razões diferentes (arbitrárias):
    # primeira: caso nele 2 ou mais estabelecimentos tenham surgido/desaparecido
    # segundo:  caso nele tenha havido alguma alteração entre 2017 e 2018 e
    # depois em 2018 e 2019, no "sentido inverso" (e.g. tenha surgido um
    # estabelecimento de saúde de 2017 e 2018 e desaparecido também um em 2018 e
    # 2019)

    # checando o primeiro caso

    outliers_caso_1 <- dif_uso_do_solo[diferenca >= 2]

    # checando o segundo caso

    outliers_caso_2 <- data.table::dcast(
      dif_uso_do_solo,
      id_hex ~ id_anos,
      value.var = "diferenca"
    )
    outliers_caso_2 <- outliers_caso_2[
      (`2017 -> 2018` < 0 & `2018 -> 2019` > 0) | (`2017 -> 2018` > 0 & `2018 -> 2019` < 0)
    ]

  }

}

acha_cnpjs_hexagonos <- function(sigla_muni, hexagonos, geometria, empregos, ano) {

  # acha código do município

  codigo_municipio <- munis_df[abrev_muni == sigla_muni]$code_muni

  # converte empregos em sf

  boa_precisao <- c("4 Estrelas", "3 Estrelas", "street_number", "route")

  empregos_geo <- empregos[codemun == substr(codigo_municipio, 1, 6)]
  empregos_geo <- empregos_geo[PrecisionDepth %chin% boa_precisao]
  empregos_geo <- sf::st_as_sf(empregos_geo, coords = c("lon", "lat"), crs = 4326)
  empregos_geo <- sf::st_transform(empregos_geo, 5880)

  # converte vetor de hexágonos como caracteres para sf

  hexagonos <- data.table::data.table(id_hex = hexagonos)
  hexagonos[geometria, on = "id_hex", geometry := i.geometry]
  hexagonos <- sf::st_as_sf(hexagonos)
  hexagonos <- sf::st_transform(hexagonos, 5880)

  # filtra apenas empregos que estejam nos hexágonos

  empregos_hex <- sf::st_intersects(empregos_geo, hexagonos, sparse = FALSE)

  if (ncol(empregos_hex) > 0) {

    # cria data.table com cnpjs em cada hexágono

    cnpjs_hexagonos <- lapply(1:nrow(hexagonos), function(i) {

      cnpjs_por_hex <- data.table::as.data.table(empregos_geo[empregos_hex[, i], ])

     if (ano == 2017) {

       cnpjs_por_hex <- cnpjs_por_hex[
         ,
         .(id_estab, total_corrigido, type_input_galileo)
         ]

     } else {

       cnpjs_por_hex <- cnpjs_por_hex[
         ,
         .(id_estab, total_corrigido, type_input_galileo, razao_social)
         ]

     }

      cnpjs_por_hex[, id_hex := hexagonos$id_hex[i]]

    })
    cnpjs_hexagonos <- data.table::rbindlist(cnpjs_hexagonos)
    data.table::setcolorder(cnpjs_hexagonos, "id_hex")
    data.table::setnames(
      cnpjs_hexagonos,
      c("total_corrigido", "type_input_galileo"),
      c("total", "input"),
      skip_absent = TRUE
    )

  } else {

    # cria data.table vazio

    cnpjs_hexagonos <- data.table::data.table(NULL)

  }

  return(cnpjs_hexagonos)

}

gera_mapas <- function(n_cores) {

  future::plan(future::multisession, workers = n_cores)

  invisible(furrr::future_map(munis_df$abrev_muni, function(i) {
    comparacao_uso_do_solo(i, "empregos", mapas = TRUE, relatorio = FALSE)
    comparacao_uso_do_solo(i, "saude", mapas = TRUE, relatorio = FALSE)
    comparacao_uso_do_solo(i, "edu", mapas = TRUE, relatorio = FALSE)
  }))

  future::plan(future::sequential)

}

gera_relatorios <- function() {

  invisible(lapply(munis_df$abrev_muni, function(i) {
    comparacao_uso_do_solo(i, "empregos", mapas = FALSE, relatorio = TRUE)
  }))

}
