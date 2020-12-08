library(ggplot2)
library(data.table)
library(dplyr)

options(future.globals.maxSize = 850 * 1024 ^ 2)


# setup -------------------------------------------------------------------


if (!exists("empregos_2017")) {

  empregos_2017 <- readr::read_rds(
    "../../data/acesso_oport/rais/2017/rais_2017_etapa9.rds"
  )
  data.table::setDT(empregos_2017)

}

if (!exists("empregos_2018")) {

  empregos_2018 <- readr::read_rds(
    "../../data/acesso_oport/rais/2018/rais_2018_etapa10.rds"
  )
  data.table::setDT(empregos_2018)

}

if (!exists("empregos_2019")) {

  empregos_2019 <- readr::read_rds(
    "../../data/acesso_oport/rais/2019/rais_2019_corrigido_geocoded_censoEscolar.rds"
  )
  data.table::setDT(empregos_2019)

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

if (!exists("limite_diferenca")) {

  limite_diferenca <- data.table::setDT(tibble::tribble(
    ~abrev_muni, ~limite,
    "for",       1250,
    "spo",       2000,
    "rio",       1800,
    "cur",       1000,
    "poa",       1250,
    "bho",       1250,
    "bsb",       2500,
    "sal",       1000,
    "man",       750,
    "rec",       1500,
    "goi",       1100,
    "bel",       500,
    "gua",       750,
    "cam",       1500,
    "slz",       1250,
    "sgo",       250,
    "mac",       450,
    "duq",       1500,
    "cgr",       250,
    "nat",       600
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

    # cria diretório temporário para salvar data.tables e gerar relatório

    diretorio_temp <- file.path(tempdir(), paste0("dados_", sigla_muni))
    unlink(diretorio_temp, recursive = TRUE)
    dir.create(diretorio_temp)

    # outlier (arbitrária) são hexágonos que superam o limite de diferença entre
    # dois anos consecutivos em uma cidade

    limite_diferenca <- limite_diferenca[abrev_muni == sigla_muni]$limite

    outliers <- dif_uso_do_solo[abs(diferenca) > limite_diferenca]

    if (nrow(outliers) == 0) message("    Nenhum outlier foi encontrado")

    # salva outliers no diretório temporário

    arquivo_outliers <- file.path(diretorio_temp, "outliers.csv")
    data.table::fwrite(outliers, arquivo_outliers)

    # para cada par de anos encontra outliers e os analisa, caso existam

    for (par_anos in comb_anos) {

      id_string <- paste0(par_anos[1], " -> ", par_anos[2])

      outliers_par <- outliers[id_anos == id_string]

      if (nrow(outliers_par) > 0) {

        empregos_min <- paste0("empregos_", par_anos[1])

        # procura CNPJs que estejam nesses hexágonos em cada ano

        cnpjs_outliers_min <- acha_cnpjs_hexagonos(
          sigla_muni,
          outliers_par$id_hex,
          geometria,
          get(paste0("empregos_", par_anos[1])),
          par_anos[1]
        )

        cnpjs_outliers_max <- acha_cnpjs_hexagonos(
          sigla_muni,
          outliers_par$id_hex,
          geometria,
          get(paste0("empregos_", par_anos[2])),
          par_anos[2]
        )

        # une as duas bases a fins de comparação

        cnpjs_outliers <- merge(
          cnpjs_outliers_min,
          cnpjs_outliers_max,
          by = c("id_hex", "id_estab"),
          all = TRUE,
          suffixes = paste0("_", par_anos)
        )

      } else {

        cnpjs_outliers <- data.table::data.table(NULL)

      }

      # salva cnps_outliers no diretório temporário

      arquivo_cnpjs <- file.path(
        diretorio_temp,
        paste0("cnpjs_", par_anos[1], "_", par_anos[2], ".csv")
      )
      suppressWarnings(data.table::fwrite(cnpjs_outliers, arquivo_cnpjs))

      if (par_anos[1] == 2017) {

        # procura estabelecimentos que em 2018/2019 tenham o input = "rais_2017"
        # mas não aparecem naquele hexágono em 2017, e acha o hexágonos em que
        # eles estavam

        if (nrow(cnpjs_outliers) == 0) {

          # se não há outliers, retorna um data.table nulo

          relacao_hexagonos <- data.table::data.table(NULL)

        } else {

          nome_coluna <- paste0("input_", par_anos[2])
          estab_estranhos <- cnpjs_outliers[get(nome_coluna) == "rais_2017" & is.na(total_2017)]

          # lida com o caso do estabelecimento existir na rais em 2017, mas em
          # outro municipio

          empregos_2017_filtrados <- empregos_2017[id_estab %chin% estab_estranhos$id_estab]
          codigo_municipio <- munis_df[abrev_muni == sigla_muni]$code_muni
          empregos_2017_filtrados <- empregos_2017_filtrados[codemun == codigo_municipio]

          if (nrow(estab_estranhos) == 0 | nrow(empregos_2017_filtrados) == 0) {

            # se não há estabelecimentos estranhos, retorna um data.table nulo

            relacao_hexagonos <- data.table::data.table(NULL)

          } else {

            # checa em que hexágonos esses estabelecimentos estavam em 2017

            hexagonos_2017 <- acha_cnpjs_hexagonos(
              sigla_muni,
              geometria$id_hex,
              geometria,
              empregos_2017_filtrados,
              2017
            )

            # cria data.table consolidando diferenças

            relacao_hexagonos <- data.table::data.table(
              id_estab = estab_estranhos$id_estab,
              id_hex_max = estab_estranhos$id_hex
            )

            relacao_hexagonos[hexagonos_2017, on = "id_estab", id_hex_2017 := id_hex]
            data.table::setnames(
              relacao_hexagonos,
              "id_hex_max",
              paste0("id_hex_", par_anos[2])
            )

          }

        }

        # salva relacao_hexagonos no diretório temporário

        arquivo_relacao <- file.path(
          diretorio_temp,
          paste0("relacao_2017_", par_anos[2], ".csv")
        )
        suppressWarnings(data.table::fwrite(relacao_hexagonos, arquivo_relacao))

      }

    }

    # gera e salva relatório

    arquivo_relatorio <- normalizePath(file.path(diretorio_muni, "empregos_outliers.html"))

    rmarkdown::render(
      "./R_daniel/esqueleto_relatorio.Rmd",
      output_file = arquivo_relatorio,
      params = list(
        cidade = nome_muni,
        limite = limite_diferenca,
        diretorio = diretorio_temp
      ),
      quiet = TRUE
    )

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

     if (ano %in% c(2017, 2019)) {

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
