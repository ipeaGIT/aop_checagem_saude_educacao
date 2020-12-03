library(ggplot2)

# setup

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

# análises e visualizações

comparacao_uso_do_solo <- function(sigla_muni, oport = c("empregos", "saude", "edu"), gerar_mapas = FALSE) {
  
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
  
  # cria diretorios para salvar resultados
  
  diretorio_raiz <- paste0("./reports/distribuicao_uso_do_solo")
  if (!dir.exists(diretorio_raiz)) dir.create(diretorio_raiz)
  
  diretorio_muni <- paste0(diretorio_raiz, "/", sigla_muni)
  if (!dir.exists(diretorio_muni)) dir.create(diretorio_muni)
  
  
  # * mapas comparativos ----------------------------------------------------
  
  
  if (gerar_mapas) {
    
    borda_cidade <- geobr::read_municipality(
      munis_df[abrev_muni == sigla_muni]$code_muni,
      year = 2010
    )
    
    geometria <- grid_2017[, .(id_hex, geometry)]
    
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
    nome_muni <- munis_df[abrev_muni == sigla_muni]$name_muni
    
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
    
  }
  
  
  # * hexagonos possivelmente problemáticos ---------------------------------
  
  
  if (oport_desejada == "empregos") {
    
    
    
  }
  
  return(uso_do_solo[])
  
}

gera_mapa_comparacoes <- function(n_cores) {
  
  future::plan(future::multisession, workers = n_cores)
  
  invisible(furrr::future_map(munis_df$abrev_muni, function(i) {
    comparacao_uso_do_solo(i, "empregos")
    comparacao_uso_do_solo(i, "saude")
    comparacao_uso_do_solo(i, "edu")
  }))
  
  future::plan(future::sequential)
  
}
