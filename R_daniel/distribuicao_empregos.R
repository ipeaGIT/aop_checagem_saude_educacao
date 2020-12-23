library(ggplot2)
library(data.table)
library(dplyr)

# setup -------------------------------------------------------------------


if (!exists("rais_2017_filtrada")){

  rais_2017_basica <- readr::read_rds(
    "../../data/acesso_oport/rais/2017/rais_2017_corrigido_geocoded_censoEscolar.rds"
  )
  data.table::setDT(rais_2017_basica)
  rais_2017_filtrada <- rais_2017_basica[
    !is.na(total),
    .(id_estab, cnae.setor, total, alto, baixo, medio, total_corrigido, codemun)
  ]

}

if (!exists("rais_2018_filtrada")){

  rais_2018_basica <- readr::read_rds(
    "../../data/acesso_oport/rais/2018/rais_2018_corrigido_geocoded_censoEscolar.rds"
  )
  data.table::setDT(rais_2018_basica)
  rais_2018_filtrada <- rais_2018_basica[
    !is.na(total),
    .(id_estab, cnae.setor, total, alto, baixo, medio, total_corrigido, codemun)
  ]

}

if (!exists("rais_2019_filtrada")){

  rais_2019_basica <- readr::read_rds(
    "../../data/acesso_oport/rais/2019/rais_2019_corrigido_geocoded_censoEscolar.rds"
  )
  data.table::setDT(rais_2019_basica)
  rais_2019_filtrada <- rais_2019_basica[
    !is.na(total),
    .(id_estab, cnae.setor, total, alto, baixo, medio, total_corrigido, codemun)
  ]

}


# análise -----------------------------------------------------------------


gerar_distribuicao <- function(ano, setor, nome_setor = setor, subtit = setor) {

  # pega rais e seleciona setor desejado

  rais <- get(paste0("rais_", ano, "_filtrada"))[cnae.setor %chin% setor]

  # classifica segundo categorias de total de empregos

  max_arred <- plyr::round_any(max(rais$total), 100, ceiling)
  rais[, cate := cut(total, seq(0, max_arred, 100), include.lowest = TRUE)]

  # gera gráfico da distribuição da contagem de empregos por categoria

  data_cont <- data.table::data.table(
    cate = factor(levels(rais$cate), levels = levels(rais$cate))
  )

  rais_cont <- rais[, .(cont = .N), keyby = .(cate)]
  data_cont[rais_cont, on = "cate", cont := i.cont]
  data_cont[is.na(cont), cont := 0]

  p1 <- ggplot(data_cont) +
    geom_col(aes(cate, cont)) +
    ggtitle("contagem de estabelecimentos") +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      axis.ticks.x = element_blank()
    )

  # gera gráfico da distribuição da soma de empregos por categoria

  data_soma <- data.table::data.table(
    cate = factor(levels(rais$cate), levels = levels(rais$cate))
  )

  rais_soma <- rais[, .(soma = sum(total)), keyby = .(cate)]
  data_soma[rais_soma, on = "cate", soma := soma]
  data_soma[is.na(soma), soma := 0]

  p2 <- ggplot(data_soma) +
    geom_col(aes(cate, soma)) +
    ggtitle("soma de empregos") +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      axis.ticks.x = element_blank()
    )

  # junta os dois graficos em um só e salva

  titulo <- cowplot::ggdraw() +
    cowplot::draw_label(
      paste0("distribuição de estabelecimentos/empregos - setor: ", nome_setor),
      fontface = "bold"
    )

  subtitulo <- cowplot::ggdraw() +
    cowplot::draw_label(
      subtit
    )

  pp <- cowplot::plot_grid(p1, p2, ncol = 2)
  pf <- cowplot::plot_grid(titulo, subtitulo, pp, nrow = 3, rel_heights = c(0.06, 0.06, 0.88))

  diretorio <- "./reports/dist_cnpjs_empregos"
  if (!dir.exists(diretorio)) dir.create(diretorio)

  arquivo <- paste0(diretorio, "/setor_", nome_setor, ".png")

  ggsave(
    arquivo,
    plot = pf,
    units = "cm",
    width = 25,
    height = 12.5,
    dpi = 150
  )

}

atualizar_graficos <- function(ano = 2017) {

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

  subtit <- c(
    "ELETRICIDADE, GÁS E OUTRAS UTILIDADES",
    "CAPTAÇÃO, TRATAMENTO E DISTRIBUIÇÃO DE ÁGUA",
    "TRANSPORTE TERRESTRE",
    "TRANSPORTE AÉREO",
    "SERVIÇOS DE ESCRITÓRIO, DE APOIO ADMINISTRATIVO E OUTROS SERVIÇOS PRESTADOS PRINCIPALMENTE ÀS EMPRESAS",
    "COLETA, TRATAMENTO E DISPOSIÇÃO DE RESÍDUOS; RECUPERAÇÃO DE MATERIAIS",
    "SELEÇÃO, AGENCIAMENTO E LOCAÇÃO DE MÃO-DE-OBRA",
    "ATIVIDADES DE VIGILÂNCIA, SEGURANÇA E INVESTIGAÇÃO",
    "CONSTRUÇÃO DE EDIFÍCIOS",
    "OBRAS DE INFRA-ESTRUTURA",
    "SERVIÇOS ESPECIALIZADOS PARA CONSTRUÇÃO",
    "ATIVIDADES DE SERVIÇOS FINANCEIROS",
    "SERVIÇOS PARA EDIFÍCIOS E ATIVIDADES PAISAGÍSTICAS",
    "ADMINISTRAÇÃO PÚBLICA, DEFESA E SEGURIDADE SOCIAL"
  )

  for (i in 1:length(setores_problematicos)) {

    gerar_distribuicao(
      ano,
      setor = setores_problematicos[i],
      subtit = subtit[i]
    )

  }

  # todos os setores agregados

  rais <- get(paste0("rais_", ano, "_filtrada"))
  todos_setores <- unique(rais$cnae.setor)

  gerar_distribuicao(ano, todos_setores, "todos", "TODOS OS SETORES")

  # todos os setores agregados, menos os problematicos

  rais <- get(paste0("rais_", ano, "_filtrada"))
  todos_setores <- unique(rais$cnae.setor)

  gerar_distribuicao(
    ano,
    todos_setores[! todos_setores %chin% setores_problematicos],
    "todos_nao_prob",
    subtit = "TODOS OS SETORES NÃO PROBLEMÁTICOS"
  )

}
