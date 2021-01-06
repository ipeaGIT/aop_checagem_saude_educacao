# Quick setup
#
# Load packages
library(tidyverse)
library(here)         # manage directories
library(ggplot2)      # data viz
library(ggthemes)     # data viz themes
library(hrbrthemes)   # data viz themes
library(sf)           # read and manipulate spatial data
library(data.table)   # fast data wrangling
library(magrittr)     # pipe operator
library(geobr)        # Brazil's spatial data
library(readr)        # rapid data read
library(tidyr)        # data manipulating
library(stringr)      # strings operations
library(lubridate)    # handle date formats
library(mapview)      # interactive maps
library(purrr)        # funcional programming
library(dplyr)        # better than data.table!
library(patchwork)    # plot composition
library(ggmap)        # geocoding
library(h3jsr)        # h3 hexagonons
library(bit64)        # viz large numbers
library(geodist)

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
