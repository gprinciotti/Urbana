# ECONOMIA URBANA - Guardas Municipais vs Criminalidade ------------------------
# AUTORES:    Helena Arruda & Vinícius Princiotti
# DATA:       01/06/2021
# DESCRIÇÃO:  Baixando dados auxiliares com ajuda da Base dos Dados

rm(list = ls())
gc()

# CallLibraries
source("code/_functions/CallLibraries.R")
packs <- c("tidyverse", "magrittr", "basedosdados")
CallLibraries(packs)

# BASE DOS DADOS ---------------------------------------------------------------
set_billing_id("urbana-315605")

##### População #####
# query <- "SELECT id_municipio, ano, populacao as pop,
# FROM `basedosdados.br_ibge_populacao.municipios`
# WHERE id_municipio >= 3500000 and id_municipio <= 3599999 and ano >= 2001"
# 
# download(query, path = "data/auxiliares/pop.csv")

bd_pop <- read_csv("data/auxiliares/pop.csv")

##### PIB #####
# query <- "SELECT id_municipio, ano, PIB as pib,
# FROM `basedosdados.br_ibge_pib.municipios`
# WHERE id_municipio >= 3500000 and id_municipio <= 3599999 and ano >= 2001"
# 
# download(query, path = "data/auxiliares/pib.csv")

bd_pib <- read_csv("data/auxiliares/pib.csv")

##### MUNICÍPIOS #####
# query <- "SELECT municipio as mun, id_municipio,
# FROM `basedosdados.br_bd_diretorios_brasil.municipio`"
# 
# download(query, path = "data/auxiliares/mun.csv")

bd_mun <- read_csv("data/auxiliares/mun.csv") %>% 
  filter(id_municipio >= 3500000 & id_municipio <= 3599999)

##### SSP-SP #####
# query <- "SELECT ano, mes, id_municipio, homicidio_doloso, lesao_corporal_dolosa,
# latrocinio, total_de_estupro as estupro,
# total_de_roubo_outros as roubo, roubo_de_veiculo as roubo_veiculo,
# furto_outros as furto, furto_de_veiculo as furto_veiculo
# FROM `basedosdados.br_sp_gov_ssp.ocorrencias_registradas`"
# 
# download(query, path = "data/auxiliares/ssp.csv")
# 
# bd_ssp <- read_csv("data/auxiliares/ssp.csv") %>%
#   group_by(id_municipio, ano) %>%
#   summarise_at(vars(-mes), sum)
