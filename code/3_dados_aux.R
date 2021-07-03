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

# FINBRA -----------------------------------------------------------------------
load("data/auxiliares/sp_finbra.Rdata")

bd_finbra <- sp %>% 
  filter(conta == "Policiamento" & coluna == "Despesas Pagas") %>% 
  select(id_municipio, ano, gastos_pol = valor) %>% 
  mutate_at(vars(everything()), as.numeric)

write_rds(bd_finbra, "data/auxiliares/bd_finbra.RDS")

rm(sp)


# Bolsa Familia ----------------------------------------------------------------

load("data/auxiliares/pbf_mds_04_19.RData")

bd_bf <- pbf %>% 
  filter(year >= 2010 &  mun_code >= 350000 & mun_code <= 359999) %>% 
  mutate_at(vars(num_families, value), as.numeric)

rm(pbf)

load("data/auxiliares/mun_codes.RData")

bd_bf <- bd_bf %>% 
  rename(id_munic_6 = mun_code) %>% 
  left_join(muns,by = "id_munic_6") %>% 
  rename(id_municipio = id_munic_7) %>% 
  dplyr::select(-id_munic_6, - estado_abrev)


write_rds(bd_bf, "data/auxiliares/bd_bf.RDS")

