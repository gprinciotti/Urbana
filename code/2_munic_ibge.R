# ECONOMIA URBANA - Guardas Municipais vs Criminalidade ------------------------
# AUTORES:    Helena Arruda & Vinícius Princiotti
# DATA:       01/06/2021
# DESCRIÇÃO:  Tratamento dos dados da Pesquisa e Informações Básicas Municipais (MUNIC/IBGE)

rm(list = ls())
gc()

# CallLibraries
source("code/_functions/CallLibraries.R")
packs <- c("tidyverse", "magrittr", "readxl")
CallLibraries(packs)

# MUNIC (IBGE) -----------------------------------------------------------------
bd_munic_19 <- read_excel("data/munic/raw/Base_MUNIC_2019.xlsx", sheet = "Segurança pública") %>% 
  filter(substr(CodMun, 1, 2) == 35) %>%
  select(id_municipio = CodMun, gm = MSEG27, gm_ano = MSEG271B, gm_efetivo = MSEG283, gm_arma = MSEG33) %>% 
  mutate(gm = case_when(gm == "Sim" ~ 1,
                        gm == "Não" ~ 0),
         gm_ano = case_when(gm_ano == "-" ~ "",
                            TRUE ~ gm_ano),
         gm_efetivo = case_when(gm_efetivo == "-" ~ "",
                            TRUE ~ gm_efetivo),
         gm_arma = case_when(gm_arma == "Apenas arma de fogo" ~ 1,
                             gm_arma == "Armas de fogo e não letais" ~ 1,
                             gm_arma == "Apenas armas não letais" ~ 0,
                             gm_arma == "Nenhum tipo de arma" ~ 0)) %>% 
  mutate_at(vars(everything()), as.numeric)

write_rds(bd_munic_19, "data/munic/bd_munic_19.RDS")
