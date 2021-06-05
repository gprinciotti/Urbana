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
  select(id_municipio = CodMun, gm = MSEG27, gm_ano = MSEG271B) %>% 
  mutate(gm = case_when(gm == "Sim" ~ 1,
                        gm == "Não" ~ 0),
         gm_ano = case_when(gm_ano == "-" ~ "",
                            TRUE ~ gm_ano)) %>% 
  mutate_at(vars(everything()), as.numeric)

write_rds(bd_munic_19, "data/munic/bd_munic_19.RDS")
