# ECONOMIA URBANA - Guardas Municipais vs Criminalidade ------------------------
# AUTORES:    Helena Arruda & Vinícius Princiotti
# DATA:       01/06/2021
# DESCRIÇÃO:  Base final

rm(list = ls())
gc()

# CallLibraries
source("code/_functions/CallLibraries.R")
packs <- c("tidyverse", "magrittr")
CallLibraries(packs)

# ABERTURA DOS DADOS -----------------------------------------------------------
bd_ssp <- read_rds("data/ssp/bd_ssp.RDS") %>% # corrigindo municípios com nomes diferentes
  mutate(mun = case_when(mun == "Biritiba Mirim" ~ "Biritiba-Mirim",
                         mun == "Embu Guaçú" ~ "Embu-Guaçu",
                         mun == "Santa Rosa do Viterbo" ~ "Santa Rosa de Viterbo",
                         mun == "Tarabaí" ~ "Tarabai",
                         mun == "Uchôa" ~ "Uchoa",
                         TRUE ~ mun))

bd_munic_19 <- read_rds("data/munic/bd_munic_19.RDS")

bd_pop <- read_csv("data/auxiliares/pop.csv")

bd_pib <- read_csv("data/auxiliares/pib.csv")

bd_mun <- read_csv("data/auxiliares/mun.csv") %>% # mantendo apenas municipios de SP
  filter(id_municipio >= 3500000 & id_municipio <= 3599999)

# SSP --------------------------------------------------------------------------

##### Roubo e furto de veículos #####
bd_ssp_rfv <- bd_ssp %>%
  filter(tipo_ocorrencia %in% c("ROUBO DE VEÍCULO", "FURTO DE VEÍCULO")) %>%
  mutate_at(vars(-mun, -tipo_ocorrencia), ~ as.numeric(.) %>% round(., digits = 0)) %>% 
  group_by(ano, mun) %>% 
  summarise_at(vars(-group_cols(), -tipo_ocorrencia), ~ sum(.x, na.rm = TRUE)) %>% 
  ungroup() %>%
  select(mun, ano, rf_veiculo = total)

##### Roubo e furto (ouros) #####
bd_ssp_rf <- bd_ssp %>%
  filter(tipo_ocorrencia %in% c("TOTAL DE ROUBO - OUTROS (1)", "FURTO - OUTROS")) %>%
  mutate_at(vars(-mun, -tipo_ocorrencia), ~ as.numeric(.) %>% round(., digits = 0)) %>% 
  group_by(ano, mun) %>% 
  summarise_at(vars(-group_cols(), -tipo_ocorrencia), ~ sum(.x, na.rm = TRUE)) %>% 
  ungroup() %>%
  select(mun, ano, rf = total)

##### Homicídio doloso #####
bd_ssp_hd <- bd_ssp %>%
  filter(tipo_ocorrencia %in% c("HOMICÍDIO DOLOSO (2)")) %>%
  mutate_at(vars(-mun, -tipo_ocorrencia), ~ as.numeric(.) %>% round(., digits = 0)) %>% 
  select(mun, ano, homicidio_doloso = total)

##### Lesão corporal dolosa #####
bd_ssp_lcd <- bd_ssp %>%
  filter(tipo_ocorrencia %in% c("LESÃO CORPORAL DOLOSA")) %>%
  mutate_at(vars(-mun, -tipo_ocorrencia), ~ as.numeric(.) %>% round(., digits = 0)) %>% 
  select(mun, ano, lcd = total)

##### Estupros #####
bd_ssp_est <- bd_ssp %>%
  filter(tipo_ocorrencia %in% c("TOTAL DE ESTUPRO (4)")) %>%
  mutate_at(vars(-mun, -tipo_ocorrencia), ~ as.numeric(.) %>% round(., digits = 0)) %>% 
  select(mun, ano, estupro = total)

bd_ssp <- inner_join(bd_ssp_rfv, bd_ssp_rf, by = c("mun", "ano")) %>% 
  inner_join(., bd_ssp_hd, by = c("mun", "ano")) %>% 
  inner_join(., bd_ssp_lcd, by = c("mun", "ano")) %>% 
  inner_join(., bd_ssp_est, by = c("mun", "ano"))

rm(bd_ssp_rfv, bd_ssp_rf, bd_ssp_hd, bd_ssp_lcd, bd_ssp_est)

# BASE FINAL -------------------------------------------------------------------

bd_final <- inner_join(bd_ssp, bd_mun, by = "mun") %>% 
  inner_join(., bd_pop, by = c("id_municipio", "ano")) %>% 
  inner_join(., bd_munic_19, by = "id_municipio") %>% 
  mutate(tx_rf_veiculo = rf_veiculo/pop*100000,
         tx_rf = rf/pop*100000,
         tx_homicidio_doloso = homicidio_doloso/pop*100000,
         tx_lcd = lcd/pop*100000,
         tx_estupro = estupro/pop*100000,
         pre_treated = case_when(gm_ano < 2001 & !is.na(gm_ano) ~ 1, TRUE ~ 0),
         treat = case_when(ano == gm_ano ~ 1, FALSE ~ 0)) %>% 
  group_by(id_municipio) %>% 
  fill(treat) %>% 
  mutate(treat = case_when(is.na(treat) ~ 0, TRUE ~ treat))

write_csv(bd_final, "data/bd_final.csv")
