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
                         TRUE ~ mun),
         jan = as.numeric(gsub(jan, pattern = "[.]", replacement = "")),
         fev = as.numeric(gsub(fev, pattern = "[.]", replacement = "")), 
         mar = as.numeric(gsub(mar, pattern = "[.]", replacement = "")),
         abr = as.numeric(gsub(abr, pattern = "[.]", replacement = "")),
         mai = as.numeric(gsub(mai, pattern = "[.]", replacement = "")),
         jun = as.numeric(gsub(jun, pattern = "[.]", replacement = "")),
         jul = as.numeric(gsub(jul, pattern = "[.]", replacement = "")),
         ago = as.numeric(gsub(ago, pattern = "[.]", replacement = "")),
         set = as.numeric(gsub(set, pattern = "[.]", replacement = "")),
         out = as.numeric(gsub(out, pattern = "[.]", replacement = "")),
         nov = as.numeric(gsub(nov, pattern = "[.]", replacement = "")),
         dez = as.numeric(gsub(dez, pattern = "[.]", replacement = "")),
         total = jan+fev+mar+abr+mai+jun+jul+ago+set+out+nov+dez) # recalculando total de crimes com variáveis corrigidas

bd_munic_19 <- read_rds("data/munic/bd_munic_19.RDS")

bd_pop <- read_csv("data/auxiliares/pop.csv")

bd_pib <- read_csv("data/auxiliares/pib.csv")

bd_mun <- read_csv("data/auxiliares/mun.csv") %>% # mantendo apenas municipios de SP
  filter(id_municipio >= 3500000 & id_municipio <= 3599999)

bd_finbra <- read_rds("data/auxiliares/bd_finbra.RDS")

bd_area <- read_rds("data/auxiliares/bd_area.RDS")

bd_bf <- read_rds("data/auxiliares/bd_bf.RDS") %>% 
  as_tibble() %>% 
  mutate(ano = as.numeric(year)) %>% 
  group_by(id_municipio, ano) %>% 
  summarise(bf_familias = mean(num_families, na.rm = TRUE)) %>% 
  select(id_municipio, ano, bf_familias)

# SSP --------------------------------------------------------------------------

##### Roubo e furto de veículos #####
bd_ssp_rfv <- bd_ssp %>%
  filter(tipo_ocorrencia %in% c("ROUBO DE VEÍCULO", "FURTO DE VEÍCULO")) %>%
  mutate_at(vars(-mun, -tipo_ocorrencia), ~ as.numeric(.) %>% round(., digits = 0)) %>% 
  group_by(ano, mun) %>% # soma de furtos e roubos de veículos por município/ano
  summarise_at(vars(-group_cols(), -tipo_ocorrencia), ~ sum(.x, na.rm = TRUE)) %>% 
  ungroup() %>%
  select(mun, ano, rf_veiculo = total)

##### Roubo e furto (outros) #####
bd_ssp_rf <- bd_ssp %>%
  filter(tipo_ocorrencia %in% c("TOTAL DE ROUBO - OUTROS (1)", "FURTO - OUTROS")) %>%
  mutate_at(vars(-mun, -tipo_ocorrencia), ~ as.numeric(.) %>% round(., digits = 0)) %>% 
  group_by(ano, mun) %>%  # soma de furtos e roubos (outros) por município/ano
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

##### Latrocínios #####
bd_ssp_lat <- bd_ssp %>%
  filter(tipo_ocorrencia %in% c("LATROCÍNIO")) %>%
  mutate_at(vars(-mun, -tipo_ocorrencia), ~ as.numeric(.) %>% round(., digits = 0)) %>% 
  select(mun, ano, latrocinio = total)

bd_ssp <- inner_join(bd_ssp_rfv, bd_ssp_rf, by = c("mun", "ano")) %>% # junção das bases dos tipos de crimes
  inner_join(., bd_ssp_hd, by = c("mun", "ano")) %>% 
  inner_join(., bd_ssp_lcd, by = c("mun", "ano")) %>% 
  inner_join(., bd_ssp_est, by = c("mun", "ano")) %>% 
  inner_join(., bd_ssp_lat, by = c("mun", "ano"))

rm(bd_ssp_rfv, bd_ssp_rf, bd_ssp_hd, bd_ssp_lcd, bd_ssp_est)

# BASE FINAL -------------------------------------------------------------------
bd_final <- inner_join(bd_ssp, bd_mun, by = "mun") %>%
  inner_join(., bd_pop, by = c("id_municipio", "ano")) %>% 
  inner_join(., bd_munic_19, by = "id_municipio") %>% 
  full_join(., bd_pib, by = c("id_municipio", "ano")) %>%  
  full_join(., bd_finbra, by = c("id_municipio", "ano")) %>% 
  inner_join(., bd_area, by = "mun") %>% 
  inner_join(., bd_bf, by = c("id_municipio", "ano")) %>% 
  mutate(tx_rf_veiculo = rf_veiculo/pop*100000, # taxas de tipos de crimes por 100 mil habitantes
         tx_rf = rf/pop*100000,
         tx_homicidio_doloso = homicidio_doloso/pop*100000,
         tx_lcd = lcd/pop*100000,
         tx_estupro = estupro/pop*100000,
         tx_latrocinio = latrocinio/pop*100000,
         pib_pc = pib/pop, # pib per capita
         gastos_pol_pc = gastos_pol/pop, # gastos com policiamento per capita
         densidade = pop/area,
         pre_treated = case_when(gm_ano < 2010 & !is.na(gm_ano) ~ 1,
                                 TRUE ~ 0), # identificando municípios que já estavam tratados no período anterior ao da amostra
         treat = case_when(ano == gm_ano ~ 1, 
                           FALSE ~ 0,
                           pre_treated == 1 ~ 1)) %>% 
  group_by(id_municipio) %>% 
  fill(treat) %>% # preenchendo variável de tratamento para períodos posteriores ao do início do tratamento
  mutate(treat = case_when(is.na(treat) ~ 0, 
                           TRUE ~ treat)) %>% 
  filter(ano %in% c(2010:2019) & pre_treated != 1) %>% # período definido para amostra: 2010 a 2019
  ungroup()

bd_final %<>% # imputando informação de PIB per capita para 2019
  group_by(id_municipio) %>%
  mutate(avg_change = mean(c(NA, diff(pib_pc)), na.rm = TRUE),
         pib_pc = case_when(ano == 2019 ~ lag(pib_pc) + avg_change,
                            TRUE ~ pib_pc)) %>% 
  select(-avg_change)

write_rds(bd_final, "data/bd_final.RDS")
