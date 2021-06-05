# ECONOMIA URBANA - Guardas Municipais vs Criminalidade ------------------------
# AUTORES:    Helena Arruda & Vinícius Princiotti
# DATA:       01/06/2021
# DESCRIÇÃO:  Webscraping dos dados de crimes da SSP-SP (por município)

rm(list = ls())
gc()

# CallLibraries
source("code/_functions/CallLibraries.R")
packs <- c("tidyverse", "magrittr", "RSelenium", "rvest")
CallLibraries(packs)

# WEBSCRAP ---------------------------------------------------------------------
# Fonte: http://www.ssp.sp.gov.br/estatistica/pesquisa.aspx

# url <- "http://www.ssp.sp.gov.br/estatistica/pesquisa.aspx" # url de acesso à SSP-SP
# 
# municipios <- list() # vetor auxiliar para guardar dados dos municípios
# 
# aux <- expand_grid(anos = 3:22, municipio = 2:646) %>%
#   mutate(seq = 1:n()) # vetor auxiliar com todas as combinações possíveis de anos & municípios
# 
# # abrindo servidor
# remDr <- rsDriver(browser = "chrome", chromever = "91.0.4472.19", port = 1234L)$client
# remDr$open(silent = FALSE) # re-abrir chrome
# 
# # webscrap
# for (i in 7842:nrow(aux)) {
#   tryCatch({
#     remDr$navigate(url) # abrindo site da SSP-SP
# 
#     remDr$findElement(using = "id",
#                       value = "conteudo_btnMensal")$clickElement() # selecionando tabelas de ocorrências mensais
# 
#     ano <- remDr$findElement(using = "xpath",
#                              value = paste0('//*[@id="conteudo_ddlAnos"]/option[', aux[i, 1], ']'))$getElementText()[[1]] # salvando ano
#     remDr$findElement(using = "xpath",
#                       value = paste0('//*[@id="conteudo_ddlAnos"]/option[', aux[i, 1], ']'))$clickElement() # selecionando ano
# 
#     mun <- remDr$findElement(using = "xpath",
#                             value = paste0('//*[@id="conteudo_ddlMunicipios"]/option[', aux[i, 2], ']'))$getElementText()[[1]] # salvando município
#     remDr$findElement(using = "xpath",
#                       value = paste0('//*[@id="conteudo_ddlMunicipios"]/option[', aux[i, 2], ']'))$clickElement() # selecionando município
# 
#     municipios[i] <- remDr$getPageSource()[[1]] %>% # salvando tabela de ocorrências da página
#       read_html() %>%
#       html_table()
# 
#     municipios[[i]] %<>% # gerando tibble já com infos do ano e DP
#       as_tibble(.name_repair = "universal") %>%
#       mutate(ano = ano, mun = mun)
# 
#     print(paste(i, sep = " - "))
# 
#   }, error = function(cont){
#     print(paste(i, "ERRO", sep = " - "))
#   })
# }
# 
# write_rds(municipios, "data/ssp/municipios.RDS") # salvando resultado do webscrap

# TRATAMENTO DA BASE -----------------------------------------------------------

municipios1 <- read_rds("data/ssp/municipios.RDS")
municipios2 <- read_rds("data/ssp/municipios2.RDS") 
# webscrap deu bug em 3 municípios específicos: Iacri, Motuca e Rinópolis
# baixei apenas esses três de novo e separadamente em municipios2.RDS

bd_ssp1 <- do.call(rbind, municipios1) %>%
  set_names(c("tipo_ocorrencia", "jan", "fev", "mar", "abr", "mai", "jun",
              "jul", "ago", "set", "out", "nov", "dez", "total", "ano", "mun")) %>% 
  filter(!mun %in% c("Iacri", "Motuca", "Rinópolis"))

bd_ssp2 <- do.call(rbind, municipios2) %>%
  set_names(c("tipo_ocorrencia", "jan", "fev", "mar", "abr", "mai", "jun",
              "jul", "ago", "set", "out", "nov", "dez", "total", "ano", "mun"))

bd_ssp <- bind_rows(bd_ssp1, bd_ssp2)
rm(municipios1, municipios2, bd_ssp1, bd_ssp2)
write_rds(bd_ssp, "data/ssp/bd_ssp.RDS")