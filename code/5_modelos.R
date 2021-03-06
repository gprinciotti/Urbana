# ECONOMIA URBANA - Guardas Municipais vs Criminalidade ------------------------
# AUTORES:    Helena Arruda & Vinícius Princiotti
# DATA:       01/06/2021
# DESCRIÇÃO:  Modelos

rm(list = ls())
gc()

# CallLibraries
source("code/_functions/CallLibraries.R")
packs <- c("tidyverse", "magrittr", "lfe", "stargazer", "did", "ggpubr")
CallLibraries(packs)

# Abertura da base final
bd_final <- read_rds("data/bd_final.RDS")

# MODELOS ----------------------------------------------------------------------

##### TWFE #####
mod1 <- felm(tx_rf_veiculo ~ treat | id_municipio + ano | 0 | id_municipio, bd_final)
mod2 <- felm(tx_rf_veiculo ~ treat + pib_pc + densidade + bf_familias | id_municipio + ano | 0 | id_municipio, bd_final)

mod3 <- felm(tx_rf ~ treat | id_municipio + ano | 0 | id_municipio, bd_final)
mod4 <- felm(tx_rf ~ treat + pib_pc + densidade + bf_familias | id_municipio + ano | 0 | id_municipio, bd_final)

mod5 <- felm(tx_latrocinio  ~ treat | id_municipio + ano | 0 | id_municipio, bd_final)
mod6 <- felm(tx_latrocinio  ~ treat + pib_pc + densidade + bf_familias | id_municipio + ano | 0 | id_municipio, bd_final)

stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
          type = "latex", align = FALSE, omit.stat = c("adj.rsq", "ser"), header = FALSE, no.space = TRUE,
          dep.var.labels.include = FALSE, dep.var.caption = "Tipo de ocorrência",
          covariate.labels = c("Entrada da GM"),
          column.labels = c("RF (veículos)", "RF (outros)", "Latrocinio"),
          column.separate = c(2, 2, 2),
          omit = c("id_municipio", "ano", "pib_pc", "densidade", "bf_familias"),
          add.lines = list(c("EF. Município", rep("Sim", 6)),
                           c("EF. Ano", rep("Sim", 6)),
                           c("Controles", rep(c("Não", "Sim"), 3))),
          title = "Efeito da entrada da Guarda Municipal sobre diferentes tipos de ocorrências criminais - TWFE - Parte 1",
          notes = "PIB per capita, densidade populacional e nº de famílias beneficiárias do Bolsa Família são controles.",
          label = "twfe_did1",
          out = "output/twfe_did1.tex")

mod7 <- felm(tx_homicidio_doloso  ~ treat | id_municipio + ano | 0 | id_municipio, bd_final)
mod8 <- felm(tx_homicidio_doloso  ~ treat + pib_pc + densidade + bf_familias | id_municipio + ano | 0 | id_municipio, bd_final)

mod9 <- felm(tx_lcd  ~ treat | id_municipio + ano | 0 | id_municipio, bd_final)
mod10 <- felm(tx_lcd  ~ treat + pib_pc + densidade + bf_familias | id_municipio + ano | 0 | id_municipio, bd_final)

mod11 <- felm(tx_estupro  ~ treat | id_municipio + ano | 0 | id_municipio, bd_final)
mod12 <- felm(tx_estupro  ~ treat + pib_pc + densidade + bf_familias | id_municipio + ano | 0 | id_municipio, bd_final)

stargazer(mod7, mod8, mod9, mod10, mod11, mod12,
          type = "latex", align = FALSE, omit.stat = c("adj.rsq", "ser"), header = FALSE, no.space = TRUE,
          dep.var.labels.include = FALSE, dep.var.caption = "Tipo de ocorrência",
          covariate.labels = c("Entrada da GM"),
          column.labels = c("Homicídio", "LCD", "Estupro"),
          column.separate = c(2, 2, 2),
          omit = c("id_municipio", "ano", "pib_pc", "densidade", "bf_familias"),
          add.lines = list(c("EF. Município", rep("Sim", 6)),
                           c("EF. Ano", rep("Sim", 6)),
                           c("Controles", rep(c("Não", "Sim"), 3))),
          title = "Efeito da entrada da Guarda Municipal sobre diferentes tipos de ocorrências criminais - TWFE - Parte 2",
          notes = "PIB per capita, densidade populacional e nº de famílias beneficiárias do Bolsa Família são controles.",
          label = "twfe_did2",
          out = "output/twfe_did2.tex")

##### CALLAWAY & SANT'ANNA #####
aux <- bd_final %>%
  arrange(id_municipio, ano) %>%
  group_by(id_municipio) %>%
  mutate(time = 1:n(),
         group = case_when(ano == gm_ano ~ time)) %>%
  fill(group) %>%
  fill(group, .direction = "up") %>%
  ungroup() %>%
  replace(is.na(.), 0)

# RF (veículos)
p1 <- att_gt(yname = "tx_rf_veiculo",
             tname = "time",
             idname = "id_municipio",
             gname = "group",
             xformla = ~pib_pc + densidade + bf_familias,
             data = aux,
             control_group = "nevertreated",
             clustervars = "id_municipio") %>% 
  aggte(type = "dynamic", na.rm = TRUE) %>%
  ggdid(ylim = c(-200, 200)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Roubo e furto de veículos",
       colour = "Período",
       x = "Ano",
       y = "Efeito médio") + 
  scale_colour_manual(labels = c("Pré-tratamento", "Pós-tratamento"),
                      values = c("lightsteelblue3", "navyblue")) +
  theme_bw() + theme(legend.position = "bottom", title = element_text(size = 12),
                     legend.text = element_text(size = 14), legend.title = element_text(size = 14),
                     axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

# RF (outros)
p2 <- att_gt(yname = "tx_rf",
             tname = "time",
             idname = "id_municipio",
             gname = "group",
             xformla = ~pib_pc + densidade + bf_familias,
             data = aux,
             control_group = "nevertreated",
             clustervars = "id_municipio") %>% 
  aggte(type = "dynamic", na.rm = TRUE) %>%
  ggdid(ylim = c(-1000, 1000)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Roubo e furto (outros)",
       colour = "Período",
       x = "Ano",
       y = "Efeito médio") + 
  scale_colour_manual(labels = c("Pré-tratamento", "Pós-tratamento"),
                      values = c("lightsteelblue3", "navyblue")) +
  theme_bw() + theme(legend.position = "bottom", title = element_text(size = 12),
                     legend.text = element_text(size = 14), legend.title = element_text(size = 14),
                     axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

# Latrocínio
p3 <- att_gt(yname = "tx_latrocinio",
             tname = "time",
             idname = "id_municipio",
             gname = "group",
             xformla = ~pib_pc + densidade + bf_familias,
             data = aux,
             control_group = "nevertreated",
             clustervars = "id_municipio") %>% 
  aggte(type = "dynamic", na.rm = TRUE) %>%
  ggdid(ylim = c(-7.5, 7.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Latrocínio",
       colour = "Período",
       x = "Ano",
       y = "Efeito médio") + 
  scale_colour_manual(labels = c("Pré-tratamento", "Pós-tratamento"),
                      values = c("lightsteelblue3", "navyblue")) +
  theme_bw() + theme(legend.position = "bottom", title = element_text(size = 12),
                     legend.text = element_text(size = 14), legend.title = element_text(size = 14),
                     axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

# Homicídio
p4 <- att_gt(yname = "tx_homicidio_doloso",
             tname = "time",
             idname = "id_municipio",
             gname = "group",
             xformla = ~pib_pc + densidade + bf_familias,
             data = aux,
             control_group = "nevertreated",
             clustervars = "id_municipio") %>% 
  aggte(type = "dynamic", na.rm = TRUE) %>%
  ggdid(ylim = c(-40, 40)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Homicídio doloso",
       colour = "Período",
       x = "Ano",
       y = "Efeito médio") + 
  scale_colour_manual(labels = c("Pré-tratamento", "Pós-tratamento"),
                      values = c("lightsteelblue3", "navyblue")) +
  theme_bw() + theme(legend.position = "bottom", title = element_text(size = 12),
                     legend.text = element_text(size = 14), legend.title = element_text(size = 14),
                     axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

# Lesão corporal
p5 <- att_gt(yname = "tx_lcd",
             tname = "time",
             idname = "id_municipio",
             gname = "group",
             xformla = ~pib_pc + densidade + bf_familias,
             data = aux,
             control_group = "nevertreated",
             clustervars = "id_municipio") %>% 
  aggte(type = "dynamic", na.rm = TRUE) %>%
  ggdid(ylim = c(-400, 400)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Lesão corporal dolosa",
       colour = "Período",
       x = "Ano",
       y = "Efeito médio") + 
  scale_colour_manual(labels = c("Pré-tratamento", "Pós-tratamento"),
                      values = c("lightsteelblue3", "navyblue")) +
  theme_bw() + theme(legend.position = "bottom", title = element_text(size = 12),
                     legend.text = element_text(size = 14), legend.title = element_text(size = 14),
                     axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

# Estupro
p6 <- att_gt(yname = "tx_estupro",
             tname = "time",
             idname = "id_municipio",
             gname = "group",
             xformla = ~pib_pc + densidade + bf_familias,
             data = aux,
             control_group = "nevertreated",
             clustervars = "id_municipio") %>% 
  aggte(type = "dynamic", na.rm = TRUE) %>%
  ggdid(ylim = c(-50, 50)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Estupro",
       colour = "Período",
       x = "Ano",
       y = "Efeito médio") + 
  scale_colour_manual(labels = c("Pré-tratamento", "Pós-tratamento"),
                      values = c("lightsteelblue2", "navyblue")) +
  theme_bw() + theme(legend.position = "bottom", title = element_text(size = 12),
                     legend.text = element_text(size = 14), legend.title = element_text(size = 14),
                     axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

# Gráfico final
ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 3, nrow = 2,
          common.legend = TRUE, legend = "bottom") %>%
  annotate_figure(bottom = text_grob("Nota: PIB per capita, densidade populacional e nº de famílias beneficiárias do Bolsa Família no município são controles.", color = "black",
                                     hjust = 1, x = 1, size = 11)) %>%
  ggexport(filename = "output/cs_did.pdf", width = 12, height = 8)

# Testes: efeitos médios
att_gt(yname = "tx_homicidio_doloso",
       tname = "time",
       idname = "id_municipio",
       gname = "group",
       xformla = ~pib_pc + densidade + bf_familias,
       data = aux,
       control_group = "nevertreated",
       clustervars = "id_municipio") %>% 
  aggte(type = "simple", na.rm = TRUE) %>% 
  summary()
