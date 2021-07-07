# ECONOMIA URBANA - Guardas Municipais vs Criminalidade ------------------------
# AUTORES:    Helena Arruda & Vinícius Princiotti
# DATA:       01/06/2021
# DESCRIÇÃO:  Base final

rm(list = ls())
gc()

# CallLibraries
source("code/_functions/CallLibraries.R")
packs <- c("tidyverse", "magrittr", "geobr", "sf", "crul" )
CallLibraries(packs)


# THEMES  -----------------------------------------------------------

# MAP THEME
map_theme <- theme_minimal()+
  
  theme(
    # AXIS
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),    
    axis.text.x= element_blank(),
    axis.text.y= element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    plot.title    = element_text(size = 15, face = "bold", hjust= .5),
    plot.subtitle = element_text(size = 12, hjust = 0.5))


# GRAPH THEME
line_theme<-list(theme_light(),
                  theme(legend.position='bottom',
                        plot.title = element_text(hjust = 0.5),
                        plot.subtitle = element_text(hjust = 0.5),
                        strip.background = element_rect(color="transparent", fill="transparent", size=1, linetype="solid"),
                        strip.text = element_text(color="black",face="bold"),
                        legend.text=element_text(size=8),
                        legend.title=element_text(size=8,hjust = 0.5),
                        legend.background=element_rect(fill='transparent',color='transparent'),
                        legend.key=element_blank(),
                        axis.title=element_text(size=8)),
                  scale_fill_manual(name='',values=rep(c('#FFC20E','#f7941d','#000000','#808080','#b30000'),times=4)),
                  scale_color_manual(name='',values=rep(c('#FFC20E','#f7941d','#000000','#808080','#b30000', "indianred2", "lightgoldenrod3", "gray81", "orange4"),times=4)),
                  scale_linetype_manual(name='',values=rep(c("solid","dashed","dotted","dotdash"),times=2)),
                  scale_shape_manual(name='',values=seq(1,5)))



# ABERTURA DOS DADOS -----------------------------------------------------------

bd_final  <- read_csv("data/bd_final.csv")


muni_2019 <- read_municipality(year = 2019)

bd_final_sf <- muni_2019 %>% 
  filter(abbrev_state == "SP") %>% 
  rename(id_municipio = code_muni) %>% 
  left_join(bd_final)



bd_munic_19 <- read_rds("data/munic/bd_munic_19.RDS")

gm_2010_2019 <- bd_munic_19 %>% 
  filter(gm == 1 ) %>% 
  group_by(gm_ano) %>% 
  summarise(sum_gm = sum(gm))

gm_2010_2019 <- gm_2010_2019 %>% 
  filter(gm_ano >= 2010) %>% 
  mutate(sum_gm = replace(sum_gm, gm_ano == 2010, sum_gm+187))

years <- c(2011:2019)

for (y in seq_along(years)) {
  
  gm_2010_2019[gm_2010_2019$gm_ano == years[y],]$sum_gm <- gm_2010_2019[gm_2010_2019$gm_ano == (years[y]-1),]$sum_gm +  gm_2010_2019[gm_2010_2019$gm_ano == (years[y]),]$sum_gm
  
}



# 2000 - 2019
gm_2010_2019 <- gm_2010_2019 %>% 
  filter(gm_ano >= 2000) %>% 
  mutate(sum_gm = replace(sum_gm, gm_ano == 2000, 150))

years <- c(2001:2019)

for (y in seq_along(years)) {
  
  gm_2010_2019[gm_2010_2019$gm_ano == years[y],]$sum_gm <- gm_2010_2019[gm_2010_2019$gm_ano == (years[y]-1),]$sum_gm +  gm_2010_2019[gm_2010_2019$gm_ano == (years[y]),]$sum_gm
  
}


# Plots --------------------------------------------------------------------------

# plot 
ggplot() +
  geom_sf(data = bd_final_sf, aes(fill = factor(gm)), color= alpha("black", 0.6)) +
  labs(fill = "")+
  scale_fill_manual(labels = c("Municípios sem GM (2019)", "Municípios com GM (2019)"),  
                    values = c("lightsteelblue2", "navyblue")) +
  labs(caption = "Fonte: Elaboração própria a partir de dados da Pesquisa de Informações Básicas Municipais (IBGE)") +
  
  map_theme

# save
ggsave(filename = "data/plots/sp_gm_2019.png", device = "png",dpi = 300)

library(viridis)

# plot by year
bd_final_sf %>%
  mutate(lg_tx_rf = log(tx_rf+1),
         lg_tx_rf_v = log(tx_rf_veiculo+1),
         lg_hom   = log(tx_homicidio_doloso +1),
         lg_lesaoc = log(tx_lcd +1),
         lg_estupro = log(tx_estupro+1)) %>% 
  filter(ano >= 2010 & ano < 2020) %>% 
ggplot() +
  geom_sf(aes(fill = lg_hom), col = "NA") +
  labs(title = ")",
       fill = "")+
  scale_fill_viridis(direction = -1)+
  labs(caption = "Fonte: Elaboração própria a partir de dados da Pesquisa de Informações Básicas Municipais (IBGE)") +
  facet_wrap(~ano)+
  
  map_theme

# save
ggsave(filename = "data/plots/homicidios_notitle.png", device = "png",dpi = 300)




gm_2010_2019 %>% 
  ggplot(aes(x= gm_ano ,y = sum_gm))+
  geom_line(col = "navyblue") +
  geom_point()+
  labs(caption = "Fonte: Elaboração própria a partir de dados da Pesquisa de Informações Básicas Municipais (IBGE)") +
  line_theme +
  scale_x_continuous(breaks = seq(2000, 2019, 1),labels = function(x) round(x, 0)) +
  guides(col=guide_legend(nrow=1,byrow=TRUE))  +
  theme(plot.caption = element_text(hjust = .1), #Default is hjust=1
        plot.caption.position =  "plot", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(plot.caption  = element_text(size = 10.5, face = "bold")) 

# save
ggsave(filename = "data/plots/sp_2010_2019.png", device = "png",dpi = 300)
