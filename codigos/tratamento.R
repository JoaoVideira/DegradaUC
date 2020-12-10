## Configuracoes
library(tidyverse)
library(sf)
library(plm)
library(lmtest)
library(geobr)
library(spdep)
library(normtest)
library(classInt)
library(spatialreg)
library(tseries)
library(units)
library(ggspatial)

## Impostantdo dados 
#municipios sudeste
sudeste<-read_municipality()%>%
  filter(code_state=="31"|code_state=="32"|code_state=="33"|code_state=="35")
plot(sudeste$geom)
class(sudeste)
glimpse(sudeste)
sudeste$code_muni<-as.character(sudeste$code_muni)

# UCs sudeste. Ano 2019
UC<-read_conservation_units()
plot(UC$geom)
class(UC)
glimpse(UC)

ggplot() +
geom_sf(data = uc_sudeste, fill= "white")

uc_sudeste<-st_intersection(UC,sudeste)

ggplot() +
  geom_sf(data = sudeste, fill= "white")+ 
  geom_sf(data = uc_sudeste, colour = "dark green",   fill = "dark green")+
  theme_minimal()  

tema_mapa <-
  theme_bw() + 
  theme(
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5,
      size = 8
    ),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = rel(0.8)),
    axis.title.x = element_text(size = rel(0.8)),
    panel.grid.major = element_line(
      color = gray(0.9),
      linetype = "dashed",
      size = 0.1
    ),
    panel.background = element_rect(fill = "white") +
      annotation_scale(location = "br", width_hint = 0.30)
  )
ggplot() +
  geom_sf(data = sudeste, fill= "white")+ 
  geom_sf(data = uc_sudeste, colour = "dark green",   fill = "dark green")+
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa

#calculando area e colocando em ha
uc_sudeste$area_uc<-st_area(uc_sudeste$geom)
uc_sudeste$area_uc<-uc_sudeste$area_uc/10000
uc_sudeste$area_uc<-set_units(uc_sudeste$area_uc, NULL)

area_uc_munic<-aggregate(uc_sudeste$area_uc, by=list(uc_sudeste$code_muni), FUN=sum, na.rm=TRUE)
area_uc_munic<-rename(area_uc_munic,code_muni=Group.1,area_uc=x)

#juntando base sf de municipio com base da area das UC por municipio
area_uc_sud<-full_join(sudeste,area_uc_munic,by=c("code_muni"))

area_uc_sud$area_uc<- replace_na(area_uc_sud$area_uc,0)

#calculando area dos municipios
area_uc_sud$area_municipio<-st_area(area_uc_sud$geom)
area_uc_sud$area_municipio<-area_uc_sud$area_municipio/10000
area_uc_sud$area_municipio<-set_units(area_uc_sud$area_municipio, NULL)


# DADOS Altitude-----------------------------------------------------------
# FONTE:ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/localidades/Geomedia_MDB/
#Dados sobre Altitude.
altitude<- read.csv2("Dados/Dados Gerais.csv", sep = ";", stringsAsFactors = FALSE)%>%
  filter(NM_UF=="RIO DE JANEIRO"|NM_UF=="MINAS GERAIS"|NM_UF=="SÃO PAULO"|NM_UF=="ESPÍRITO SANTO")
altitude$LONG<-as.double(altitude$LONG)
altitude$LAT<-as.double(altitude$LAT)
crs<-st_crs(area_uc_sud) 
altitude<-as_tibble(altitude)
class(altitude)

#base na classe sf
altitude_sf<-st_as_sf(altitude,coords = c("LONG","LAT"),crs=crs)%>%
group_by(NM_MUNICIPIO)%>%
  summarise(Altitude = mean(ALT))
altitude_sf$NM_MUNICIPIO<-str_to_title(altitude_sf$NM_MUNICIPIO)
altitude_sf<-rename(altitude_sf,name_muni=NM_MUNICIPIO)
altitude<-st_drop_geometry(altitude_sf)

uc_sud<-left_join(area_uc_sud,altitude,by=c("name_muni"))

ggplot()+
  geom_sf(data = uc_sud, fill="white")+
  geom_sf(data=uc_sud,aes(fill = Altitude))+
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1)+
  geom_sf(data = uc_sudeste, colour = "dark green",   fill = "dark green")+
   annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa

# Dados imoveis rurais
imoveis_es<-read.csv2("Dados/Imoveis_ES_01_12_2020.csv", sep = ";", stringsAsFactors = FALSE,encoding = "UTF-8")
imoveis_es$CÓDIGO.DO.MUNICÍPIO..IBGE.<-as.character(imoveis_es$CÓDIGO.DO.MUNICÍPIO..IBGE.)
imoveis_es[,6] <- gsub("[,]", ".",imoveis_es[,6])
imoveis_es$ÁREA.TOTAL<-as.double(imoveis_es$ÁREA.TOTAL)

imoveis_sp<-read.csv2("Dados/Imoveis_SP_01_12_2020.csv", sep = ";", stringsAsFactors = FALSE,encoding = "UTF-8")
imoveis_sp$CÓDIGO.DO.MUNICÍPIO..IBGE.<-as.character(imoveis_sp$CÓDIGO.DO.MUNICÍPIO..IBGE.)
imoveis_sp[,6] <- gsub("[,]", ".",imoveis_sp[,6])
imoveis_sp$ÁREA.TOTAL<-as.double(imoveis_sp$ÁREA.TOTAL)

imoveis_mg<-read.csv2("Dados/Imoveis_MG_01_12_2020.csv", sep = ";", stringsAsFactors = FALSE,encoding = "UTF-8")
imoveis_mg$CÓDIGO.DO.MUNICÍPIO..IBGE.<-as.character(imoveis_mg$CÓDIGO.DO.MUNICÍPIO..IBGE.)
imoveis_mg[,6] <- gsub("[,]", ".",imoveis_mg[,6])
imoveis_mg$ÁREA.TOTAL<-as.double(imoveis_mg$ÁREA.TOTAL)

imoveis_rj<-read.csv2("Dados/Imoveis_RJ_01_12_2020.csv", sep = ";", stringsAsFactors = FALSE,encoding = "UTF-8")
imoveis_rj$CÓDIGO.DO.MUNICÍPIO..IBGE.<-as.character(imoveis_rj$CÓDIGO.DO.MUNICÍPIO..IBGE.)
imoveis_rj[,6] <- gsub("[,]", ".",imoveis_rj[,6])
imoveis_rj$ÁREA.TOTAL<-as.double(imoveis_rj$ÁREA.TOTAL)

#instalando pacote ineq
install.packages("ineq")
library(ineq)

prop_munic_es<-aggregate(imoveis_es$ÁREA.TOTAL, by=list(imoveis_es$CÓDIGO.DO.MUNICÍPIO..IBGE.), FUN=Gini, na.rm=TRUE)
prop_munic_es$code_muni<-prop_munic_es$Group.1
prop_munic_es$Gini<-prop_munic_es$x
prop_munic_es<-prop_munic_es[,-c(1,2)]

prop_munic_sp<-aggregate(imoveis_sp$ÁREA.TOTAL, by=list(imoveis_sp$CÓDIGO.DO.MUNICÍPIO..IBGE.), FUN=Gini, na.rm=TRUE)
prop_munic_sp$code_muni<-prop_munic_sp$Group.1
prop_munic_sp$Gini<-prop_munic_sp$x
prop_munic_sp<-prop_munic_sp[,-c(1,2)]

prop_munic_mg<-aggregate(imoveis_mg$ÁREA.TOTAL, by=list(imoveis_mg$CÓDIGO.DO.MUNICÍPIO..IBGE.), FUN=Gini, na.rm=TRUE)
prop_munic_mg$code_muni<-prop_munic_mg$Group.1
prop_munic_mg$Gini<-prop_munic_mg$x
prop_munic_mg<-prop_munic_mg[,-c(1,2)]

prop_munic_rj<-aggregate(imoveis_rj$ÁREA.TOTAL, by=list(imoveis_rj$CÓDIGO.DO.MUNICÍPIO..IBGE.), FUN=Gini, na.rm=TRUE)
prop_munic_rj$code_muni<-prop_munic_rj$Group.1
prop_munic_rj$Gini<-prop_munic_rj$x
prop_munic_rj<-prop_munic_rj[,-c(1,2)]

gini_imoveis<-bind_rows(prop_munic_es,prop_munic_mg,prop_munic_rj,prop_munic_sp, id=NULL)
uc_sud<-left_join(uc_sud,gini_imoveis,by = "code_muni")

ggplot()+
  geom_sf(data = uc_sud, fill="white")+
  geom_sf(data=uc_sud,aes(fill = Gini.x))+
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       direction = 1)+
  geom_sf(data = uc_sudeste, colour = "dark green",   fill = "dark green")+
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa

# DADOS população (estimativas da população)-----------------------------------------------------------
# FONTE:https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html?=&t=downloads
#Dados sobre população.
populacao<- read.csv2("Dados/Populacao2019.csv", sep = ";", stringsAsFactors = FALSE)%>%
  filter(UF=="RJ"|UF=="MG"|UF=="SP"|UF=="ES")%>%
  select(-c(6:14))

names(populacao)<-c("UF","COD.UF","COD.MUNI","name_muni","populacao")

populacao$name_muni<- str_to_title(populacao$name_muni)
populacao<-select(populacao,-c(1:3))
populacao[,2] <- gsub("[.]", "",populacao[,2])
populacao$populacao<-as.numeric(populacao$populacao)
code_muni<-as.data.frame(sudeste$code_muni)
populacao<-cbind(populacao,code_muni)
populacao$code_muni<-populacao$`sudeste$code_muni`
populacao<-populacao[,-3]

uc_sud<-left_join(uc_sud,populacao,by = "code_muni")
glimpse(uc_sud)
summary(uc_sud)
uc_sud<-distinct(uc_sud$code_muni.x)

#-PAMTabela 5457 - Área plantada ou destinada à colheita, área colhida, quantidade produzida, rendimento médio 
#e valor da produção das lavouras temporárias e permanentes.
#Fonte:https://sidra.ibge.gov.br/tabela/5457
area_plantada<- read.csv2("Dados/areaplantada.csv", sep = ";", stringsAsFactors = FALSE)
area_plantada<-area_plantada[2244:3905,]
area_plantada$Valor.producao<-as.numeric(area_plantada$Valor.producao)
area_plantada$code_muni<-area_plantada$Cód.
area_plantada<-area_plantada[,-1]

uc_sud<-left_join(uc_sud,area_plantada,by="code_muni")

#PPM-Tabela 3939 - Efetivo dos rebanhos, por tipo de rebanho
#Fonte:https://sidra.ibge.gov.br/tabela/3939
efetrebanho<-read.csv2("Dados/efetivorebanho.csv", sep = ";", stringsAsFactors = FALSE)%>%
  select(-c(3:9))
efetrebanho<-efetrebanho[2245:3909,]
efetrebanho$code_muni<-efetrebanho$Cód.
efetrebanho<-efetrebanho[,-1]

uc_sud<-left_join(uc_sud,efetrebanho,by="code_muni")

#criando variaveis
uc_sud<-uc_sud[,-c(9,13,15)]
#proporcao de area de UC nos municipios
uc_sud$prop_uc<-uc_sud$area_uc/uc_sud$area_municipio

#densidade Populacional
uc_sud$denpop<-uc_sud$populacao/uc_sud$area_municipio

ggplot()+
  geom_sf(data = uc_sud, fill="white")+
  geom_sf(data=uc_sud,aes(fill = denpop))+
  scale_fill_distiller(type = "seq",
                       palette = "Purples",
                       direction = 1)+
  geom_sf(data = uc_sudeste, colour = "dark green",   fill = "dark green")+
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa

# Valor da producao agricola por hectare
uc_sud$prod_hectare<-uc_sud$Valor.producao/uc_sud$area_municipio

ggplot()+
  geom_sf(data = uc_sud, fill="white")+
  geom_sf(data=uc_sud,aes(fill = prod_hectare))+
  scale_fill_distiller(type = "seq",
                       palette = "Oranges",
                       direction = 1)+
  geom_sf(data = uc_sudeste, colour = "dark green",   fill = "dark green")+
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa

# quantidade rebanho por hectare
uc_sud$reabanho_hectare<-uc_sud$Rebanho/uc_sud$area_municipio

ggplot()+
  geom_sf(data = uc_sud, fill="white")+
  geom_sf(data=uc_sud,aes(fill = reabanho_hectare))+
  scale_fill_distiller(type = "seq",
                       palette = "Greys",
                       direction = 1)+
  geom_sf(data = uc_sudeste, colour = "dark green",   fill = "dark green")+
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa
