#Teste Prático - Processo Seletivo TSE
#Natasha Nuto Smidt


#Dados
#Local de votação

local <- read_delim("~/TSE/Banco/eleitorado_local_votacao_2024.csv", 
                    delim = ";", 
                    locale = locale(encoding = "latin1"))

local <- local %>%
  group_by(SG_UF, NM_MUNICIPIO) %>%
  summarise(Zonas = n_distinct(NR_ZONA),
            Locais = n_distinct(NR_LOCAL_VOTACAO),
            Seções = n_distinct(NR_SECAO),
            .groups = 'drop')

#Seções

uf <- c('AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'ES', 'GO', 'MA', 'MG', 'MS', 'MT', 'PA', 'PB', 'PE', 'PI', 'PR', 'RJ', 'RN', 
        'RO', 'RR', 'RS', 'SC', 'SE', 'SP', 'TO')

options(timeout=600)

secoes <- lapply(uf, function(x) {
  temp<- tempfile()
  download.file(paste0("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitor_secao/perfil_eleitor_secao_2024_", x, ".zip"),temp)
  dados <- read.csv(unz(temp, paste0("perfil_eleitor_secao_2024_", x, ".csv")), sep  = ";", encoding='latin1', fileEncoding = 'latin1')
  unlink(temp)
  
  dados <- dados %>%
    group_by(SG_UF,NM_MUNICIPIO) %>%
    summarise(Eleitorado = n(),
              EleitoradoFeminino = sum(DS_GENERO == "FEMININO"),
              EleitoradoMasculino = sum(DS_GENERO == "MASCULINO"),
              .groups = 'drop')  %>%
    mutate(PorcentagemF = round(100*(EleitoradoFeminino / Eleitorado),2),
           PorcentagemM = round(100*(EleitoradoMasculino / Eleitorado),2))
  
  
  return(dados)}) %>% bind_rows()


#Validação

temp <- tempfile()
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_2024.zip",temp)
secoes <- read.csv(unz(temp, "perfil_eleitorado_2024.csv"), sep = ";", encoding = "latin1")
unlink(temp)

secoes <- secoes %>%
  group_by(SG_UF,NM_MUNICIPIO) %>%
  summarise(Eleitorado = n(),
            EleitoradoFeminino = sum(DS_GENERO == "FEMININO"),
            EleitoradoMasculino = sum(DS_GENERO == "MASCULINO"),
            .groups = 'drop')  %>%
  mutate(PorcentagemF = round(100*(EleitoradoFeminino / Eleitorado),2),
         PorcentagemM = round(100*(EleitoradoMasculino / Eleitorado),2))

#Tabela


tabela <- merge (local, secoes, by = c("SG_UF","NM_MUNICIPIO")) %>%
  rename(UF = SG_UF,
         Município = NM_MUNICIPIO) %>%
  mutate(Município = str_to_title(Município)) %>%
  arrange(Município)

write.csv(tabela, file = "Eleitorado2024.csv", row.names = FALSE)


#Mapa Eleitorado com Deficiência por UF

mapa <- lapply(uf, function(i) {
  t <- tempfile()
  download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitor_deficiente/perfil_eleitor_deficiencia_2024.zip",t)
  banco <- read.csv(unz(t, paste0("perfil_eleitor_deficiencia_2024_", i ,".csv")), sep  = ";", encoding='latin1')
  unlink(t)
  
  banco <- banco %>%
    group_by(SG_UF) %>%
    summarise(Eleitorado = n()) %>%
    bind_rows()
  
  return(banco)}) %>% bind_rows()
  

#Depois de fazer a função e meu computador rodar por 10 minutos, descobri que tinha um arquivo "BRASIL" 
#com todas as UF's. Então, pude usar como correção (validação), ambos dataframes "mapa" e "brasil" possuem as 
#mesmas informações.

t <- tempfile()
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitor_deficiente/perfil_eleitor_deficiencia_2024.zip",t)
brasil <- read.csv(unz(t,("perfil_eleitor_deficiencia_2024_BRASIL.csv")), sep  = ";", encoding='latin1')
unlink(t)

brasil <- brasil %>%
  group_by(SG_UF) %>%
  summarise(Eleitorado = n())

#Mapa

library(geobr)

teste <- read_state(code_state = "all", year = 2020) %>%
  filter(abbrev_state != "DF") %>%
  rename(SG_UF = abbrev_state)
  

grafico <- merge(teste,mapa, by = "SG_UF")

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())


ggplot() +
  geom_sf(data=grafico, aes(fill=Eleitorado), color= "black", size=.15) +
  labs(subtitle="Eleitorado com Deficiência, segundo UF", size=8) +
  scale_fill_distiller(palette = "Blues", name="Eleitorado", limits = c(3000,500000), direction=1, labels = scales::number_format(accuracy = 1, big.mark = ".")) +
  theme_minimal() +
  no_axis

ggsave("Mapa.png", width = 158, height = 93, units = "mm")

#EXTRA:
#Durante um projeto que gerenciei na empresa júnior ESTAT sobre Cafeicultura no Brasil,
#foi utilizado um mapa interativo do pacote "leaflet", como já tenho o código, segue 
#a versão interativa do mapa acima.



library(sf)
library(leaflet)
library(htmltools)
library(RColorBrewer)

mybins <- c(0, 100000, 200000, 300000, 400000, 500000)
mypalette <- colorBin(
  palette = "Blues",
  na.color = "transparent", bins = mybins
)

mytext <- paste(
  "UF: ", grafico$SG_UF, "<br/>",
  "Eleitorado com Deficiência: ", grafico$Eleitorado, "<br/>",
  sep = ""
) %>%
  lapply(htmltools::HTML)

library(ggplot2)
library(magrittr)
library(sf)

simplificado <- st_simplify(grafico, preserveTopology = TRUE, dTolerance = 1000)

mapainterativo <- leaflet(simplificado) %>%
  addProviderTiles("CartoDB.Positron") %>%  
  setView(lng = -55, lat = -15, zoom = 4) %>%
  addPolygons(
    fillColor = ~ mypalette(Eleitorado),
    stroke = TRUE,
    fillOpacity = 0.9,
    color = "black",
    weight = 0.3,
    label = mytext,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = mypalette, values = ~Eleitorado, opacity = 0.9,
    title = "Eleitorado com Deficiência", position = "bottomleft"
  )

mapainterativo

htmlwidgets::saveWidget(mapainterativo, file=paste0( getwd(), "/MapaInterativo.html"))


#Canditados

candidaturas <- lapply(uf, function(y) {
  tt <- tempfile()
  download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2024.zip",tt)
  candidatos <- read.csv(unz(tt, paste0("consulta_cand_2024_", y ,".csv")), sep  = ";", encoding='latin1')
  unlink(tt)
  
  return(candidatos)}) %>% bind_rows()


#Validação

tt <- tempfile()
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2024.zip",tt)
candidatos2 <- read.csv(unz(tt, paste0("consulta_cand_2024_BRASIL.csv")), sep  = ";", encoding='latin1')
unlink(tt) #Correto, ambos os dataframes estão iguais


capitais <- c("RIO BRANCO", "MACEIÓ", "MANAUS", "MACAPÁ", "SALVADOR", "FORTALEZA", "VITÓRIA",
              "GOIÂNIA", "SÃO LUÍS", "BELO HORIZONTE", "CAMPO GRANDE", "CUIABÁ", "BELÉM",
              "JOÃO PESSOA", "RECIFE", "TERESINA", "CURITIBA", "RIO DE JANEIRO", "NATAL", "PORTO VELHO", "BOA VISTA",
              "PORTO ALEGRE", "FLORIANÓPOLIS", "ARACAJU", "SÃO PAULO", "PALMAS")

uf <- c('AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'ES', 'GO', 'MA', 'MG', 'MS', 'MT', 'PA', 'PB', 'PE', 'PI', 'PR', 'RJ', 'RN', 
        'RO', 'RR', 'RS', 'SC', 'SE', 'SP', 'TO')


r <- c("RIO BRANCO", "FORTALEZA", "MACEIÓ")

candidatos <- candidaturas %>%
  select(SG_UF, NM_UE, NM_CANDIDATO, DS_CARGO, NR_TURNO, DS_SIT_TOT_TURNO) %>%
  filter(DS_CARGO == "PREFEITO", DS_SIT_TOT_TURNO == "ELEITO") %>%
  rename(UF = SG_UF,
         Cidade = NM_UE,
         Candidato = NM_CANDIDATO,
         Cargo = DS_CARGO, 
         Turno = NR_TURNO,
         Situação = DS_SIT_TOT_TURNO) 

capitais <- c("RIO BRANCO", "MACEIÓ", "MANAUS", "MACAPÁ", "SALVADOR", "FORTALEZA", "VITÓRIA",
              "GOIÂNIA", "SÃO LUÍS", "BELO HORIZONTE", "CAMPO GRANDE", "CUIABÁ", "BELÉM",
              "JOÃO PESSOA", "RECIFE", "TERESINA", "CURITIBA", "RIO DE JANEIRO", "NATAL", "PORTO VELHO", "BOA VISTA",
              "PORTO ALEGRE", "FLORIANÓPOLIS", "ARACAJU", "SÃO PAULO", "PALMAS")

uf <- c('AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'ES', 'GO', 'MA', 'MG', 'MS', 'MT', 'PA', 'PB', 'PE', 'PI', 'PR', 'RJ', 'RN', 
        'RO', 'RR', 'RS', 'SC', 'SE', 'SP', 'TO')


tabela2 <- lapply(seq_along(uf), function(w) {
  candidatos %>%
    filter(UF == uf[w]) %>%
    filter(Cidade == capitais[w])
}) %>% bind_rows()

tabela2 <- tabela2 %>%
  mutate(Cidade = str_to_title(Cidade),
         Cargo = str_to_title(Cargo),
         Candidato = str_to_title(Candidato),
         Situação = str_to_title(Situação)) %>%
  mutate(Turno = case_when(Turno == 1 ~ "1º turno",
                           Turno == 2 ~ "2º turno"))

write.csv(tabela2, file = "Candidatos2024.csv", row.names = FALSE)
