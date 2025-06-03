#Arquivos recebidos
candidatos1 <- read.csv("~/consulta_cand_2024_BRASIL.csv", sep=";", comment.char="#", encoding = "latin1")
situacao <- read.csv("~/consulta_cand_complementar_2024_BRASIL.csv", sep=";", comment.char="#", encoding = "latin1")


#Atividade 2

#Candidatos - 2 Turno


duplicados <- candidatos1 %>%
  filter(duplicated(SQ_CANDIDATO) | duplicated(SQ_CANDIDATO, fromLast = TRUE)) %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, NR_TURNO, DS_SIT_TOT_TURNO) %>%
  distinct()


duplicados2 <- situacao %>%
  filter(duplicated(SQ_CANDIDATO) | duplicated(SQ_CANDIDATO, fromLast = TRUE)) %>%
  select(SQ_CANDIDATO, DS_SITUACAO_DIPLOMACAO) 


dados_completos <- duplicados %>%
  left_join(duplicados2, by = "SQ_CANDIDATO") %>%
  distinct() 

teste <- dados_completos %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO",
         DS_SITUACAO_DIPLOMACAO != "Diplomado")

teste2 <- dados_completos %>%
  filter(
    !DS_SIT_TOT_TURNO %in% c("ELEITO", "2º TURNO"),
    DS_SITUACAO_DIPLOMACAO == "Diplomado"
  ) %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, DS_SIT_TOT_TURNO, DS_SITUACAO_DIPLOMACAO)

teste3 <- duplicados %>%
  filter(SQ_CANDIDATO == "260001926363")

teste4 <- duplicados2 %>%
  filter(SQ_CANDIDATO == "260001926363" )

#Candidatos - 1 Turno


situacaosemduplicados <- situacao %>%
  group_by(SQ_CANDIDATO) %>%
  filter(n() == 1) %>%
  ungroup()

candidatos1semduplicados <- candidatos1 %>%
  add_count(SQ_CANDIDATO) %>%
  filter(n == 1) %>%
  select(-n)


#Banco

banco <- candidatos1semduplicados %>%
  inner_join(situacaosemduplicados, by = "SQ_CANDIDATO") %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, DS_SIT_TOT_TURNO, DS_SITUACAO_DIPLOMACAO)

eleitossemdiplomação <- banco %>%
  filter(DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"),
         DS_SITUACAO_DIPLOMACAO != "Diplomado")

qtd <- eleitossemdiplomação %>%
  count(DS_SIT_TOT_TURNO, DS_SITUACAO_DIPLOMACAO) %>%
  arrange(desc(n))

write.csv(qtd, "C:/Users/natasha.smidt/Documents/TSE/qtd.csv")
write.csv(eleitossemdiplomação, "C:/Users/natasha.smidt/Documents/TSE/eleitos_sem_diplomacao.csv")

suplentessemdiplomação <- banco %>%
  filter(DS_SIT_TOT_TURNO == "SUPLENTE",
         DS_SITUACAO_DIPLOMACAO != "Diplomado")

qtd2 <- suplentessemdiplomação %>%
  count(DS_SIT_TOT_TURNO, DS_SITUACAO_DIPLOMACAO) %>%
  arrange(desc(n))

write.csv(qtd2, "C:/Users/natasha.smidt/Documents/TSE/qtd2.csv")
write.csv(suplentessemdiplomação, "C:/Users/natasha.smidt/Documents/TSE/suplentes_nao_diplomacao.csv")

#Ao considerar "ELEITO", "ELEITO POR MÉDIA" e "ELEITO POR QP" tem-se apenas "Pendente" e "Aguardando diplomação". No entanto,
#se considerar "SUPLENTE" tem-se "Cassado", "Não diplomável", "Pendente" e "Aguardando diplomação".

naoeleitosdiplomados <- banco %>%
  filter(
    !DS_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP", "SUPLENTE"),
    DS_SITUACAO_DIPLOMACAO == "Diplomado"
  )


naoeleitosdiplomados <- rbind(naoeleitosdiplomados, teste2)

write.csv(naoeleitosdiplomados, "C:/Users/natasha.smidt/Documents/TSE/diplomados_nao_eleitos.csv")

#Apenas "#NULO".