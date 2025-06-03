#Arquivos recebidos
candidatos1 <- read.csv("~/consulta_cand_2024_BRASIL.csv", sep=";", comment.char="#", encoding = "latin1")

#Arquivo PDA
temp <- tempfile()
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2024.zip",temp)
candidatos2 <- read.csv(unz(temp, "consulta_cand_2024_BRASIL.csv"), sep = ";", encoding = "latin1")
unlink(temp)

#Atividade 1

diferencabancos <- anti_join(candidatos2, candidatos1, by = "SQ_CANDIDATO")

write.csv(diferencabancos, file = "C:/Users/natasha.smidt/Documents/TSE/diferenca_arq_pda.csv")

candidatos22 <- semi_join(candidatos2, candidatos1, by = "SQ_CANDIDATO") %>%
  rename(DS_SIT_TOT_TURNO2 = DS_SIT_TOT_TURNO)


#Removendo os duplicados


duplicados <- candidatos1 %>%
  filter(duplicated(SQ_CANDIDATO) | duplicated(SQ_CANDIDATO, fromLast = TRUE)) %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, NR_TURNO, DS_SIT_TOT_TURNO) %>%
  distinct()

candidatos1semduplicados <- candidatos1 %>%
  add_count(SQ_CANDIDATO) %>%
  filter(n == 1) %>%
  select(-n)

duplicados3 <- candidatos22 %>%
  filter(duplicated(SQ_CANDIDATO) | duplicated(SQ_CANDIDATO, fromLast = TRUE)) %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, DS_SIT_TOT_TURNO2) %>%
  rename(DS_SIT_TOT_TURNO = DS_SIT_TOT_TURNO2) %>%
  distinct()

candidatos2semduplicados <- candidatos22 %>%
  group_by(SQ_CANDIDATO) %>%
  filter(n() == 1) %>%  
  ungroup()

diferencaduplicados <- anti_join(
  duplicados,
  duplicados3,
  by = c("SQ_CANDIDATO", "NM_CANDIDATO", "DS_SIT_TOT_TURNO")
)

diferencaduplicados_rev <- anti_join(
  duplicados3,
  duplicados,
  by = c("SQ_CANDIDATO", "NM_CANDIDATO", "DS_SIT_TOT_TURNO")
)


#Não há diferença nos duplicados, ou seja, nos candidatos que foram para o segundo turno os dois bancos estão corretos.


#Banco

sittot <- candidatos1semduplicados %>%
  inner_join(candidatos2semduplicados, by = "SQ_CANDIDATO") %>%
  select(
    SQ_CANDIDATO,
    NM_CANDIDATO = NM_CANDIDATO.x,
    DS_SIT_TOT_TURNO,
    DS_SIT_TOT_TURNO2
  )

sittot_diferentes <- sittot %>%
  filter(DS_SIT_TOT_TURNO != DS_SIT_TOT_TURNO2)

sittot_diferentes_filtrado <- sittot_diferentes %>%
  filter(
    !(DS_SIT_TOT_TURNO == "#NULO#" & DS_SIT_TOT_TURNO2 == "#NULO")
  ) %>%
  rename(
    SIT_TOT_ARQ = DS_SIT_TOT_TURNO,
    SIT_TOT_PDA = DS_SIT_TOT_TURNO2
  )

teste <- sittot_diferentes %>%
  filter(DS_SIT_TOT_TURNO == "#NULO#",
         DS_SIT_TOT_TURNO2 == "#NULO")

teste2 <- sittot_diferentes_filtrado %>%
  count(SIT_TOT_ARQ, SIT_TOT_PDA) %>%
  arrange(desc(n))


write.csv(sittot_diferentes_filtrado, file = "C:/Users/natasha.smidt/Documents/TSE/situacao_totalizacao.csv")
write.csv(teste2, file = "C:/Users/natasha.smidt/Documents/TSE/quantidade.csv")

#363 informações de situação diferentes entre os bancos.

