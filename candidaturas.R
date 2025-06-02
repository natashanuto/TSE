#Arquivos recebidos
candidatos1 <- read.csv("~/consulta_cand_2024_BRASIL.csv", sep=";", comment.char="#", encoding = "latin1")
situacao <- read.csv("~/consulta_cand_complementar_2024_BRASIL.csv", sep=";", comment.char="#", encoding = "latin1")

#Arquivo PDA
temp <- tempfile()
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2024.zip",temp)
candidatos2 <- read.csv(unz(temp, "consulta_cand_2024_BRASIL.csv"), sep = ";", encoding = "latin1")
unlink(temp)

#Atividade 1




#Atividade 2

#Banco

banco <- merge(candidatos1, situacao, by = "SQ_CANDIDATO") %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, DS_SIT_TOT_TURNO, DS_SITUACAO_DIPLOMACAO)

dados_comparados <- candidatos1 %>%
  inner_join(situacao, by = "SQ_CANDIDATO") %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, DS_SIT_TOT_TURNO, DS_SITUACAO_DIPLOMACAO)

sum(duplicated(candidatos1$SQ_CANDIDATO)) 
sum(duplicated(situacao$SQ_CANDIDATO)) 




