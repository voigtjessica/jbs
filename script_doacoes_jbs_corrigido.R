#Script Corre??o

library(electionsBR)
library(dplyr)
library(ggplot2)
library(scales)
library(extrafont)
library(tidyr)
library(stringr )
loadfonts(device="win")

#TSE: 
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\JBS\\github")
receitas_candidatos <-  tryCatch(read.table("receitas_candidatos_2014_brasil.txt",  colClasses = "character", header = T, sep = ";",
                                            stringsAsFactors = F, fill = T))
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\JBS\\github")
save(receitas_candidatos, file="receitas_candidatos.RData")

load("receitas_candidatos.RData")

load(file="lista_cnpjs_jbs.Rdata")
lista_cnpjs <- lista_cnpjs_jbs %>%
  distinct(CNPJ, .keep_all = T) %>%
  mutate(CNPJ = str_trim(CNPJ)) %>%
  mutate(ID = 1:n()) %>%
  select(5, 1,2,3,4)

idx <- which(names(receitas_candidatos) %in% c("CPF.do.candidato", "CPF.CNPJ.do.doador.origin?rio","CPF.CNPJ.do.doador.origin?rio",
                                               "CPF.CNPJ.do.doador", "UF", "Sigla..Partido", "Cargo", "Nome.candidato", "Valor.receita",
                                               "Numero.Recibo.Eleitoral", "Numero.do.documento"))

receitas_candidatos <- receitas_candidatos %>%
  mutate(CNPJ = ifelse(CPF.CNPJ.do.doador.origin?rio == "#NULO", CPF.CNPJ.do.doador,
                       ifelse(CPF.CNPJ.do.doador %in% lista_cnpjs$CNPJ, CPF.CNPJ.do.doador, CPF.CNPJ.do.doador.origin?rio)))

doadores_2014_full <- receitas_candidatos %>%
  left_join(lista_cnpjs, by = "CNPJ") %>%
  mutate(Valor.receita = as.numeric(gsub(",", "\\.", Valor.receita))) %>%
  mutate( ID = 1:n()) %>%
  mutate(Cargo = toupper(Cargo))

#Banco Leo Barone (Elections.br)

load("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/info_depfed_2014.RData")

seg_turno <- info_depfed_2014 %>%                #corre??o para candidatos ao executivo n?o aparecerem 2 vezes
  filter(NUM_TURNO == 2,
         DESC_SIT_TOT_TURNO == "ELEITO") %>%
  select(CPF_CANDIDATO, DESC_SIT_TOT_TURNO ) %>%
  rename(seg_turno = DESC_SIT_TOT_TURNO,
         CPF.do.candidato = CPF_CANDIDATO)

#escolheremos as seguintes situa??es de candidatura: 

info_2014_final <- info_depfed_2014 %>%
  rename(CPF.do.candidato = CPF_CANDIDATO,
         Cargo = DESCRICAO_CARGO) %>%
  mutate(DES_SITUACAO_CANDIDATURA = ifelse(CPF.do.candidato == "45334773487", "DEFERIDO", 
                                           DES_SITUACAO_CANDIDATURA)) %>%
  right_join(doadores_2014_full, by = c("CPF.do.candidato", "Cargo")) %>% #Crit?rios do cargo s?o do TSE
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO"|
           DES_SITUACAO_CANDIDATURA == "DEFERIDO COM RECURSO"|
           DES_SITUACAO_CANDIDATURA == "INDEFERIDO COM RECURSO" |
           DES_SITUACAO_CANDIDATURA == "SUBSTITUTO PENDENTE DE JULGAMENTO") %>%
  left_join(seg_turno) %>%
  mutate(seg_turno = ifelse(is.na(seg_turno)&DESC_SIT_TOT_TURNO=="2? TURNO",
                            "N?O ELEITO", seg_turno)) %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "2? TURNO" , seg_turno,
                                     DESC_SIT_TOT_TURNO))%>%
  mutate(agrupador = ifelse(agrupador == "JBS", "JBS", NA))  %>%
  mutate(receita_corrigida = round(Valor.receita*1.2027770, 0)) %>%
  distinct(ID, .keep_all = TRUE)

#total JBS: 362.239.750
#total JBS: 435.693.640

###

aux1_receita_por_candidato <- info_2014_final %>%
  group_by(CPF.do.candidato, Cargo,
           NOME_URNA_CANDIDATO, Sigla..Partido, UF, agrupador, DESC_SIT_TOT_TURNO) %>%
  summarise(receita_corrigida = sum(receita_corrigida)) %>%
  ungroup()   #aqui eu somo todas as receitas e divido em JBS e NA

aux2_receita_por_candidato <- aux1_receita_por_candidato %>%  
  group_by(CPF.do.candidato, Cargo) %>%
  summarise(Receita_total_candidato = sum(receita_corrigida))
#aqui eu s? somo o total que cada cara arrecadou para juntas com a planilha do lado

aux3_receita_por_candidato <- aux1_receita_por_candidato %>%
  left_join(aux2_receita_por_candidato, by = c("CPF.do.candidato","Cargo")) %>%
  mutate(participacao = round( receita_corrigida/Receita_total_candidato, 2)) %>%
  distinct(CPF.do.candidato, Cargo, agrupador, .keep_all = TRUE ) %>%
  select(CPF.do.candidato, Cargo, NOME_URNA_CANDIDATO, Sigla..Partido, UF, agrupador, DESC_SIT_TOT_TURNO,
         receita_corrigida, Receita_total_candidato, participacao)
#aqui eu junto as receitas parciais da JBS quando elas existem (aux1) com as receitas totais (aux2)

aux4_receita_por_candidato <- aux3_receita_por_candidato %>%
  mutate(JBS = ifelse(agrupador == "JBS", 1,0)) %>%
  filter(JBS == 1) %>%
  select(CPF.do.candidato, Cargo, JBS)
#aqui eu crio uma coluna para indicar quem recebeu financiamento e quem n?o recebeu.
# Essa coluna ? necess?ria porque na planilha acima os financiados pela JBS tem duas linhas
# Sendo assim, marcarei abaixo por CPF (e n?o por origem da receita) quem foi e quem n?o foi
# financiado.

receita_por_candidato <- aux3_receita_por_candidato %>%
  left_join(aux4_receita_por_candidato, by=c("CPF.do.candidato", "Cargo")) %>%
  mutate(JBS = ifelse(is.na(JBS), 0, JBS)) %>%
  mutate(jogar_fora = ifelse(is.na(agrupador)&JBS == 1, 1,0 )) %>%
  filter(jogar_fora != 1) %>%     #jogo fora as linhas extras dos financiados pela JBS
  rename(receita_jbs = receita_corrigida) %>%
  mutate(receita_jbs = ifelse(is.na(agrupador),0, receita_jbs)) %>%
  mutate(Cargo = ifelse(Cargo == "DEPUTADO DISTRITAL", "DEPUTADO ESTADUAL", Cargo)) %>%
  mutate(participacao = ifelse(is.na(agrupador),0,participacao))
#A planilha acima cont?m os financiamentos totais de todo mundo e os financiamentos
# da JBS na coluna receita_jbs daqueles que receberam do conglomerado.

setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
write.table(receita_por_candidato, file="receita_por_candidato.csv", sep=";", row.names = F) 

#Quanto cada cargo ganhou?

ganhos_cargo <- receita_por_candidato %>%
  group_by(Cargo) %>%
  summarise(doacoes_jbs = sum(receita_jbs),
            total_arrecadado = sum(Receita_total_candidato))

ganhos_cargo_total <- c("TOTAL", sum(ganhos_cargo$doacoes_jbs), sum(ganhos_cargo$total_arrecadado))

ganhos_cargo <- ganhos_cargo %>%
  rbind(ganhos_cargo_total) %>%
  mutate(total_arrecadado = as.numeric(total_arrecadado),
         doacoes_jbs = as.numeric(doacoes_jbs),
         participacao_jbs = doacoes_jbs/total_arrecadado) 


setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
write.table(ganhos_cargo, file="ganhos_cargo.csv", sep=";", row.names = F) 

#candidatos financiados e eleitos

candidatos_financiados <- receita_por_candidato %>%
  mutate(num= ifelse(JBS==1,1,0)) %>%
  group_by(Cargo) %>%
  summarise(financiados = sum(num)) %>%
  mutate(financiados = ifelse(Cargo == "Presidente", 4, financiados)) #Marina e Eduardo Campos foram considerados um s? candidato

setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
write.table(candidatos_financiados, file="candidatos_financiados.csv")

#Tabela 1

tabela1 <- candidatos_financiados %>%
  left_join(ganhos_cargo) 

tabela1_total <- c("Total", sum(tabela1$financiados), sum(tabela1$doacoes_jbs),
                   sum(tabela1$total_arrecadado), sum(tabela1$doacoes_jbs)/sum(tabela1$total_arrecadado)
)

tabela1 <- tabela1 %>%
  rbind(tabela1_total) %>%
  select(Cargo, financiados, doacoes_jbs, participacao_jbs)

setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
write.table(tabela1, file="tabela1.csv", sep=";", row.names = F)
#Quanto a JBS deu por partido

#Gr?fico 2
#Dados do ?s Claras

doacoes <- c(103000*2.7575348, 12863000*1.9236159, 65393000*1.5098914, 435693640) #esse valor j? t? corrigido para abril 2017
ano <- c(2002, 2006, 2010, 2014)
graf2_bd <- data.frame(ano, doacoes, tipo= "total") 


library(readxl)
JBS_doacoes_2002_a_2010_eleitos <- read_excel("~/Financiamento JBS/Arquivos/correcao/JBS_doacoes_2002_a_2010_eleitos.xlsx", 
                                              col_types = c("numeric", "text", "text", 
                                                            "text", "text", "numeric", "numeric", 
                                                            "text", "text", "numeric"))


montante_cand_eleito_2014 <- receita_por_candidato %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP", "ELEITO", DESC_SIT_TOT_TURNO),
         DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", "ELEITO", DESC_SIT_TOT_TURNO),
         ano = 2014) %>%
  filter(agrupador == "JBS",
         DESC_SIT_TOT_TURNO == "ELEITO") %>%
  group_by(ano) %>%
  summarise(doacoes = round(sum(receita_jbs),0)) %>%
  mutate(tipo = "eleito")

montante_candidatos_eleitos_ano <- JBS_doacoes_2002_a_2010_eleitos %>%
  mutate(Resultado = ifelse(Resultado == "Eleito por m?dia" , "ELEITO", Resultado),
         Resultado = ifelse(Resultado == "Eleito" , "ELEITO", Resultado)) %>%
  group_by(ano) %>%
  summarise(doacoes = round(sum(doacoes_corrigidas),0)) %>%
  mutate(tipo = "eleito") %>%
  bind_rows(montante_cand_eleito_2014)


graf2_bd_v1 <- graf2_bd %>%
  bind_rows(montante_candidatos_eleitos_ano)  %>%
  mutate(doacoes_milhoes = round(doacoes/1000000),3)

cores2 <- c("#f2c218", "#b5b2aa")
#install.packages("ggrepel") 
library("ggrepel")


graf2 <- ggplot(graf2_bd_v1, aes(x=ano, y=doacoes_milhoes,
                                 group=tipo, colour = tipo)) + 
  geom_line(size = 1) + scale_x_continuous(breaks = c(2002, 2006, 2010,2014)) +
  labs(title="", 
       subtitle="", 
       caption="", 
       y="Doa??es em milh?es",
       x=" ",
       color=NULL, size=12) +
  scale_color_manual(labels = c("Financiamento candidatos eleitos", "Financiamento total"), 
                     values =  (cores2)) + 
  theme(legend.position="bottom") + 
  theme(axis.text.x = element_text(vjust=0.5, size = 12)) + 
  theme(legend.text=element_text(size=12))

graf2

setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
ggsave(graf2, file="graf2.png", width = 12, height = 8, scale = .75)


graf2_bd_v2 <- graf2_bd_v1 %>%
  select(ano, doacoes, tipo) %>%
  spread(tipo, doacoes)

write.table(graf2_bd_v2, file="graf2_bd_v2.csv", sep=";", row.names = F, dec =",")

#Gr?ficoDoacoes e receitas (todos os cargos)
#Dados do ?s Claras

receita_total_2014 <- sum(receita_por_candidato$Receita_total_candidato)
#5.237.702.321

receitas_totais <- c(792546932*2.7575348,  1729042577*1.9236159, 
                     3666605190*1.5098914, 5237702321 ) #esse valor j? t? corrigido para abril 2017
doacoes_jbs <- c(103000*2.7575348, 12863000*1.9236159, 65393000*1.5098914, 435693640)
ano <- c(2002, 2006, 2010, 2014)
grafdoacoesreceitas_bd1 <- data.frame(ano, doacoes_jbs, receitas_totais) 

grafdoacoesreceitas_bd <-grafdoacoesreceitas_bd1 %>%
  gather(tipo, valor, doacoes_jbs:receitas_totais) %>%
  mutate(valor = valor/1000000000)

grafdoacoesreceitas <- ggplot(grafdoacoesreceitas_bd, aes(x=ano, y=valor,
                                                          group=tipo, colour = tipo)) + 
  geom_line(size = 1) + scale_x_continuous(breaks = c(2002, 2006, 2010,2014)) +
  labs(title="", 
       subtitle="", 
       caption="", 
       y="Doa??es em bilh?es",
       x=" ",
       color=NULL, size=12) +
  scale_color_manual(labels = c("Doa??es JBS", "Receitas totais"), 
                     values =  (cores2)) + 
  theme(legend.position="bottom") + 
  theme(axis.text.x = element_text(vjust=0.5, size = 12)) + 
  theme(legend.text=element_text(size=12)) + scale_y_continuous(breaks = seq(0, 5, by = 0.5))

ggsave(grafdoacoesreceitas, file="grafdoacoesreceitas.png", width = 12, height = 8, scale = .75)


#Gr?fico 3
library(tidyr)

graf3_bd <- receita_por_candidato %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP" | 
                                       DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", "ELEITO", DESC_SIT_TOT_TURNO)) %>%
  filter(DESC_SIT_TOT_TURNO == "ELEITO") %>%
  group_by(Cargo) %>%
  summarise(doacoes_jbs_eleitos = sum(receita_jbs)) %>%
  left_join(ganhos_cargo) %>%
  select(Cargo, doacoes_jbs_eleitos, doacoes_jbs) %>%
  mutate(eficiencia = doacoes_jbs_eleitos/doacoes_jbs) %>%
  select(Cargo, eficiencia) %>%
  mutate(nao_eleito =  1 - eficiencia) %>%
  rename(eleito = eficiencia) %>%
  gather(Financiados, perc_recebido, -Cargo) %>%
  mutate(ordem = ifelse(Cargo == "PRESIDENTE", 1,0),
         ordem = ifelse(Cargo == "DEPUTADO FEDERAL", 2,ordem),
         ordem = ifelse(Cargo == "SENADOR", 3,ordem),
         ordem = ifelse(Cargo == "GOVERNADOR", 4,ordem),
         ordem = ifelse(Cargo == "DEPUTADO ESTADUAL", 5,ordem),
         ordemeleito = ifelse( Financiados == "eleito" ,2,1))


graf3_bd$Cargo <- reorder(graf3_bd$Cargo, graf3_bd$ordem)
graf3_bd$Financiados <- reorder(graf3_bd$Financiados, graf3_bd$ordemeleito)


cores <- c("#f7e6a8", "#f7cb2e")


graf3 <- graf3_bd %>%
  mutate(perc_label = paste(round(perc_recebido,2)*100, "%", sep=" " )) %>%
  ggplot(aes(x=Cargo, y=perc_recebido, fill = Financiados, label = perc_label)) + 
  geom_bar(stat="identity") + geom_text(position = "stack", aes(vjust=2)) + 
  scale_y_continuous(labels = percent) + ggtitle(" ") + 
  scale_fill_manual(values=cores, labels=c("Doado para candidatos n?o eleitos", 
                                           "Doado a candidatos eleitos")) + labs(x="", y="Percentual recebido", fill="") + 
  theme(legend.position="bottom")

setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
ggsave(graf3, file="graf3.png", width = 12, height = 8, scale = .75)


#grafico 4

doacoes_partido <- receita_por_candidato %>%
  filter(agrupador == "JBS") %>%
  group_by(Sigla..Partido) %>%
  summarise(doacoes_jbs_partido = sum(receita_jbs)) %>%
  mutate(doacoes_jbs_partido_milhao = round(doacoes_jbs_partido/1000000,1))

doacoes_partido$Sigla..Partido <- reorder(doacoes_partido$Sigla..Partido, doacoes_partido$doacoes_jbs_partido_milhao)



setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
write.table(doacoes_partido, file="doacoes_partido.csv")

graf4  <- ggplot(doacoes_partido , 
                 aes(x=Sigla..Partido, y=doacoes_jbs_partido_milhao,
                     label = doacoes_jbs_partido_milhao)) + 
  geom_segment(aes(x=Sigla..Partido, 
                   xend=Sigla..Partido, 
                   y=0, 
                   yend=doacoes_jbs_partido_milhao )) + 
  geom_point(stat='identity', fill="#f7cb2e", 
             colour="#f7cb2e",pch=21, size=13) +
  coord_flip() + geom_text(color="black", size=5) +
  labs(title="", 
       subtitle="", 
       caption="",
       y = "Doa??es em milh?es", 
       x = "", size = 3) + 
  theme(axis.text.x = element_text(vjust=0.6),
        axis.text.y = element_text(vjust=1.2)) 


ggsave(graf4, file="graf4.png", width = 12, height = 8)

#Gr?fico 5 
#participa??o da jbs no financiamento dos partidos

graf5_db <- receita_por_candidato %>%
  group_by(Sigla..Partido) %>%
  summarise(doacoes_jbs_partido = sum(receita_jbs),
            receita_total_partido = sum(Receita_total_candidato)) %>%
  mutate(participacao = round(doacoes_jbs_partido/receita_total_partido,2 )) %>%
  mutate(doacoes_jbs_milhao = doacoes_jbs_partido/1000000) %>%
  filter(doacoes_jbs_partido != 0)


theme_set(theme_classic())

# ordenando por participa??o
graf5_db <- graf5_db %>%
  graf5_db$Sigla..Partido <- factor(graf5_db$Sigla..Partido, levels = graf5_db$Sigla..Partido[order(graf5_db$participacao)]) 
graf5_db$Sigla..Partido  # notice the changed order of factor levels

graf5 <- ggplot(graf5_db, 
                aes(doacoes_jbs_milhao, participacao, label = Sigla..Partido), colour = class) + 
  geom_text(size=5, alpha=.5, position=position_jitter(width=.5,height=.01)) +
  geom_smooth(method="loess", se=F) +
  scale_y_continuous(labels=scales::percent) +
  labs(title="", 
       subtitle= " ", 
       caption="",
       x = "Doa??es em R$ milh?es",
       y = "Participa??o da JBS no total arrecadado pelo partido") +theme(axis.text=element_text(size=13),
                                                                          axis.title=element_text(size=13))


setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
ggsave(graf5, file="graf5.png", width = 12, height = 10) 


#Tabela 2
#financiamento JBS - candidatos ? presid?ncia

tabela2 <- receita_por_candidato %>%
  filter(Cargo == "PRESIDENTE",
         agrupador == "JBS")%>%
  mutate(NOME_URNA_CANDIDATO = ifelse(NOME_URNA_CANDIDATO == "EDUARDO CAMPOS", 
                                      "MARINA SILVA", NOME_URNA_CANDIDATO)) %>%
  group_by(NOME_URNA_CANDIDATO, Sigla..Partido) %>%
  summarise(doacoes_jbs = sum(receita_jbs),
            Receita_total_candidato = sum(Receita_total_candidato)) %>%
  mutate(participacao = round(doacoes_jbs/Receita_total_candidato,2)) 

tabela2_total <- data.frame(NOME_URNA_CANDIDATO = "Total",
                            Sigla..Partido = " ",
                            doacoes_jbs = sum(tabela2$doacoes_jbs),
                            Receita_total_candidato = sum(tabela2$Receita_total_candidato),
                            participacao = sum(tabela2$doacoes_jbs) / sum(tabela2$Receita_total_candidato))

tabela2 <- tabela2 %>%
  bind_rows(tabela2_total) %>%
  mutate(doacoes_jbs_milhoes = round(doacoes_jbs/1000000,2),
         Receita_total_candidato_milhoes = round(Receita_total_candidato/1000000,2)) %>%
  select(NOME_URNA_CANDIDATO,Sigla..Partido, doacoes_jbs_milhoes, Receita_total_candidato_milhoes, 
         participacao) %>%
  arrange(desc(doacoes_jbs_milhoes)) 

write.table(tabela2, file="tabela2.csv", sep=";", row.names = FALSE)

#Tabela 3
#candidatos financiados ao senado por partido

tabela3 <- receita_por_candidato %>%
  filter(agrupador == "JBS",
         Cargo == "SENADOR") %>%
  mutate(num = 1) %>%
  mutate(eleito = ifelse(DESC_SIT_TOT_TURNO == "ELEITO", 1,0)) %>%
  group_by(Sigla..Partido) %>%
  summarise(candidatos_financiados = sum(num),
            candidatos_eleitos = sum(eleito),
            doacoes_jbs_partido = sum(receita_jbs)) %>%
  mutate(doacoes_jbs_partido = round(doacoes_jbs_partido/1000000,2)) %>%
  arrange(desc(doacoes_jbs_partido))

write.table(tabela3, file="tabela3.csv", sep=";", row.names = FALSE)


tabela3_senadores_uf <- receita_por_candidato %>%
  filter(Cargo == "SENADOR") %>%
  mutate(eleito= ifelse(DESC_SIT_TOT_TURNO == "ELEITO",
                        ifelse(agrupador == "JBS", 1,0),0)) %>%
  group_by(UF) %>%
  summarise(doacoes_senado_jbs_uf_total = sum(receita_jbs),
            doacoes_senado_totais_uf = sum(Receita_total_candidato),
            candidatos_eleitos = sum(eleito)) %>%
  mutate(participacao = round(doacoes_senado_jbs_uf_total/doacoes_senado_totais_uf,2)) %>%
  mutate(doacoes_senado_jbs_uf_total = round(doacoes_senado_jbs_uf_total/1000000,2),
         doacoes_senado_totais_uf= round(doacoes_senado_totais_uf/1000000,2)) 

x <- receita_por_candidato %>%
  filter(Cargo == "SENADOR",
         UF =="RR")

write.table(tabela3_senadores_uf, file="tabela3_senadores_uf.csv", sep=";", row.names = FALSE)


#GR?FICO 6
#SENADORES FINANCIADOS PELA JBS POR BANCADA (2010 e 2014)

library(readr)
senadores_eleitos_2010 <- read_delim("~/Financiamento JBS/Arquivos/senadores_eleitos_2010.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

graf6_bd2010 <-  senadores_eleitos_2010  %>%
  mutate(eleito=1,
         financiado = ifelse(agrupador == "JBS",1,0),
         financiado = ifelse(is.na(financiado),0,financiado)) %>%
  group_by(Sigla..Partido) %>%
  summarise(senadores_eleitos = sum(eleito),
            financiado = sum(financiado))


graf6_bd2010

graf6_bd <- receita_por_candidato %>%
  filter(Cargo == "SENADOR",
         DESC_SIT_TOT_TURNO == "ELEITO") %>%
  mutate(eleito =1,
         financiado = ifelse(agrupador == "JBS", 1,0),
         financiado = ifelse(is.na(financiado),0, financiado)) %>%
  group_by(Sigla..Partido) %>%
  summarise(senadores_eleitos = sum(eleito),
            financiado = sum(financiado)) %>%
  bind_rows(graf6_bd2010) %>%
  group_by(Sigla..Partido) %>%
  summarise(senadores_eleitos = sum(senadores_eleitos),
            financiado = sum(financiado)) %>%
  mutate(nao_financiados = senadores_eleitos - financiado) %>%
  gather(situacao , senadores , senadores_eleitos:nao_financiados) %>%
  filter(situacao != "senadores_eleitos") %>%
  mutate(senadores = as.numeric(senadores))

graf6_bd$Sigla..Partido <- reorder(graf7_bd$Sigla..Partido, -rep(graf7_bd$senadores))
graf6_bd$situacao <- reorder(graf7_bd$situacao, ifelse(graf7_bd$situacao == "financiados_jbs",2,1))

graf6 <- graf6_bd %>%
  ggplot(aes(x=Sigla..Partido, y=senadores, fill = situacao)) + 
  geom_bar(stat="identity", position = "stack") + 
  ggtitle("") + 
  scale_fill_manual(values=cores, labels=c("Sem financiamento", "Financiados")) + 
  labs(x="", y="Senadores", fill="") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  scale_y_continuous(breaks=pretty_breaks(n=3), limits=c(0,30)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


graf6

setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
ggsave(graf6, file="graf6.png", width = 12, height = 8, scale = .75)

#Tabela x - senadores eleitos financiados pela jbs por uf

tabelax_2010 <-  senadores_eleitos_2010  %>%
  mutate(financiado = ifelse(agrupador == "JBS",1,0),
         financiado = ifelse(is.na(financiado),0,financiado)) %>%
  group_by(UF) %>%
  summarise(financiados_jbs = sum(financiado))


tabelax <- receita_por_candidato %>%
  filter(Cargo == "SENADOR",
         DESC_SIT_TOT_TURNO == "ELEITO") %>%
  mutate(agrupador = ifelse(is.na(agrupador),0,agrupador),
         financiado = ifelse(agrupador == "JBS", 1,0)) %>%
  group_by(UF) %>%
  summarise(financiados_jbs = sum(financiado)) %>%
  bind_rows(tabelax_2010) %>%
  group_by(UF) %>%
  summarise(financiados_jbs = sum(financiados_jbs))

setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
write.table(tabelax, file="tabelax.csv", sep=";", row.names = F, dec = ",")


#Tabela 4
tabela4 <- receita_por_candidato %>%
  filter(agrupador == "JBS",
         Cargo == "DEPUTADO FEDERAL") %>%
  mutate(candidato = 1,
         eleito = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP" | 
                           DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA" |
                           DESC_SIT_TOT_TURNO == "ELEITO",1,0)) %>%
  group_by(Sigla..Partido) %>%
  summarise(candidatos_financiados = sum(candidato),
            candidatos_eleitos = sum(eleito),
            doacoes_jbs = sum(receita_jbs)/1000000)

setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
write.table(tabela4, file="tabela4.csv", sep=";", row.names = FALSE, dec=",")

tabela4_alternativa <- receita_por_candidato %>%
  filter(agrupador == "JBS",
         Cargo == "DEPUTADO FEDERAL") %>%
  mutate(candidato = 1,
         eleito = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP" | 
                           DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA" |
                           DESC_SIT_TOT_TURNO == "ELEITO",1,0)) %>%
  group_by(Sigla..Partido) %>%
  summarise(candidatos_financiados = sum(candidato),
            candidatos_eleitos = sum(eleito),
            doacoes_jbs = sum(receita_jbs)/1000000)


#que partido que recebeu mais no senado em 2010?

library(readxl)
receitas_senadores_2010 <- read_excel("~/Financiamento JBS/Arquivos/correcao/receitas_senadores_2010.xlsx", 
                                      col_types = c("text", "text", "text", 
                                                    "text", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "text"))
View(receitas_senadores_2010)

tabela_receita_senadores_2010 <- receitas_senadores_2010 %>%
  group_by(partido) %>%
  summarize(doacoes_jbs_corrigidas_cand_eleitos = sum(doacoes_jbs_corrigidas),
            total_recebido_corrigido_pelos_cand_eleitos = sum(total_recebido_corrigido))


write.table(tabela_receita_senadores_2010, file="tabela_receita_senadores_2010.csv", sep=";",
            row.names = F, dec=",")

#Gr?fico 10 - financiamento por partido para deputado federal

graf10_bd <- receita_por_candidato %>%
  filter(agrupador == "JBS",
         Cargo == "DEPUTADO FEDERAL") %>%
  group_by(Sigla..Partido) %>%
  summarise(doacoes_jbs_partido = sum(receita_jbs)) %>%
  mutate(doacoes_jbs_partido_milhao = round(doacoes_jbs_partido/1000000,1))

graf10_bd$Sigla..Partido <- reorder(graf10_bd$Sigla..Partido, graf10_bd$doacoes_jbs_partido_milhao)


graf10  <- ggplot(graf10_bd , 
                  aes(x=Sigla..Partido, y=doacoes_jbs_partido_milhao,
                      label = doacoes_jbs_partido_milhao)) + 
  geom_segment(aes(x=Sigla..Partido, 
                   xend=Sigla..Partido, 
                   y=0, 
                   yend=doacoes_jbs_partido_milhao )) + 
  geom_point(stat='identity', fill="#f7cb2e", 
             colour="#f7cb2e",pch=21, size=13) +
  coord_flip() + geom_text(color="black", size=5) +
  labs(title="", 
       subtitle="", 
       caption="",
       y = "Doa??es em milh?es", 
       x = "", size = 3) + 
  theme(axis.text.x = element_text(vjust=0.6),
        axis.text.y = element_text(vjust=1.2)) 

graf10

ggsave(graf10, file="graf10.png", width = 12, height = 8)
setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
write.table(doacoes_partido, file="doacoes_partido.csv")


#Gr?fico 11 - deputados federais financiados e elitos 

graf11_bd <- receita_por_candidato %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP", "ELEITO", DESC_SIT_TOT_TURNO),
         DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", "ELEITO", DESC_SIT_TOT_TURNO)) %>%
  filter(Cargo == "DEPUTADO FEDERAL", 
         DESC_SIT_TOT_TURNO == "ELEITO") %>%
  mutate(eleito =1,
         financiado = ifelse(agrupador == "JBS", 1,0),
         financiado = ifelse(is.na(financiado),0, financiado)) %>%
  group_by(Sigla..Partido) %>%
  summarise(dep_fed_eleitos = sum(eleito),
            dep_fed_financiado = sum(financiado)) %>%
  mutate(nao_financiados = dep_fed_eleitos - dep_fed_financiado) %>%
  gather(situacao , deputados_federais , dep_fed_eleitos:nao_financiados) %>%
  filter(situacao != "dep_fed_eleitos") 

graf11_bd$Sigla..Partido <- reorder(graf11_bd$Sigla..Partido, -rep(graf11_bd$deputados_federais))
graf11_bd$situacao <- reorder(graf11_bd$situacao, ifelse(graf11_bd$situacao == "dep_fed_financiado",2,1))

graf11 <- graf11_bd %>%
  ggplot(aes(x=Sigla..Partido, y=deputados_federais, fill = situacao)) + 
  geom_bar(stat="identity", position = "stack") + 
  ggtitle("") + 
  scale_fill_manual(values=cores, labels=c("Sem financiamento", "Financiados")) + 
  labs(x="", y="Dep. federais", fill="") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  scale_y_continuous(breaks=pretty_breaks(n=3), limits=c(0,70)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


graf11

setwd("C:/Users/jvoig/Documents/Financiamento JBS/Arquivos/correcao")
ggsave(graf11, file="graf11.png", width = 12, height = 8, scale = .75)

# Tabela

tabela5_depfed_uf <- receita_por_candidato %>%
  filter(Cargo == "DEPUTADO FEDERAL") %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP", "ELEITO",
                                     ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", "ELEITO", DESC_SIT_TOT_TURNO))) %>%
  mutate(eleito= ifelse(DESC_SIT_TOT_TURNO == "ELEITO",
                        ifelse(agrupador == "JBS", 1,0),0),
         eleito = ifelse(is.na(eleito),0,eleito)) %>%
  group_by(UF) %>%
  summarise(doacoes_depfed_jbs_uf_total = sum(receita_jbs),
            doacoes_depfed_totais_uf = sum(Receita_total_candidato),
            candidatos_eleitos = sum(eleito)) %>%
  mutate(participacao = round(doacoes_depfed_jbs_uf_total/doacoes_depfed_totais_uf,2)) %>%
  mutate(doacoes_depfed_jbs_uf_total = round(doacoes_depfed_jbs_uf_total/1000000,2),
         doacoes_depfed_totais_uf = round(doacoes_depfed_totais_uf/1000000,2)) 

View(tabela5_depfed_uf)
write.table(tabela5_depfed_uf, file="tabela5_depfed_uf.csv", sep=";", row.names = FALSE)


#Gr?fico que no meu est? como 10 e como a ju est? como 8
#Deputado federal
#Composi??o da bancada por UF

bancada_dep_fed <- receita_por_candidato %>%
  filter(Cargo == "DEPUTADO FEDERAL") %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP", "ELEITO", DESC_SIT_TOT_TURNO),
         DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", 
                                     "ELEITO", DESC_SIT_TOT_TURNO)) %>%
  filter(DESC_SIT_TOT_TURNO == "ELEITO") %>%
  mutate(financiado = ifelse(agrupador == "JBS", 1,0),
         financiado = ifelse(is.na(financiado),0,financiado),
         nao_financiado = ifelse(financiado == 1, 0, 1)) %>%
  group_by(UF) %>%
  summarise(candidatos_fiananciados = sum(financiado),
            nao_financiado = sum(nao_financiado)) %>%
  mutate(totalbancada = candidatos_fiananciados+nao_financiado) %>%
  gather(tipo, deputados, candidatos_fiananciados:totalbancada) %>%
  mutate(total_bancada = ifelse(tipo == "totalbancada", deputados, 0),
         deputados = ifelse(tipo=="totalbancada",0,deputados)) %>%
  group_by(UF) %>%
  summarise(total_bancada = sum(total_bancada)) %>%
  ungroup()

graf12_bdv2 <- receita_por_candidato %>%
  filter(Cargo == "DEPUTADO FEDERAL") %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP", "ELEITO", DESC_SIT_TOT_TURNO),
         DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", 
                                     "ELEITO", DESC_SIT_TOT_TURNO)) %>%
  filter(DESC_SIT_TOT_TURNO == "ELEITO") %>%
  mutate(financiado = ifelse(agrupador == "JBS", 1,0),
         financiado = ifelse(is.na(financiado),0,financiado),
         nao_financiado = ifelse(financiado == 1, 0, 1)) %>%
  group_by(UF) %>%
  summarise(candidatos_fiananciados = sum(financiado),
            nao_financiado = sum(nao_financiado)) %>%
  gather(tipo, deputados, candidatos_fiananciados:nao_financiado) %>%
  left_join(bancada_dep_fed) %>%
  mutate(participacao = round(deputados/total_bancada,2)*100,
         participacao2 = ifelse(tipo == "candidatos_fiananciados", participacao, 0)) 

graf12_bdv2$UF <- reorder(graf12_bdv2$UF, desc(graf12_bdv2$participacao2))
graf12_bdv2$tipo <- reorder(graf12_bdv2$tipo, ifelse(graf12_bdv2$tipo == "candidatos_fiananciados",2,1))


graf12v2 <- ggplot(graf12_bdv2, aes(x=UF, y=deputados, fill = tipo)) + 
  geom_bar(stat="identity", position = "stack") + 
  geom_text(data=graf12_bd, aes(x =UF , y = deputados , label = paste0(participacao,"%")),
            size=3, vjust=0.5, check_overlap = TRUE, position = position_stack(vjust = 0.5)) + theme(legend.position="bottom") +
  ggtitle("") + 
  scale_fill_manual(values=cores, labels=c("Sem financiamento", "Financiados")) + 
  labs(x="", y="Dep. federais", fill="") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  scale_y_continuous(breaks=pretty_breaks(n=3), limits=c(0,70)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(graf12v2, file="graf12v2.png", width = 12, height = 8, scale = .75)

#GOVERNADORES
tabela6 <- receita_por_candidato %>%
  filter(Cargo == "GOVERNADOR") %>%
  mutate(financiado = ifelse(agrupador == "JBS", 1, 0),
         financiado = ifelse(is.na(agrupador),0,financiado),
         eleito = ifelse(agrupador == "JBS" & DESC_SIT_TOT_TURNO == "ELEITO", 1,0),
         eleito = ifelse(is.na(eleito),0,eleito)) %>%
  group_by(Sigla..Partido) %>%
  summarise(candidatos_financiados = sum(financiado),
            candidatos_eleitos = sum(eleito),
            doacoes_jbs_partido = sum(receita_jbs),
            receitas_totais_partido_governador = sum(Receita_total_candidato)) %>%
  mutate(participacao_campanha_partido_gov = round(doacoes_jbs_partido/receitas_totais_partido_governador,2),
         doacoes_jbs_partido = round(doacoes_jbs_partido/1000000,2),
         receitas_totais_partido_governador = round(receitas_totais_partido_governador/1000000,2)) %>%
  arrange(desc(doacoes_jbs_partido)) %>%
  filter(candidatos_financiados != 0)

write.table(tabela6, file="tabela6.csv", sep=";", row.names = FALSE, dec=",")

########
#GR?FICO 13 - FINANCIAMENTO DOS PARTIDO PARA GOVERNADORES

tabela6$Sigla..Partido <- reorder(tabela6$Sigla..Partido, -desc(tabela6$doacoes_jbs_partido))

graf13  <- ggplot(tabela6, 
                  aes(x=Sigla..Partido, y=doacoes_jbs_partido,
                      label = doacoes_jbs_partido)) + 
  geom_segment(aes(x=Sigla..Partido, 
                   xend=Sigla..Partido, 
                   y=0, 
                   yend=doacoes_jbs_partido )) + 
  geom_point(stat='identity', fill="#f7cb2e", 
             colour="#f7cb2e",pch=21, size=13) +
  coord_flip() + geom_text(color="black", size=5) +
  labs(title="", 
       subtitle="", 
       caption="",
       y = "Doa??es em milh?es", 
       x = "", size = 3) + 
  theme(axis.text.x = element_text(vjust=0.6),
        axis.text.y = element_text(vjust=1.2)) 

ggsave(graf13, file="graf13.png", width = 12, height = 8, scale = .75)


#Tabela 7
tabela7 <- receita_por_candidato %>%
  filter(Cargo == "GOVERNADOR") %>%
  mutate(financiado = ifelse(agrupador == "JBS", 1, 0),
         financiado = ifelse(is.na(agrupador),0,financiado),
         eleito = ifelse(agrupador == "JBS" & DESC_SIT_TOT_TURNO == "ELEITO", 1,0),
         eleito = ifelse(is.na(eleito),0,eleito)) %>%
  group_by(UF) %>%
  summarise(candidatos_financiados = sum(financiado),
            candidatos_eleitos = sum(eleito),
            doacoes_jbs_uf = sum(receita_jbs),
            receitas_totais_uf_governador = sum(Receita_total_candidato)) %>%
  mutate(participacao_campanha_uf_gov = round(doacoes_jbs_uf/receitas_totais_uf_governador,2),
         doacoes_jbs_uf = round(doacoes_jbs_uf/1000000,2),
         receitas_totais_uf_governador = round(receitas_totais_uf_governador/1000000,2)) %>%
  arrange(desc(doacoes_jbs_uf)) %>%
  filter(candidatos_financiados != 0)

write.table(tabela7, file="tabela7.csv", sep=";", row.names = FALSE, dec=",")



#Deputado Estadual
#Tabela 8
tabela8 <- receita_por_candidato %>%
  filter(Cargo == "DEPUTADO ESTADUAL") %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP", "ELEITO", DESC_SIT_TOT_TURNO),
         DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", "ELEITO", DESC_SIT_TOT_TURNO),
         financiado = ifelse(agrupador == "JBS", 1, 0),
         financiado = ifelse(is.na(agrupador),0,financiado),
         eleito = ifelse(agrupador == "JBS" & DESC_SIT_TOT_TURNO == "ELEITO", 1,0),
         eleito = ifelse(is.na(eleito),0,eleito)) %>%
  group_by(Sigla..Partido) %>%
  summarise(candidatos_financiados = sum(financiado),
            candidatos_eleitos = sum(eleito),
            doacoes_jbs_partido = sum(receita_jbs),
            receitas_totais_partido_depest = sum(Receita_total_candidato)) %>%
  mutate(participacao_campanha_partido_depest = round(doacoes_jbs_partido/receitas_totais_partido_depest,2),
         doacoes_jbs_partido = round(doacoes_jbs_partido/1000000,2),
         receitas_totais_partido_depest = round(receitas_totais_partido_depest/1000000,2)) %>%
  arrange(desc(doacoes_jbs_partido)) %>%
  filter(candidatos_financiados != 0)

write.table(tabela8, file="tabela8.csv", sep=";", row.names = FALSE, dec=",")


#Tabela 9 
# financiamento por uf

tabela9 <- receita_por_candidato %>%
  filter(Cargo == "DEPUTADO ESTADUAL") %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP", "ELEITO", DESC_SIT_TOT_TURNO),
         DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", "ELEITO", DESC_SIT_TOT_TURNO),
         financiado = ifelse(agrupador == "JBS", 1, 0),
         financiado = ifelse(is.na(agrupador),0,financiado),
         eleito = ifelse(agrupador == "JBS" & DESC_SIT_TOT_TURNO == "ELEITO", 1,0),
         eleito = ifelse(is.na(eleito),0,eleito)) %>%
  group_by(UF) %>%
  summarise(candidatos_financiados = sum(financiado),
            candidatos_eleitos = sum(eleito),
            doacoes_jbs_uf = sum(receita_jbs),
            receitas_totais_uf_depest = sum(Receita_total_candidato)) %>%
  mutate(participacao_campanha_uf_depest = round(doacoes_jbs_uf/receitas_totais_uf_depest,2),
         doacoes_jbs_uf = round(doacoes_jbs_uf/1000000,2),
         receitas_totais_uf_depest = round(receitas_totais_uf_depest/1000000,2)) %>%
  arrange(desc(doacoes_jbs_uf)) %>%
  filter(candidatos_financiados != 0)

write.table(tabela9, file="tabela9.csv", sep=";", row.names = FALSE, dec=",")


###
# Gr?fico 13
# Deputados estaduais financiados pela JBS por uf

bancada_deputado_estadual <- receita_por_candidato %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP", "ELEITO", DESC_SIT_TOT_TURNO),
         DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", "ELEITO", DESC_SIT_TOT_TURNO),
         num=1) %>%
  filter(Cargo == "DEPUTADO ESTADUAL",
         DESC_SIT_TOT_TURNO == "ELEITO") %>%
  group_by(UF) %>%
  summarise(total_dep_est = sum(num))


graf14_bd <- receita_por_candidato %>%
  mutate(DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR QP", "ELEITO", DESC_SIT_TOT_TURNO),
         DESC_SIT_TOT_TURNO = ifelse(DESC_SIT_TOT_TURNO == "ELEITO POR M?DIA", "ELEITO", DESC_SIT_TOT_TURNO),
         financiado = ifelse(agrupador == "JBS", 1, 0),
         financiado = ifelse(is.na(agrupador),0,financiado),
         num=1) %>%
  filter(Cargo == "DEPUTADO ESTADUAL",
         DESC_SIT_TOT_TURNO == "ELEITO") %>%
  group_by(UF) %>%
  summarise(dep_est_financiados = sum(financiado),
            total_dep_est = sum(num)) %>%
  mutate(nao_financiados = total_dep_est - dep_est_financiados) %>%
  select(1,2,4) %>%
  gather(tipo, deputados, dep_est_financiados:nao_financiados) %>%
  left_join(bancada_deputado_estadual)

graf14_bd$UF <- reorder(graf14_bd$UF, desc(graf14_bd$total_dep_est))
graf14_bd$tipo <- reorder(graf14_bd$tipo, ifelse(graf14_bd$tipo == "dep_est_financiados", 2,1))

graf14 <- ggplot(graf14_bd, aes(x=UF, y=deputados, fill = tipo)) + 
  geom_bar(stat="identity", position = "stack") + theme(legend.position="bottom") +
  ggtitle("") + 
  scale_fill_manual(values=cores, labels=c("Sem financiamento", "Financiados")) + 
  labs(x="", y="Dep. estaduais", fill="") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  scale_y_continuous(breaks=pretty_breaks(n=3), limits=c(0,100)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(graf14, file="graf14.png", width = 12, height = 10, scale = .75)


#Deputados Estaduais financiados por partido
#gR?FICO 15

tabela8$Sigla..Partido <- reorder(tabela8$Sigla..Partido, -desc(tabela8$doacoes_jbs_partido))

graf15  <- ggplot(tabela8, 
                  aes(x=Sigla..Partido, y=doacoes_jbs_partido,
                      label = doacoes_jbs_partido)) + 
  geom_segment(aes(x=Sigla..Partido, 
                   xend=Sigla..Partido, 
                   y=0, 
                   yend=doacoes_jbs_partido )) + 
  geom_point(stat='identity', fill="#f7cb2e", 
             colour="#f7cb2e",pch=21, size=13) +
  coord_flip() + geom_text(color="black", size=5) +
  labs(title="", 
       subtitle="", 
       caption="",
       y = "Doa??es em milh?es", 
       x = "", size = 3) + 
  theme(axis.text.x = element_text(vjust=0.6),
        axis.text.y = element_text(vjust=1.2)) 

ggsave(graf15, file="graf15.png", width = 12, height = 12, scale = .75)
