#install.packages("janitor")
#install.packages("caret")

library(dplyr)
library(readxl)
library(janitor)
library(here)
library(lubridate)
library(ggplot2)
library(lsr)
library(tidyverse)
library(caret)
library(e1071)

# Ler ficheiros
acidentes_2010 <- read_excel("acidentes-2010.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2011 <- read_excel("acidentes-2011.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2012 <- read_excel("acidentes-2012.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2013 <- read_excel("acidentes-2013.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2014 <- read_excel("acidentes-2014.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2015 <- read_excel("acidentes-2015.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2016 <- read_excel("acidentes-2016.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2017 <- read_excel("acidentes-2017.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2018 <- read_excel("acidentes-2018.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2019 <- read_excel("acidentes-2019.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)

# Juntar todos os anos
acidentes <- rbind(acidentes_2010, acidentes_2011, acidentes_2012, acidentes_2013, 
                   acidentes_2014, acidentes_2015, acidentes_2016, acidentes_2017, 
                   acidentes_2018, acidentes_2019)

acidentes_samp <- read_excel("acidentes-sample.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                        , .name_repair = make_clean_names)

# Tratamento da coluna datahora
acidentes$datahora <- as_datetime(acidentes$datahora)

class(acidentes$datahora)
summary(acidentes$datahora)

# Resumo do dataset
summary(acidentes)
glimpse(acidentes)

# Estudo dos NA's
summary(is.na(acidentes))
summary(acidentes$sentidos)

sum(is.na(acidentes))/(nrow(acidentes)*ncol(acidentes))
                       
# Variáveis com alto teor em NA's
summary(acidentes$pov_proxima)
summary(acidentes$nome_arruamento)
summary(acidentes$km)
summary(acidentes$sentidos)
summary(acidentes$obras_arte) # túneis, pontes e estradas - substituir NA's por estradas
summary(acidentes$sinais)

#eliminação de na's de acordo com a nossa interpretação do manual de preenchimento
acidentes <- acidentes[-which(is.na(acidentes$velocidade_local)),]
acidentes <- acidentes[-which(is.na(acidentes$velocidade_geral)),]
acidentes <- acidentes[-which(is.na(acidentes$caracteristicas_tecnicas1)),]
acidentes <- acidentes[-which(is.na(acidentes$cond_aderencia)),]
acidentes$pov_proxima[is.na(acidentes$pov_proxima)] <- "Fora das localidades"
acidentes$nome_arruamento[is.na(acidentes$nome_arruamento)] <- "Via fora de localidade"
acidentes <- acidentes[-which(is.na(acidentes$estado_conservacao)),]
acidentes$km[is.na(acidentes$km)] <- "Não aplicável"
acidentes$factores_atmosfericos[is.na(acidentes$factores_atmosfericos)] <- "Outro"
acidentes <- acidentes[-which(is.na(acidentes$reg_circulacao1)),]
acidentes <- acidentes[-which(is.na(acidentes$interseccao_vias)),]
acidentes <- acidentes[-which(is.na(acidentes$luminosidade)),]
acidentes <- acidentes[-which(is.na(acidentes$marca_via)),]
acidentes$obras_arte[is.na(acidentes$obras_arte)] <- "Estrada/Outro"
acidentes <- acidentes[-which(is.na(acidentes$obstaculos)),]
acidentes$sinais[is.na(acidentes$sinais)] <- "Sem sinalização"
acidentes$sinais_luminosos[is.na(acidentes$sinais_luminosos)] <- "Outros"
acidentes$sentidos[is.na(acidentes$sentidos)] <- "Estrada sem separador central"
acidentes <- acidentes[-which(is.na(acidentes$tipo_piso)),]
acidentes <- acidentes[-which(is.na(acidentes$tracado_1)),]
acidentes <- acidentes[-which(is.na(acidentes$tracado_2)),]
acidentes <- acidentes[-which(is.na(acidentes$tracado_3)),]
acidentes <- acidentes[-which(is.na(acidentes$tracado_4)),]
acidentes <- acidentes[-which(is.na(acidentes$via_transito)),]
acidentes <- acidentes %>% select(-latitude_gps, -longitude_gps)

# Passar para factor as variáveis que são character
acidentes <- acidentes %>% mutate_if(is.character, as.factor)
summary(is.na(acidentes))
glimpse(acidentes)

# Visualização de outliers
boxplot(acidentes$num_feridos_ligeiros_a_30_dias, col="#14BFB8", horizontal = TRUE, main="Feridos ligeiros")
boxplot.stats(acidentes$num_feridos_ligeiros_a_30_dias)$out

boxplot(acidentes$num_feridos_graves_a_30_dias, col="#14BFB8", horizontal = TRUE, main="Feridos graves")
boxplot.stats(acidentes$num_feridos_graves_a_30_dias)$out

boxplot(acidentes$num_mortos_a_30_dias, col="#14BFB8", horizontal = TRUE, main="Mortos")
boxplot.stats(acidentes$num_mortos_a_30_dias)$out

# Visualização dos dados
barplot(prop.table(table(acidentes$entidades_fiscalizadoras)), col="#14BFB8", main="Entidades fiscalizadoras")
barplot(prop.table(table(acidentes$dia_da_semana)), col="#14BFB8", main="Dia da semana")
barplot(prop.table(table(acidentes$caracteristicas_tecnicas1)), col="#14BFB8", main="Características técnicas")

hist(table(acidentes$hora), col="#14BFB8", main="Hora")

# Dataset com os acidentes e as respetivas datas
data_acidentes <- acidentes %>% select(id_acidente, datahora, dia_da_semana)
data_acidentes$ano_mes_dia <- format(data_acidentes$datahora, "%Y-%m-%d")
data_acidentes$ano <- format(data_acidentes$datahora, "%Y")
data_acidentes$mes <- format(data_acidentes$datahora, "%m")
data_acidentes$dia <- format(data_acidentes$datahora, "%d")

# Gráfico de acidentes desde 2010 até 2019
ggplot(data_acidentes, aes(x = as.Date(ano_mes_dia))) +
  geom_line(stat = "count", aes(group=1), color = "blue") +
  labs(title = "Ocorrência de Acidentes 2010-2019", x = "Data", y = "Número de Acidentes") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", 
               limits = c(as.Date("2010-01-01"), as.Date("2019-12-31")))

# Número médio de acidentes ao longo de um ano, que reflete em cada dia a média de acidentes de 2010 até 2019 naquele mesmo dia
nr_acidentes <- data_acidentes %>% select(mes_dia = datahora, mes, dia)
nr_acidentes$mes_dia <- format(nr_acidentes$mes_dia, "%m-%d")
nr_acidentes <- nr_acidentes %>% group_by(mes_dia) %>% summarise(numero_acidentes=n())
nr_acidentes$numero_acidentes <- nr_acidentes$numero_acidentes / 10

ggplot(nr_acidentes, aes(x = mes_dia, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes durante um ano", x = "Data", y = "Número de Acidentes")

nr_acidentes_mes <- data_acidentes %>% group_by(mes) %>% summarise(numero_acidentes=n()/10)
nr_acidentes_mes <- nr_acidentes_mes %>% rows_update(nr_acidentes_mes %>% filter(mes %in% c("01", "03", "05", "07", "08", "10", "12")) %>% 
                                                       mutate(numero_acidentes = numero_acidentes/31))
nr_acidentes_mes <- nr_acidentes_mes %>% rows_update(nr_acidentes_mes %>% filter(mes %in% c("04", "06", "09", "11")) %>% 
                                                       mutate(numero_acidentes = numero_acidentes/30))
nr_acidentes_mes <- nr_acidentes_mes %>% rows_update(nr_acidentes_mes %>% filter(mes == "02") %>% 
                                                       mutate(numero_acidentes = numero_acidentes/28.2))


ggplot(nr_acidentes_mes, aes(x = mes, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes médio ao longo dos meses", x = "Mês", y = "Número de Acidentes")

mes_geral <- data_acidentes %>% group_by(dia) %>% summarise(numero_acidentes=n()/120)
mes_geral[31, 2] = mes_geral[31, 2] * 12 / 7
mes_geral[30, 2] = mes_geral[30, 2] * 12 / 11
mes_geral[29, 2] = mes_geral[29, 2] * 12 / 11.2

mean(mes_geral$numero_acidentes)

ggplot(mes_geral, aes(x = dia, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes ao longo de um mês", x = "Dia", y = "Número de Acidentes")

d = 0
s = 0

nr_sem <- c(1:52)
nr_acidentes_sem <- c()

for (i in 1:nrow(nr_acidentes)) {
  if (d == 7) {
    nr_acidentes_sem <- c(nr_acidentes_sem, s/7)
    d = 0
    s = 0
  }
  
  d = d + 1
  s = s + nr_acidentes[i, 2]
}

nr_acidentes_sem <- unlist(nr_acidentes_sem)

acidentes_sem <- data.frame(semana = nr_sem)
acidentes_sem$numero_acidentes <- nr_acidentes_sem

ggplot(acidentes_sem, aes(x = semana, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes por semanas", x = "Semana", y = "Número de Acidentes")

# Acidentes 5 dias antes do Natal e 2 dias depois
acidentes_natal <- nr_acidentes %>% filter(mes_dia == "12-10" | mes_dia == "12-11" | mes_dia == "12-12" | mes_dia == "12-13" | mes_dia == "12-14" | mes_dia == "12-15" | mes_dia == "12-16" | mes_dia == "12-17" | mes_dia == "12-18" | mes_dia == "12-19" | mes_dia == "12-20" | mes_dia == "12-21" | mes_dia == "12-22" | mes_dia == "12-23" | mes_dia == "12-24" | mes_dia == "12-25" | mes_dia == "12-26" | mes_dia == "12-27")

ggplot(acidentes_natal, aes(x = mes_dia, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes em periodo natalício", x = "Dia", y = "Número de Acidentes")

# Acidentes em dezembro
acidentes_dez <- nr_acidentes %>% filter(grepl("12-", mes_dia))

ggplot(acidentes_dez, aes(x = mes_dia, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes em dezembro", x = "Dia", y = "Número de Acidentes")

# Acidentes no carnaval
dia_carnaval <- c(as_date("2010-02-16"), as_date("2011-03-08"), as_date("2012-02-21"), 
                  as_date("2013-02-12"), as_date("2014-03-04"), as_date("2015-02-17"), 
                  as_date("2016-02-09"), as_date("2017-02-28"), as_date("2018-02-13"),
                  as_date("2019-03-05"))

antes_c <- c(as_date(dia_carnaval[1] - 5))
depois_c <- c(as_date(dia_carnaval[1] + 3))

for (i in 2:10) {
  antes_c <- c(antes_c, as_date(dia_carnaval[i] - 5))
  depois_c <- c(depois_c, as_date(dia_carnaval[i] + 3))
}

acidentes_carnaval <- data_acidentes %>% filter(between(as_date(ano_mes_dia), antes_c[1], depois_c[1]) | 
                                                  between(as_date(ano_mes_dia), antes_c[2], depois_c[2]) | 
                                                  between(as_date(ano_mes_dia), antes_c[3], depois_c[3]) | 
                                                  between(as_date(ano_mes_dia), antes_c[4], depois_c[4]) | 
                                                  between(as_date(ano_mes_dia), antes_c[5], depois_c[5]) | 
                                                  between(as_date(ano_mes_dia), antes_c[6], depois_c[6]) | 
                                                  between(as_date(ano_mes_dia), antes_c[7], depois_c[7]) | 
                                                  between(as_date(ano_mes_dia), antes_c[8], depois_c[8]) | 
                                                  between(as_date(ano_mes_dia), antes_c[9], depois_c[9]) | 
                                                  between(as_date(ano_mes_dia), antes_c[10], depois_c[10])) %>% 
  group_by(ano_mes_dia) %>% summarise(numero_acidentes=n())

ggplot(acidentes_carnaval, aes(x = ano_mes_dia, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes em vários periodos de carnaval 2010-2019", x = "Dia", y = "Número de Acidentes")

acidentes_carnaval <- acidentes_carnaval %>%
  mutate(posicao = rep(1:9, length.out = n())) %>%
  group_by(posicao) %>% summarise(numero_acidentes = sum(numero_acidentes)/10)
  

ggplot(acidentes_carnaval, aes(x = posicao, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes no periodo de carnaval", x = "Dia", y = "Número de Acidentes")

# Acidentes em fevereiro
acidentes_fev <- nr_acidentes %>% filter(grepl("02-", mes_dia))

ggplot(acidentes_fev, aes(x = mes_dia, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes em fevereiro", x = "Dia", y = "Número de Acidentes")

# resolução problema 2
hist(acidentes$num_feridos_ligeiros_a_30_dias)
hist(acidentes$num_feridos_graves_a_30_dias)
hist(acidentes$num_mortos_a_30_dias)

# acidentes where num_mortos_a_30_dias > 0 or num_feridos_graves_a_30_dias > 2

prob2 <- acidentes %>% 
  mutate(grave = ifelse(num_mortos_a_30_dias > 0 | num_feridos_graves_a_30_dias > 0, 1, 0)) %>% 
  select(-num_mortos_a_30_dias, -num_feridos_graves_a_30_dias, -num_feridos_ligeiros_a_30_dias)

prob2 <- prob2 %>%
  mutate(natureza = factor(ifelse(natureza=="Atropelamento com fuga" | natureza=="Atropelamento de animais" | natureza=="Atropelamento de peões", "Atropelamento", 
                                  ifelse(natureza=="Colisão choque em cadeia" | natureza=="Colisão com fuga" | natureza=="Colisão com outras situações" | natureza=="Colisão com veiculo ou obstáculo na faixa de rodagem" | natureza=="Colisão frontal" | natureza=="Colisão lateral com outro veículo em movimento" | natureza=="Colisão traseira com outro veículo em movimento", "Colisão", "Despiste"))))

# Vamos fazer uma amostra dos dados para poder fazer algoritmos com o mesmo
set.seed(2023)
prob2s <- createDataPartition(prob2$grave, p = 0.01, list = FALSE)
amostra <- prob2[prob2s, ]

amostra$grave <- as.factor(amostra$grave)

prop.table(table(amostra$grave))
xtabs(~grave + natureza, data = amostra)

set.seed(2023)
conjunto_indices <- sample(1:nrow(amostra), size = 0.7 * nrow(amostra))
conjunto_treino <- amostra[conjunto_indices, ]
conjunto_teste <- amostra[-conjunto_indices, ]

prop.table(table(conjunto_treino$grave))
prop.table(table(conjunto_teste$grave))

#Analise de correlações
cramersV(conjunto_treino$grave, conjunto_treino$distrito)
cramersV(conjunto_treino$grave, conjunto_treino$entidades_fiscalizadoras)
cramersV(conjunto_treino$grave, conjunto_treino$velocidade_geral)
cramersV(conjunto_treino$grave, conjunto_treino$dia_da_semana)
cramersV(conjunto_treino$grave, conjunto_treino$caracteristicas_tecnicas1)
cramersV(conjunto_treino$grave, conjunto_treino$cond_aderencia)
cramersV(conjunto_treino$grave, conjunto_treino$concelho)
cramersV(conjunto_treino$grave, conjunto_treino$freguesia)
cramersV(conjunto_treino$grave, conjunto_treino$pov_proxima)
cramersV(conjunto_treino$grave, conjunto_treino$nome_arruamento)
cramersV(conjunto_treino$grave, conjunto_treino$tipos_vias)
cramersV(conjunto_treino$grave, conjunto_treino$cod_via)
cramersV(conjunto_treino$grave, conjunto_treino$estado_conservacao)
cramersV(conjunto_treino$grave, conjunto_treino$km)
cramersV(conjunto_treino$grave, conjunto_treino$factores_atmosfericos)
cramersV(conjunto_treino$grave, conjunto_treino$reg_circulacao1)
cramersV(conjunto_treino$grave, conjunto_treino$interseccao_vias)
cramersV(conjunto_treino$grave, conjunto_treino$localizacoes)
cramersV(conjunto_treino$grave, conjunto_treino$luminosidade)
cramersV(conjunto_treino$grave, conjunto_treino$marca_via)
cramersV(conjunto_treino$grave, conjunto_treino$natureza)
cramersV(conjunto_treino$grave, conjunto_treino$obras_arte)
cramersV(conjunto_treino$grave, conjunto_treino$obstaculos)
cramersV(conjunto_treino$grave, conjunto_treino$sentidos)
cramersV(conjunto_treino$grave, conjunto_treino$sinais)
cramersV(conjunto_treino$grave, conjunto_treino$tipo_piso)
cramersV(conjunto_treino$grave, conjunto_treino$tracado_1)
cramersV(conjunto_treino$grave, conjunto_treino$tracado_2)
cramersV(conjunto_treino$grave, conjunto_treino$tracado_3)
cramersV(conjunto_treino$grave, conjunto_treino$tracado_4)
cramersV(conjunto_treino$grave, conjunto_treino$via_transito)
cramersV(conjunto_treino$grave, conjunto_treino$velocidade_local)

# Variáveis relevantes: freguesia, pov_proxima, nome_arruamento, km
length(levels(amostra$freguesia))
length(levels(amostra$pov_proxima))
length(levels(amostra$nome_arruamento))
length(levels(amostra$km))

#analisar correlações entre variaveis de alta correlação, natureza, km, codvia, nome arruamento, pov_proxima, freguesia,, concelho, distrito, velocidade geral, velocidade local
#dentro das variaveis de alta correlação que dizem respeito a localização escolhemos a com maior correlação pois elas sao correlacionadas entre si o mesmo para as velociades
# algoritmo regressão logistica de forma naive

modelo_logistico <- glm(grave ~ distrito + natureza + cond_aderencia, data = conjunto_treino, family = "binomial")
summary(modelo_logistico)


# previsão dos dados
previsao <- predict(modelo_logistico, newdata = conjunto_teste, type = "response")
previsao <- ifelse(previsao > 0.5, 1, 0)

# number of coefficients in the model
length(coef(modelo_logistico))

#accuracy of the prediction
mean(previsao == conjunto_teste$grave)
#0.9172043
#0.9290323 - 3 niveis na natureza








# Modelo SVM
modelo_svm <- svm(grave ~ distrito + natureza + cond_aderencia + sinais + tracado_1, data = conjunto_treino, kernel = "linear")

#predict with test data
previsao <- predict(modelo_svm, newdata = conjunto_teste, type = "link")

# number of coefficients in the model
length(coef(modelo_svm))

#accuracy of the prediction
mean(previsao == conjunto_teste$grave)


#0.9311828 vs 0.9333333