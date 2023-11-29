#install.packages("janitor")

library(dplyr)
library(readxl)
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(ggplot2)

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

# Tratamento da coluna datahora
acidentes$datahora <- as_datetime(acidentes$datahora)
class(acidentes$datahora)
summary(acidentes$datahora)

# Passar para factor as variáveis que são character
acidentes <- acidentes %>% mutate_if(is.character, as.factor)

# Resumo do dataset
summary(acidentes)
glimpse(acidentes)

# Estudo dos NA's
summary(is.na(acidentes))

# Variáveis com alto teor em NA's
summary(acidentes$pov_proxima)
summary(acidentes$nome_arruamento)
summary(acidentes$km)
summary(acidentes$sentidos)
summary(acidentes$obras_arte) # túneis, pontes e estradas - substituir NA's por estradas
summary(acidentes$sinais)

# Latitude Longitude são relevantes e nao devemos apagar NA? Sentidos podemos apagar NA's'. Características Tecnicas1: ND - caminho/estradas não oficiais


# Tratamento de outliers

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
acidentes_carnaval <- nr_acidentes %>% filter(mes_dia == "02-16" | mes_dia == "02-17" | mes_dia == "02-18" | mes_dia == "02-19" | mes_dia == "02-20" | mes_dia == "02-21" | mes_dia == "02-22" | mes_dia == "02-23")

ggplot(acidentes_carnaval, aes(x = mes_dia, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes no periodo de carnaval", x = "Dia", y = "Número de Acidentes")

# Acidentes em fevereiro
acidentes_fev <- nr_acidentes %>% filter(grepl("02-", mes_dia))

ggplot(acidentes_fev, aes(x = mes_dia, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes em fevereiro", x = "Dia", y = "Número de Acidentes")
