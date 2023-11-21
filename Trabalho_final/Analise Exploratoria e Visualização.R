#install.packages("janitor")

library(dplyr)
library(readxl)
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(ggplot2)
library(xts)

# Limpar variáveis
rm(list=ls())

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

# Dataset com os acidentes e as respetivas datas
data_acidentes <- acidentes %>% select(id_acidente, datahora, dia_da_semana)
data_acidentes$ano_mes_dia <- format(data_acidentes$datahora, "%Y-%m-%d")
data_acidentes$mes_dia <- format(data_acidentes$datahora, "%m-%d")
data_acidentes$mes <- format(data_acidentes$datahora, "%m")
data_acidentes$dia <- format(data_acidentes$datahora, "%d")

# Dataset de acidentes no Natal (2010-2019)
natal <- data_acidentes %>% filter(data_acidentes$mes_dia == "12-25")

# Dataset de acidentes no dia 23 de Julho (2010-2019)
dia_normal <- data_acidentes %>% filter(data_acidentes$mes_dia == "07-23")

nrow(natal) > nrow(dia_normal)

# Isto significa que o dia de Natal ao longo destes anos não apresenta um número significativo de acidentes 

# Gráfico de acidentes desde 2010 até 2019
ggplot(data_acidentes, aes(x = as.Date(ano_mes_dia))) +
  geom_line(stat = "count", aes(group=1), color = "blue") +
  labs(title = "Ocorrência de Acidentes 2010-2019", x = "Data", y = "Número de Acidentes") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", 
               limits = c(as.Date("2010-01-01"), as.Date("2019-12-31")))

# Número médio de acidentes ao longo de um ano, que reflete em cada dia a média de acidentes de 2010 até 2019 naquele mesmo dia
nr_acidentes <- data_acidentes %>% group_by(mes_dia) %>% summarise(numero_acidentes=n())
nr_acidentes$numero_acidentes <- nr_acidentes$numero_acidentes / 10

ggplot(nr_acidentes, aes(x = mes_dia, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes durante um ano", x = "Data", y = "Número de Acidentes")

nr_acidentes_mes <- data_acidentes %>% group_by(mes) %>% summarise(numero_acidentes=n()/10)

ggplot(nr_acidentes_mes, aes(x = mes, y = numero_acidentes, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes ao longo dos meses", x = "Mês", y = "Número de Acidentes")

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
