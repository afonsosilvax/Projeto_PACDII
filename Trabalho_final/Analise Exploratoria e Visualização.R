library(dplyr)
library(readxl)
library(tidyverse)
#install.packages("janitor")
library(janitor) # for datacleaning
library(here)
library(lubridate)
library(ggplot2)
library(xts)

rm(list=ls()) # limpar variáveis

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

acidentes <- rbind(acidentes_2010, acidentes_2011, acidentes_2012, acidentes_2013, 
                   acidentes_2014, acidentes_2015, acidentes_2016, acidentes_2017, 
                   acidentes_2018, acidentes_2019)

# Tratamento das datas
acidentes$datahora <- as_datetime(acidentes$datahora)
class(acidentes$datahora)
summary(acidentes$datahora)

# Passar para factor
acidentes <- acidentes %>% mutate_if(is.character, as.factor)
summary(acidentes)
glimpse(acidentes)
summary(is.na(acidentes))

# Variáveis com alto teor em NA's
summary(acidentes$pov_proxima)
summary(acidentes$nome_arruamento)
summary(acidentes$km)
summary(acidentes$sentidos)
summary(acidentes$obras_arte) # Túneis, pontes - substituir NA's por estradas
summary(acidentes$sinais)

#Latitude Longitude são relevantes  e nao devemos apagar NA?
#Sentidos podemos apagar NA's'

# Características Tecnicas1: ND - caminho/estradas não oficiais
            



#Acidentes duplicados
acidentes <- acidentes %>% remove_empty(c("rows", "cols"))
id_duplicado <- acidentes %>% get_dupes(contains("id_acidente"))

ncol(id_duplicado)
nrow(id_duplicado)

#Tratamento de duplicados

#problema 1
problema1<-acidentes[,c(1,2,9)]

problema1$dia_mes <- format(problema1$datahora, "%m-%d")
filtered_df <- problema1[problema1$dia_mes == "12-25", ]
filtered_df1 <- problema1[problema1$dia_mes == "07-25", ]

ggplot(problema1, aes(x = as.Date(datahora) , group = 1)) +
  geom_line(stat = "count", color = "skyblue") +
  labs(title = "Ocorrência de Acidentes ao Longo do Ano", x = "Data", y = "Número de Acidentes")

df<-problema1[,c(1,4)]

nr_acidentes <- problema1 %>% group_by(problema1$dia_mes) %>% summarise(numero_acidentes=n())
nr_acidentes$numero_acidentes<-nr_acidentes$numero_acidentes / 10

ggplot(nr_acidentes, aes(x =`problema1$dia_mes`, y = numero_acidentes, group = 1)) +
  geom_line(color = "skyblue") +
  geom_point(color = "red") +
  labs(title = "Evolução do Número de Acidentes ao Longo dos Dias", x = "Data", y = "Número de Acidentes")

to.monthly(problema1$datahora)
to.monthly(as.xts(problema1$datahora))

as.xts(problema1$datahora)
