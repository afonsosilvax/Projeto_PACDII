library(dplyr)
library(readxl)
library(tidyverse)

rm(list=ls()) # limpar variáveis

acidentes_2010 = read_excel("acidentes-2010.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")
acidentes_2011 = read_excel("acidentes-2011.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")
acidentes_2012 = read_excel("acidentes-2012.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")
acidentes_2013 = read_excel("acidentes-2013.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")
acidentes_2014 = read_excel("acidentes-2014.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")
acidentes_2015 = read_excel("acidentes-2015.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")
acidentes_2016 = read_excel("acidentes-2016.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")
acidentes_2017 = read_excel("acidentes-2017.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")
acidentes_2018 = read_excel("acidentes-2018.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")
acidentes_2019 = read_excel("acidentes-2019.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO")

acidentes <- rbind(acidentes_2010, acidentes_2011, acidentes_2012, acidentes_2013, 
                   acidentes_2014, acidentes_2015, acidentes_2016, acidentes_2017, 
                   acidentes_2018, acidentes_2019)

attach(acidentes)

Datahora <- as.Date(Datahora, "%d%b%Y:%H:%M:%S")
class(Datahora)

acidentes <- acidentes %>% mutate_if(is.character, as.factor)

summary(acidentes)
summary(is.na(acidentes))
