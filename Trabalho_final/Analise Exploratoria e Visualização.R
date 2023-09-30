library(dplyr)
library(readxl)
library(tidyverse)
#install.packages("janitor")
library(janitor) # for datacleaning
library(here)
library(lubridate)

rm(list=ls()) # limpar variáveis

acidentes_2010 <- read_excel("acidentes-2010.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2011 <- read_excel("acidentes-2011.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2012 <- read_excel("acidentes-2012.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2013 <- read_excel("acidentes-2013.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2014 <- read_excel("acidentes-2014.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2015 <- read_excel("acidentes-2015.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2016 <- read_excel("acidentes-2016.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2017 <- read_excel("acidentes-2017.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2018 <- read_excel("acidentes-2018.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
                             , .name_repair = make_clean_names)
acidentes_2019 <- read_excel("acidentes-2019.xlsx", sheet = 1, col_names = TRUE, na = "NÃO DEFINIDO"
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

# Variáveis com alto teor em NA's
summary(acidentes$certificado_adr)
summary(acidentes$veiculo_especial)
summary(acidentes$tipo_veiculo)

#Acidentes duplicados
acidentes <- acidentes %>% remove_empty(c("rows", "cols"))
id_duplicado <- acidentes %>% get_dupes(contains("id_acidente"))

ncol(id_duplicado)
nrow(id_duplicado)

#Tratamento de duplicados