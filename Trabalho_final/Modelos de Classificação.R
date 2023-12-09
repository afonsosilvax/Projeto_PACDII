install.packages("gbm")

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
library(randomForest)
library(gbm)

# Leitura dos dados
acidentes <- read_excel("acidentes-sample.xlsx", sheet = 3, col_names = TRUE, na = "NÃO DEFINIDO"
                        , .name_repair = make_clean_names)

# Nota: Estamos a assumir que esta amostra é representativa da população.

# Tratamento da coluna datahora
acidentes$datahora <- as_datetime(acidentes$datahora)
class(acidentes$datahora)

# Resumo do dataset
glimpse(acidentes)

sum(is.na(acidentes))/(nrow(acidentes)*ncol(acidentes))

# Eliminação de na's de acordo com a nossa interpretação do manual de preenchimento
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
summary(is.na(acidentes))

# Tratamento de variáveis
acidentes$velocidade_local <- as.integer(acidentes$velocidade_local)
acidentes$velocidade_local <- as.factor(acidentes$velocidade_local)
acidentes$velocidade_geral <- as.integer(acidentes$velocidade_geral)
acidentes$velocidade_geral <- as.factor(acidentes$velocidade_geral)

acidentes <- acidentes %>% mutate_if(is.character, as.factor)

acidentes$dia <- format(acidentes$datahora, "%d")
acidentes$mes <- format(acidentes$datahora, "%m")
acidentes$hora <- format(acidentes$datahora, "%H:%M:%S")

glimpse(acidentes)


# resolução problema 2
hist(acidentes$num_feridos_ligeiros_a_30_dias)
hist(acidentes$num_feridos_graves_a_30_dias)
hist(acidentes$num_mortos_a_30_dias)

# Acidentes graves: num_mortos_a_30_dias > 0 ou num_feridos_graves_a_30_dias > 0

prob2 <- acidentes %>% 
  mutate(gravidade = ifelse(num_mortos_a_30_dias > 0 | num_feridos_graves_a_30_dias > 0, "Grave", "Nao Grave"), 
         hora = format(acidentes$datahora, "%H")) %>%
  select(-num_mortos_a_30_dias, -num_feridos_graves_a_30_dias, -num_feridos_ligeiros_a_30_dias, -id_acidente, -datahora, -dia, -latitude_gps, -longitude_gps)

prob2$hora <- as.integer(prob2$hora)
prob2$mes <- as.integer(prob2$mes)
prob2$mes <- as.factor(prob2$mes)
prob2$gravidade <- as.factor(prob2$gravidade)

glimpse(prob2)

# Vamos fazer uma amostra dos dados para poder fazer algoritmos com o mesmo
set.seed(2022)
prob2s <- sample(1:nrow(prob2), nrow(prob2) * 0.70)
amostra <- prob2[prob2s, ]

prop.table(table(amostra$gravidade))
xtabs(~gravidade + natureza, data = amostra)

set.seed(2022)
conjunto_indices <- sample(1:nrow(amostra), size = 0.90 * nrow(amostra))
conjunto_treino <- amostra[conjunto_indices, ]
conjunto_teste <- amostra[-conjunto_indices, ]

prop.table(table(conjunto_treino$gravidade))
prop.table(table(conjunto_teste$gravidade))

# Under-sampling
train_down <- downSample(conjunto_treino[, -36], conjunto_treino$gravidade)
train_down <- train_down %>% rename(gravidade = Class)
prop.table(table(train_down$gravidade))

plot(train_down$gravidade, main = "Conjunto de treino equilibrado por Under-sampling",xlab = "Valores da target",
     ylab = "nº de observações",col = "orange")

#Analise de correlações
cramersV(train_down$gravidade, train_down$mes)
cramersV(train_down$gravidade, train_down$hora)
cramersV(train_down$gravidade, train_down$entidades_fiscalizadoras)
cramersV(train_down$gravidade, train_down$velocidade_local)
cramersV(train_down$gravidade, train_down$velocidade_geral)
cramersV(train_down$gravidade, train_down$dia_da_semana)
cramersV(train_down$gravidade, train_down$caracteristicas_tecnicas1)
cramersV(train_down$gravidade, train_down$cond_aderencia)
cramersV(train_down$gravidade, train_down$distrito)
cramersV(train_down$gravidade, train_down$concelho)
cramersV(train_down$gravidade, train_down$freguesia)
cramersV(train_down$gravidade, train_down$pov_proxima)
cramersV(train_down$gravidade, train_down$nome_arruamento)
cramersV(train_down$gravidade, train_down$tipos_vias)
cramersV(train_down$gravidade, train_down$cod_via)
cramersV(train_down$gravidade, train_down$estado_conservacao)
cramersV(train_down$gravidade, train_down$km)
cramersV(train_down$gravidade, train_down$factores_atmosfericos)
cramersV(train_down$gravidade, train_down$reg_circulacao1)
cramersV(train_down$gravidade, train_down$interseccao_vias)
cramersV(train_down$gravidade, train_down$localizacoes)
cramersV(train_down$gravidade, train_down$luminosidade)
cramersV(train_down$gravidade, train_down$marca_via)
cramersV(train_down$gravidade, train_down$natureza)
cramersV(train_down$gravidade, train_down$obras_arte)
cramersV(train_down$gravidade, train_down$obstaculos)
cramersV(train_down$gravidade, train_down$sentidos)
cramersV(train_down$gravidade, train_down$sinais)
cramersV(train_down$gravidade, train_down$sinais_luminosos)
cramersV(train_down$gravidade, train_down$tipo_piso)
cramersV(train_down$gravidade, train_down$tracado_1)
cramersV(train_down$gravidade, train_down$tracado_2)
cramersV(train_down$gravidade, train_down$tracado_3)
cramersV(train_down$gravidade, train_down$tracado_4)
cramersV(train_down$gravidade, train_down$via_transito)

# Variáveis relevantes: freguesia, pov_proxima, nome_arruamento, km
length(levels(amostra$concelho))
length(levels(amostra$pov_proxima))
length(levels(amostra$nome_arruamento))
length(levels(amostra$km))

#analisar correlações entre variaveis de alta correlação, natureza, km, codvia, nome arruamento, pov_proxima, freguesia,, concelho, distrito, velocidade geral, velocidade local
#dentro das variaveis de alta correlação que dizem respeito a localização escolhemos a com maior correlação pois elas sao correlacionadas entre si o mesmo para as velociades
# algoritmo regressão logistica de forma naive

modelo_logistico <- glm(relevel(gravidade, ref="Nao Grave") ~ mes + distrito + localizacoes + luminosidade + natureza, data = train_down, family = "binomial")
summary(modelo_logistico)
length(coef(modelo_logistico))

# previsão dos dados
previsao <- predict(modelo_logistico, conjunto_teste, type="response")
previsao <- data.frame(cbind(probabilidade = round(previsao, 3), previsao = ifelse(previsao > 0.5, "Grave", "Nao Grave"), classe = ifelse(conjunto_teste$gravidade==1, "Grave", "Nao Grave")))

(matriz_confusao <- table(conjunto_teste$gravidade, previsao$previsao))

(sensibilidade <- matriz_confusao[1, 1]/(matriz_confusao[1, 1] + matriz_confusao[1, 2]))
(especificidade <- matriz_confusao[2, 2]/(matriz_confusao[2, 2] + matriz_confusao[2, 1]))
(precisao <- matriz_confusao[1, 1]/(matriz_confusao[1, 1] + matriz_confusao[2, 1]))

#accuracy of the prediction
(accuracy <- mean(previsao$previsao == conjunto_teste$gravidade))

prop.table(table(conjunto_teste$gravidade))



# Modelo SVM
modelo_svm <- svm(gravidade ~ mes + distrito + localizacoes + luminosidade + natureza, data = train_down, kernel = "linear")
summary(modelo_svm)

#predict with test data
previsao <- data.frame(previsao = predict(modelo_svm, newdata = conjunto_teste, type = "link"))

# number of coefficients in the model
length(coef(modelo_svm))

(matriz_confusao <- table(conjunto_teste$gravidade, previsao$previsao))
# precison: 0.625

(sensibilidade <- matriz_confusao[1, 1]/(matriz_confusao[1, 1] + matriz_confusao[1, 2]))
(especificidade <- matriz_confusao[2, 2]/(matriz_confusao[2, 2] + matriz_confusao[2, 1]))
(precisao <- matriz_confusao[1, 1]/(matriz_confusao[1, 1] + matriz_confusao[2, 1]))

#accuracy of the prediction
(accuracy <- mean(previsao$previsao == conjunto_teste$gravidade))

# Modelo Random Forest
set.seed(2022)
modelo_rf <- randomForest(gravidade ~ mes + hora + distrito + localizacoes + luminosidade + natureza, data = train_down, ntree = 1000, importance = TRUE)
summary(modelo_rf)

# previsão dos dados
previsao <- predict(modelo_rf, conjunto_teste, type="response")
previsao <- data.frame(previsao = previsao, classe = ifelse(conjunto_teste$gravidade==1, "Grave", "Nao Grave"))

(matriz_confusao <- table(conjunto_teste$gravidade, previsao$previsao))

(sensibilidade <- matriz_confusao[1, 1]/(matriz_confusao[1, 1] + matriz_confusao[1, 2]))
(especificidade <- matriz_confusao[2, 2]/(matriz_confusao[2, 2] + matriz_confusao[2, 1]))
(precisao <- matriz_confusao[1, 1]/(matriz_confusao[1, 1] + matriz_confusao[2, 1]))

#accuracy of the prediction
(accuracy <- mean(previsao$previsao == conjunto_teste$gravidade))

#accuracy of the prediction
mean(previsao$previsao == conjunto_teste$gravidade)
# 0.6861213

# Modelo Gradient Boosting
set.seed(2022)
modelo_gb <- gbm(ifelse(gravidade=="Nao Grave", 0, 1) ~ mes + hora + velocidade_geral + distrito + localizacoes + luminosidade + natureza, data = train_down, distribution = "bernoulli", n.trees = 500, interaction.depth = 4, shrinkage = 0.01, cv.folds = 5, verbose = TRUE)
summary(modelo_gb)

previsao <- predict(modelo_gb, conjunto_teste, type="response")
previsao <- data.frame(cbind(probabilidade = round(previsao, 3), previsao = ifelse(previsao > 0.5, "Grave", "Nao Grave"), classe = ifelse(conjunto_teste$gravidade==1, "Grave", "Nao Grave")))

(matriz_confusao <- table(conjunto_teste$gravidade, previsao$previsao))
# precison: 0.6130952

(sensibilidade <- matriz_confusao[1, 1]/(matriz_confusao[1, 1] + matriz_confusao[1, 2]))
(especificidade <- matriz_confusao[2, 2]/(matriz_confusao[2, 2] + matriz_confusao[2, 1]))
(precisao <- matriz_confusao[1, 1]/(matriz_confusao[1, 1] + matriz_confusao[2, 1]))

#accuracy of the prediction
mean(previsao$previsao == conjunto_teste$gravidade)
#0.6360294