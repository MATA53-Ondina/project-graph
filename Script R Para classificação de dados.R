# Pacotes Utilizados
rm(ls())
require(readr)
require(psych)
require(kableExtra)
require(ggplot2)
require(tidyr)
require(e1071)
require(caret)
library(caret)


#Carregando Banco de dados
setwd(":\\DadosR\\Dados")
options(scipen = 99)
df <- read_delim("Producao_Cientifica.csv",";", escape_double = FALSE, trim_ws = TRUE)
summary(df)
head(df)


# Removendo variaveis que nao sao uteis para a predicao
id_variaveis_remover <- c(1,2,7,10)
#Gerando uma amostra de 10000 observações aleatorias do banco
set.seed(16072018)
dfzinho <- df[sample(1:265187, 10000),-id_variaveis_remover]


# Categorizando variavei artigos periodiocos (Pelos quartis)
quantis1 <- quantile(dfzinho$`Artigos Periodicos`, probs = c(0.25,0.5,0.75,1))
dfzinho$`Artigos Periodicos1` <- factor(cut(dfzinho$`Artigos Periodicos`, breaks = c(0,quantis1), include.lowest = T))

# Categorizando variavei artigos periodiocos (Abaixo de 8 ou acima de 8)
quantis2 <- quantile(dfzinho$`Artigos Periodicos`, probs = c(0.5,1))
dfzinho$`Artigos Periodicos1` <- factor(cut(dfzinho$`Artigos Periodicos`, breaks = c(0,quantis2), include.lowest = T))

# Categorizando variavei tabalhhos anais (Pelos quartis)
quantis3 <- quantile(dfzinho$`Trabalhos Anais de Congresso`, probs = c(0.25,0.5,0.75,1))
dfzinho$`Trabalhos Anais de Congresso1` <- factor(cut(dfzinho$`Trabalhos Anais de Congresso`, breaks = c(0,quantis3), include.lowest = T))

# Categorizando variavei tabalhhos anais (Abaixo de 17 ou acima de 17)
quantis4 <- quantile(dfzinho$`Trabalhos Anais de Congresso`, probs = c(0.5,1))
dfzinho$`Trabalhos Anais de Congresso1` <- factor(cut(dfzinho$`Trabalhos Anais de Congresso`, breaks = c(0,quantis4), include.lowest = T))

# Remover variaveis que foram categorizadas
dfzinho <- dfzinho[, -c(1)]

# separando base em treino e teste
set.seed(16072018)
treino <- sample(1:10000, 0.7*10000)  
teste <- setdiff(1:10000,treino)

prod_treino <- dfzinho[treino,]
prod_teste <- dfzinho[teste,]


# Predizendo e classificando Trabalhos Anais de Congresso 2 classes utilizando svm
modelo_teste <- svm(`Trabalhos Anais de Congresso1` ~ .,drop = FALSE, scale = FALSE,data = prod_treino)
modelo_teste2 <- svm(`Trabalhos Anais de Congresso1` ~ . , kernel = "linear" , data = prod_treino) 
modelo_teste3 <- svm(`Trabalhos Anais de Congresso1` ~ . , kernel = "polynomial" , data = prod_treino) 
modelo_teste4 <- svm(`Trabalhos Anais de Congresso1` ~ . , kernel = "sigmoid" , data = prod_treino)

# Gerando predição e acurácia do metodo
predicao_1 <- predict(modelo_teste,prod_teste[,-9])
tb_1 <- table(predicao_1,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_1 <- (sum(diag(tb_1)))/nrow(prod_teste)
acuracia_1

#Gerando gráfico com quantidade de erros e acertos
par(mfrow=c(2,2))
col <- c("#ed3b3b", "#0099ff")
fourfoldplot(tb_1, color = col, std = c("margins", "ind.max", "all.max"), conf.level = 0, margin = 1, main = paste0(acuracia_1*100, "%"))

# Gerando predição e acurácia do metodo
predicao_linear <- predict(modelo_teste2,prod_teste[,-9])
tb_linear <- table(predicao_linear,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_svm_linear <- (sum(diag(tb_linear)))/nrow(prod_teste)
acuracia_svm_linear

#Gerando gráfico com quantidade de erros e acertos
fourfoldplot(tb_linear, color = col, std = c("margins", "ind.max", "all.max"), conf.level = 0, margin = 1, main = paste0(round(acuracia_svm_linear*100,1), "%"))

# Gerando predição e acurácia do metodo
predicao_poly <- predict(modelo_teste3,prod_teste[,-9])
tb_poly <- table(predicao_poly,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_svm_poly <- (sum(diag(tb_poly)))/nrow(prod_teste)
acuracia_svm_poly

#Gerando gráfico com quantidade de erros e acertos
fourfoldplot(tb_poly, color = col, std = c("margins", "ind.max", "all.max"), conf.level = 0, margin = 1, main = paste0(round(acuracia_svm_poly*100,1), "%"))

# Gerando predição e acurácia do metodo
predicao_sig <- predict(modelo_teste4,prod_teste[,-9])
tb_sig <- table(predicao_sig,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_svm_sig <- (sum(diag(tb_sig)))/nrow(prod_teste)
acuracia_svm_sig

#Gerando gráfico com quantidade de erros e acertos
fourfoldplot(tb_sig, color = col, std = c("margins", "ind.max", "all.max"), conf.level = 0, margin = 1, main = paste0(round(acuracia_svm_sig*100,1), "%"))


# Predizendo e classificando Trabalhos Anais de Congresso Utilizando o knn para 2 classes
knn <- train(as.character(`Trabalhos Anais de Congresso1`) ~ ., method="knn", prod_treino)

predicao_knn2 <- predict(knn, newdata = prod_teste)
tb_knn2 <- table(predicao_knn2,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_knn2 <- 1-(sum(diag(tb_knn2)))/nrow(prod_teste)
acuracia_knn2

#Gerando gráfico com quantidade de erros e acertos
fourfoldplot(tb_knn2, color = col, conf.level = 0, margin = 1, main = paste0(round(acuracia_knn2*100,1), "%"))


# Predizendo e classificando Trabalhos Anais de Congresso Utilizando o naive bayes para 2 classes
nb <- train(as.character(`Trabalhos Anais de Congresso1`) ~ ., method="nb", prod_treino)

predicao_nb2 <- predict(nb, newdata = prod_teste)
tb_nb2 <- table(predicao_nb2,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_nb2 <- 1-(sum(diag(tb_nb2)))/nrow(prod_teste)
acuracia_nb2

#Gerando gráfico com quantidade de erros e acertos
fourfoldplot(tb_nb2, color = col, conf.level = 0, margin = 1, main = paste0(round(acuracia_nb2*100,1), "%"))


#-------------------------------- 4 Classes ---------------------------

#Predizendo e classificando Trabalhos Anais de Congresso 4 classes utilizando svm

modelo_teste <- svm(`Trabalhos Anais de Congresso1` ~ .,drop = FALSE, scale = FALSE,data = prod_treino)
modelo_teste2 <- svm(`Trabalhos Anais de Congresso1` ~ . , kernel = "linear" , data = prod_treino) 
modelo_teste3 <- svm(`Trabalhos Anais de Congresso1` ~ . , kernel = "polynomial" , data = prod_treino) 
modelo_teste4 <- svm(`Trabalhos Anais de Congresso1` ~ . , kernel = "sigmoid" , data = prod_treino)

predicao_1 <- predict(modelo_teste,prod_teste[,-9])
tb_1 <- table(predicao_1,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_1 <- (sum(diag(tb_1)))/nrow(prod_teste)
acuracia_1

predicao_linear <- predict(modelo_teste2,prod_teste[,-9])
tb_linear <- table(predicao_linear,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_svm_linear <- (sum(diag(tb_linear)))/nrow(prod_teste)
acuracia_svm_linear

predicao_poly <- predict(modelo_teste3,prod_teste[,-9])
tb_poly <- table(predicao_poly,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_svm_poly <- (sum(diag(tb_poly)))/nrow(prod_teste)
acuracia_svm_poly

predicao_sig <- predict(modelo_teste4,prod_teste[,-9])
tb_sig <- table(predicao_sig,prod_teste$`Trabalhos Anais de Congresso1`)
acuracia_svm_sig <- (sum(diag(tb_sig)))/nrow(prod_teste)
acuracia_svm_sig


# Predizendo e classificando Trabalhos Anais de Congresso Utilizando o knn para 4 classes
knn <- train(as.character(`Trabalhos Anais de Congresso1`) ~ ., method="knn", prod_treino)

predicao_knn2 <- predict(knn, newdata = prod_teste)
tb_knn2 <- confusionMatrix(predicao_knn2,prod_teste$`Trabalhos Anais de Congresso1`)
col <- c("#ed3b3b", "#0099ff")
fourfoldplot(tb_knn2, color = col, conf.level = 0, margin = 1)
acuracia_knn2 <- 1-(sum(diag(tb_knn2)))/nrow(prod_teste)
acuracia_knn2

# Predizendo e classificando Trabalhos Anais de Congresso Utilizando o naive bayes para 4 classes
nb <- train(as.character(`Trabalhos Anais de Congresso1`) ~ ., method="nb", prod_treino)

predicao_nb2 <- predict(nb, newdata = prod_teste)
tb_nb2 <- table(predicao_nb2,prod_teste$`Trabalhos Anais de Congresso1`)
col <- c("#ed3b3b", "#0099ff")
fourfoldplot(tb_nb2, color = col, conf.level = 0, margin = 1)
acuracia_nb2 <- 1-(sum(diag(tb_nb2)))/nrow(prod_teste)
acuracia_nb2



write.csv(dfzinho, "teste.xls", row.names = FALSE)
