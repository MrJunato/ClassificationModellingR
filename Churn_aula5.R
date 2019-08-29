#Carregando dados e criando uma base de dados nova para a base tratada
Churn_Modelling <- read.csv("Churn_Modelling.csv", header = TRUE)
dados <- Churn_Modelling

#Transformando em factor
tofac <- c(
  "Exited"
  ,"Gender"
  ,"Geography"
  ,"NumOfProducts"
  ,"HasCrCard"
  ,"IsActiveMember")

# transformar para fator por referencia
for(i in tofac){
  dados[,i] <- as.factor(dados[,i])
}
str(dados)

#Retirando colunas sem importância
dados$RowNumber <- NULL
dados$CustomerId <- NULL
dados$Surname <- NULL
summary(dados)

#Transformar NA em 0
dados[is.na(dados)] <- 0

#Criando função para normalizar valores para mínimo 0 e máximo 1
padronizar01 <- function(x) {
  ( ( x-min(x) ) / ( max(x)-min(x) ) )
}

#Criando função para normalizar transformando média em 0 e desvio padrão em 1
#isso faz com que os dados formem uma normal
padronizarmedia <- function(x) {
  ( ( x-mean(x) ) / sd(x) )
}

#Padronizando salario para 0 e 1 e vendo o resultado
salario01 <- padronizar01(dados$EstimatedSalary)
summary(salario01)

#Padronizando salario para media 0 desvio padrão 1 vendo o resultado
salariomedia <- padronizarmedia(dados$EstimatedSalary)
summary(salariomedia)

dados$EstimatedSalary <- salariomedia #Substituindo coluna de salario pela normalizada pela media 0

#Normalizando e substituindo todas as colunas numéricas###
balancemedia <- padronizarmedia(dados$Balance)           #
dados$Balance <- balancemedia                            #
                                                         #
creditscoremedia <- padronizarmedia(dados$CreditScore)   #
dados$CreditScore <- creditscoremedia                    #
                                                         #
agemedia <- padronizarmedia(dados$Age)                   #
dados$Age <- agemedia                                    #
                                                         #
tenuremedia <- padronizarmedia(dados$Tenure)             #
dados$Tenure <- tenuremedia                              #
##########################################################

# Data Partition
set.seed(123)
ind <- sample(2, nrow(dados), replace = TRUE, prob = c(0.7, 0.3))
train <- dados[ind==1,]
test <- dados[ind==2,]

library(ROSE) 
rose <- ROSE(Exited~., data = train, N = 5000, seed=111)$data
table(rose$Exited)
summary(rose)

##############################################################
library(caret)
fit_knn_k1 <- knn3(Exited ~ ., data=rose, k = 1)

fit_knn_k10 <- knn3(Exited ~ ., data=rose, k = 10)

library("rpart")
fit_dt <- rpart(Exited ~ ., data=rose)

fit_dt_cp <- rpart(Exited ~ ., data=rose,
                   control = rpart.control(cp = 0.001, minsplit = 1))

library(C50)
fit_c50 <- C5.0(Exited ~ ., data=rose)

library(randomForest)
fit_rf <- randomForest(Exited ~ ., data=rose)

# Predicao
pred_knn_k1 <- predict(fit_knn_k1, test, type = "class")
pred_knn_k10 <- predict(fit_knn_k10, test, type = "class")
pred_dt <- predict(fit_dt, test, type = "class")
pred_c50 <- predict(fit_c50, test, type = "class")
pred_rf <- predict(fit_rf, test, type = "class")

View(test)

# conferencia
confusionMatrix(pred_knn_k1, test$Exited, positive = '1')
confusionMatrix(pred_knn_k10, test$Exited, positive = '1')
confusionMatrix(pred_dt, test$Exited, positive = '1')
confusionMatrix(pred_c50, test$Exited, positive = '1')
confusionMatrix(pred_rf, test$Exited, positive = '1')
