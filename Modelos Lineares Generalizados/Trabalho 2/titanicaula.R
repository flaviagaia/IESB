# FALTA ENTENDER ESSE 1-P QUE É O RELEVEL E O FACTOR

# Carregando o arquivo e as bibliotecas                                                                                                                                                                                                                               
#install.packages("titanic") # Base do R titanic
library(titanic)
help(titanic)
library(lattice)
library(latticeExtra)
library(pscl)
library(ROCR)
library(PRROC)
library(ROCit)

# Análise exploratória
head(titanic_train) # Olhando o dataset
summary(titanic_train) # Resumos de resultados
ncol(titanic_train) # Total de observações
nrow(titanic_train) # Quantidade de variaveis
str(titanic_train) # Categoria das variáveis
head(titanic_test) # para teste

# Tratar os dados
titanic_train$Survived <- as.factor(titanic_train$Survived) #impor para o R considerar remissivo como categórica.
titanic_train$Survived <- relevel(titanic_train$Survived,ref="1") #categoria do sucesso

# Regressão logística - modelo pensado
#m0 <- glm(Survived ~ Age + Sex + Pclass, data = titanic_train, family = binomial(link = "logit"))
#summary(m0)

# Regressão logística - modelo completo com todas as variáveis
#mc <- glm(Survived ~ ., data = titanic_train, family = binomial(link = "logit"))
#summary(mc)

# Realiza seleção de preditoras com stepwise via BIC.
#m1 <- step(mc, k = log(nrow(titanic_train)))
#summary(m1)

# A função stepwise indicou que o melhor modelo é o abaixo devido ao valor do AIC e o o número de variáveis
#Step:  AIC=4849.73
#Survived ~ PassengerId + Pclass + Name + Sex + Age

# Modelo que a stepwize sugere:

m1 <- glm(Survived ~ PassengerId + Pclass + Name + Sex + Age, data = titanic_train, family = binomial(link = "logit"))
summary(m1)

anova(m1, test="Chisq") # função que da os parâmetros para construir e usar o pi-value

pR2(m1)


# odds ratio.
OR <- exp(m1$coefficients)

# Realiza predição.
yp <- predict(m1, type = "response")
yp

# Erro de classificação.
tb <- table(round(yp), titanic_train$Survived)
tb

# Percentual de acertos.
sum(diag(tb))/sum(tb)

# ROC.

p <- predict(m1, titanic_test)
pr <- prediction(1-p, titanic_test$Survived)#ATENÇÃO: está 1-p pq no relevel está ref="1".
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize=TRUE)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#.
PRROC_obj <- roc.curve(scores.class0 = 1-p, weights.class0=titanic_test$Survived,
                       curve=TRUE)#ATENÇÃO: está 1-p pq no relevel está ref="1".
plot(PRROC_obj)

#.

ROCit_obj <- rocit(score=1-p,class=titanic_test$Survived)#ATENÇÃO: está 1-p pq no relevel está ref="1".
plot(ROCit_obj)
                       