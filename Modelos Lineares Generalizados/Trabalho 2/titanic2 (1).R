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

# An�lise explorat�ria
head(titanic_train) # Olhando o dataset
summary(titanic_train) # Resumos de resultados
ncol(titanic_train) # Total de observa��es
nrow(titanic_train) # Quantidade de variaveis
str(titanic_train) # Categoria das vari�veis
head(titanic_test) # para teste
#omit rows with NA in any column of data frame
titanic_train <- na.omit(titanic_train)
# Selecionando colunas

# Tratar os dados
titanic_train$Survived <- as.factor(titanic_train$Survived) #impor para o R considerar remissivo como categ�rica.
titanic_train$Survived <- relevel(titanic_train$Survived,ref="1") #categoria do sucesso

# Regress�o log�stica - modelo pensado
#m0 <- glm(Survived ~ Age + Sex + Pclass, data = titanic_train, family = binomial(link = "logit"))
#summary(m0)

# Regress�o log�stica - modelo completo com todas as vari�veis
m0 <- glm(Survived ~ ., data = titanic_train, family = binomial(link = "logit"))
summary(m0)

# Realiza sele��o de preditoras com stepwise via BIC.
m1 <- step(m0, k = log(nrow(titanic_train)))
summary(m1)

# A fun��o stepwise indicou que o melhor modelo � o abaixo devido ao valor do AIC e o o n�mero de vari�veis
#Step:  AIC=4849.73
#Survived ~ PassengerId + Pclass + Name + Sex + Age

# Modelo que a stepwize sugere:

#m1 <- glm(Survived ~ PassengerId + Pclass + Name + Sex + Age, data = titanic_train, family = binomial(link = "logit"))
#summary(m1)

anova(m1, test="Chisq") # fun��o que da os par�metros para construir e usar o pi-value

pR2(m1)


# odds ratio.
OR <- exp(m1$coefficients)
OR
# Realiza predi��o.
yp <- predict(m1, type = "response")
yp

# Erro de classifica��o.
tb <- table(round(yp), titanic_train$Survived)
tb

# Percentual de acertos.
sum(diag(tb))/sum(tb)

# ROC.

p <- predict(m1, titanic_test)
pr <- prediction(1-p, titanic_test$Survived)#ATEN��O: est� 1-p pq no relevel est� ref="1".
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize=TRUE)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#.
PRROC_obj <- roc.curve(scores.class0 = 1-p, weights.class0=titanic_test$Survived,
                       curve=TRUE)#ATEN��O: est� 1-p pq no relevel est� ref="1".
plot(PRROC_obj)

#.

ROCit_obj <- rocit(score=1-p,class=titanic_test$Survived)#ATEN��O: est� 1-p pq no relevel est� ref="1".
plot(ROCit_obj)
                       