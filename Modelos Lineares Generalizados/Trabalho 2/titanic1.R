# https://smolski.github.io/livroavancado/reglog.html#regressao-logistica-multipla-com-variavel-categorica 

# Carregando o arquivo
#install.packages("titanic") # Base do R titanic
library(titanic)
help(titanic)

# Análise exploratória
head(titanic_train) # Olhando o dataset
summary(titanic_train) # Resumos de resultados
ncol(titanic_train) # Total de observações
nrow(titanic_train) # Quantidade de variaveis
str(titanic_train) # Categoria das variáveis


# Determinando a regressão
mylogit <- glm(Survived ~ Age + Sex + Pclass, data = titanic_train, 
               family = binomial(link="logit"))

# Resultado
summary(mylogit)

anova(mylogit, test = "Chisq")

# Criação de novo modelo com base no anterior retirando a classe
mylogit2=update(mylogit,~. - Pclass)
# 
anova(mylogit,mylogit2, test = "Chisq")

exp(cbind(OR = coef(mylogit), confint(mylogit)))

# Predição das probabilidades de Uma mulher de 22 anos, que estava na 3 classe sobreviver

pred=data.frame(Age=22,
                Sex=factor("famale"),
                Pclass=factor(3)
                )
pred$prob=predict(mylogit, newdata=pred, type="response")
pred