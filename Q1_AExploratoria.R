# Importando os dadose as bibliotecas:

library(tidyverse)
library(gridExtra)
library(readr)

ENEM_2019 <- read_delim("C:/Users/RAP/Downloads/TrabalhoInferencia/ENEM_2019.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
ENEM_2018 <- read_delim("C:/Users/RAP/Downloads/TrabalhoInferencia/ENEM_2018.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
ENEM_2017 <- read_delim("C:/Users/RAP/Downloads/TrabalhoInferencia/ENEM_2017.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

# 1-Faça uma análise da variável NU_Nota_MT, em nível Brasil. Calcule estatísticas de resumo, o histograma e coloque
# títulos em suas análises. O que você pode afirmar dos candidatos em relação a nota de matemática?


#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
# Média:
mediamt_br2019 = mean(ENEM_2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_br2019 = sd(ENEM_2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_br2019 = cv(ENEM_2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_br2019 <- quantile(ENEM_2019$NU_NOTA_MT, probs = 0.25)
Q2mt_br2019 <- quantile(ENEM_2019$NU_NOTA_MT, probs = 0.50)
Q3mt_br2019 <- quantile(ENEM_2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_br2019 = hist(ENEM_2019$NU_NOTA_MT,  
                       main = "Nota de Matemática no Enem 2019",
                       xlab = "Nota de Matemática", ylab = "Frequência", 
                       col = c("BLUE"), 
                       border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de matemática em 2019?")
cat("Em média os candidatos tiram", mediamt_br2019, "pontos em Matemática na prova do Enem no Brasil em 2019, com um desvio padrão de", DPmt_br2019,", tendo um coeficiente de variação de", CVmt_br2019,", 25% das notas são menores ou iguais a", Q1mt_br2019, ", 50% das notas são menores ou iguais a", Q2mt_br2019, ", e 75% das notas são menores ou iguais a", Q3mt_br2019, ", já o histograma nos mostra as notas mais frequentes 0 e 425 e que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////
# Média:
mediamt_br2018 = mean(ENEM_2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_br2018 = sd(ENEM_2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_br2018 = cv(ENEM_2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_br2018 <- quantile(ENEM_2018$NU_NOTA_MT, probs = 0.25)
Q2mt_br2018 <- quantile(ENEM_2018$NU_NOTA_MT, probs = 0.50)
Q3mt_br2018 <- quantile(ENEM_2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_br2018 = hist(ENEM_2018$NU_NOTA_MT,  
                           main = "Nota de Matemática no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("YELLOW"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de matemática em 2018?")
cat("Em média os candidatos tiram", mediamt_br2018, "pontos em Matemática na prova do Enem no Brasil em 2018, com um desvio padrão de", DPmt_br2018,", coeficiente de variação de", CVmt_br2018,", 25% das notas são menores ou iguais a", Q1mt_br2018, ", 50% das notas são menores ou iguais a", Q2mt_br2018, "e 75% das notas são menores ou iguais a", Q3mt_br2018, ", já o histograma nos mostra as notas frequentes, 0 e 500, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////
# Média:
mediamt_br2017 = mean(ENEM_2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_br2017 = sd(ENEM_2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_br2017 = cv(ENEM_2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_br2017 <- quantile(ENEM_2017$NU_NOTA_MT, probs = 0.25)
Q2mt_br2017 <- quantile(ENEM_2017$NU_NOTA_MT, probs = 0.50)
Q3mt_br2017 <- quantile(ENEM_2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_br2017 = hist(ENEM_2017$NU_NOTA_MT,  
                           main = "Nota de Matemática no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de matemática em 2017?")
cat("Em média os candidatos tiram", mediamt_br2017, "pontos em Matemática na prova do Enem no Brasil em 2017, com um desvio padrão de", DPmt_br2017,",coeficiente de variação de", CVmt_br2017,", 25% das notas são menores ou iguais a", Q1mt_br2017, ", 50% das notas são menores ou iguais a", Q2mt_br2017, "e 75% das notas são menores ou iguais a", Q3mt_br2017, ", já o histograma nos mostra as notas mais frequentes 0 e 450, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )
