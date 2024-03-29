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

# 1-Fa�a uma an�lise da vari�vel NU_Nota_MT, em n�vel Brasil. Calcule estat�sticas de resumo, o histograma e coloque
# t�tulos em suas an�lises. O que voc� pode afirmar dos candidatos em rela��o a nota de matem�tica?


#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
# M�dia:
mediamt_br2019 = mean(ENEM_2019$NU_NOTA_MT) 
# Desvio padr�o: determina o grau de dispers�o dos dados a partir da m�dia.
DPmt_br2019 = sd(ENEM_2019$NU_NOTA_MT)
# Coeficiente de varia��o: descreve a varia��o nos dados em rela��o � m�dia
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_br2019 = cv(ENEM_2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_br2019 <- quantile(ENEM_2019$NU_NOTA_MT, probs = 0.25)
Q2mt_br2019 <- quantile(ENEM_2019$NU_NOTA_MT, probs = 0.50)
Q3mt_br2019 <- quantile(ENEM_2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_br2019 = hist(ENEM_2019$NU_NOTA_MT,  
                       main = "Nota de Matem�tica no Enem 2019",
                       xlab = "Nota de Matem�tica", ylab = "Frequ�ncia", 
                       col = c("BLUE"), 
                       border = FALSE)

# Interpreta��o:
cat("O que voc� pode afirmar dos candidatos em rela��o a nota de matem�tica em 2019?")
cat("Em m�dia os candidatos tiram", mediamt_br2019, "pontos em Matem�tica na prova do Enem no Brasil em 2019, com um desvio padr�o de", DPmt_br2019,", tendo um coeficiente de varia��o de", CVmt_br2019,", 25% das notas s�o menores ou iguais a", Q1mt_br2019, ", 50% das notas s�o menores ou iguais a", Q2mt_br2019, ", e 75% das notas s�o menores ou iguais a", Q3mt_br2019, ", j� o histograma nos mostra as notas mais frequentes 0 e 425 e que os dados s�o assim�tricos � direita, ou seja a maioria das notas � baixa e apenas algumas s�o altas." )

#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////
# M�dia:
mediamt_br2018 = mean(ENEM_2018$NU_NOTA_MT) 
# Desvio padr�o: determina o grau de dispers�o dos dados a partir da m�dia.
DPmt_br2018 = sd(ENEM_2018$NU_NOTA_MT)
# Coeficiente de varia��o: descreve a varia��o nos dados em rela��o � m�dia
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_br2018 = cv(ENEM_2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_br2018 <- quantile(ENEM_2018$NU_NOTA_MT, probs = 0.25)
Q2mt_br2018 <- quantile(ENEM_2018$NU_NOTA_MT, probs = 0.50)
Q3mt_br2018 <- quantile(ENEM_2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_br2018 = hist(ENEM_2018$NU_NOTA_MT,  
                           main = "Nota de Matem�tica no Enem 2018",
                           xlab = "Nota de Matem�tica", ylab = "Frequ�ncia", 
                           col = c("YELLOW"), 
                           border = FALSE)

# Interpreta��o:
cat("O que voc� pode afirmar dos candidatos em rela��o a nota de matem�tica em 2018?")
cat("Em m�dia os candidatos tiram", mediamt_br2018, "pontos em Matem�tica na prova do Enem no Brasil em 2018, com um desvio padr�o de", DPmt_br2018,", coeficiente de varia��o de", CVmt_br2018,", 25% das notas s�o menores ou iguais a", Q1mt_br2018, ", 50% das notas s�o menores ou iguais a", Q2mt_br2018, "e 75% das notas s�o menores ou iguais a", Q3mt_br2018, ", j� o histograma nos mostra as notas frequentes, 0 e 500, que os dados s�o assim�tricos � direita, ou seja a maioria das notas � baixa e apenas algumas s�o altas." )

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////
# M�dia:
mediamt_br2017 = mean(ENEM_2017$NU_NOTA_MT) 
# Desvio padr�o: determina o grau de dispers�o dos dados a partir da m�dia.
DPmt_br2017 = sd(ENEM_2017$NU_NOTA_MT)
# Coeficiente de varia��o: descreve a varia��o nos dados em rela��o � m�dia
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_br2017 = cv(ENEM_2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_br2017 <- quantile(ENEM_2017$NU_NOTA_MT, probs = 0.25)
Q2mt_br2017 <- quantile(ENEM_2017$NU_NOTA_MT, probs = 0.50)
Q3mt_br2017 <- quantile(ENEM_2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_br2017 = hist(ENEM_2017$NU_NOTA_MT,  
                           main = "Nota de Matem�tica no Enem 2017",
                           xlab = "Nota de Matem�tica", ylab = "Frequ�ncia", 
                           col = c("violet"), 
                           border = FALSE)

# Interpreta��o:
cat("O que voc� pode afirmar dos candidatos em rela��o a nota de matem�tica em 2017?")
cat("Em m�dia os candidatos tiram", mediamt_br2017, "pontos em Matem�tica na prova do Enem no Brasil em 2017, com um desvio padr�o de", DPmt_br2017,",coeficiente de varia��o de", CVmt_br2017,", 25% das notas s�o menores ou iguais a", Q1mt_br2017, ", 50% das notas s�o menores ou iguais a", Q2mt_br2017, "e 75% das notas s�o menores ou iguais a", Q3mt_br2017, ", j� o histograma nos mostra as notas mais frequentes 0 e 450, que os dados s�o assim�tricos � direita, ou seja a maioria das notas � baixa e apenas algumas s�o altas." )
