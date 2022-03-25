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

#3- Repita a sua análise para as variáveis NU_Nota_CH, NU_Nota_CN, NU_Nota_LC, NU_Nota_Redacao,
#Brasil e por Unidade da Federação da escola (SG_UF_RESIDENCIA). Calcule estatísticas de resumo, o histograma e
#coloque títulos em suas análises. O que você pode afirmar dos candidatos em relação às notas no Brasil e nos estados?
  
# Brasil 

# ------------ NU_Nota_CH 

#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
# Média:
mediach_br2019 = mean(ENEM_2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_br2019 = sd(ENEM_2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}	

CVch_br2019 = cv(ENEM_2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_br2019 <- quantile(ENEM_2019$NU_NOTA_CH, probs = 0.25)
Q2ch_br2019 <- quantile(ENEM_2019$NU_NOTA_CH, probs = 0.50)
Q3ch_br2019 <- quantile(ENEM_2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_br2019 = hist(ENEM_2019$NU_NOTA_CH,  
                           main = "Nota de Ciencias Humanas no Enem 2019",
                           xlab = "Nota de Ciencias Humanas", ylab = "Frequência", 
                           col = c("red"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Ciencias Humanas em 2019?")
cat("Em média os candidatos tiram", mediach_br2019, "pontos em Ciencias Humanas na prova do Enem no Brasil em 2019, que se desvia da média em cerca de", DPch_br2019,"pontos, tendo um coeficiente de variação de", CVch_br2019,", 25% das notas são menores ou iguais a", Q1ch_br2019, ", 50% das notas são menores ou iguais a", Q2ch_br2019, ", e 75% das notas são menores ou iguais a", Q3ch_br2019, ", já o histograma nos mostra as notas mais frequentes 0 e 525 e que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////
# Média:
mediach_br2018 = mean(ENEM_2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_br2018 = sd(ENEM_2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_br2018 = cv(ENEM_2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_br2018 <- quantile(ENEM_2018$NU_NOTA_CH, probs = 0.25)
Q2ch_br2018 <- quantile(ENEM_2018$NU_NOTA_CH, probs = 0.50)
Q3ch_br2018 <- quantile(ENEM_2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_br2018 = hist(ENEM_2018$NU_NOTA_CH,  
                           main = "Nota de Ciencias Humanas no Enem 2018",
                           xlab = "Nota de Ciencias Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Ciencias Humanas em 2018?")
cat("Em média os candidatos tiram", mediach_br2018, "pontos em Ciencias Humanas na prova do Enem no Brasil em 2018, que se desvia da média em cerca de", DPch_br2018,"pontos, com um coeficiente de variação de", CVch_br2018,", 25% das notas são menores ou iguais a", Q1ch_br2018, ", 50% das notas são menores ou iguais a", Q2ch_br2018, "e 75% das notas são menores ou iguais a", Q3ch_br2018, ", já o histograma nos mostra as notas frequentes, 0 e 500, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////
# Média:
mediach_br2017 = mean(ENEM_2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_br2017 = sd(ENEM_2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_br2017 = cv(ENEM_2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_br2017 <- quantile(ENEM_2017$NU_NOTA_CH, probs = 0.25)
Q2ch_br2017 <- quantile(ENEM_2017$NU_NOTA_CH, probs = 0.50)
Q3ch_br2017 <- quantile(ENEM_2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_br2017 = hist(ENEM_2017$NU_NOTA_CH,  
                           main = "Nota de Ciencias Humanas no Enem 2017",
                           xlab = "Nota de Ciencias Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Ciencias Humanas em 2017?")
cat("Em média os candidatos tiram", mediach_br2017, "pontos em Ciencias Humanas na prova do Enem no Brasil em 2017, que se desvia da média em cerca de", DPch_br2017,"pontos, com um coeficiente de variação de", CVch_br2017,", 25% das notas são menores ou iguais a", Q1ch_br2017, ", 50% das notas são menores ou iguais a", Q2ch_br2017, "e 75% das notas são menores ou iguais a", Q3ch_br2017, ", já o histograma nos mostra as notas mais frequentes 0 e 450, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )


# ------------ NU_Nota_CN

#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
# Média:
mediacn_br2019 = mean(ENEM_2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPcn_br2019 = sd(ENEM_2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}	

CVcn_br2019 = cv(ENEM_2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1cn_br2019 <- quantile(ENEM_2019$NU_NOTA_CN, probs = 0.25)
Q2cn_br2019 <- quantile(ENEM_2019$NU_NOTA_CN, probs = 0.50)
Q3cn_br2019 <- quantile(ENEM_2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramacn_br2019 = hist(ENEM_2019$NU_NOTA_CN,  
                           main = "Nota de Ciencias da Natureza no Enem 2019",
                           xlab = "Nota de Ciencias da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Ciencias da Natureza em 2019?")
cat("Em média os candidatos tiram", mediacn_br2019, "pontos em Ciencias da Natureza na prova do Enem no Brasil em 2019, que se desvia da média em cerca de", DPcn_br2019,"pontos, tendo um coeficiente de variação de", CVcn_br2019,", 25% das notas são menores ou iguais a", Q1cn_br2019, ", 50% das notas são menores ou iguais a", Q2cn_br2019, ", e 75% das notas são menores ou iguais a", Q3cn_br2019, ", já o histograma nos mostra as notas mais frequentes 0 e 425 e que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////
# Média:
mediacn_br2018 = mean(ENEM_2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPcn_br2018 = sd(ENEM_2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVcn_br2018 = cv(ENEM_2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1cn_br2018 <- quantile(ENEM_2018$NU_NOTA_CN, probs = 0.25)
Q2cn_br2018 <- quantile(ENEM_2018$NU_NOTA_CN, probs = 0.50)
Q3cn_br2018 <- quantile(ENEM_2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramacn_br2018 = hist(ENEM_2018$NU_NOTA_CN,  
                           main = "Nota de Ciencias da Natureza no Enem 2018",
                           xlab = "Nota de Ciencias da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Ciencias da Natureza em 2018?")
cat("Em média os candidatos tiram", mediacn_br2018, "pontos em Ciencias da Natureza na prova do Enem no Brasil em 2018, que se desvia da média em cerca de", DPcn_br2018,"pontos, com um coeficiente de variação de", CVcn_br2018,", 25% das notas são menores ou iguais a", Q1cn_br2018, ", 50% das notas são menores ou iguais a", Q2cn_br2018, "e 75% das notas são menores ou iguais a", Q3cn_br2018, ", já o histograma nos mostra as notas frequentes, 0 e 500, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////
# Média:
mediacn_br2017 = mean(ENEM_2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPcn_br2017 = sd(ENEM_2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVcn_br2017 = cv(ENEM_2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1cn_br2017 <- quantile(ENEM_2017$NU_NOTA_CN, probs = 0.25)
Q2cn_br2017 <- quantile(ENEM_2017$NU_NOTA_CN, probs = 0.50)
Q3cn_br2017 <- quantile(ENEM_2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramacn_br2017 = hist(ENEM_2017$NU_NOTA_CN,  
                           main = "Nota de Ciencias da Natureza no Enem 2017",
                           xlab = "Nota de Ciencias da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Ciencias da Natureza em 2017?")
cat("Em média os candidatos tiram", mediacn_br2017, "pontos em Ciencias da Natureza na prova do Enem no Brasil em 2017, que se desvia da média em cerca de", DPcn_br2017,"pontos, com um coeficiente de variação de", CVcn_br2017,", 25% das notas são menores ou iguais a", Q1cn_br2017, ", 50% das notas são menores ou iguais a", Q2cn_br2017, "e 75% das notas são menores ou iguais a", Q3cn_br2017, ", já o histograma nos mostra as notas mais frequentes 0 e 450, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

# ------------ NU_Nota_LC

#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
# Média:
mediaLC_br2019 = mean(ENEM_2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_br2019 = sd(ENEM_2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}	

CVLC_br2019 = cv(ENEM_2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_br2019 <- quantile(ENEM_2019$NU_NOTA_LC, probs = 0.25)
Q2LC_br2019 <- quantile(ENEM_2019$NU_NOTA_LC, probs = 0.50)
Q3LC_br2019 <- quantile(ENEM_2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_br2019 = hist(ENEM_2019$NU_NOTA_LC,  
                           main = "Nota de Linguagens e Códigos no Enem 2019",
                           xlab = "Nota de Linguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Linguagens e Códigos em 2019?")
cat("Em média os candidatos tiram", mediaLC_br2019, "pontos em Linguagens e Códigos na prova do Enem no Brasil em 2019, que se desvia da média em cerca de", DPLC_br2019,"pontos, tendo um coeficiente de variação de", CVLC_br2019,", 25% das notas são menores ou iguais a", Q1LC_br2019, ", 50% das notas são menores ou iguais a", Q2LC_br2019, ", e 75% das notas são menores ou iguais a", Q3LC_br2019, ", já o histograma nos mostra as notas mais frequentes 0 e 425 e que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////
# Média:
mediaLC_br2018 = mean(ENEM_2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_br2018 = sd(ENEM_2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_br2018 = cv(ENEM_2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_br2018 <- quantile(ENEM_2018$NU_NOTA_LC, probs = 0.25)
Q2LC_br2018 <- quantile(ENEM_2018$NU_NOTA_LC, probs = 0.50)
Q3LC_br2018 <- quantile(ENEM_2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_br2018 = hist(ENEM_2018$NU_NOTA_LC,  
                           main = "Nota de Linguagens e Códigos no Enem 2018",
                           xlab = "Nota de Linguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Linguagens e Códigos em 2018?")
cat("Em média os candidatos tiram", mediaLC_br2018, "pontos em Linguagens e Códigos na prova do Enem no Brasil em 2018, que se desvia da média em cerca de", DPLC_br2018,"pontos, com um coeficiente de variação de", CVLC_br2018,", 25% das notas são menores ou iguais a", Q1LC_br2018, ", 50% das notas são menores ou iguais a", Q2LC_br2018, "e 75% das notas são menores ou iguais a", Q3LC_br2018, ", já o histograma nos mostra as notas frequentes, 0 e 500, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////
# Média:
mediaLC_br2017 = mean(ENEM_2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_br2017 = sd(ENEM_2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_br2017 = cv(ENEM_2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_br2017 <- quantile(ENEM_2017$NU_NOTA_LC, probs = 0.25)
Q2LC_br2017 <- quantile(ENEM_2017$NU_NOTA_LC, probs = 0.50)
Q3LC_br2017 <- quantile(ENEM_2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_br2017 = hist(ENEM_2017$NU_NOTA_LC,  
                           main = "Nota de Linguagens e Códigos no Enem 2017",
                           xlab = "Nota de Linguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Linguagens e Códigos em 2017?")
cat("Em média os candidatos tiram", mediaLC_br2017, "pontos em Linguagens e Códigos na prova do Enem no Brasil em 2017, que se desvia da média em cerca de", DPLC_br2017,"pontos, com um coeficiente de variação de", CVLC_br2017,", 25% das notas são menores ou iguais a", Q1LC_br2017, ", 50% das notas são menores ou iguais a", Q2LC_br2017, "e 75% das notas são menores ou iguais a", Q3LC_br2017, ", já o histograma nos mostra as notas mais frequentes 0 e 450, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

# ------------ NU_Nota_Redacao

#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
# Média:
mediaREDACAO_br2019 = mean(ENEM_2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPREDACAO_br2019 = sd(ENEM_2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}	

CVREDACAO_br2019 = cv(ENEM_2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1REDACAO_br2019 <- quantile(ENEM_2019$NU_NOTA_REDACAO, probs = 0.25)
Q2REDACAO_br2019 <- quantile(ENEM_2019$NU_NOTA_REDACAO, probs = 0.50)
Q3REDACAO_br2019 <- quantile(ENEM_2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaREDACAO_br2019 = hist(ENEM_2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Redacao em 2019?")
cat("Em média os candidatos tiram", mediaREDACAO_br2019, "pontos em Redacao na prova do Enem no Brasil em 2019, que se desvia da média em cerca de", DPREDACAO_br2019,"pontos, tendo um coeficiente de variação de", CVREDACAO_br2019,", 25% das notas são menores ou iguais a", Q1REDACAO_br2019, ", 50% das notas são menores ou iguais a", Q2REDACAO_br2019, ", e 75% das notas são menores ou iguais a", Q3REDACAO_br2019, ", já o histograma nos mostra as notas mais frequentes 0 e 425 e que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////
# Média:
mediaREDACAO_br2018 = mean(ENEM_2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPREDACAO_br2018 = sd(ENEM_2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVREDACAO_br2018 = cv(ENEM_2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1REDACAO_br2018 <- quantile(ENEM_2018$NU_NOTA_REDACAO, probs = 0.25)
Q2REDACAO_br2018 <- quantile(ENEM_2018$NU_NOTA_REDACAO, probs = 0.50)
Q3REDACAO_br2018 <- quantile(ENEM_2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaREDACAO_br2018 = hist(ENEM_2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Redacao em 2018?")
cat("Em média os candidatos tiram", mediaREDACAO_br2018, "pontos em Redacao na prova do Enem no Brasil em 2018, que se desvia da média em cerca de", DPREDACAO_br2018,"pontos, com um coeficiente de variação de", CVREDACAO_br2018,", 25% das notas são menores ou iguais a", Q1REDACAO_br2018, ", 50% das notas são menores ou iguais a", Q2REDACAO_br2018, "e 75% das notas são menores ou iguais a", Q3REDACAO_br2018, ", já o histograma nos mostra as notas frequentes, 0 e 500, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////
# Média:
mediaREDACAO_br2017 = mean(ENEM_2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPREDACAO_br2017 = sd(ENEM_2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVREDACAO_br2017 = cv(ENEM_2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1REDACAO_br2017 <- quantile(ENEM_2017$NU_NOTA_REDACAO, probs = 0.25)
Q2REDACAO_br2017 <- quantile(ENEM_2017$NU_NOTA_REDACAO, probs = 0.50)
Q3REDACAO_br2017 <- quantile(ENEM_2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaREDACAO_br2017 = hist(ENEM_2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de Redacao em 2017?")
cat("Em média os candidatos tiram", mediaREDACAO_br2017, "pontos em Redacao na prova do Enem no Brasil em 2017, que se desvia da média em cerca de", DPREDACAO_br2017,"pontos, com um coeficiente de variação de", CVREDACAO_br2017,", 25% das notas são menores ou iguais a", Q1REDACAO_br2017, ", 50% das notas são menores ou iguais a", Q2REDACAO_br2017, "e 75% das notas são menores ou iguais a", Q3REDACAO_br2017, ", já o histograma nos mostra as notas mais frequentes 0 e 450, que os dados são assimétricos à direita, ou seja a maioria das notas é baixa e apenas algumas são altas." )

# SG_UF_RESIDENCIA

# ------------ NU_Nota_CH 

#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
library(dplyr)

# Acre

AC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediach_ac2019 = mean(AC2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ac2019 = sd(AC2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ac2019 = cv(AC2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ac2019 <- quantile(AC2019$NU_NOTA_CH, probs = 0.25)
Q2ch_ac2019 <- quantile(AC2019$NU_NOTA_CH, probs = 0.50)
Q3ch_ac2019 <- quantile(AC2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ac2019 = hist(AC2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Acre no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_ac2019, ", desvio padrão:", DPch_ac2019, ", coeficiente de variação:", CVch_ac2019, ", primeiro quartil:", Q1ch_ac2019, ", segundo quartil:", Q2ch_ac2019, ", terceiro quartil:", Q3ch_ac2019)

# Alagoas

AL2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AL')
# Média:
mediach_al2019 = mean(AL2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_al2019 = sd(AL2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_al2019 = cv(AL2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_al2019 <- quantile(AL2019$NU_NOTA_CH, probs = 0.25)
Q2ch_al2019 <- quantile(AL2019$NU_NOTA_CH, probs = 0.50)
Q3ch_al2019 <- quantile(AL2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_al2019 = hist(AL2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Alagoas no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_al2019, ", desvio padrão:", DPch_al2019, ", coeficiente de variação:", CVch_al2019, ", primeiro quartil:", Q1ch_al2019, ", segundo quartil:", Q2ch_al2019, ", terceiro quartil:", Q3ch_al2019)

# Amapá

AP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AP') 
# Média:
mediach_ap2019 = mean(AP2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ap2019 = sd(AP2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ap2019 = cv(AP2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ap2019 <- quantile(AP2019$NU_NOTA_CH, probs = 0.25)
Q2ch_ap2019 <- quantile(AP2019$NU_NOTA_CH, probs = 0.50)
Q3ch_ap2019 <- quantile(AP2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ap2019 = hist(AP2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Amapá no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_ap2019, ", desvio padrão:", DPch_ap2019, ", coeficiente de variação:", CVch_ap2019, ", primeiro quartil:", Q1ch_ap2019, ", segundo quartil:", Q2ch_ap2019, ", terceiro quartil:", Q3ch_ap2019)

# Amazonas

AM2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AM') 
# Média:
mediach_am2019 = mean(AM2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_am2019 = sd(AM2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_am2019 = cv(AM2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_am2019 <- quantile(AM2019$NU_NOTA_CH, probs = 0.25)
Q2ch_am2019 <- quantile(AM2019$NU_NOTA_CH, probs = 0.50)
Q3ch_am2019 <- quantile(AM2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_am2019 = hist(AM2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Amazonas no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_am2019, ", desvio padrão:", DPch_am2019, ", coeficiente de variação:", CVch_am2019, ", primeiro quartil:", Q1ch_am2019, ", segundo quartil:", Q2ch_am2019, ", terceiro quartil:", Q3ch_am2019)

# Bahia

BA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='BA') 
# Média:
mediach_ba2019 = mean(BA2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ba2019 = sd(BA2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ba2019 = cv(BA2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ba2019 <- quantile(BA2019$NU_NOTA_CH, probs = 0.25)
Q2ch_ba2019 <- quantile(BA2019$NU_NOTA_CH, probs = 0.50)
Q3ch_ba2019 <- quantile(BA2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ba2019 = hist(BA2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Bahia no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_ba2019, ", desvio padrão:", DPch_ba2019, ", coeficiente de variação:", CVch_ba2019, ", primeiro quartil:", Q1ch_ba2019, ", segundo quartil:", Q2ch_ba2019, ", terceiro quartil:", Q3ch_ba2019)

# Ceará

CE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='CE') 
# Média:
mediach_ce2019 = mean(CE2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ce2019 = sd(CE2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ce2019 = cv(CE2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ce2019 <- quantile(CE2019$NU_NOTA_CH, probs = 0.25)
Q2ch_ce2019 <- quantile(CE2019$NU_NOTA_CH, probs = 0.50)
Q3ch_ce2019 <- quantile(CE2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ce2019 = hist(CE2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Ceará no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_ce2019, ", desvio padrão:", DPch_ce2019, ", coeficiente de variação:", CVch_ce2019, ", primeiro quartil:", Q1ch_ce2019, ", segundo quartil:", Q2ch_ce2019, ", terceiro quartil:", Q3ch_ce2019)

# Distrito Federal

DF2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='DF') 
# Média:
mediach_df2019 = mean(DF2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_df2019 = sd(DF2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_df2019 = cv(DF2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_df2019 <- quantile(DF2019$NU_NOTA_CH, probs = 0.25)
Q2ch_df2019 <- quantile(DF2019$NU_NOTA_CH, probs = 0.50)
Q3ch_df2019 <- quantile(DF2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_df2019 = hist(DF2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Distrito Federal no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_df2019, ", desvio padrão:", DPch_df2019, ", coeficiente de variação:", CVch_df2019, ", primeiro quartil:", Q1ch_df2019, ", segundo quartil:", Q2ch_df2019, ", terceiro quartil:", Q3ch_df2019)

# Espírito Santo

ES2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='ES') 
# Média:
mediach_es2019 = mean(ES2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_es2019 = sd(ES2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_es2019 = cv(ES2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_es2019 <- quantile(ES2019$NU_NOTA_CH, probs = 0.25)
Q2ch_es2019 <- quantile(ES2019$NU_NOTA_CH, probs = 0.50)
Q3ch_es2019 <- quantile(ES2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_es2019 = hist(ES2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Espírito Santo no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_es2019, ", desvio padrão:", DPch_es2019, ", coeficiente de variação:", CVch_es2019, ", primeiro quartil:", Q1ch_es2019, ", segundo quartil:", Q2ch_es2019, ", terceiro quartil:", Q3ch_es2019)

# Goiás

GO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='GO') 
# Média:
mediach_go2019 = mean(GO2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_go2019 = sd(GO2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_go2019 = cv(GO2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_go2019 <- quantile(GO2019$NU_NOTA_CH, probs = 0.25)
Q2ch_go2019 <- quantile(GO2019$NU_NOTA_CH, probs = 0.50)
Q3ch_go2019 <- quantile(GO2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_go2019 = hist(GO2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Goiás no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_go2019, ", desvio padrão:", DPch_go2019, ", coeficiente de variação:", CVch_go2019, ", primeiro quartil:", Q1ch_go2019, ", segundo quartil:", Q2ch_go2019, ", terceiro quartil:", Q3ch_go2019)

# Maranhão

MA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MA') 
# Média:
mediach_ma2019 = mean(MA2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ma2019 = sd(MA2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ma2019 = cv(MA2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ma2019 <- quantile(MA2019$NU_NOTA_CH, probs = 0.25)
Q2ch_ma2019 <- quantile(MA2019$NU_NOTA_CH, probs = 0.50)
Q3ch_ma2019 <- quantile(MA2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ma2019 = hist(MA2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Maranhão no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_ma2019, ", desvio padrão:", DPch_ma2019, ", coeficiente de variação:", CVch_ma2019, ", primeiro quartil:", Q1ch_ma2019, ", segundo quartil:", Q2ch_ma2019, ", terceiro quartil:", Q3ch_ma2019)

# Mato Grosso

MT2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MT') 
# Média:
mediach_MT2019 = mean(MT2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_MT2019 = sd(MT2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_MT2019 = cv(MT2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_MT2019 <- quantile(MT2019$NU_NOTA_CH, probs = 0.25)
Q2ch_MT2019 <- quantile(MT2019$NU_NOTA_CH, probs = 0.50)
Q3ch_MT2019 <- quantile(MT2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_MT2019 = hist(MT2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Mato Grosso no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_MT2019, ", desvio padrão:", DPch_MT2019, ", coeficiente de variação:", CVch_MT2019, ", primeiro quartil:", Q1ch_MT2019, ", segundo quartil:", Q2ch_MT2019, ", terceiro quartil:", Q3ch_MT2019)

# Mato Grosso do Sul

MS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MS') 
# Média:
mediach_ms2019 = mean(MS2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ms2019 = sd(MS2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ms2019 = cv(MS2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ms2019 <- quantile(MS2019$NU_NOTA_CH, probs = 0.25)
Q2ch_ms2019 <- quantile(MS2019$NU_NOTA_CH, probs = 0.50)
Q3ch_ms2019 <- quantile(MS2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ms2019 = hist(MS2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Mato Grosso do Sul no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_ms2019, ", desvio padrão:", DPch_ms2019, ", coeficiente de variação:", CVch_ms2019, ", primeiro quartil:", Q1ch_ms2019, ", segundo quartil:", Q2ch_ms2019, ", terceiro quartil:", Q3ch_ms2019)

# Minas Gerais

MG2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MG') 
# Média:
mediach_mg2019 = mean(MG2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_mg2019 = sd(MG2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_mg2019 = cv(MG2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_mg2019 <- quantile(MG2019$NU_NOTA_CH, probs = 0.25)
Q2ch_mg2019 <- quantile(MG2019$NU_NOTA_CH, probs = 0.50)
Q3ch_mg2019 <- quantile(MG2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_mg2019 = hist(MG2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Minas Gerais no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_mg2019, ", desvio padrão:", DPch_mg2019, ", coeficiente de variação:", CVch_mg2019, ", primeiro quartil:", Q1ch_mg2019, ", segundo quartil:", Q2ch_mg2019, ", terceiro quartil:", Q3ch_mg2019)

# Pará

PA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PA') 
# Média:
mediach_pa2019 = mean(PA2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pa2019 = sd(PA2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pa2019 = cv(PA2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pa2019 <- quantile(PA2019$NU_NOTA_CH, probs = 0.25)
Q2ch_pa2019 <- quantile(PA2019$NU_NOTA_CH, probs = 0.50)
Q3ch_pa2019 <- quantile(PA2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pa2019 = hist(PA2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Pará no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_pa2019, ", desvio padrão:", DPch_pa2019, ", coeficiente de variação:", CVch_pa2019, ", primeiro quartil:", Q1ch_pa2019, ", segundo quartil:", Q2ch_pa2019, ", terceiro quartil:", Q3ch_pa2019)

# Paraíba

PB2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PB') 
# Média:
mediach_pb2019 = mean(PB2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pb2019 = sd(PB2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pb2019 = cv(PB2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pb2019 <- quantile(PB2019$NU_NOTA_CH, probs = 0.25)
Q2ch_pb2019 <- quantile(PB2019$NU_NOTA_CH, probs = 0.50)
Q3ch_pb2019 <- quantile(PB2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pb2019 = hist(PB2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Paraíba no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_pb2019, ", desvio padrão:", DPch_pb2019, ", coeficiente de variação:", CVch_pb2019, ", primeiro quartil:", Q1ch_pb2019, ", segundo quartil:", Q2ch_pb2019, ", terceiro quartil:", Q3ch_pb2019)

# Paraná

PR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PR') 
# Média:
mediach_pr2019 = mean(PR2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pr2019 = sd(PR2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pr2019 = cv(PR2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pr2019 <- quantile(PR2019$NU_NOTA_CH, probs = 0.25)
Q2ch_pr2019 <- quantile(PR2019$NU_NOTA_CH, probs = 0.50)
Q3ch_pr2019 <- quantile(PR2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pr2019 = hist(PR2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Paraná no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_pr2019, ", desvio padrão:", DPch_pr2019, ", coeficiente de variação:", CVch_pr2019, ", primeiro quartil:", Q1ch_pr2019, ", segundo quartil:", Q2ch_pr2019, ", terceiro quartil:", Q3ch_pr2019)

# Pernambuco

PE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PE') 
# Média:
mediach_pe2019 = mean(PE2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pe2019 = sd(PE2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pe2019 = cv(PE2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pe2019 <- quantile(PE2019$NU_NOTA_CH, probs = 0.25)
Q2ch_pe2019 <- quantile(PE2019$NU_NOTA_CH, probs = 0.50)
Q3ch_pe2019 <- quantile(PE2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pe2019 = hist(PE2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Pernambuco no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_pe2019, ", desvio padrão:", DPch_pe2019, ", coeficiente de variação:", CVch_pe2019, ", peimeiro quartil:", Q1ch_pe2019, ", segundo quartil:", Q2ch_pe2019, ", terceiro quartil:", Q3ch_pe2019)

# Piauí

PI2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PI') 
# Média:
mediach_pi2019 = mean(PI2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pi2019 = sd(PI2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pi2019 = cv(PI2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pi2019 <- quantile(PI2019$NU_NOTA_CH, probs = 0.25)
Q2ch_pi2019 <- quantile(PI2019$NU_NOTA_CH, probs = 0.50)
Q3ch_pi2019 <- quantile(PI2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pi2019 = hist(PI2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Piauí no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_pi2019, ", desvio padrão:", DPch_pi2019, ", coeficiente de variação:", CVch_pi2019, ", primeiro quartil:", Q1ch_pi2019, ", segundo quartil:", Q2ch_pi2019, ", terceiro quartil:", Q3ch_pi2019)

# Rio de Janeiro

RJ2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediach_rj2019 = mean(RJ2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rj2019 = sd(RJ2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rj2019 = cv(RJ2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rj2019 <- quantile(RJ2019$NU_NOTA_CH, probs = 0.25)
Q2ch_rj2019 <- quantile(RJ2019$NU_NOTA_CH, probs = 0.50)
Q3ch_rj2019 <- quantile(RJ2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rj2019 = hist(RJ2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Rio de Janeiro no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_rj2019, ", desvio padrão:", DPch_rj2019, ", coeficiente de variação:", CVch_rj2019, ", primeiro quartil:", Q1ch_rj2019, ", segundo quartil:", Q2ch_rj2019, ", terceiro quartil:", Q3ch_rj2019)

# Rio Grande do Norte

RN2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RN') 
# Média:
mediach_rn2019 = mean(RN2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rn2019 = sd(RN2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rn2019 = cv(RN2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rn2019 <- quantile(RN2019$NU_NOTA_CH, probs = 0.25)
Q2ch_rn2019 <- quantile(RN2019$NU_NOTA_CH, probs = 0.50)
Q3ch_rn2019 <- quantile(RN2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rn2019 = hist(RN2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Rio Grande do Norte no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_rn2019, ", desvio padrão:", DPch_rn2019, ", coeficiente de variação:", CVch_rn2019, ", primeiro quartil:", Q1ch_rn2019, ", segundo quartil:", Q2ch_rn2019, ", terceiro quartil:", Q3ch_rn2019)

# Rio Grande do Sul

RS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RS') 
# Média:
mediach_rs2019 = mean(RS2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rs2019 = sd(RS2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rs2019 = cv(RS2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rs2019 <- quantile(RS2019$NU_NOTA_CH, probs = 0.25)
Q2ch_rs2019 <- quantile(RS2019$NU_NOTA_CH, probs = 0.50)
Q3ch_rs2019 <- quantile(RS2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rs2019 = hist(RS2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Rio Grande do Sul no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_rs2019, ", desvio padrão:", DPch_rs2019, ", coeficiente de variação:", CVch_rs2019, ", primeiro quartil:", Q1ch_rs2019, ", segundo quartil:", Q2ch_rs2019, ", terceira quartil:", Q3ch_rs2019)

# Rondônia

RO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RO') 
# Média:
mediach_ro2019 = mean(RO2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ro2019 = sd(RO2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ro2019 = cv(RO2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ro2019 <- quantile(RO2019$NU_NOTA_CH, probs = 0.25)
Q2ch_ro2019 <- quantile(RO2019$NU_NOTA_CH, probs = 0.50)
Q3ch_ro2019 <- quantile(RO2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ro2019 = hist(RO2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Rondônia no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_ro2019, ", desvio padrão:", DPch_ro2019, ", coeficiente de variação:", CVch_ro2019, ", primeiro quartil:", Q1ch_ro2019, ", segundo quartil:", Q2ch_ro2019, ", terceiro quartil:", Q3ch_ro2019)

# Roraima

RR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RR') 
# Média:
mediach_rr2019 = mean(RR2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rr2019 = sd(RR2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rr2019 = cv(RR2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rr2019 <- quantile(RR2019$NU_NOTA_CH, probs = 0.25)
Q2ch_rr2019 <- quantile(RR2019$NU_NOTA_CH, probs = 0.50)
Q3ch_rr2019 <- quantile(RR2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rr2019 = hist(RR2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Roraima no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_rr2019, ", desvio padrão:", DPch_rr2019, ", coeficiente de variação:", CVch_rr2019, ", primeiro quartil:", Q1ch_rr2019, ", segundo quartil:", Q2ch_rr2019, ", terceiro quartil:", Q3ch_rr2019)

# Santa Catarina

SC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SC') 
# Média:
mediach_sc2019 = mean(SC2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_sc2019 = sd(SC2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_sc2019 = cv(SC2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_sc2019 <- quantile(SC2019$NU_NOTA_CH, probs = 0.25)
Q2ch_sc2019 <- quantile(SC2019$NU_NOTA_CH, probs = 0.50)
Q3ch_sc2019 <- quantile(SC2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_sc2019 = hist(SC2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Santa Catarina  no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_sc2019, ", desvio padrão:", DPch_sc2019, ", coeficiente de variação:", CVch_sc2019, ", primeiro quartil:", Q1ch_sc2019, ", segundo quartil:", Q2ch_sc2019, ", terceiro quartil:", Q3ch_sc2019)

# São Paulo

SP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SP') 
# Média:
mediach_sp2019 = mean(SP2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_sp2019 = sd(SP2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_sp2019 = cv(SP2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_sp2019 <- quantile(SP2019$NU_NOTA_CH, probs = 0.25)
Q2ch_sp2019 <- quantile(SP2019$NU_NOTA_CH, probs = 0.50)
Q3ch_sp2019 <- quantile(SP2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_sp2019 = hist(SP2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de São Paulo  no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_sp2019, ", desvio padrão:", DPch_sp2019, ", coeficiente de variação:", CVch_sp2019, ", primeiro quartil:", Q1ch_sp2019, ", segundo quartil:", Q2ch_sp2019, ", terceiro quartil:", Q3ch_sp2019)

# Sergipe

SE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SE') 
# Média:
mediach_se2019 = mean(SE2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_se2019 = sd(SE2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_se2019 = cv(SE2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_se2019 <- quantile(SE2019$NU_NOTA_CH, probs = 0.25)
Q2ch_se2019 <- quantile(SE2019$NU_NOTA_CH, probs = 0.50)
Q3ch_se2019 <- quantile(SE2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_se2019 = hist(SE2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Sergipe no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_se2019, ", desvio padrão:", DPch_se2019, ", coeficiente de variação:", CVch_se2019, ", primeiro quartil:", Q1ch_se2019, ", segundo quartil:", Q2ch_se2019, ", terceiro quartil:", Q3ch_se2019)

# Tocantins

TO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='TO') 
# Média:
mediach_to2019 = mean(TO2019$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_to2019 = sd(TO2019$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_to2019 = cv(TO2019$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_to2019 <- quantile(TO2019$NU_NOTA_CH, probs = 0.25)
Q2ch_to2019 <- quantile(TO2019$NU_NOTA_CH, probs = 0.50)
Q3ch_to2019 <- quantile(TO2019$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_to2019 = hist(TO2019$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Tocantins no Enem 2019",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de Ciências Humanas do Enem 2019 --> média:", mediach_to2019, ", desvio padrão:", DPch_to2019, ", coeficiente de variação:", CVch_to2019, ", primeiro quartil:", Q1ch_to2019, ", segundo quartil:", Q2ch_to2019, ", terceiro quartil:", Q3ch_to2019)


#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////

# Acre

AC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediach_ac2018 = mean(AC2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ac2018 = sd(AC2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ac2018 = cv(AC2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ac2018 <- quantile(AC2018$NU_NOTA_CH, probs = 0.25)
Q2ch_ac2018 <- quantile(AC2018$NU_NOTA_CH, probs = 0.50)
Q3ch_ac2018 <- quantile(AC2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ac2018 = hist(AC2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Acre no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_ac2018, ", desvio padrão:", DPch_ac2018, ", coeficiente de variação:", CVch_ac2018, ", primeiro quartil:", Q1ch_ac2018, ", segundo quartil:", Q2ch_ac2018, ", terceiro quartil:", Q3ch_ac2018)

# Alagoas

AL2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AL')
# Média:
mediach_al2018 = mean(AL2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_al2018 = sd(AL2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_al2018 = cv(AL2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_al2018 <- quantile(AL2018$NU_NOTA_CH, probs = 0.25)
Q2ch_al2018 <- quantile(AL2018$NU_NOTA_CH, probs = 0.50)
Q3ch_al2018 <- quantile(AL2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_al2018 = hist(AL2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Alagoas no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_al2018, ", desvio padrão:", DPch_al2018, ", coeficiente de variação:", CVch_al2018, ", primeiro quartil:", Q1ch_al2018, ", segundo quartil:", Q2ch_al2018, ", terceiro quartil:", Q3ch_al2018)

# Amapá

AP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AP') 
# Média:
mediach_ap2018 = mean(AP2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ap2018 = sd(AP2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ap2018 = cv(AP2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ap2018 <- quantile(AP2018$NU_NOTA_CH, probs = 0.25)
Q2ch_ap2018 <- quantile(AP2018$NU_NOTA_CH, probs = 0.50)
Q3ch_ap2018 <- quantile(AP2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ap2018 = hist(AP2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Amapá no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_ap2018, ", desvio padrão:", DPch_ap2018, ", coeficiente de variação:", CVch_ap2018, ", primeiro quartil:", Q1ch_ap2018, ", segundo quartil:", Q2ch_ap2018, ", terceiro quartil:", Q3ch_ap2018)

# Amazonas

AM2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AM') 
# Média:
mediach_am2018 = mean(AM2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_am2018 = sd(AM2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_am2018 = cv(AM2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_am2018 <- quantile(AM2018$NU_NOTA_CH, probs = 0.25)
Q2ch_am2018 <- quantile(AM2018$NU_NOTA_CH, probs = 0.50)
Q3ch_am2018 <- quantile(AM2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_am2018 = hist(AM2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Amazonas no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_am2018, ", desvio padrão:", DPch_am2018, ", coeficiente de variação:", CVch_am2018, ", primeiro quartil:", Q1ch_am2018, ", segundo quartil:", Q2ch_am2018, ", terceiro quartil:", Q3ch_am2018)

# Bahia

BA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='BA') 
# Média:
mediach_ba2018 = mean(BA2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ba2018 = sd(BA2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ba2018 = cv(BA2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ba2018 <- quantile(BA2018$NU_NOTA_CH, probs = 0.25)
Q2ch_ba2018 <- quantile(BA2018$NU_NOTA_CH, probs = 0.50)
Q3ch_ba2018 <- quantile(BA2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ba2018 = hist(BA2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Bahia no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_ba2018, ", desvio padrão:", DPch_ba2018, ", coeficiente de variação:", CVch_ba2018, ", primeiro quartil:", Q1ch_ba2018, ", segundo quartil:", Q2ch_ba2018, ", terceiro quartil:", Q3ch_ba2018)

# Ceará

CE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='CE') 
# Média:
mediach_ce2018 = mean(CE2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ce2018 = sd(CE2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ce2018 = cv(CE2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ce2018 <- quantile(CE2018$NU_NOTA_CH, probs = 0.25)
Q2ch_ce2018 <- quantile(CE2018$NU_NOTA_CH, probs = 0.50)
Q3ch_ce2018 <- quantile(CE2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ce2018 = hist(CE2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Ceará no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_ce2018, ", desvio padrão:", DPch_ce2018, ", coeficiente de variação:", CVch_ce2018, ", primeiro quartil:", Q1ch_ce2018, ", segundo quartil:", Q2ch_ce2018, ", terceiro quartil:", Q3ch_ce2018)

# Distrito Federal

DF2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='DF') 
# Média:
mediach_df2018 = mean(DF2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_df2018 = sd(DF2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_df2018 = cv(DF2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_df2018 <- quantile(DF2018$NU_NOTA_CH, probs = 0.25)
Q2ch_df2018 <- quantile(DF2018$NU_NOTA_CH, probs = 0.50)
Q3ch_df2018 <- quantile(DF2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_df2018 = hist(DF2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Distrito Federal no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_df2018, ", desvio padrão:", DPch_df2018, ", coeficiente de variação:", CVch_df2018, ", primeiro quartil:", Q1ch_df2018, ", segundo quartil:", Q2ch_df2018, ", terceiro quartil:", Q3ch_df2018)

# Espírito Santo

ES2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='ES') 
# Média:
mediach_es2018 = mean(ES2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_es2018 = sd(ES2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_es2018 = cv(ES2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_es2018 <- quantile(ES2018$NU_NOTA_CH, probs = 0.25)
Q2ch_es2018 <- quantile(ES2018$NU_NOTA_CH, probs = 0.50)
Q3ch_es2018 <- quantile(ES2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_es2018 = hist(ES2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Espírito Santo no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_es2018, ", desvio padrão:", DPch_es2018, ", coeficiente de variação:", CVch_es2018, ", primeiro quartil:", Q1ch_es2018, ", segundo quartil:", Q2ch_es2018, ", terceiro quartil:", Q3ch_es2018)

# Goiás

GO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='GO') 
# Média:
mediach_go2018 = mean(GO2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_go2018 = sd(GO2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_go2018 = cv(GO2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_go2018 <- quantile(GO2018$NU_NOTA_CH, probs = 0.25)
Q2ch_go2018 <- quantile(GO2018$NU_NOTA_CH, probs = 0.50)
Q3ch_go2018 <- quantile(GO2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_go2018 = hist(GO2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Goiás no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_go2018, ", desvio padrão:", DPch_go2018, ", coeficiente de variação:", CVch_go2018, ", primeiro quartil:", Q1ch_go2018, ", segundo quartil:", Q2ch_go2018, ", terceiro quartil:", Q3ch_go2018)

# Maranhão

MA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MA') 
# Média:
mediach_ma2018 = mean(MA2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ma2018 = sd(MA2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ma2018 = cv(MA2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ma2018 <- quantile(MA2018$NU_NOTA_CH, probs = 0.25)
Q2ch_ma2018 <- quantile(MA2018$NU_NOTA_CH, probs = 0.50)
Q3ch_ma2018 <- quantile(MA2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ma2018 = hist(MA2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Maranhão no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_ma2018, ", desvio padrão:", DPch_ma2018, ", coeficiente de variação:", CVch_ma2018, ", primeiro quartil:", Q1ch_ma2018, ", segundo quartil:", Q2ch_ma2018, ", terceiro quartil:", Q3ch_ma2018)

# Mato Grosso

MT2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MT') 
# Média:
mediach_MT2018 = mean(MT2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_MT2018 = sd(MT2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_MT2018 = cv(MT2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_MT2018 <- quantile(MT2018$NU_NOTA_CH, probs = 0.25)
Q2ch_MT2018 <- quantile(MT2018$NU_NOTA_CH, probs = 0.50)
Q3ch_MT2018 <- quantile(MT2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_MT2018 = hist(MT2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Mato Grosso no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_MT2018, ", desvio padrão:", DPch_MT2018, ", coeficiente de variação:", CVch_MT2018, ", primeiro quartil:", Q1ch_MT2018, ", segundo quartil:", Q2ch_MT2018, ", terceiro quartil:", Q3ch_MT2018)

# Mato Grosso do Sul

MS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MS') 
# Média:
mediach_ms2018 = mean(MS2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ms2018 = sd(MS2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ms2018 = cv(MS2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ms2018 <- quantile(MS2018$NU_NOTA_CH, probs = 0.25)
Q2ch_ms2018 <- quantile(MS2018$NU_NOTA_CH, probs = 0.50)
Q3ch_ms2018 <- quantile(MS2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ms2018 = hist(MS2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Mato Grosso do Sul no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_ms2018, ", desvio padrão:", DPch_ms2018, ", coeficiente de variação:", CVch_ms2018, ", primeiro quartil:", Q1ch_ms2018, ", segundo quartil:", Q2ch_ms2018, ", terceiro quartil:", Q3ch_ms2018)

# Minas Gerais

MG2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MG') 
# Média:
mediach_mg2018 = mean(MG2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_mg2018 = sd(MG2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_mg2018 = cv(MG2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_mg2018 <- quantile(MG2018$NU_NOTA_CH, probs = 0.25)
Q2ch_mg2018 <- quantile(MG2018$NU_NOTA_CH, probs = 0.50)
Q3ch_mg2018 <- quantile(MG2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_mg2018 = hist(MG2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Minas Gerais no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_mg2018, ", desvio padrão:", DPch_mg2018, ", coeficiente de variação:", CVch_mg2018, ", primeiro quartil:", Q1ch_mg2018, ", segundo quartil:", Q2ch_mg2018, ", terceiro quartil:", Q3ch_mg2018)

# Pará

PA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PA') 
# Média:
mediach_pa2018 = mean(PA2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pa2018 = sd(PA2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pa2018 = cv(PA2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pa2018 <- quantile(PA2018$NU_NOTA_CH, probs = 0.25)
Q2ch_pa2018 <- quantile(PA2018$NU_NOTA_CH, probs = 0.50)
Q3ch_pa2018 <- quantile(PA2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pa2018 = hist(PA2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Pará no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_pa2018, ", desvio padrão:", DPch_pa2018, ", coeficiente de variação:", CVch_pa2018, ", primeiro quartil:", Q1ch_pa2018, ", segundo quartil:", Q2ch_pa2018, ", terceiro quartil:", Q3ch_pa2018)

# Paraíba

PB2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PB') 
# Média:
mediach_pb2018 = mean(PB2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pb2018 = sd(PB2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pb2018 = cv(PB2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pb2018 <- quantile(PB2018$NU_NOTA_CH, probs = 0.25)
Q2ch_pb2018 <- quantile(PB2018$NU_NOTA_CH, probs = 0.50)
Q3ch_pb2018 <- quantile(PB2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pb2018 = hist(PB2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Paraíba no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_pb2018, ", desvio padrão:", DPch_pb2018, ", coeficiente de variação:", CVch_pb2018, ", primeiro quartil:", Q1ch_pb2018, ", segundo quartil:", Q2ch_pb2018, ", terceiro quartil:", Q3ch_pb2018)

# Paraná

PR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PR') 
# Média:
mediach_pr2018 = mean(PR2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pr2018 = sd(PR2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pr2018 = cv(PR2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pr2018 <- quantile(PR2018$NU_NOTA_CH, probs = 0.25)
Q2ch_pr2018 <- quantile(PR2018$NU_NOTA_CH, probs = 0.50)
Q3ch_pr2018 <- quantile(PR2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pr2018 = hist(PR2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Paraná no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_pr2018, ", desvio padrão:", DPch_pr2018, ", coeficiente de variação:", CVch_pr2018, ", primeiro quartil:", Q1ch_pr2018, ", segundo quartil:", Q2ch_pr2018, ", terceiro quartil:", Q3ch_pr2018)

# Pernambuco

PE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PE') 
# Média:
mediach_pe2018 = mean(PE2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pe2018 = sd(PE2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pe2018 = cv(PE2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pe2018 <- quantile(PE2018$NU_NOTA_CH, probs = 0.25)
Q2ch_pe2018 <- quantile(PE2018$NU_NOTA_CH, probs = 0.50)
Q3ch_pe2018 <- quantile(PE2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pe2018 = hist(PE2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Pernambuco no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_pe2018, ", desvio padrão:", DPch_pe2018, ", coeficiente de variação:", CVch_pe2018, ", peimeiro quartil:", Q1ch_pe2018, ", segundo quartil:", Q2ch_pe2018, ", terceiro quartil:", Q3ch_pe2018)

# Piauí

PI2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PI') 
# Média:
mediach_pi2018 = mean(PI2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pi2018 = sd(PI2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pi2018 = cv(PI2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pi2018 <- quantile(PI2018$NU_NOTA_CH, probs = 0.25)
Q2ch_pi2018 <- quantile(PI2018$NU_NOTA_CH, probs = 0.50)
Q3ch_pi2018 <- quantile(PI2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pi2018 = hist(PI2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Piauí no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_pi2018, ", desvio padrão:", DPch_pi2018, ", coeficiente de variação:", CVch_pi2018, ", primeiro quartil:", Q1ch_pi2018, ", segundo quartil:", Q2ch_pi2018, ", terceiro quartil:", Q3ch_pi2018)

# Rio de Janeiro

RJ2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediach_rj2018 = mean(RJ2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rj2018 = sd(RJ2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rj2018 = cv(RJ2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rj2018 <- quantile(RJ2018$NU_NOTA_CH, probs = 0.25)
Q2ch_rj2018 <- quantile(RJ2018$NU_NOTA_CH, probs = 0.50)
Q3ch_rj2018 <- quantile(RJ2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rj2018 = hist(RJ2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Rio de Janeiro no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_rj2018, ", desvio padrão:", DPch_rj2018, ", coeficiente de variação:", CVch_rj2018, ", primeiro quartil:", Q1ch_rj2018, ", segundo quartil:", Q2ch_rj2018, ", terceiro quartil:", Q3ch_rj2018)

# Rio Grande do Norte

RN2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RN') 
# Média:
mediach_rn2018 = mean(RN2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rn2018 = sd(RN2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rn2018 = cv(RN2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rn2018 <- quantile(RN2018$NU_NOTA_CH, probs = 0.25)
Q2ch_rn2018 <- quantile(RN2018$NU_NOTA_CH, probs = 0.50)
Q3ch_rn2018 <- quantile(RN2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rn2018 = hist(RN2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Rio Grande do Norte no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_rn2018, ", desvio padrão:", DPch_rn2018, ", coeficiente de variação:", CVch_rn2018, ", primeiro quartil:", Q1ch_rn2018, ", segundo quartil:", Q2ch_rn2018, ", terceiro quartil:", Q3ch_rn2018)

# Rio Grande do Sul

RS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RS') 
# Média:
mediach_rs2018 = mean(RS2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rs2018 = sd(RS2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rs2018 = cv(RS2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rs2018 <- quantile(RS2018$NU_NOTA_CH, probs = 0.25)
Q2ch_rs2018 <- quantile(RS2018$NU_NOTA_CH, probs = 0.50)
Q3ch_rs2018 <- quantile(RS2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rs2018 = hist(RS2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Rio Grande do Sul no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_rs2018, ", desvio padrão:", DPch_rs2018, ", coeficiente de variação:", CVch_rs2018, ", primeiro quartil:", Q1ch_rs2018, ", segundo quartil:", Q2ch_rs2018, ", terceira quartil:", Q3ch_rs2018)

# Rondônia

RO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RO') 
# Média:
mediach_ro2018 = mean(RO2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ro2018 = sd(RO2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ro2018 = cv(RO2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ro2018 <- quantile(RO2018$NU_NOTA_CH, probs = 0.25)
Q2ch_ro2018 <- quantile(RO2018$NU_NOTA_CH, probs = 0.50)
Q3ch_ro2018 <- quantile(RO2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ro2018 = hist(RO2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Rondônia no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_ro2018, ", desvio padrão:", DPch_ro2018, ", coeficiente de variação:", CVch_ro2018, ", primeiro quartil:", Q1ch_ro2018, ", segundo quartil:", Q2ch_ro2018, ", terceiro quartil:", Q3ch_ro2018)

# Roraima

RR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RR') 
# Média:
mediach_rr2018 = mean(RR2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rr2018 = sd(RR2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rr2018 = cv(RR2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rr2018 <- quantile(RR2018$NU_NOTA_CH, probs = 0.25)
Q2ch_rr2018 <- quantile(RR2018$NU_NOTA_CH, probs = 0.50)
Q3ch_rr2018 <- quantile(RR2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rr2018 = hist(RR2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Roraima no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_rr2018, ", desvio padrão:", DPch_rr2018, ", coeficiente de variação:", CVch_rr2018, ", primeiro quartil:", Q1ch_rr2018, ", segundo quartil:", Q2ch_rr2018, ", terceiro quartil:", Q3ch_rr2018)

# Santa Catarina

SC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SC') 
# Média:
mediach_sc2018 = mean(SC2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_sc2018 = sd(SC2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_sc2018 = cv(SC2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_sc2018 <- quantile(SC2018$NU_NOTA_CH, probs = 0.25)
Q2ch_sc2018 <- quantile(SC2018$NU_NOTA_CH, probs = 0.50)
Q3ch_sc2018 <- quantile(SC2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_sc2018 = hist(SC2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Santa Catarina  no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_sc2018, ", desvio padrão:", DPch_sc2018, ", coeficiente de variação:", CVch_sc2018, ", primeiro quartil:", Q1ch_sc2018, ", segundo quartil:", Q2ch_sc2018, ", terceiro quartil:", Q3ch_sc2018)

# São Paulo

SP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SP') 
# Média:
mediach_sp2018 = mean(SP2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_sp2018 = sd(SP2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_sp2018 = cv(SP2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_sp2018 <- quantile(SP2018$NU_NOTA_CH, probs = 0.25)
Q2ch_sp2018 <- quantile(SP2018$NU_NOTA_CH, probs = 0.50)
Q3ch_sp2018 <- quantile(SP2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_sp2018 = hist(SP2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de São Paulo  no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_sp2018, ", desvio padrão:", DPch_sp2018, ", coeficiente de variação:", CVch_sp2018, ", primeiro quartil:", Q1ch_sp2018, ", segundo quartil:", Q2ch_sp2018, ", terceiro quartil:", Q3ch_sp2018)

# Sergipe

SE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SE') 
# Média:
mediach_se2018 = mean(SE2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_se2018 = sd(SE2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_se2018 = cv(SE2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_se2018 <- quantile(SE2018$NU_NOTA_CH, probs = 0.25)
Q2ch_se2018 <- quantile(SE2018$NU_NOTA_CH, probs = 0.50)
Q3ch_se2018 <- quantile(SE2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_se2018 = hist(SE2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Sergipe no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_se2018, ", desvio padrão:", DPch_se2018, ", coeficiente de variação:", CVch_se2018, ", primeiro quartil:", Q1ch_se2018, ", segundo quartil:", Q2ch_se2018, ", terceiro quartil:", Q3ch_se2018)

# Tocantins

TO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='TO') 
# Média:
mediach_to2018 = mean(TO2018$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_to2018 = sd(TO2018$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_to2018 = cv(TO2018$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_to2018 <- quantile(TO2018$NU_NOTA_CH, probs = 0.25)
Q2ch_to2018 <- quantile(TO2018$NU_NOTA_CH, probs = 0.50)
Q3ch_to2018 <- quantile(TO2018$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_to2018 = hist(TO2018$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Tocantins no Enem 2018",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de Ciências Humanas do Enem 2018 --> média:", mediach_to2018, ", desvio padrão:", DPch_to2018, ", coeficiente de variação:", CVch_to2018, ", primeiro quartil:", Q1ch_to2018, ", segundo quartil:", Q2ch_to2018, ", terceiro quartil:", Q3ch_to2018)

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////

# Acre

AC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediach_ac2017 = mean(AC2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ac2017 = sd(AC2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ac2017 = cv(AC2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ac2017 <- quantile(AC2017$NU_NOTA_CH, probs = 0.25)
Q2ch_ac2017 <- quantile(AC2017$NU_NOTA_CH, probs = 0.50)
Q3ch_ac2017 <- quantile(AC2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ac2017 = hist(AC2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Acre no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_ac2017, ", desvio padrão:", DPch_ac2017, ", coeficiente de variação:", CVch_ac2017, ", primeiro quartil:", Q1ch_ac2017, ", segundo quartil:", Q2ch_ac2017, ", terceiro quartil:", Q3ch_ac2017)

# Alagoas

AL2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AL')
# Média:
mediach_al2017 = mean(AL2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_al2017 = sd(AL2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_al2017 = cv(AL2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_al2017 <- quantile(AL2017$NU_NOTA_CH, probs = 0.25)
Q2ch_al2017 <- quantile(AL2017$NU_NOTA_CH, probs = 0.50)
Q3ch_al2017 <- quantile(AL2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_al2017 = hist(AL2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Alagoas no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_al2017, ", desvio padrão:", DPch_al2017, ", coeficiente de variação:", CVch_al2017, ", primeiro quartil:", Q1ch_al2017, ", segundo quartil:", Q2ch_al2017, ", terceiro quartil:", Q3ch_al2017)

# Amapá

AP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AP') 
# Média:
mediach_ap2017 = mean(AP2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ap2017 = sd(AP2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ap2017 = cv(AP2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ap2017 <- quantile(AP2017$NU_NOTA_CH, probs = 0.25)
Q2ch_ap2017 <- quantile(AP2017$NU_NOTA_CH, probs = 0.50)
Q3ch_ap2017 <- quantile(AP2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ap2017 = hist(AP2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Amapá no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_ap2017, ", desvio padrão:", DPch_ap2017, ", coeficiente de variação:", CVch_ap2017, ", primeiro quartil:", Q1ch_ap2017, ", segundo quartil:", Q2ch_ap2017, ", terceiro quartil:", Q3ch_ap2017)

# Amazonas

AM2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AM') 
# Média:
mediach_am2017 = mean(AM2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_am2017 = sd(AM2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_am2017 = cv(AM2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_am2017 <- quantile(AM2017$NU_NOTA_CH, probs = 0.25)
Q2ch_am2017 <- quantile(AM2017$NU_NOTA_CH, probs = 0.50)
Q3ch_am2017 <- quantile(AM2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_am2017 = hist(AM2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Amazonas no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_am2017, ", desvio padrão:", DPch_am2017, ", coeficiente de variação:", CVch_am2017, ", primeiro quartil:", Q1ch_am2017, ", segundo quartil:", Q2ch_am2017, ", terceiro quartil:", Q3ch_am2017)

# Bahia

BA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='BA') 
# Média:
mediach_ba2017 = mean(BA2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ba2017 = sd(BA2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ba2017 = cv(BA2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ba2017 <- quantile(BA2017$NU_NOTA_CH, probs = 0.25)
Q2ch_ba2017 <- quantile(BA2017$NU_NOTA_CH, probs = 0.50)
Q3ch_ba2017 <- quantile(BA2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ba2017 = hist(BA2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Bahia no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_ba2017, ", desvio padrão:", DPch_ba2017, ", coeficiente de variação:", CVch_ba2017, ", primeiro quartil:", Q1ch_ba2017, ", segundo quartil:", Q2ch_ba2017, ", terceiro quartil:", Q3ch_ba2017)

# Ceará

CE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='CE') 
# Média:
mediach_ce2017 = mean(CE2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ce2017 = sd(CE2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ce2017 = cv(CE2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ce2017 <- quantile(CE2017$NU_NOTA_CH, probs = 0.25)
Q2ch_ce2017 <- quantile(CE2017$NU_NOTA_CH, probs = 0.50)
Q3ch_ce2017 <- quantile(CE2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ce2017 = hist(CE2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Ceará no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_ce2017, ", desvio padrão:", DPch_ce2017, ", coeficiente de variação:", CVch_ce2017, ", primeiro quartil:", Q1ch_ce2017, ", segundo quartil:", Q2ch_ce2017, ", terceiro quartil:", Q3ch_ce2017)

# Distrito Federal

DF2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='DF') 
# Média:
mediach_df2017 = mean(DF2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_df2017 = sd(DF2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_df2017 = cv(DF2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_df2017 <- quantile(DF2017$NU_NOTA_CH, probs = 0.25)
Q2ch_df2017 <- quantile(DF2017$NU_NOTA_CH, probs = 0.50)
Q3ch_df2017 <- quantile(DF2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_df2017 = hist(DF2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Distrito Federal no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_df2017, ", desvio padrão:", DPch_df2017, ", coeficiente de variação:", CVch_df2017, ", primeiro quartil:", Q1ch_df2017, ", segundo quartil:", Q2ch_df2017, ", terceiro quartil:", Q3ch_df2017)

# Espírito Santo

ES2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='ES') 
# Média:
mediach_es2017 = mean(ES2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_es2017 = sd(ES2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_es2017 = cv(ES2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_es2017 <- quantile(ES2017$NU_NOTA_CH, probs = 0.25)
Q2ch_es2017 <- quantile(ES2017$NU_NOTA_CH, probs = 0.50)
Q3ch_es2017 <- quantile(ES2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_es2017 = hist(ES2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Espírito Santo no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_es2017, ", desvio padrão:", DPch_es2017, ", coeficiente de variação:", CVch_es2017, ", primeiro quartil:", Q1ch_es2017, ", segundo quartil:", Q2ch_es2017, ", terceiro quartil:", Q3ch_es2017)

# Goiás

GO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='GO') 
# Média:
mediach_go2017 = mean(GO2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_go2017 = sd(GO2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_go2017 = cv(GO2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_go2017 <- quantile(GO2017$NU_NOTA_CH, probs = 0.25)
Q2ch_go2017 <- quantile(GO2017$NU_NOTA_CH, probs = 0.50)
Q3ch_go2017 <- quantile(GO2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_go2017 = hist(GO2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Goiás no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_go2017, ", desvio padrão:", DPch_go2017, ", coeficiente de variação:", CVch_go2017, ", primeiro quartil:", Q1ch_go2017, ", segundo quartil:", Q2ch_go2017, ", terceiro quartil:", Q3ch_go2017)

# Maranhão

MA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MA') 
# Média:
mediach_ma2017 = mean(MA2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ma2017 = sd(MA2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ma2017 = cv(MA2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ma2017 <- quantile(MA2017$NU_NOTA_CH, probs = 0.25)
Q2ch_ma2017 <- quantile(MA2017$NU_NOTA_CH, probs = 0.50)
Q3ch_ma2017 <- quantile(MA2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ma2017 = hist(MA2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Maranhão no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_ma2017, ", desvio padrão:", DPch_ma2017, ", coeficiente de variação:", CVch_ma2017, ", primeiro quartil:", Q1ch_ma2017, ", segundo quartil:", Q2ch_ma2017, ", terceiro quartil:", Q3ch_ma2017)

# Mato Grosso

MT2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MT') 
# Média:
mediach_MT2017 = mean(MT2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_MT2017 = sd(MT2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_MT2017 = cv(MT2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_MT2017 <- quantile(MT2017$NU_NOTA_CH, probs = 0.25)
Q2ch_MT2017 <- quantile(MT2017$NU_NOTA_CH, probs = 0.50)
Q3ch_MT2017 <- quantile(MT2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_MT2017 = hist(MT2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Mato Grosso no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_MT2017, ", desvio padrão:", DPch_MT2017, ", coeficiente de variação:", CVch_MT2017, ", primeiro quartil:", Q1ch_MT2017, ", segundo quartil:", Q2ch_MT2017, ", terceiro quartil:", Q3ch_MT2017)

# Mato Grosso do Sul

MS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MS') 
# Média:
mediach_ms2017 = mean(MS2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ms2017 = sd(MS2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ms2017 = cv(MS2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ms2017 <- quantile(MS2017$NU_NOTA_CH, probs = 0.25)
Q2ch_ms2017 <- quantile(MS2017$NU_NOTA_CH, probs = 0.50)
Q3ch_ms2017 <- quantile(MS2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ms2017 = hist(MS2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Mato Grosso do Sul no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_ms2017, ", desvio padrão:", DPch_ms2017, ", coeficiente de variação:", CVch_ms2017, ", primeiro quartil:", Q1ch_ms2017, ", segundo quartil:", Q2ch_ms2017, ", terceiro quartil:", Q3ch_ms2017)

# Minas Gerais

MG2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MG') 
# Média:
mediach_mg2017 = mean(MG2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_mg2017 = sd(MG2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_mg2017 = cv(MG2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_mg2017 <- quantile(MG2017$NU_NOTA_CH, probs = 0.25)
Q2ch_mg2017 <- quantile(MG2017$NU_NOTA_CH, probs = 0.50)
Q3ch_mg2017 <- quantile(MG2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_mg2017 = hist(MG2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Minas Gerais no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_mg2017, ", desvio padrão:", DPch_mg2017, ", coeficiente de variação:", CVch_mg2017, ", primeiro quartil:", Q1ch_mg2017, ", segundo quartil:", Q2ch_mg2017, ", terceiro quartil:", Q3ch_mg2017)

# Pará

PA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PA') 
# Média:
mediach_pa2017 = mean(PA2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pa2017 = sd(PA2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pa2017 = cv(PA2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pa2017 <- quantile(PA2017$NU_NOTA_CH, probs = 0.25)
Q2ch_pa2017 <- quantile(PA2017$NU_NOTA_CH, probs = 0.50)
Q3ch_pa2017 <- quantile(PA2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pa2017 = hist(PA2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Pará no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_pa2017, ", desvio padrão:", DPch_pa2017, ", coeficiente de variação:", CVch_pa2017, ", primeiro quartil:", Q1ch_pa2017, ", segundo quartil:", Q2ch_pa2017, ", terceiro quartil:", Q3ch_pa2017)

# Paraíba

PB2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PB') 
# Média:
mediach_pb2017 = mean(PB2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pb2017 = sd(PB2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pb2017 = cv(PB2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pb2017 <- quantile(PB2017$NU_NOTA_CH, probs = 0.25)
Q2ch_pb2017 <- quantile(PB2017$NU_NOTA_CH, probs = 0.50)
Q3ch_pb2017 <- quantile(PB2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pb2017 = hist(PB2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Paraíba no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_pb2017, ", desvio padrão:", DPch_pb2017, ", coeficiente de variação:", CVch_pb2017, ", primeiro quartil:", Q1ch_pb2017, ", segundo quartil:", Q2ch_pb2017, ", terceiro quartil:", Q3ch_pb2017)

# Paraná

PR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PR') 
# Média:
mediach_pr2017 = mean(PR2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pr2017 = sd(PR2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pr2017 = cv(PR2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pr2017 <- quantile(PR2017$NU_NOTA_CH, probs = 0.25)
Q2ch_pr2017 <- quantile(PR2017$NU_NOTA_CH, probs = 0.50)
Q3ch_pr2017 <- quantile(PR2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pr2017 = hist(PR2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Paraná no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_pr2017, ", desvio padrão:", DPch_pr2017, ", coeficiente de variação:", CVch_pr2017, ", primeiro quartil:", Q1ch_pr2017, ", segundo quartil:", Q2ch_pr2017, ", terceiro quartil:", Q3ch_pr2017)

# Pernambuco

PE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PE') 
# Média:
mediach_pe2017 = mean(PE2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pe2017 = sd(PE2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pe2017 = cv(PE2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pe2017 <- quantile(PE2017$NU_NOTA_CH, probs = 0.25)
Q2ch_pe2017 <- quantile(PE2017$NU_NOTA_CH, probs = 0.50)
Q3ch_pe2017 <- quantile(PE2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pe2017 = hist(PE2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Pernambuco no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_pe2017, ", desvio padrão:", DPch_pe2017, ", coeficiente de variação:", CVch_pe2017, ", peimeiro quartil:", Q1ch_pe2017, ", segundo quartil:", Q2ch_pe2017, ", terceiro quartil:", Q3ch_pe2017)

# Piauí

PI2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PI') 
# Média:
mediach_pi2017 = mean(PI2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_pi2017 = sd(PI2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_pi2017 = cv(PI2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_pi2017 <- quantile(PI2017$NU_NOTA_CH, probs = 0.25)
Q2ch_pi2017 <- quantile(PI2017$NU_NOTA_CH, probs = 0.50)
Q3ch_pi2017 <- quantile(PI2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_pi2017 = hist(PI2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Piauí no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_pi2017, ", desvio padrão:", DPch_pi2017, ", coeficiente de variação:", CVch_pi2017, ", primeiro quartil:", Q1ch_pi2017, ", segundo quartil:", Q2ch_pi2017, ", terceiro quartil:", Q3ch_pi2017)

# Rio de Janeiro

RJ2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediach_rj2017 = mean(RJ2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rj2017 = sd(RJ2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rj2017 = cv(RJ2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rj2017 <- quantile(RJ2017$NU_NOTA_CH, probs = 0.25)
Q2ch_rj2017 <- quantile(RJ2017$NU_NOTA_CH, probs = 0.50)
Q3ch_rj2017 <- quantile(RJ2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rj2017 = hist(RJ2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Rio de Janeiro no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_rj2017, ", desvio padrão:", DPch_rj2017, ", coeficiente de variação:", CVch_rj2017, ", primeiro quartil:", Q1ch_rj2017, ", segundo quartil:", Q2ch_rj2017, ", terceiro quartil:", Q3ch_rj2017)

# Rio Grande do Norte

RN2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RN') 
# Média:
mediach_rn2017 = mean(RN2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rn2017 = sd(RN2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rn2017 = cv(RN2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rn2017 <- quantile(RN2017$NU_NOTA_CH, probs = 0.25)
Q2ch_rn2017 <- quantile(RN2017$NU_NOTA_CH, probs = 0.50)
Q3ch_rn2017 <- quantile(RN2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rn2017 = hist(RN2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas da Rio Grande do Norte no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_rn2017, ", desvio padrão:", DPch_rn2017, ", coeficiente de variação:", CVch_rn2017, ", primeiro quartil:", Q1ch_rn2017, ", segundo quartil:", Q2ch_rn2017, ", terceiro quartil:", Q3ch_rn2017)

# Rio Grande do Sul

RS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RS') 
# Média:
mediach_rs2017 = mean(RS2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rs2017 = sd(RS2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rs2017 = cv(RS2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rs2017 <- quantile(RS2017$NU_NOTA_CH, probs = 0.25)
Q2ch_rs2017 <- quantile(RS2017$NU_NOTA_CH, probs = 0.50)
Q3ch_rs2017 <- quantile(RS2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rs2017 = hist(RS2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas do Rio Grande do Sul no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_rs2017, ", desvio padrão:", DPch_rs2017, ", coeficiente de variação:", CVch_rs2017, ", primeiro quartil:", Q1ch_rs2017, ", segundo quartil:", Q2ch_rs2017, ", terceira quartil:", Q3ch_rs2017)

# Rondônia

RO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RO') 
# Média:
mediach_ro2017 = mean(RO2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_ro2017 = sd(RO2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_ro2017 = cv(RO2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_ro2017 <- quantile(RO2017$NU_NOTA_CH, probs = 0.25)
Q2ch_ro2017 <- quantile(RO2017$NU_NOTA_CH, probs = 0.50)
Q3ch_ro2017 <- quantile(RO2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_ro2017 = hist(RO2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Rondônia no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_ro2017, ", desvio padrão:", DPch_ro2017, ", coeficiente de variação:", CVch_ro2017, ", primeiro quartil:", Q1ch_ro2017, ", segundo quartil:", Q2ch_ro2017, ", terceiro quartil:", Q3ch_ro2017)

# Roraima

RR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RR') 
# Média:
mediach_rr2017 = mean(RR2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_rr2017 = sd(RR2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_rr2017 = cv(RR2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_rr2017 <- quantile(RR2017$NU_NOTA_CH, probs = 0.25)
Q2ch_rr2017 <- quantile(RR2017$NU_NOTA_CH, probs = 0.50)
Q3ch_rr2017 <- quantile(RR2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_rr2017 = hist(RR2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Roraima no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_rr2017, ", desvio padrão:", DPch_rr2017, ", coeficiente de variação:", CVch_rr2017, ", primeiro quartil:", Q1ch_rr2017, ", segundo quartil:", Q2ch_rr2017, ", terceiro quartil:", Q3ch_rr2017)

# Santa Catarina

SC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SC') 
# Média:
mediach_sc2017 = mean(SC2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_sc2017 = sd(SC2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_sc2017 = cv(SC2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_sc2017 <- quantile(SC2017$NU_NOTA_CH, probs = 0.25)
Q2ch_sc2017 <- quantile(SC2017$NU_NOTA_CH, probs = 0.50)
Q3ch_sc2017 <- quantile(SC2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_sc2017 = hist(SC2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Santa Catarina  no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_sc2017, ", desvio padrão:", DPch_sc2017, ", coeficiente de variação:", CVch_sc2017, ", primeiro quartil:", Q1ch_sc2017, ", segundo quartil:", Q2ch_sc2017, ", terceiro quartil:", Q3ch_sc2017)

# São Paulo

SP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SP') 
# Média:
mediach_sp2017 = mean(SP2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_sp2017 = sd(SP2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_sp2017 = cv(SP2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_sp2017 <- quantile(SP2017$NU_NOTA_CH, probs = 0.25)
Q2ch_sp2017 <- quantile(SP2017$NU_NOTA_CH, probs = 0.50)
Q3ch_sp2017 <- quantile(SP2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_sp2017 = hist(SP2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de São Paulo  no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_sp2017, ", desvio padrão:", DPch_sp2017, ", coeficiente de variação:", CVch_sp2017, ", primeiro quartil:", Q1ch_sp2017, ", segundo quartil:", Q2ch_sp2017, ", terceiro quartil:", Q3ch_sp2017)

# Sergipe

SE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SE') 
# Média:
mediach_se2017 = mean(SE2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_se2017 = sd(SE2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_se2017 = cv(SE2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_se2017 <- quantile(SE2017$NU_NOTA_CH, probs = 0.25)
Q2ch_se2017 <- quantile(SE2017$NU_NOTA_CH, probs = 0.50)
Q3ch_se2017 <- quantile(SE2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_se2017 = hist(SE2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Sergipe no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_se2017, ", desvio padrão:", DPch_se2017, ", coeficiente de variação:", CVch_se2017, ", primeiro quartil:", Q1ch_se2017, ", segundo quartil:", Q2ch_se2017, ", terceiro quartil:", Q3ch_se2017)

# Tocantins

TO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='TO') 
# Média:
mediach_to2017 = mean(TO2017$NU_NOTA_CH) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPch_to2017 = sd(TO2017$NU_NOTA_CH)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVch_to2017 = cv(TO2017$NU_NOTA_CH)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1ch_to2017 <- quantile(TO2017$NU_NOTA_CH, probs = 0.25)
Q2ch_to2017 <- quantile(TO2017$NU_NOTA_CH, probs = 0.50)
Q3ch_to2017 <- quantile(TO2017$NU_NOTA_CH, probs = 0.75)
# Histograma: 
histogramach_to2017 = hist(TO2017$NU_NOTA_CH,  
                           main = "Nota de Ciências Humanas de Tocantins no Enem 2017",
                           xlab = "Nota de Ciências Humanas", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de Ciências Humanas do Enem 2017 --> média:", mediach_to2017, ", desvio padrão:", DPch_to2017, ", coeficiente de variação:", CVch_to2017, ", primeiro quartil:", Q1ch_to2017, ", segundo quartil:", Q2ch_to2017, ", terceiro quartil:", Q3ch_to2017)

# ------------ NU_Nota_CN

#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
library(dplyr)

# Acre

AC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediaCN_ac2019 = mean(AC2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ac2019 = sd(AC2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ac2019 = cv(AC2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ac2019 <- quantile(AC2019$NU_NOTA_CN, probs = 0.25)
Q2CN_ac2019 <- quantile(AC2019$NU_NOTA_CN, probs = 0.50)
Q3CN_ac2019 <- quantile(AC2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ac2019 = hist(AC2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Acre no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_ac2019, ", desvio padrão:", DPCN_ac2019, ", coeficiente de variação:", CVCN_ac2019, ", primeiro quartil:", Q1CN_ac2019, ", segundo quartil:", Q2CN_ac2019, ", terceiro quartil:", Q3CN_ac2019)

# Alagoas

AL2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AL')
# Média:
mediaCN_al2019 = mean(AL2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_al2019 = sd(AL2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_al2019 = cv(AL2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_al2019 <- quantile(AL2019$NU_NOTA_CN, probs = 0.25)
Q2CN_al2019 <- quantile(AL2019$NU_NOTA_CN, probs = 0.50)
Q3CN_al2019 <- quantile(AL2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_al2019 = hist(AL2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Alagoas no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_al2019, ", desvio padrão:", DPCN_al2019, ", coeficiente de variação:", CVCN_al2019, ", primeiro quartil:", Q1CN_al2019, ", segundo quartil:", Q2CN_al2019, ", terceiro quartil:", Q3CN_al2019)

# Amapá

AP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AP') 
# Média:
mediaCN_ap2019 = mean(AP2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ap2019 = sd(AP2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ap2019 = cv(AP2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ap2019 <- quantile(AP2019$NU_NOTA_CN, probs = 0.25)
Q2CN_ap2019 <- quantile(AP2019$NU_NOTA_CN, probs = 0.50)
Q3CN_ap2019 <- quantile(AP2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ap2019 = hist(AP2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Amapá no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_ap2019, ", desvio padrão:", DPCN_ap2019, ", coeficiente de variação:", CVCN_ap2019, ", primeiro quartil:", Q1CN_ap2019, ", segundo quartil:", Q2CN_ap2019, ", terceiro quartil:", Q3CN_ap2019)

# Amazonas

AM2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AM') 
# Média:
mediaCN_am2019 = mean(AM2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_am2019 = sd(AM2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_am2019 = cv(AM2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_am2019 <- quantile(AM2019$NU_NOTA_CN, probs = 0.25)
Q2CN_am2019 <- quantile(AM2019$NU_NOTA_CN, probs = 0.50)
Q3CN_am2019 <- quantile(AM2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_am2019 = hist(AM2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Amazonas no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_am2019, ", desvio padrão:", DPCN_am2019, ", coeficiente de variação:", CVCN_am2019, ", primeiro quartil:", Q1CN_am2019, ", segundo quartil:", Q2CN_am2019, ", terceiro quartil:", Q3CN_am2019)

# Bahia

BA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='BA') 
# Média:
mediaCN_ba2019 = mean(BA2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ba2019 = sd(BA2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ba2019 = cv(BA2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ba2019 <- quantile(BA2019$NU_NOTA_CN, probs = 0.25)
Q2CN_ba2019 <- quantile(BA2019$NU_NOTA_CN, probs = 0.50)
Q3CN_ba2019 <- quantile(BA2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ba2019 = hist(BA2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Bahia no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_ba2019, ", desvio padrão:", DPCN_ba2019, ", coeficiente de variação:", CVCN_ba2019, ", primeiro quartil:", Q1CN_ba2019, ", segundo quartil:", Q2CN_ba2019, ", terceiro quartil:", Q3CN_ba2019)

# Ceará

CE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='CE') 
# Média:
mediaCN_ce2019 = mean(CE2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ce2019 = sd(CE2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ce2019 = cv(CE2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ce2019 <- quantile(CE2019$NU_NOTA_CN, probs = 0.25)
Q2CN_ce2019 <- quantile(CE2019$NU_NOTA_CN, probs = 0.50)
Q3CN_ce2019 <- quantile(CE2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ce2019 = hist(CE2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Ceará no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_ce2019, ", desvio padrão:", DPCN_ce2019, ", coeficiente de variação:", CVCN_ce2019, ", primeiro quartil:", Q1CN_ce2019, ", segundo quartil:", Q2CN_ce2019, ", terceiro quartil:", Q3CN_ce2019)

# Distrito Federal

DF2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='DF') 
# Média:
mediaCN_df2019 = mean(DF2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_df2019 = sd(DF2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_df2019 = cv(DF2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_df2019 <- quantile(DF2019$NU_NOTA_CN, probs = 0.25)
Q2CN_df2019 <- quantile(DF2019$NU_NOTA_CN, probs = 0.50)
Q3CN_df2019 <- quantile(DF2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_df2019 = hist(DF2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Distrito Federal no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_df2019, ", desvio padrão:", DPCN_df2019, ", coeficiente de variação:", CVCN_df2019, ", primeiro quartil:", Q1CN_df2019, ", segundo quartil:", Q2CN_df2019, ", terceiro quartil:", Q3CN_df2019)

# Espírito Santo

ES2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='ES') 
# Média:
mediaCN_es2019 = mean(ES2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_es2019 = sd(ES2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_es2019 = cv(ES2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_es2019 <- quantile(ES2019$NU_NOTA_CN, probs = 0.25)
Q2CN_es2019 <- quantile(ES2019$NU_NOTA_CN, probs = 0.50)
Q3CN_es2019 <- quantile(ES2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_es2019 = hist(ES2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Espírito Santo no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_es2019, ", desvio padrão:", DPCN_es2019, ", coeficiente de variação:", CVCN_es2019, ", primeiro quartil:", Q1CN_es2019, ", segundo quartil:", Q2CN_es2019, ", terceiro quartil:", Q3CN_es2019)

# Goiás

GO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='GO') 
# Média:
mediaCN_go2019 = mean(GO2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_go2019 = sd(GO2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_go2019 = cv(GO2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_go2019 <- quantile(GO2019$NU_NOTA_CN, probs = 0.25)
Q2CN_go2019 <- quantile(GO2019$NU_NOTA_CN, probs = 0.50)
Q3CN_go2019 <- quantile(GO2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_go2019 = hist(GO2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Goiás no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_go2019, ", desvio padrão:", DPCN_go2019, ", coeficiente de variação:", CVCN_go2019, ", primeiro quartil:", Q1CN_go2019, ", segundo quartil:", Q2CN_go2019, ", terceiro quartil:", Q3CN_go2019)

# Maranhão

MA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MA') 
# Média:
mediaCN_ma2019 = mean(MA2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ma2019 = sd(MA2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ma2019 = cv(MA2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ma2019 <- quantile(MA2019$NU_NOTA_CN, probs = 0.25)
Q2CN_ma2019 <- quantile(MA2019$NU_NOTA_CN, probs = 0.50)
Q3CN_ma2019 <- quantile(MA2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ma2019 = hist(MA2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Maranhão no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_ma2019, ", desvio padrão:", DPCN_ma2019, ", coeficiente de variação:", CVCN_ma2019, ", primeiro quartil:", Q1CN_ma2019, ", segundo quartil:", Q2CN_ma2019, ", terceiro quartil:", Q3CN_ma2019)

# Mato Grosso

MT2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MT') 
# Média:
mediaCN_MT2019 = mean(MT2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_MT2019 = sd(MT2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_MT2019 = cv(MT2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_MT2019 <- quantile(MT2019$NU_NOTA_CN, probs = 0.25)
Q2CN_MT2019 <- quantile(MT2019$NU_NOTA_CN, probs = 0.50)
Q3CN_MT2019 <- quantile(MT2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_MT2019 = hist(MT2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Mato Grosso no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_MT2019, ", desvio padrão:", DPCN_MT2019, ", coeficiente de variação:", CVCN_MT2019, ", primeiro quartil:", Q1CN_MT2019, ", segundo quartil:", Q2CN_MT2019, ", terceiro quartil:", Q3CN_MT2019)

# Mato Grosso do Sul

MS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MS') 
# Média:
mediaCN_ms2019 = mean(MS2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ms2019 = sd(MS2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ms2019 = cv(MS2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ms2019 <- quantile(MS2019$NU_NOTA_CN, probs = 0.25)
Q2CN_ms2019 <- quantile(MS2019$NU_NOTA_CN, probs = 0.50)
Q3CN_ms2019 <- quantile(MS2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ms2019 = hist(MS2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Mato Grosso do Sul no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_ms2019, ", desvio padrão:", DPCN_ms2019, ", coeficiente de variação:", CVCN_ms2019, ", primeiro quartil:", Q1CN_ms2019, ", segundo quartil:", Q2CN_ms2019, ", terceiro quartil:", Q3CN_ms2019)

# Minas Gerais

MG2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MG') 
# Média:
mediaCN_mg2019 = mean(MG2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_mg2019 = sd(MG2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_mg2019 = cv(MG2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_mg2019 <- quantile(MG2019$NU_NOTA_CN, probs = 0.25)
Q2CN_mg2019 <- quantile(MG2019$NU_NOTA_CN, probs = 0.50)
Q3CN_mg2019 <- quantile(MG2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_mg2019 = hist(MG2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Minas Gerais no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_mg2019, ", desvio padrão:", DPCN_mg2019, ", coeficiente de variação:", CVCN_mg2019, ", primeiro quartil:", Q1CN_mg2019, ", segundo quartil:", Q2CN_mg2019, ", terceiro quartil:", Q3CN_mg2019)

# Pará

PA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PA') 
# Média:
mediaCN_pa2019 = mean(PA2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pa2019 = sd(PA2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pa2019 = cv(PA2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pa2019 <- quantile(PA2019$NU_NOTA_CN, probs = 0.25)
Q2CN_pa2019 <- quantile(PA2019$NU_NOTA_CN, probs = 0.50)
Q3CN_pa2019 <- quantile(PA2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pa2019 = hist(PA2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Pará no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_pa2019, ", desvio padrão:", DPCN_pa2019, ", coeficiente de variação:", CVCN_pa2019, ", primeiro quartil:", Q1CN_pa2019, ", segundo quartil:", Q2CN_pa2019, ", terceiro quartil:", Q3CN_pa2019)

# Paraíba

PB2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PB') 
# Média:
mediaCN_pb2019 = mean(PB2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pb2019 = sd(PB2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pb2019 = cv(PB2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pb2019 <- quantile(PB2019$NU_NOTA_CN, probs = 0.25)
Q2CN_pb2019 <- quantile(PB2019$NU_NOTA_CN, probs = 0.50)
Q3CN_pb2019 <- quantile(PB2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pb2019 = hist(PB2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Paraíba no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_pb2019, ", desvio padrão:", DPCN_pb2019, ", coeficiente de variação:", CVCN_pb2019, ", primeiro quartil:", Q1CN_pb2019, ", segundo quartil:", Q2CN_pb2019, ", terceiro quartil:", Q3CN_pb2019)

# Paraná

PR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PR') 
# Média:
mediaCN_pr2019 = mean(PR2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pr2019 = sd(PR2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pr2019 = cv(PR2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pr2019 <- quantile(PR2019$NU_NOTA_CN, probs = 0.25)
Q2CN_pr2019 <- quantile(PR2019$NU_NOTA_CN, probs = 0.50)
Q3CN_pr2019 <- quantile(PR2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pr2019 = hist(PR2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Paraná no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_pr2019, ", desvio padrão:", DPCN_pr2019, ", coeficiente de variação:", CVCN_pr2019, ", primeiro quartil:", Q1CN_pr2019, ", segundo quartil:", Q2CN_pr2019, ", terceiro quartil:", Q3CN_pr2019)

# Pernambuco

PE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PE') 
# Média:
mediaCN_pe2019 = mean(PE2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pe2019 = sd(PE2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pe2019 = cv(PE2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pe2019 <- quantile(PE2019$NU_NOTA_CN, probs = 0.25)
Q2CN_pe2019 <- quantile(PE2019$NU_NOTA_CN, probs = 0.50)
Q3CN_pe2019 <- quantile(PE2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pe2019 = hist(PE2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Pernambuco no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_pe2019, ", desvio padrão:", DPCN_pe2019, ", coeficiente de variação:", CVCN_pe2019, ", peimeiro quartil:", Q1CN_pe2019, ", segundo quartil:", Q2CN_pe2019, ", terceiro quartil:", Q3CN_pe2019)

# Piauí

PI2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PI') 
# Média:
mediaCN_pi2019 = mean(PI2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pi2019 = sd(PI2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pi2019 = cv(PI2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pi2019 <- quantile(PI2019$NU_NOTA_CN, probs = 0.25)
Q2CN_pi2019 <- quantile(PI2019$NU_NOTA_CN, probs = 0.50)
Q3CN_pi2019 <- quantile(PI2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pi2019 = hist(PI2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Piauí no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_pi2019, ", desvio padrão:", DPCN_pi2019, ", coeficiente de variação:", CVCN_pi2019, ", primeiro quartil:", Q1CN_pi2019, ", segundo quartil:", Q2CN_pi2019, ", terceiro quartil:", Q3CN_pi2019)

# Rio de Janeiro

RJ2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediaCN_rj2019 = mean(RJ2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rj2019 = sd(RJ2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rj2019 = cv(RJ2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rj2019 <- quantile(RJ2019$NU_NOTA_CN, probs = 0.25)
Q2CN_rj2019 <- quantile(RJ2019$NU_NOTA_CN, probs = 0.50)
Q3CN_rj2019 <- quantile(RJ2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rj2019 = hist(RJ2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Rio de Janeiro no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_rj2019, ", desvio padrão:", DPCN_rj2019, ", coeficiente de variação:", CVCN_rj2019, ", primeiro quartil:", Q1CN_rj2019, ", segundo quartil:", Q2CN_rj2019, ", terceiro quartil:", Q3CN_rj2019)

# Rio Grande do Norte

RN2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RN') 
# Média:
mediaCN_rn2019 = mean(RN2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rn2019 = sd(RN2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rn2019 = cv(RN2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rn2019 <- quantile(RN2019$NU_NOTA_CN, probs = 0.25)
Q2CN_rn2019 <- quantile(RN2019$NU_NOTA_CN, probs = 0.50)
Q3CN_rn2019 <- quantile(RN2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rn2019 = hist(RN2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Rio Grande do Norte no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_rn2019, ", desvio padrão:", DPCN_rn2019, ", coeficiente de variação:", CVCN_rn2019, ", primeiro quartil:", Q1CN_rn2019, ", segundo quartil:", Q2CN_rn2019, ", terceiro quartil:", Q3CN_rn2019)

# Rio Grande do Sul

RS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RS') 
# Média:
mediaCN_rs2019 = mean(RS2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rs2019 = sd(RS2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rs2019 = cv(RS2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rs2019 <- quantile(RS2019$NU_NOTA_CN, probs = 0.25)
Q2CN_rs2019 <- quantile(RS2019$NU_NOTA_CN, probs = 0.50)
Q3CN_rs2019 <- quantile(RS2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rs2019 = hist(RS2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Rio Grande do Sul no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_rs2019, ", desvio padrão:", DPCN_rs2019, ", coeficiente de variação:", CVCN_rs2019, ", primeiro quartil:", Q1CN_rs2019, ", segundo quartil:", Q2CN_rs2019, ", terceira quartil:", Q3CN_rs2019)

# Rondônia

RO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RO') 
# Média:
mediaCN_ro2019 = mean(RO2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ro2019 = sd(RO2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ro2019 = cv(RO2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ro2019 <- quantile(RO2019$NU_NOTA_CN, probs = 0.25)
Q2CN_ro2019 <- quantile(RO2019$NU_NOTA_CN, probs = 0.50)
Q3CN_ro2019 <- quantile(RO2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ro2019 = hist(RO2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Rondônia no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_ro2019, ", desvio padrão:", DPCN_ro2019, ", coeficiente de variação:", CVCN_ro2019, ", primeiro quartil:", Q1CN_ro2019, ", segundo quartil:", Q2CN_ro2019, ", terceiro quartil:", Q3CN_ro2019)

# Roraima

RR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RR') 
# Média:
mediaCN_rr2019 = mean(RR2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rr2019 = sd(RR2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rr2019 = cv(RR2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rr2019 <- quantile(RR2019$NU_NOTA_CN, probs = 0.25)
Q2CN_rr2019 <- quantile(RR2019$NU_NOTA_CN, probs = 0.50)
Q3CN_rr2019 <- quantile(RR2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rr2019 = hist(RR2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Roraima no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_rr2019, ", desvio padrão:", DPCN_rr2019, ", coeficiente de variação:", CVCN_rr2019, ", primeiro quartil:", Q1CN_rr2019, ", segundo quartil:", Q2CN_rr2019, ", terceiro quartil:", Q3CN_rr2019)

# Santa Catarina

SC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SC') 
# Média:
mediaCN_sc2019 = mean(SC2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_sc2019 = sd(SC2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_sc2019 = cv(SC2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_sc2019 <- quantile(SC2019$NU_NOTA_CN, probs = 0.25)
Q2CN_sc2019 <- quantile(SC2019$NU_NOTA_CN, probs = 0.50)
Q3CN_sc2019 <- quantile(SC2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_sc2019 = hist(SC2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Santa Catarina  no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_sc2019, ", desvio padrão:", DPCN_sc2019, ", coeficiente de variação:", CVCN_sc2019, ", primeiro quartil:", Q1CN_sc2019, ", segundo quartil:", Q2CN_sc2019, ", terceiro quartil:", Q3CN_sc2019)

# São Paulo

SP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SP') 
# Média:
mediaCN_sp2019 = mean(SP2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_sp2019 = sd(SP2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_sp2019 = cv(SP2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_sp2019 <- quantile(SP2019$NU_NOTA_CN, probs = 0.25)
Q2CN_sp2019 <- quantile(SP2019$NU_NOTA_CN, probs = 0.50)
Q3CN_sp2019 <- quantile(SP2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_sp2019 = hist(SP2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de São Paulo  no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_sp2019, ", desvio padrão:", DPCN_sp2019, ", coeficiente de variação:", CVCN_sp2019, ", primeiro quartil:", Q1CN_sp2019, ", segundo quartil:", Q2CN_sp2019, ", terceiro quartil:", Q3CN_sp2019)

# Sergipe

SE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SE') 
# Média:
mediaCN_se2019 = mean(SE2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_se2019 = sd(SE2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_se2019 = cv(SE2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_se2019 <- quantile(SE2019$NU_NOTA_CN, probs = 0.25)
Q2CN_se2019 <- quantile(SE2019$NU_NOTA_CN, probs = 0.50)
Q3CN_se2019 <- quantile(SE2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_se2019 = hist(SE2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Sergipe no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_se2019, ", desvio padrão:", DPCN_se2019, ", coeficiente de variação:", CVCN_se2019, ", primeiro quartil:", Q1CN_se2019, ", segundo quartil:", Q2CN_se2019, ", terceiro quartil:", Q3CN_se2019)

# Tocantins

TO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='TO') 
# Média:
mediaCN_to2019 = mean(TO2019$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_to2019 = sd(TO2019$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_to2019 = cv(TO2019$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_to2019 <- quantile(TO2019$NU_NOTA_CN, probs = 0.25)
Q2CN_to2019 <- quantile(TO2019$NU_NOTA_CN, probs = 0.50)
Q3CN_to2019 <- quantile(TO2019$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_to2019 = hist(TO2019$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Tocantins no Enem 2019",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de Ciências da Natureza do Enem 2019 --> média:", mediaCN_to2019, ", desvio padrão:", DPCN_to2019, ", coeficiente de variação:", CVCN_to2019, ", primeiro quartil:", Q1CN_to2019, ", segundo quartil:", Q2CN_to2019, ", terceiro quartil:", Q3CN_to2019)


#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////

# Acre

AC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediaCN_ac2018 = mean(AC2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ac2018 = sd(AC2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ac2018 = cv(AC2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ac2018 <- quantile(AC2018$NU_NOTA_CN, probs = 0.25)
Q2CN_ac2018 <- quantile(AC2018$NU_NOTA_CN, probs = 0.50)
Q3CN_ac2018 <- quantile(AC2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ac2018 = hist(AC2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Acre no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_ac2018, ", desvio padrão:", DPCN_ac2018, ", coeficiente de variação:", CVCN_ac2018, ", primeiro quartil:", Q1CN_ac2018, ", segundo quartil:", Q2CN_ac2018, ", terceiro quartil:", Q3CN_ac2018)

# Alagoas

AL2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AL')
# Média:
mediaCN_al2018 = mean(AL2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_al2018 = sd(AL2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_al2018 = cv(AL2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_al2018 <- quantile(AL2018$NU_NOTA_CN, probs = 0.25)
Q2CN_al2018 <- quantile(AL2018$NU_NOTA_CN, probs = 0.50)
Q3CN_al2018 <- quantile(AL2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_al2018 = hist(AL2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Alagoas no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_al2018, ", desvio padrão:", DPCN_al2018, ", coeficiente de variação:", CVCN_al2018, ", primeiro quartil:", Q1CN_al2018, ", segundo quartil:", Q2CN_al2018, ", terceiro quartil:", Q3CN_al2018)

# Amapá

AP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AP') 
# Média:
mediaCN_ap2018 = mean(AP2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ap2018 = sd(AP2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ap2018 = cv(AP2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ap2018 <- quantile(AP2018$NU_NOTA_CN, probs = 0.25)
Q2CN_ap2018 <- quantile(AP2018$NU_NOTA_CN, probs = 0.50)
Q3CN_ap2018 <- quantile(AP2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ap2018 = hist(AP2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Amapá no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_ap2018, ", desvio padrão:", DPCN_ap2018, ", coeficiente de variação:", CVCN_ap2018, ", primeiro quartil:", Q1CN_ap2018, ", segundo quartil:", Q2CN_ap2018, ", terceiro quartil:", Q3CN_ap2018)

# Amazonas

AM2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AM') 
# Média:
mediaCN_am2018 = mean(AM2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_am2018 = sd(AM2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_am2018 = cv(AM2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_am2018 <- quantile(AM2018$NU_NOTA_CN, probs = 0.25)
Q2CN_am2018 <- quantile(AM2018$NU_NOTA_CN, probs = 0.50)
Q3CN_am2018 <- quantile(AM2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_am2018 = hist(AM2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Amazonas no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_am2018, ", desvio padrão:", DPCN_am2018, ", coeficiente de variação:", CVCN_am2018, ", primeiro quartil:", Q1CN_am2018, ", segundo quartil:", Q2CN_am2018, ", terceiro quartil:", Q3CN_am2018)

# Bahia

BA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='BA') 
# Média:
mediaCN_ba2018 = mean(BA2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ba2018 = sd(BA2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ba2018 = cv(BA2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ba2018 <- quantile(BA2018$NU_NOTA_CN, probs = 0.25)
Q2CN_ba2018 <- quantile(BA2018$NU_NOTA_CN, probs = 0.50)
Q3CN_ba2018 <- quantile(BA2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ba2018 = hist(BA2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Bahia no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_ba2018, ", desvio padrão:", DPCN_ba2018, ", coeficiente de variação:", CVCN_ba2018, ", primeiro quartil:", Q1CN_ba2018, ", segundo quartil:", Q2CN_ba2018, ", terceiro quartil:", Q3CN_ba2018)

# Ceará

CE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='CE') 
# Média:
mediaCN_ce2018 = mean(CE2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ce2018 = sd(CE2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ce2018 = cv(CE2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ce2018 <- quantile(CE2018$NU_NOTA_CN, probs = 0.25)
Q2CN_ce2018 <- quantile(CE2018$NU_NOTA_CN, probs = 0.50)
Q3CN_ce2018 <- quantile(CE2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ce2018 = hist(CE2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Ceará no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_ce2018, ", desvio padrão:", DPCN_ce2018, ", coeficiente de variação:", CVCN_ce2018, ", primeiro quartil:", Q1CN_ce2018, ", segundo quartil:", Q2CN_ce2018, ", terceiro quartil:", Q3CN_ce2018)

# Distrito Federal

DF2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='DF') 
# Média:
mediaCN_df2018 = mean(DF2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_df2018 = sd(DF2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_df2018 = cv(DF2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_df2018 <- quantile(DF2018$NU_NOTA_CN, probs = 0.25)
Q2CN_df2018 <- quantile(DF2018$NU_NOTA_CN, probs = 0.50)
Q3CN_df2018 <- quantile(DF2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_df2018 = hist(DF2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Distrito Federal no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_df2018, ", desvio padrão:", DPCN_df2018, ", coeficiente de variação:", CVCN_df2018, ", primeiro quartil:", Q1CN_df2018, ", segundo quartil:", Q2CN_df2018, ", terceiro quartil:", Q3CN_df2018)

# Espírito Santo

ES2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='ES') 
# Média:
mediaCN_es2018 = mean(ES2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_es2018 = sd(ES2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_es2018 = cv(ES2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_es2018 <- quantile(ES2018$NU_NOTA_CN, probs = 0.25)
Q2CN_es2018 <- quantile(ES2018$NU_NOTA_CN, probs = 0.50)
Q3CN_es2018 <- quantile(ES2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_es2018 = hist(ES2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Espírito Santo no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_es2018, ", desvio padrão:", DPCN_es2018, ", coeficiente de variação:", CVCN_es2018, ", primeiro quartil:", Q1CN_es2018, ", segundo quartil:", Q2CN_es2018, ", terceiro quartil:", Q3CN_es2018)

# Goiás

GO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='GO') 
# Média:
mediaCN_go2018 = mean(GO2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_go2018 = sd(GO2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_go2018 = cv(GO2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_go2018 <- quantile(GO2018$NU_NOTA_CN, probs = 0.25)
Q2CN_go2018 <- quantile(GO2018$NU_NOTA_CN, probs = 0.50)
Q3CN_go2018 <- quantile(GO2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_go2018 = hist(GO2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Goiás no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_go2018, ", desvio padrão:", DPCN_go2018, ", coeficiente de variação:", CVCN_go2018, ", primeiro quartil:", Q1CN_go2018, ", segundo quartil:", Q2CN_go2018, ", terceiro quartil:", Q3CN_go2018)

# Maranhão

MA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MA') 
# Média:
mediaCN_ma2018 = mean(MA2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ma2018 = sd(MA2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ma2018 = cv(MA2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ma2018 <- quantile(MA2018$NU_NOTA_CN, probs = 0.25)
Q2CN_ma2018 <- quantile(MA2018$NU_NOTA_CN, probs = 0.50)
Q3CN_ma2018 <- quantile(MA2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ma2018 = hist(MA2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Maranhão no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_ma2018, ", desvio padrão:", DPCN_ma2018, ", coeficiente de variação:", CVCN_ma2018, ", primeiro quartil:", Q1CN_ma2018, ", segundo quartil:", Q2CN_ma2018, ", terceiro quartil:", Q3CN_ma2018)

# Mato Grosso

MT2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MT') 
# Média:
mediaCN_MT2018 = mean(MT2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_MT2018 = sd(MT2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_MT2018 = cv(MT2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_MT2018 <- quantile(MT2018$NU_NOTA_CN, probs = 0.25)
Q2CN_MT2018 <- quantile(MT2018$NU_NOTA_CN, probs = 0.50)
Q3CN_MT2018 <- quantile(MT2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_MT2018 = hist(MT2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Mato Grosso no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_MT2018, ", desvio padrão:", DPCN_MT2018, ", coeficiente de variação:", CVCN_MT2018, ", primeiro quartil:", Q1CN_MT2018, ", segundo quartil:", Q2CN_MT2018, ", terceiro quartil:", Q3CN_MT2018)

# Mato Grosso do Sul

MS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MS') 
# Média:
mediaCN_ms2018 = mean(MS2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ms2018 = sd(MS2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ms2018 = cv(MS2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ms2018 <- quantile(MS2018$NU_NOTA_CN, probs = 0.25)
Q2CN_ms2018 <- quantile(MS2018$NU_NOTA_CN, probs = 0.50)
Q3CN_ms2018 <- quantile(MS2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ms2018 = hist(MS2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Mato Grosso do Sul no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_ms2018, ", desvio padrão:", DPCN_ms2018, ", coeficiente de variação:", CVCN_ms2018, ", primeiro quartil:", Q1CN_ms2018, ", segundo quartil:", Q2CN_ms2018, ", terceiro quartil:", Q3CN_ms2018)

# Minas Gerais

MG2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MG') 
# Média:
mediaCN_mg2018 = mean(MG2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_mg2018 = sd(MG2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_mg2018 = cv(MG2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_mg2018 <- quantile(MG2018$NU_NOTA_CN, probs = 0.25)
Q2CN_mg2018 <- quantile(MG2018$NU_NOTA_CN, probs = 0.50)
Q3CN_mg2018 <- quantile(MG2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_mg2018 = hist(MG2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Minas Gerais no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_mg2018, ", desvio padrão:", DPCN_mg2018, ", coeficiente de variação:", CVCN_mg2018, ", primeiro quartil:", Q1CN_mg2018, ", segundo quartil:", Q2CN_mg2018, ", terceiro quartil:", Q3CN_mg2018)

# Pará

PA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PA') 
# Média:
mediaCN_pa2018 = mean(PA2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pa2018 = sd(PA2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pa2018 = cv(PA2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pa2018 <- quantile(PA2018$NU_NOTA_CN, probs = 0.25)
Q2CN_pa2018 <- quantile(PA2018$NU_NOTA_CN, probs = 0.50)
Q3CN_pa2018 <- quantile(PA2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pa2018 = hist(PA2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Pará no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_pa2018, ", desvio padrão:", DPCN_pa2018, ", coeficiente de variação:", CVCN_pa2018, ", primeiro quartil:", Q1CN_pa2018, ", segundo quartil:", Q2CN_pa2018, ", terceiro quartil:", Q3CN_pa2018)

# Paraíba

PB2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PB') 
# Média:
mediaCN_pb2018 = mean(PB2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pb2018 = sd(PB2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pb2018 = cv(PB2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pb2018 <- quantile(PB2018$NU_NOTA_CN, probs = 0.25)
Q2CN_pb2018 <- quantile(PB2018$NU_NOTA_CN, probs = 0.50)
Q3CN_pb2018 <- quantile(PB2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pb2018 = hist(PB2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Paraíba no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_pb2018, ", desvio padrão:", DPCN_pb2018, ", coeficiente de variação:", CVCN_pb2018, ", primeiro quartil:", Q1CN_pb2018, ", segundo quartil:", Q2CN_pb2018, ", terceiro quartil:", Q3CN_pb2018)

# Paraná

PR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PR') 
# Média:
mediaCN_pr2018 = mean(PR2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pr2018 = sd(PR2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pr2018 = cv(PR2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pr2018 <- quantile(PR2018$NU_NOTA_CN, probs = 0.25)
Q2CN_pr2018 <- quantile(PR2018$NU_NOTA_CN, probs = 0.50)
Q3CN_pr2018 <- quantile(PR2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pr2018 = hist(PR2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Paraná no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_pr2018, ", desvio padrão:", DPCN_pr2018, ", coeficiente de variação:", CVCN_pr2018, ", primeiro quartil:", Q1CN_pr2018, ", segundo quartil:", Q2CN_pr2018, ", terceiro quartil:", Q3CN_pr2018)

# Pernambuco

PE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PE') 
# Média:
mediaCN_pe2018 = mean(PE2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pe2018 = sd(PE2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pe2018 = cv(PE2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pe2018 <- quantile(PE2018$NU_NOTA_CN, probs = 0.25)
Q2CN_pe2018 <- quantile(PE2018$NU_NOTA_CN, probs = 0.50)
Q3CN_pe2018 <- quantile(PE2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pe2018 = hist(PE2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Pernambuco no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_pe2018, ", desvio padrão:", DPCN_pe2018, ", coeficiente de variação:", CVCN_pe2018, ", peimeiro quartil:", Q1CN_pe2018, ", segundo quartil:", Q2CN_pe2018, ", terceiro quartil:", Q3CN_pe2018)

# Piauí

PI2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PI') 
# Média:
mediaCN_pi2018 = mean(PI2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pi2018 = sd(PI2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pi2018 = cv(PI2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pi2018 <- quantile(PI2018$NU_NOTA_CN, probs = 0.25)
Q2CN_pi2018 <- quantile(PI2018$NU_NOTA_CN, probs = 0.50)
Q3CN_pi2018 <- quantile(PI2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pi2018 = hist(PI2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Piauí no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_pi2018, ", desvio padrão:", DPCN_pi2018, ", coeficiente de variação:", CVCN_pi2018, ", primeiro quartil:", Q1CN_pi2018, ", segundo quartil:", Q2CN_pi2018, ", terceiro quartil:", Q3CN_pi2018)

# Rio de Janeiro

RJ2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediaCN_rj2018 = mean(RJ2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rj2018 = sd(RJ2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rj2018 = cv(RJ2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rj2018 <- quantile(RJ2018$NU_NOTA_CN, probs = 0.25)
Q2CN_rj2018 <- quantile(RJ2018$NU_NOTA_CN, probs = 0.50)
Q3CN_rj2018 <- quantile(RJ2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rj2018 = hist(RJ2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Rio de Janeiro no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_rj2018, ", desvio padrão:", DPCN_rj2018, ", coeficiente de variação:", CVCN_rj2018, ", primeiro quartil:", Q1CN_rj2018, ", segundo quartil:", Q2CN_rj2018, ", terceiro quartil:", Q3CN_rj2018)

# Rio Grande do Norte

RN2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RN') 
# Média:
mediaCN_rn2018 = mean(RN2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rn2018 = sd(RN2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rn2018 = cv(RN2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rn2018 <- quantile(RN2018$NU_NOTA_CN, probs = 0.25)
Q2CN_rn2018 <- quantile(RN2018$NU_NOTA_CN, probs = 0.50)
Q3CN_rn2018 <- quantile(RN2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rn2018 = hist(RN2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Rio Grande do Norte no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_rn2018, ", desvio padrão:", DPCN_rn2018, ", coeficiente de variação:", CVCN_rn2018, ", primeiro quartil:", Q1CN_rn2018, ", segundo quartil:", Q2CN_rn2018, ", terceiro quartil:", Q3CN_rn2018)

# Rio Grande do Sul

RS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RS') 
# Média:
mediaCN_rs2018 = mean(RS2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rs2018 = sd(RS2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rs2018 = cv(RS2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rs2018 <- quantile(RS2018$NU_NOTA_CN, probs = 0.25)
Q2CN_rs2018 <- quantile(RS2018$NU_NOTA_CN, probs = 0.50)
Q3CN_rs2018 <- quantile(RS2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rs2018 = hist(RS2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Rio Grande do Sul no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_rs2018, ", desvio padrão:", DPCN_rs2018, ", coeficiente de variação:", CVCN_rs2018, ", primeiro quartil:", Q1CN_rs2018, ", segundo quartil:", Q2CN_rs2018, ", terceira quartil:", Q3CN_rs2018)

# Rondônia

RO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RO') 
# Média:
mediaCN_ro2018 = mean(RO2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ro2018 = sd(RO2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ro2018 = cv(RO2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ro2018 <- quantile(RO2018$NU_NOTA_CN, probs = 0.25)
Q2CN_ro2018 <- quantile(RO2018$NU_NOTA_CN, probs = 0.50)
Q3CN_ro2018 <- quantile(RO2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ro2018 = hist(RO2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Rondônia no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_ro2018, ", desvio padrão:", DPCN_ro2018, ", coeficiente de variação:", CVCN_ro2018, ", primeiro quartil:", Q1CN_ro2018, ", segundo quartil:", Q2CN_ro2018, ", terceiro quartil:", Q3CN_ro2018)

# Roraima

RR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RR') 
# Média:
mediaCN_rr2018 = mean(RR2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rr2018 = sd(RR2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rr2018 = cv(RR2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rr2018 <- quantile(RR2018$NU_NOTA_CN, probs = 0.25)
Q2CN_rr2018 <- quantile(RR2018$NU_NOTA_CN, probs = 0.50)
Q3CN_rr2018 <- quantile(RR2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rr2018 = hist(RR2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Roraima no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_rr2018, ", desvio padrão:", DPCN_rr2018, ", coeficiente de variação:", CVCN_rr2018, ", primeiro quartil:", Q1CN_rr2018, ", segundo quartil:", Q2CN_rr2018, ", terceiro quartil:", Q3CN_rr2018)

# Santa Catarina

SC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SC') 
# Média:
mediaCN_sc2018 = mean(SC2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_sc2018 = sd(SC2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_sc2018 = cv(SC2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_sc2018 <- quantile(SC2018$NU_NOTA_CN, probs = 0.25)
Q2CN_sc2018 <- quantile(SC2018$NU_NOTA_CN, probs = 0.50)
Q3CN_sc2018 <- quantile(SC2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_sc2018 = hist(SC2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Santa Catarina  no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_sc2018, ", desvio padrão:", DPCN_sc2018, ", coeficiente de variação:", CVCN_sc2018, ", primeiro quartil:", Q1CN_sc2018, ", segundo quartil:", Q2CN_sc2018, ", terceiro quartil:", Q3CN_sc2018)

# São Paulo

SP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SP') 
# Média:
mediaCN_sp2018 = mean(SP2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_sp2018 = sd(SP2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_sp2018 = cv(SP2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_sp2018 <- quantile(SP2018$NU_NOTA_CN, probs = 0.25)
Q2CN_sp2018 <- quantile(SP2018$NU_NOTA_CN, probs = 0.50)
Q3CN_sp2018 <- quantile(SP2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_sp2018 = hist(SP2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de São Paulo  no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_sp2018, ", desvio padrão:", DPCN_sp2018, ", coeficiente de variação:", CVCN_sp2018, ", primeiro quartil:", Q1CN_sp2018, ", segundo quartil:", Q2CN_sp2018, ", terceiro quartil:", Q3CN_sp2018)

# Sergipe

SE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SE') 
# Média:
mediaCN_se2018 = mean(SE2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_se2018 = sd(SE2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_se2018 = cv(SE2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_se2018 <- quantile(SE2018$NU_NOTA_CN, probs = 0.25)
Q2CN_se2018 <- quantile(SE2018$NU_NOTA_CN, probs = 0.50)
Q3CN_se2018 <- quantile(SE2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_se2018 = hist(SE2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Sergipe no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_se2018, ", desvio padrão:", DPCN_se2018, ", coeficiente de variação:", CVCN_se2018, ", primeiro quartil:", Q1CN_se2018, ", segundo quartil:", Q2CN_se2018, ", terceiro quartil:", Q3CN_se2018)

# Tocantins

TO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='TO') 
# Média:
mediaCN_to2018 = mean(TO2018$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_to2018 = sd(TO2018$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_to2018 = cv(TO2018$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_to2018 <- quantile(TO2018$NU_NOTA_CN, probs = 0.25)
Q2CN_to2018 <- quantile(TO2018$NU_NOTA_CN, probs = 0.50)
Q3CN_to2018 <- quantile(TO2018$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_to2018 = hist(TO2018$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Tocantins no Enem 2018",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de Ciências da Natureza do Enem 2018 --> média:", mediaCN_to2018, ", desvio padrão:", DPCN_to2018, ", coeficiente de variação:", CVCN_to2018, ", primeiro quartil:", Q1CN_to2018, ", segundo quartil:", Q2CN_to2018, ", terceiro quartil:", Q3CN_to2018)

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////

# Acre

AC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediaCN_ac2017 = mean(AC2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ac2017 = sd(AC2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ac2017 = cv(AC2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ac2017 <- quantile(AC2017$NU_NOTA_CN, probs = 0.25)
Q2CN_ac2017 <- quantile(AC2017$NU_NOTA_CN, probs = 0.50)
Q3CN_ac2017 <- quantile(AC2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ac2017 = hist(AC2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Acre no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_ac2017, ", desvio padrão:", DPCN_ac2017, ", coeficiente de variação:", CVCN_ac2017, ", primeiro quartil:", Q1CN_ac2017, ", segundo quartil:", Q2CN_ac2017, ", terceiro quartil:", Q3CN_ac2017)

# Alagoas

AL2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AL')
# Média:
mediaCN_al2017 = mean(AL2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_al2017 = sd(AL2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_al2017 = cv(AL2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_al2017 <- quantile(AL2017$NU_NOTA_CN, probs = 0.25)
Q2CN_al2017 <- quantile(AL2017$NU_NOTA_CN, probs = 0.50)
Q3CN_al2017 <- quantile(AL2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_al2017 = hist(AL2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Alagoas no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_al2017, ", desvio padrão:", DPCN_al2017, ", coeficiente de variação:", CVCN_al2017, ", primeiro quartil:", Q1CN_al2017, ", segundo quartil:", Q2CN_al2017, ", terceiro quartil:", Q3CN_al2017)

# Amapá

AP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AP') 
# Média:
mediaCN_ap2017 = mean(AP2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ap2017 = sd(AP2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ap2017 = cv(AP2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ap2017 <- quantile(AP2017$NU_NOTA_CN, probs = 0.25)
Q2CN_ap2017 <- quantile(AP2017$NU_NOTA_CN, probs = 0.50)
Q3CN_ap2017 <- quantile(AP2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ap2017 = hist(AP2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Amapá no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_ap2017, ", desvio padrão:", DPCN_ap2017, ", coeficiente de variação:", CVCN_ap2017, ", primeiro quartil:", Q1CN_ap2017, ", segundo quartil:", Q2CN_ap2017, ", terceiro quartil:", Q3CN_ap2017)

# Amazonas

AM2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AM') 
# Média:
mediaCN_am2017 = mean(AM2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_am2017 = sd(AM2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_am2017 = cv(AM2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_am2017 <- quantile(AM2017$NU_NOTA_CN, probs = 0.25)
Q2CN_am2017 <- quantile(AM2017$NU_NOTA_CN, probs = 0.50)
Q3CN_am2017 <- quantile(AM2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_am2017 = hist(AM2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Amazonas no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_am2017, ", desvio padrão:", DPCN_am2017, ", coeficiente de variação:", CVCN_am2017, ", primeiro quartil:", Q1CN_am2017, ", segundo quartil:", Q2CN_am2017, ", terceiro quartil:", Q3CN_am2017)

# Bahia

BA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='BA') 
# Média:
mediaCN_ba2017 = mean(BA2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ba2017 = sd(BA2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ba2017 = cv(BA2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ba2017 <- quantile(BA2017$NU_NOTA_CN, probs = 0.25)
Q2CN_ba2017 <- quantile(BA2017$NU_NOTA_CN, probs = 0.50)
Q3CN_ba2017 <- quantile(BA2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ba2017 = hist(BA2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Bahia no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_ba2017, ", desvio padrão:", DPCN_ba2017, ", coeficiente de variação:", CVCN_ba2017, ", primeiro quartil:", Q1CN_ba2017, ", segundo quartil:", Q2CN_ba2017, ", terceiro quartil:", Q3CN_ba2017)

# Ceará

CE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='CE') 
# Média:
mediaCN_ce2017 = mean(CE2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ce2017 = sd(CE2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ce2017 = cv(CE2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ce2017 <- quantile(CE2017$NU_NOTA_CN, probs = 0.25)
Q2CN_ce2017 <- quantile(CE2017$NU_NOTA_CN, probs = 0.50)
Q3CN_ce2017 <- quantile(CE2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ce2017 = hist(CE2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Ceará no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_ce2017, ", desvio padrão:", DPCN_ce2017, ", coeficiente de variação:", CVCN_ce2017, ", primeiro quartil:", Q1CN_ce2017, ", segundo quartil:", Q2CN_ce2017, ", terceiro quartil:", Q3CN_ce2017)

# Distrito Federal

DF2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='DF') 
# Média:
mediaCN_df2017 = mean(DF2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_df2017 = sd(DF2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_df2017 = cv(DF2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_df2017 <- quantile(DF2017$NU_NOTA_CN, probs = 0.25)
Q2CN_df2017 <- quantile(DF2017$NU_NOTA_CN, probs = 0.50)
Q3CN_df2017 <- quantile(DF2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_df2017 = hist(DF2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Distrito Federal no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_df2017, ", desvio padrão:", DPCN_df2017, ", coeficiente de variação:", CVCN_df2017, ", primeiro quartil:", Q1CN_df2017, ", segundo quartil:", Q2CN_df2017, ", terceiro quartil:", Q3CN_df2017)

# Espírito Santo

ES2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='ES') 
# Média:
mediaCN_es2017 = mean(ES2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_es2017 = sd(ES2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_es2017 = cv(ES2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_es2017 <- quantile(ES2017$NU_NOTA_CN, probs = 0.25)
Q2CN_es2017 <- quantile(ES2017$NU_NOTA_CN, probs = 0.50)
Q3CN_es2017 <- quantile(ES2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_es2017 = hist(ES2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Espírito Santo no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_es2017, ", desvio padrão:", DPCN_es2017, ", coeficiente de variação:", CVCN_es2017, ", primeiro quartil:", Q1CN_es2017, ", segundo quartil:", Q2CN_es2017, ", terceiro quartil:", Q3CN_es2017)

# Goiás

GO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='GO') 
# Média:
mediaCN_go2017 = mean(GO2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_go2017 = sd(GO2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_go2017 = cv(GO2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_go2017 <- quantile(GO2017$NU_NOTA_CN, probs = 0.25)
Q2CN_go2017 <- quantile(GO2017$NU_NOTA_CN, probs = 0.50)
Q3CN_go2017 <- quantile(GO2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_go2017 = hist(GO2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Goiás no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_go2017, ", desvio padrão:", DPCN_go2017, ", coeficiente de variação:", CVCN_go2017, ", primeiro quartil:", Q1CN_go2017, ", segundo quartil:", Q2CN_go2017, ", terceiro quartil:", Q3CN_go2017)

# Maranhão

MA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MA') 
# Média:
mediaCN_ma2017 = mean(MA2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ma2017 = sd(MA2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ma2017 = cv(MA2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ma2017 <- quantile(MA2017$NU_NOTA_CN, probs = 0.25)
Q2CN_ma2017 <- quantile(MA2017$NU_NOTA_CN, probs = 0.50)
Q3CN_ma2017 <- quantile(MA2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ma2017 = hist(MA2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Maranhão no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_ma2017, ", desvio padrão:", DPCN_ma2017, ", coeficiente de variação:", CVCN_ma2017, ", primeiro quartil:", Q1CN_ma2017, ", segundo quartil:", Q2CN_ma2017, ", terceiro quartil:", Q3CN_ma2017)

# Mato Grosso

MT2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MT') 
# Média:
mediaCN_MT2017 = mean(MT2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_MT2017 = sd(MT2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_MT2017 = cv(MT2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_MT2017 <- quantile(MT2017$NU_NOTA_CN, probs = 0.25)
Q2CN_MT2017 <- quantile(MT2017$NU_NOTA_CN, probs = 0.50)
Q3CN_MT2017 <- quantile(MT2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_MT2017 = hist(MT2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Mato Grosso no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_MT2017, ", desvio padrão:", DPCN_MT2017, ", coeficiente de variação:", CVCN_MT2017, ", primeiro quartil:", Q1CN_MT2017, ", segundo quartil:", Q2CN_MT2017, ", terceiro quartil:", Q3CN_MT2017)

# Mato Grosso do Sul

MS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MS') 
# Média:
mediaCN_ms2017 = mean(MS2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ms2017 = sd(MS2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ms2017 = cv(MS2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ms2017 <- quantile(MS2017$NU_NOTA_CN, probs = 0.25)
Q2CN_ms2017 <- quantile(MS2017$NU_NOTA_CN, probs = 0.50)
Q3CN_ms2017 <- quantile(MS2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ms2017 = hist(MS2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Mato Grosso do Sul no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_ms2017, ", desvio padrão:", DPCN_ms2017, ", coeficiente de variação:", CVCN_ms2017, ", primeiro quartil:", Q1CN_ms2017, ", segundo quartil:", Q2CN_ms2017, ", terceiro quartil:", Q3CN_ms2017)

# Minas Gerais

MG2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MG') 
# Média:
mediaCN_mg2017 = mean(MG2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_mg2017 = sd(MG2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_mg2017 = cv(MG2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_mg2017 <- quantile(MG2017$NU_NOTA_CN, probs = 0.25)
Q2CN_mg2017 <- quantile(MG2017$NU_NOTA_CN, probs = 0.50)
Q3CN_mg2017 <- quantile(MG2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_mg2017 = hist(MG2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Minas Gerais no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_mg2017, ", desvio padrão:", DPCN_mg2017, ", coeficiente de variação:", CVCN_mg2017, ", primeiro quartil:", Q1CN_mg2017, ", segundo quartil:", Q2CN_mg2017, ", terceiro quartil:", Q3CN_mg2017)

# Pará

PA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PA') 
# Média:
mediaCN_pa2017 = mean(PA2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pa2017 = sd(PA2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pa2017 = cv(PA2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pa2017 <- quantile(PA2017$NU_NOTA_CN, probs = 0.25)
Q2CN_pa2017 <- quantile(PA2017$NU_NOTA_CN, probs = 0.50)
Q3CN_pa2017 <- quantile(PA2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pa2017 = hist(PA2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Pará no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_pa2017, ", desvio padrão:", DPCN_pa2017, ", coeficiente de variação:", CVCN_pa2017, ", primeiro quartil:", Q1CN_pa2017, ", segundo quartil:", Q2CN_pa2017, ", terceiro quartil:", Q3CN_pa2017)

# Paraíba

PB2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PB') 
# Média:
mediaCN_pb2017 = mean(PB2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pb2017 = sd(PB2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pb2017 = cv(PB2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pb2017 <- quantile(PB2017$NU_NOTA_CN, probs = 0.25)
Q2CN_pb2017 <- quantile(PB2017$NU_NOTA_CN, probs = 0.50)
Q3CN_pb2017 <- quantile(PB2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pb2017 = hist(PB2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Paraíba no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_pb2017, ", desvio padrão:", DPCN_pb2017, ", coeficiente de variação:", CVCN_pb2017, ", primeiro quartil:", Q1CN_pb2017, ", segundo quartil:", Q2CN_pb2017, ", terceiro quartil:", Q3CN_pb2017)

# Paraná

PR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PR') 
# Média:
mediaCN_pr2017 = mean(PR2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pr2017 = sd(PR2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pr2017 = cv(PR2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pr2017 <- quantile(PR2017$NU_NOTA_CN, probs = 0.25)
Q2CN_pr2017 <- quantile(PR2017$NU_NOTA_CN, probs = 0.50)
Q3CN_pr2017 <- quantile(PR2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pr2017 = hist(PR2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Paraná no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_pr2017, ", desvio padrão:", DPCN_pr2017, ", coeficiente de variação:", CVCN_pr2017, ", primeiro quartil:", Q1CN_pr2017, ", segundo quartil:", Q2CN_pr2017, ", terceiro quartil:", Q3CN_pr2017)

# Pernambuco

PE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PE') 
# Média:
mediaCN_pe2017 = mean(PE2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pe2017 = sd(PE2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pe2017 = cv(PE2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pe2017 <- quantile(PE2017$NU_NOTA_CN, probs = 0.25)
Q2CN_pe2017 <- quantile(PE2017$NU_NOTA_CN, probs = 0.50)
Q3CN_pe2017 <- quantile(PE2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pe2017 = hist(PE2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Pernambuco no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_pe2017, ", desvio padrão:", DPCN_pe2017, ", coeficiente de variação:", CVCN_pe2017, ", peimeiro quartil:", Q1CN_pe2017, ", segundo quartil:", Q2CN_pe2017, ", terceiro quartil:", Q3CN_pe2017)

# Piauí

PI2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PI') 
# Média:
mediaCN_pi2017 = mean(PI2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_pi2017 = sd(PI2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_pi2017 = cv(PI2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_pi2017 <- quantile(PI2017$NU_NOTA_CN, probs = 0.25)
Q2CN_pi2017 <- quantile(PI2017$NU_NOTA_CN, probs = 0.50)
Q3CN_pi2017 <- quantile(PI2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_pi2017 = hist(PI2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Piauí no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_pi2017, ", desvio padrão:", DPCN_pi2017, ", coeficiente de variação:", CVCN_pi2017, ", primeiro quartil:", Q1CN_pi2017, ", segundo quartil:", Q2CN_pi2017, ", terceiro quartil:", Q3CN_pi2017)

# Rio de Janeiro

RJ2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediaCN_rj2017 = mean(RJ2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rj2017 = sd(RJ2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rj2017 = cv(RJ2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rj2017 <- quantile(RJ2017$NU_NOTA_CN, probs = 0.25)
Q2CN_rj2017 <- quantile(RJ2017$NU_NOTA_CN, probs = 0.50)
Q3CN_rj2017 <- quantile(RJ2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rj2017 = hist(RJ2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Rio de Janeiro no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_rj2017, ", desvio padrão:", DPCN_rj2017, ", coeficiente de variação:", CVCN_rj2017, ", primeiro quartil:", Q1CN_rj2017, ", segundo quartil:", Q2CN_rj2017, ", terceiro quartil:", Q3CN_rj2017)

# Rio Grande do Norte

RN2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RN') 
# Média:
mediaCN_rn2017 = mean(RN2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rn2017 = sd(RN2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rn2017 = cv(RN2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rn2017 <- quantile(RN2017$NU_NOTA_CN, probs = 0.25)
Q2CN_rn2017 <- quantile(RN2017$NU_NOTA_CN, probs = 0.50)
Q3CN_rn2017 <- quantile(RN2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rn2017 = hist(RN2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza da Rio Grande do Norte no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_rn2017, ", desvio padrão:", DPCN_rn2017, ", coeficiente de variação:", CVCN_rn2017, ", primeiro quartil:", Q1CN_rn2017, ", segundo quartil:", Q2CN_rn2017, ", terceiro quartil:", Q3CN_rn2017)

# Rio Grande do Sul

RS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RS') 
# Média:
mediaCN_rs2017 = mean(RS2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rs2017 = sd(RS2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rs2017 = cv(RS2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rs2017 <- quantile(RS2017$NU_NOTA_CN, probs = 0.25)
Q2CN_rs2017 <- quantile(RS2017$NU_NOTA_CN, probs = 0.50)
Q3CN_rs2017 <- quantile(RS2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rs2017 = hist(RS2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza do Rio Grande do Sul no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_rs2017, ", desvio padrão:", DPCN_rs2017, ", coeficiente de variação:", CVCN_rs2017, ", primeiro quartil:", Q1CN_rs2017, ", segundo quartil:", Q2CN_rs2017, ", terceira quartil:", Q3CN_rs2017)

# Rondônia

RO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RO') 
# Média:
mediaCN_ro2017 = mean(RO2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_ro2017 = sd(RO2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_ro2017 = cv(RO2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_ro2017 <- quantile(RO2017$NU_NOTA_CN, probs = 0.25)
Q2CN_ro2017 <- quantile(RO2017$NU_NOTA_CN, probs = 0.50)
Q3CN_ro2017 <- quantile(RO2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_ro2017 = hist(RO2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Rondônia no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_ro2017, ", desvio padrão:", DPCN_ro2017, ", coeficiente de variação:", CVCN_ro2017, ", primeiro quartil:", Q1CN_ro2017, ", segundo quartil:", Q2CN_ro2017, ", terceiro quartil:", Q3CN_ro2017)

# Roraima

RR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RR') 
# Média:
mediaCN_rr2017 = mean(RR2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_rr2017 = sd(RR2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_rr2017 = cv(RR2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_rr2017 <- quantile(RR2017$NU_NOTA_CN, probs = 0.25)
Q2CN_rr2017 <- quantile(RR2017$NU_NOTA_CN, probs = 0.50)
Q3CN_rr2017 <- quantile(RR2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_rr2017 = hist(RR2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Roraima no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_rr2017, ", desvio padrão:", DPCN_rr2017, ", coeficiente de variação:", CVCN_rr2017, ", primeiro quartil:", Q1CN_rr2017, ", segundo quartil:", Q2CN_rr2017, ", terceiro quartil:", Q3CN_rr2017)

# Santa Catarina

SC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SC') 
# Média:
mediaCN_sc2017 = mean(SC2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_sc2017 = sd(SC2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_sc2017 = cv(SC2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_sc2017 <- quantile(SC2017$NU_NOTA_CN, probs = 0.25)
Q2CN_sc2017 <- quantile(SC2017$NU_NOTA_CN, probs = 0.50)
Q3CN_sc2017 <- quantile(SC2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_sc2017 = hist(SC2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Santa Catarina  no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_sc2017, ", desvio padrão:", DPCN_sc2017, ", coeficiente de variação:", CVCN_sc2017, ", primeiro quartil:", Q1CN_sc2017, ", segundo quartil:", Q2CN_sc2017, ", terceiro quartil:", Q3CN_sc2017)

# São Paulo

SP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SP') 
# Média:
mediaCN_sp2017 = mean(SP2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_sp2017 = sd(SP2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_sp2017 = cv(SP2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_sp2017 <- quantile(SP2017$NU_NOTA_CN, probs = 0.25)
Q2CN_sp2017 <- quantile(SP2017$NU_NOTA_CN, probs = 0.50)
Q3CN_sp2017 <- quantile(SP2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_sp2017 = hist(SP2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de São Paulo  no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_sp2017, ", desvio padrão:", DPCN_sp2017, ", coeficiente de variação:", CVCN_sp2017, ", primeiro quartil:", Q1CN_sp2017, ", segundo quartil:", Q2CN_sp2017, ", terceiro quartil:", Q3CN_sp2017)

# Sergipe

SE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SE') 
# Média:
mediaCN_se2017 = mean(SE2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_se2017 = sd(SE2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_se2017 = cv(SE2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_se2017 <- quantile(SE2017$NU_NOTA_CN, probs = 0.25)
Q2CN_se2017 <- quantile(SE2017$NU_NOTA_CN, probs = 0.50)
Q3CN_se2017 <- quantile(SE2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_se2017 = hist(SE2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Sergipe no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_se2017, ", desvio padrão:", DPCN_se2017, ", coeficiente de variação:", CVCN_se2017, ", primeiro quartil:", Q1CN_se2017, ", segundo quartil:", Q2CN_se2017, ", terceiro quartil:", Q3CN_se2017)

# Tocantins

TO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='TO') 
# Média:
mediaCN_to2017 = mean(TO2017$NU_NOTA_CN) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPCN_to2017 = sd(TO2017$NU_NOTA_CN)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVCN_to2017 = cv(TO2017$NU_NOTA_CN)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1CN_to2017 <- quantile(TO2017$NU_NOTA_CN, probs = 0.25)
Q2CN_to2017 <- quantile(TO2017$NU_NOTA_CN, probs = 0.50)
Q3CN_to2017 <- quantile(TO2017$NU_NOTA_CN, probs = 0.75)
# Histograma: 
histogramaCN_to2017 = hist(TO2017$NU_NOTA_CN,  
                           main = "Nota de Ciências da Natureza de Tocantins no Enem 2017",
                           xlab = "Nota de Ciências da Natureza", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de Ciências da Natureza do Enem 2017 --> média:", mediaCN_to2017, ", desvio padrão:", DPCN_to2017, ", coeficiente de variação:", CVCN_to2017, ", primeiro quartil:", Q1CN_to2017, ", segundo quartil:", Q2CN_to2017, ", terceiro quartil:", Q3CN_to2017)

# ------------ NU_Nota_LC

#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
library(dplyr)

# Acre

AC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediaLC_ac2019 = mean(AC2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ac2019 = sd(AC2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ac2019 = cv(AC2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ac2019 <- quantile(AC2019$NU_NOTA_LC, probs = 0.25)
Q2LC_ac2019 <- quantile(AC2019$NU_NOTA_LC, probs = 0.50)
Q3LC_ac2019 <- quantile(AC2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ac2019 = hist(AC2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Acre no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_ac2019, ", desvio padrão:", DPLC_ac2019, ", coeficiente de variação:", CVLC_ac2019, ", primeiro quartil:", Q1LC_ac2019, ", segundo quartil:", Q2LC_ac2019, ", terceiro quartil:", Q3LC_ac2019)

# Alagoas

AL2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AL')
# Média:
mediaLC_al2019 = mean(AL2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_al2019 = sd(AL2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_al2019 = cv(AL2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_al2019 <- quantile(AL2019$NU_NOTA_LC, probs = 0.25)
Q2LC_al2019 <- quantile(AL2019$NU_NOTA_LC, probs = 0.50)
Q3LC_al2019 <- quantile(AL2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_al2019 = hist(AL2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Alagoas no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_al2019, ", desvio padrão:", DPLC_al2019, ", coeficiente de variação:", CVLC_al2019, ", primeiro quartil:", Q1LC_al2019, ", segundo quartil:", Q2LC_al2019, ", terceiro quartil:", Q3LC_al2019)

# Amapá

AP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AP') 
# Média:
mediaLC_ap2019 = mean(AP2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ap2019 = sd(AP2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ap2019 = cv(AP2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ap2019 <- quantile(AP2019$NU_NOTA_LC, probs = 0.25)
Q2LC_ap2019 <- quantile(AP2019$NU_NOTA_LC, probs = 0.50)
Q3LC_ap2019 <- quantile(AP2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ap2019 = hist(AP2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Amapá no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_ap2019, ", desvio padrão:", DPLC_ap2019, ", coeficiente de variação:", CVLC_ap2019, ", primeiro quartil:", Q1LC_ap2019, ", segundo quartil:", Q2LC_ap2019, ", terceiro quartil:", Q3LC_ap2019)

# Amazonas

AM2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AM') 
# Média:
mediaLC_am2019 = mean(AM2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_am2019 = sd(AM2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_am2019 = cv(AM2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_am2019 <- quantile(AM2019$NU_NOTA_LC, probs = 0.25)
Q2LC_am2019 <- quantile(AM2019$NU_NOTA_LC, probs = 0.50)
Q3LC_am2019 <- quantile(AM2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_am2019 = hist(AM2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Amazonas no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_am2019, ", desvio padrão:", DPLC_am2019, ", coeficiente de variação:", CVLC_am2019, ", primeiro quartil:", Q1LC_am2019, ", segundo quartil:", Q2LC_am2019, ", terceiro quartil:", Q3LC_am2019)

# Bahia

BA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='BA') 
# Média:
mediaLC_ba2019 = mean(BA2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ba2019 = sd(BA2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ba2019 = cv(BA2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ba2019 <- quantile(BA2019$NU_NOTA_LC, probs = 0.25)
Q2LC_ba2019 <- quantile(BA2019$NU_NOTA_LC, probs = 0.50)
Q3LC_ba2019 <- quantile(BA2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ba2019 = hist(BA2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Bahia no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_ba2019, ", desvio padrão:", DPLC_ba2019, ", coeficiente de variação:", CVLC_ba2019, ", primeiro quartil:", Q1LC_ba2019, ", segundo quartil:", Q2LC_ba2019, ", terceiro quartil:", Q3LC_ba2019)

# Ceará

CE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='CE') 
# Média:
mediaLC_ce2019 = mean(CE2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ce2019 = sd(CE2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ce2019 = cv(CE2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ce2019 <- quantile(CE2019$NU_NOTA_LC, probs = 0.25)
Q2LC_ce2019 <- quantile(CE2019$NU_NOTA_LC, probs = 0.50)
Q3LC_ce2019 <- quantile(CE2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ce2019 = hist(CE2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Ceará no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_ce2019, ", desvio padrão:", DPLC_ce2019, ", coeficiente de variação:", CVLC_ce2019, ", primeiro quartil:", Q1LC_ce2019, ", segundo quartil:", Q2LC_ce2019, ", terceiro quartil:", Q3LC_ce2019)

# Distrito Federal

DF2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='DF') 
# Média:
mediaLC_df2019 = mean(DF2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_df2019 = sd(DF2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_df2019 = cv(DF2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_df2019 <- quantile(DF2019$NU_NOTA_LC, probs = 0.25)
Q2LC_df2019 <- quantile(DF2019$NU_NOTA_LC, probs = 0.50)
Q3LC_df2019 <- quantile(DF2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_df2019 = hist(DF2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Distrito Federal no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_df2019, ", desvio padrão:", DPLC_df2019, ", coeficiente de variação:", CVLC_df2019, ", primeiro quartil:", Q1LC_df2019, ", segundo quartil:", Q2LC_df2019, ", terceiro quartil:", Q3LC_df2019)

# Espírito Santo

ES2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='ES') 
# Média:
mediaLC_es2019 = mean(ES2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_es2019 = sd(ES2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_es2019 = cv(ES2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_es2019 <- quantile(ES2019$NU_NOTA_LC, probs = 0.25)
Q2LC_es2019 <- quantile(ES2019$NU_NOTA_LC, probs = 0.50)
Q3LC_es2019 <- quantile(ES2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_es2019 = hist(ES2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Espírito Santo no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_es2019, ", desvio padrão:", DPLC_es2019, ", coeficiente de variação:", CVLC_es2019, ", primeiro quartil:", Q1LC_es2019, ", segundo quartil:", Q2LC_es2019, ", terceiro quartil:", Q3LC_es2019)

# Goiás

GO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='GO') 
# Média:
mediaLC_go2019 = mean(GO2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_go2019 = sd(GO2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_go2019 = cv(GO2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_go2019 <- quantile(GO2019$NU_NOTA_LC, probs = 0.25)
Q2LC_go2019 <- quantile(GO2019$NU_NOTA_LC, probs = 0.50)
Q3LC_go2019 <- quantile(GO2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_go2019 = hist(GO2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Goiás no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_go2019, ", desvio padrão:", DPLC_go2019, ", coeficiente de variação:", CVLC_go2019, ", primeiro quartil:", Q1LC_go2019, ", segundo quartil:", Q2LC_go2019, ", terceiro quartil:", Q3LC_go2019)

# Maranhão

MA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MA') 
# Média:
mediaLC_ma2019 = mean(MA2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ma2019 = sd(MA2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ma2019 = cv(MA2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ma2019 <- quantile(MA2019$NU_NOTA_LC, probs = 0.25)
Q2LC_ma2019 <- quantile(MA2019$NU_NOTA_LC, probs = 0.50)
Q3LC_ma2019 <- quantile(MA2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ma2019 = hist(MA2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Maranhão no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_ma2019, ", desvio padrão:", DPLC_ma2019, ", coeficiente de variação:", CVLC_ma2019, ", primeiro quartil:", Q1LC_ma2019, ", segundo quartil:", Q2LC_ma2019, ", terceiro quartil:", Q3LC_ma2019)

# Mato Grosso

MT2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MT') 
# Média:
mediaLC_MT2019 = mean(MT2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_MT2019 = sd(MT2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_MT2019 = cv(MT2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_MT2019 <- quantile(MT2019$NU_NOTA_LC, probs = 0.25)
Q2LC_MT2019 <- quantile(MT2019$NU_NOTA_LC, probs = 0.50)
Q3LC_MT2019 <- quantile(MT2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_MT2019 = hist(MT2019$NU_NOTA_LC,  
                           main = "Nota de Linguagens e Códigos do Mato Grosso no Enem 2019",
                           xlab = "Nota de Linguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_MT2019, ", desvio padrão:", DPLC_MT2019, ", coeficiente de variação:", CVLC_MT2019, ", primeiro quartil:", Q1LC_MT2019, ", segundo quartil:", Q2LC_MT2019, ", terceiro quartil:", Q3LC_MT2019)

# Mato Grosso do Sul

MS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MS') 
# Média:
mediaLC_ms2019 = mean(MS2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ms2019 = sd(MS2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ms2019 = cv(MS2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ms2019 <- quantile(MS2019$NU_NOTA_LC, probs = 0.25)
Q2LC_ms2019 <- quantile(MS2019$NU_NOTA_LC, probs = 0.50)
Q3LC_ms2019 <- quantile(MS2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ms2019 = hist(MS2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Mato Grosso do Sul no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_ms2019, ", desvio padrão:", DPLC_ms2019, ", coeficiente de variação:", CVLC_ms2019, ", primeiro quartil:", Q1LC_ms2019, ", segundo quartil:", Q2LC_ms2019, ", terceiro quartil:", Q3LC_ms2019)

# Minas Gerais

MG2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MG') 
# Média:
mediaLC_mg2019 = mean(MG2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_mg2019 = sd(MG2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_mg2019 = cv(MG2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_mg2019 <- quantile(MG2019$NU_NOTA_LC, probs = 0.25)
Q2LC_mg2019 <- quantile(MG2019$NU_NOTA_LC, probs = 0.50)
Q3LC_mg2019 <- quantile(MG2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_mg2019 = hist(MG2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Minas Gerais no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_mg2019, ", desvio padrão:", DPLC_mg2019, ", coeficiente de variação:", CVLC_mg2019, ", primeiro quartil:", Q1LC_mg2019, ", segundo quartil:", Q2LC_mg2019, ", terceiro quartil:", Q3LC_mg2019)

# Pará

PA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PA') 
# Média:
mediaLC_pa2019 = mean(PA2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pa2019 = sd(PA2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pa2019 = cv(PA2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pa2019 <- quantile(PA2019$NU_NOTA_LC, probs = 0.25)
Q2LC_pa2019 <- quantile(PA2019$NU_NOTA_LC, probs = 0.50)
Q3LC_pa2019 <- quantile(PA2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pa2019 = hist(PA2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Pará no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_pa2019, ", desvio padrão:", DPLC_pa2019, ", coeficiente de variação:", CVLC_pa2019, ", primeiro quartil:", Q1LC_pa2019, ", segundo quartil:", Q2LC_pa2019, ", terceiro quartil:", Q3LC_pa2019)

# Paraíba

PB2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PB') 
# Média:
mediaLC_pb2019 = mean(PB2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pb2019 = sd(PB2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pb2019 = cv(PB2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pb2019 <- quantile(PB2019$NU_NOTA_LC, probs = 0.25)
Q2LC_pb2019 <- quantile(PB2019$NU_NOTA_LC, probs = 0.50)
Q3LC_pb2019 <- quantile(PB2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pb2019 = hist(PB2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Paraíba no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_pb2019, ", desvio padrão:", DPLC_pb2019, ", coeficiente de variação:", CVLC_pb2019, ", primeiro quartil:", Q1LC_pb2019, ", segundo quartil:", Q2LC_pb2019, ", terceiro quartil:", Q3LC_pb2019)

# Paraná

PR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PR') 
# Média:
mediaLC_pr2019 = mean(PR2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pr2019 = sd(PR2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pr2019 = cv(PR2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pr2019 <- quantile(PR2019$NU_NOTA_LC, probs = 0.25)
Q2LC_pr2019 <- quantile(PR2019$NU_NOTA_LC, probs = 0.50)
Q3LC_pr2019 <- quantile(PR2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pr2019 = hist(PR2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Paraná no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_pr2019, ", desvio padrão:", DPLC_pr2019, ", coeficiente de variação:", CVLC_pr2019, ", primeiro quartil:", Q1LC_pr2019, ", segundo quartil:", Q2LC_pr2019, ", terceiro quartil:", Q3LC_pr2019)

# Pernambuco

PE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PE') 
# Média:
mediaLC_pe2019 = mean(PE2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pe2019 = sd(PE2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pe2019 = cv(PE2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pe2019 <- quantile(PE2019$NU_NOTA_LC, probs = 0.25)
Q2LC_pe2019 <- quantile(PE2019$NU_NOTA_LC, probs = 0.50)
Q3LC_pe2019 <- quantile(PE2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pe2019 = hist(PE2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Pernambuco no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_pe2019, ", desvio padrão:", DPLC_pe2019, ", coeficiente de variação:", CVLC_pe2019, ", peimeiro quartil:", Q1LC_pe2019, ", segundo quartil:", Q2LC_pe2019, ", terceiro quartil:", Q3LC_pe2019)

# Piauí

PI2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PI') 
# Média:
mediaLC_pi2019 = mean(PI2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pi2019 = sd(PI2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pi2019 = cv(PI2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pi2019 <- quantile(PI2019$NU_NOTA_LC, probs = 0.25)
Q2LC_pi2019 <- quantile(PI2019$NU_NOTA_LC, probs = 0.50)
Q3LC_pi2019 <- quantile(PI2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pi2019 = hist(PI2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Piauí no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_pi2019, ", desvio padrão:", DPLC_pi2019, ", coeficiente de variação:", CVLC_pi2019, ", primeiro quartil:", Q1LC_pi2019, ", segundo quartil:", Q2LC_pi2019, ", terceiro quartil:", Q3LC_pi2019)

# Rio de Janeiro

RJ2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediaLC_rj2019 = mean(RJ2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rj2019 = sd(RJ2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rj2019 = cv(RJ2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rj2019 <- quantile(RJ2019$NU_NOTA_LC, probs = 0.25)
Q2LC_rj2019 <- quantile(RJ2019$NU_NOTA_LC, probs = 0.50)
Q3LC_rj2019 <- quantile(RJ2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rj2019 = hist(RJ2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Rio de Janeiro no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_rj2019, ", desvio padrão:", DPLC_rj2019, ", coeficiente de variação:", CVLC_rj2019, ", primeiro quartil:", Q1LC_rj2019, ", segundo quartil:", Q2LC_rj2019, ", terceiro quartil:", Q3LC_rj2019)

# Rio Grande do Norte

RN2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RN') 
# Média:
mediaLC_rn2019 = mean(RN2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rn2019 = sd(RN2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rn2019 = cv(RN2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rn2019 <- quantile(RN2019$NU_NOTA_LC, probs = 0.25)
Q2LC_rn2019 <- quantile(RN2019$NU_NOTA_LC, probs = 0.50)
Q3LC_rn2019 <- quantile(RN2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rn2019 = hist(RN2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Rio Grande do Norte no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_rn2019, ", desvio padrão:", DPLC_rn2019, ", coeficiente de variação:", CVLC_rn2019, ", primeiro quartil:", Q1LC_rn2019, ", segundo quartil:", Q2LC_rn2019, ", terceiro quartil:", Q3LC_rn2019)

# Rio Grande do Sul

RS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RS') 
# Média:
mediaLC_rs2019 = mean(RS2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rs2019 = sd(RS2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rs2019 = cv(RS2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rs2019 <- quantile(RS2019$NU_NOTA_LC, probs = 0.25)
Q2LC_rs2019 <- quantile(RS2019$NU_NOTA_LC, probs = 0.50)
Q3LC_rs2019 <- quantile(RS2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rs2019 = hist(RS2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Rio Grande do Sul no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_rs2019, ", desvio padrão:", DPLC_rs2019, ", coeficiente de variação:", CVLC_rs2019, ", primeiro quartil:", Q1LC_rs2019, ", segundo quartil:", Q2LC_rs2019, ", terceira quartil:", Q3LC_rs2019)

# Rondônia

RO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RO') 
# Média:
mediaLC_ro2019 = mean(RO2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ro2019 = sd(RO2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ro2019 = cv(RO2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ro2019 <- quantile(RO2019$NU_NOTA_LC, probs = 0.25)
Q2LC_ro2019 <- quantile(RO2019$NU_NOTA_LC, probs = 0.50)
Q3LC_ro2019 <- quantile(RO2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ro2019 = hist(RO2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Rondônia no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_ro2019, ", desvio padrão:", DPLC_ro2019, ", coeficiente de variação:", CVLC_ro2019, ", primeiro quartil:", Q1LC_ro2019, ", segundo quartil:", Q2LC_ro2019, ", terceiro quartil:", Q3LC_ro2019)

# Roraima

RR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RR') 
# Média:
mediaLC_rr2019 = mean(RR2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rr2019 = sd(RR2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rr2019 = cv(RR2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rr2019 <- quantile(RR2019$NU_NOTA_LC, probs = 0.25)
Q2LC_rr2019 <- quantile(RR2019$NU_NOTA_LC, probs = 0.50)
Q3LC_rr2019 <- quantile(RR2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rr2019 = hist(RR2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Roraima no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_rr2019, ", desvio padrão:", DPLC_rr2019, ", coeficiente de variação:", CVLC_rr2019, ", primeiro quartil:", Q1LC_rr2019, ", segundo quartil:", Q2LC_rr2019, ", terceiro quartil:", Q3LC_rr2019)

# Santa Catarina

SC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SC') 
# Média:
mediaLC_sc2019 = mean(SC2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_sc2019 = sd(SC2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_sc2019 = cv(SC2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_sc2019 <- quantile(SC2019$NU_NOTA_LC, probs = 0.25)
Q2LC_sc2019 <- quantile(SC2019$NU_NOTA_LC, probs = 0.50)
Q3LC_sc2019 <- quantile(SC2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_sc2019 = hist(SC2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Santa Catarina  no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_sc2019, ", desvio padrão:", DPLC_sc2019, ", coeficiente de variação:", CVLC_sc2019, ", primeiro quartil:", Q1LC_sc2019, ", segundo quartil:", Q2LC_sc2019, ", terceiro quartil:", Q3LC_sc2019)

# São Paulo

SP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SP') 
# Média:
mediaLC_sp2019 = mean(SP2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_sp2019 = sd(SP2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_sp2019 = cv(SP2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_sp2019 <- quantile(SP2019$NU_NOTA_LC, probs = 0.25)
Q2LC_sp2019 <- quantile(SP2019$NU_NOTA_LC, probs = 0.50)
Q3LC_sp2019 <- quantile(SP2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_sp2019 = hist(SP2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de São Paulo  no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_sp2019, ", desvio padrão:", DPLC_sp2019, ", coeficiente de variação:", CVLC_sp2019, ", primeiro quartil:", Q1LC_sp2019, ", segundo quartil:", Q2LC_sp2019, ", terceiro quartil:", Q3LC_sp2019)

# Sergipe

SE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SE') 
# Média:
mediaLC_se2019 = mean(SE2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_se2019 = sd(SE2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_se2019 = cv(SE2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_se2019 <- quantile(SE2019$NU_NOTA_LC, probs = 0.25)
Q2LC_se2019 <- quantile(SE2019$NU_NOTA_LC, probs = 0.50)
Q3LC_se2019 <- quantile(SE2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_se2019 = hist(SE2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Sergipe no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_se2019, ", desvio padrão:", DPLC_se2019, ", coeficiente de variação:", CVLC_se2019, ", primeiro quartil:", Q1LC_se2019, ", segundo quartil:", Q2LC_se2019, ", terceiro quartil:", Q3LC_se2019)

# Tocantins

TO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='TO') 
# Média:
mediaLC_to2019 = mean(TO2019$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_to2019 = sd(TO2019$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_to2019 = cv(TO2019$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_to2019 <- quantile(TO2019$NU_NOTA_LC, probs = 0.25)
Q2LC_to2019 <- quantile(TO2019$NU_NOTA_LC, probs = 0.50)
Q3LC_to2019 <- quantile(TO2019$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_to2019 = hist(TO2019$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Tocantins no Enem 2019",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota deLinguagens e Códigos do Enem 2019 --> média:", mediaLC_to2019, ", desvio padrão:", DPLC_to2019, ", coeficiente de variação:", CVLC_to2019, ", primeiro quartil:", Q1LC_to2019, ", segundo quartil:", Q2LC_to2019, ", terceiro quartil:", Q3LC_to2019)


#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////

# Acre

AC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediaLC_ac2018 = mean(AC2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ac2018 = sd(AC2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ac2018 = cv(AC2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ac2018 <- quantile(AC2018$NU_NOTA_LC, probs = 0.25)
Q2LC_ac2018 <- quantile(AC2018$NU_NOTA_LC, probs = 0.50)
Q3LC_ac2018 <- quantile(AC2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ac2018 = hist(AC2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Acre no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_ac2018, ", desvio padrão:", DPLC_ac2018, ", coeficiente de variação:", CVLC_ac2018, ", primeiro quartil:", Q1LC_ac2018, ", segundo quartil:", Q2LC_ac2018, ", terceiro quartil:", Q3LC_ac2018)

# Alagoas

AL2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AL')
# Média:
mediaLC_al2018 = mean(AL2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_al2018 = sd(AL2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_al2018 = cv(AL2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_al2018 <- quantile(AL2018$NU_NOTA_LC, probs = 0.25)
Q2LC_al2018 <- quantile(AL2018$NU_NOTA_LC, probs = 0.50)
Q3LC_al2018 <- quantile(AL2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_al2018 = hist(AL2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Alagoas no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_al2018, ", desvio padrão:", DPLC_al2018, ", coeficiente de variação:", CVLC_al2018, ", primeiro quartil:", Q1LC_al2018, ", segundo quartil:", Q2LC_al2018, ", terceiro quartil:", Q3LC_al2018)

# Amapá

AP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AP') 
# Média:
mediaLC_ap2018 = mean(AP2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ap2018 = sd(AP2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ap2018 = cv(AP2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ap2018 <- quantile(AP2018$NU_NOTA_LC, probs = 0.25)
Q2LC_ap2018 <- quantile(AP2018$NU_NOTA_LC, probs = 0.50)
Q3LC_ap2018 <- quantile(AP2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ap2018 = hist(AP2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Amapá no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_ap2018, ", desvio padrão:", DPLC_ap2018, ", coeficiente de variação:", CVLC_ap2018, ", primeiro quartil:", Q1LC_ap2018, ", segundo quartil:", Q2LC_ap2018, ", terceiro quartil:", Q3LC_ap2018)

# Amazonas

AM2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AM') 
# Média:
mediaLC_am2018 = mean(AM2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_am2018 = sd(AM2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_am2018 = cv(AM2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_am2018 <- quantile(AM2018$NU_NOTA_LC, probs = 0.25)
Q2LC_am2018 <- quantile(AM2018$NU_NOTA_LC, probs = 0.50)
Q3LC_am2018 <- quantile(AM2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_am2018 = hist(AM2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Amazonas no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_am2018, ", desvio padrão:", DPLC_am2018, ", coeficiente de variação:", CVLC_am2018, ", primeiro quartil:", Q1LC_am2018, ", segundo quartil:", Q2LC_am2018, ", terceiro quartil:", Q3LC_am2018)

# Bahia

BA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='BA') 
# Média:
mediaLC_ba2018 = mean(BA2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ba2018 = sd(BA2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ba2018 = cv(BA2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ba2018 <- quantile(BA2018$NU_NOTA_LC, probs = 0.25)
Q2LC_ba2018 <- quantile(BA2018$NU_NOTA_LC, probs = 0.50)
Q3LC_ba2018 <- quantile(BA2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ba2018 = hist(BA2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Bahia no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_ba2018, ", desvio padrão:", DPLC_ba2018, ", coeficiente de variação:", CVLC_ba2018, ", primeiro quartil:", Q1LC_ba2018, ", segundo quartil:", Q2LC_ba2018, ", terceiro quartil:", Q3LC_ba2018)

# Ceará

CE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='CE') 
# Média:
mediaLC_ce2018 = mean(CE2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ce2018 = sd(CE2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ce2018 = cv(CE2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ce2018 <- quantile(CE2018$NU_NOTA_LC, probs = 0.25)
Q2LC_ce2018 <- quantile(CE2018$NU_NOTA_LC, probs = 0.50)
Q3LC_ce2018 <- quantile(CE2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ce2018 = hist(CE2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Ceará no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_ce2018, ", desvio padrão:", DPLC_ce2018, ", coeficiente de variação:", CVLC_ce2018, ", primeiro quartil:", Q1LC_ce2018, ", segundo quartil:", Q2LC_ce2018, ", terceiro quartil:", Q3LC_ce2018)

# Distrito Federal

DF2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='DF') 
# Média:
mediaLC_df2018 = mean(DF2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_df2018 = sd(DF2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_df2018 = cv(DF2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_df2018 <- quantile(DF2018$NU_NOTA_LC, probs = 0.25)
Q2LC_df2018 <- quantile(DF2018$NU_NOTA_LC, probs = 0.50)
Q3LC_df2018 <- quantile(DF2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_df2018 = hist(DF2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Distrito Federal no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_df2018, ", desvio padrão:", DPLC_df2018, ", coeficiente de variação:", CVLC_df2018, ", primeiro quartil:", Q1LC_df2018, ", segundo quartil:", Q2LC_df2018, ", terceiro quartil:", Q3LC_df2018)

# Espírito Santo

ES2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='ES') 
# Média:
mediaLC_es2018 = mean(ES2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_es2018 = sd(ES2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_es2018 = cv(ES2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_es2018 <- quantile(ES2018$NU_NOTA_LC, probs = 0.25)
Q2LC_es2018 <- quantile(ES2018$NU_NOTA_LC, probs = 0.50)
Q3LC_es2018 <- quantile(ES2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_es2018 = hist(ES2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Espírito Santo no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_es2018, ", desvio padrão:", DPLC_es2018, ", coeficiente de variação:", CVLC_es2018, ", primeiro quartil:", Q1LC_es2018, ", segundo quartil:", Q2LC_es2018, ", terceiro quartil:", Q3LC_es2018)

# Goiás

GO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='GO') 
# Média:
mediaLC_go2018 = mean(GO2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_go2018 = sd(GO2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_go2018 = cv(GO2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_go2018 <- quantile(GO2018$NU_NOTA_LC, probs = 0.25)
Q2LC_go2018 <- quantile(GO2018$NU_NOTA_LC, probs = 0.50)
Q3LC_go2018 <- quantile(GO2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_go2018 = hist(GO2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Goiás no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_go2018, ", desvio padrão:", DPLC_go2018, ", coeficiente de variação:", CVLC_go2018, ", primeiro quartil:", Q1LC_go2018, ", segundo quartil:", Q2LC_go2018, ", terceiro quartil:", Q3LC_go2018)

# Maranhão

MA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MA') 
# Média:
mediaLC_ma2018 = mean(MA2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ma2018 = sd(MA2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ma2018 = cv(MA2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ma2018 <- quantile(MA2018$NU_NOTA_LC, probs = 0.25)
Q2LC_ma2018 <- quantile(MA2018$NU_NOTA_LC, probs = 0.50)
Q3LC_ma2018 <- quantile(MA2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ma2018 = hist(MA2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Maranhão no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_ma2018, ", desvio padrão:", DPLC_ma2018, ", coeficiente de variação:", CVLC_ma2018, ", primeiro quartil:", Q1LC_ma2018, ", segundo quartil:", Q2LC_ma2018, ", terceiro quartil:", Q3LC_ma2018)

# Mato Grosso

MT2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MT') 
# Média:
mediaLC_MT2018 = mean(MT2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_MT2018 = sd(MT2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_MT2018 = cv(MT2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_MT2018 <- quantile(MT2018$NU_NOTA_LC, probs = 0.25)
Q2LC_MT2018 <- quantile(MT2018$NU_NOTA_LC, probs = 0.50)
Q3LC_MT2018 <- quantile(MT2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_MT2018 = hist(MT2018$NU_NOTA_LC,  
                           main = "Nota de Linguagens e Códigos do Mato Grosso no Enem 2018",
                           xlab = "Nota de Linguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_MT2018, ", desvio padrão:", DPLC_MT2018, ", coeficiente de variação:", CVLC_MT2018, ", primeiro quartil:", Q1LC_MT2018, ", segundo quartil:", Q2LC_MT2018, ", terceiro quartil:", Q3LC_MT2018)

# Mato Grosso do Sul

MS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MS') 
# Média:
mediaLC_ms2018 = mean(MS2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ms2018 = sd(MS2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ms2018 = cv(MS2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ms2018 <- quantile(MS2018$NU_NOTA_LC, probs = 0.25)
Q2LC_ms2018 <- quantile(MS2018$NU_NOTA_LC, probs = 0.50)
Q3LC_ms2018 <- quantile(MS2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ms2018 = hist(MS2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Mato Grosso do Sul no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_ms2018, ", desvio padrão:", DPLC_ms2018, ", coeficiente de variação:", CVLC_ms2018, ", primeiro quartil:", Q1LC_ms2018, ", segundo quartil:", Q2LC_ms2018, ", terceiro quartil:", Q3LC_ms2018)

# Minas Gerais

MG2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MG') 
# Média:
mediaLC_mg2018 = mean(MG2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_mg2018 = sd(MG2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_mg2018 = cv(MG2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_mg2018 <- quantile(MG2018$NU_NOTA_LC, probs = 0.25)
Q2LC_mg2018 <- quantile(MG2018$NU_NOTA_LC, probs = 0.50)
Q3LC_mg2018 <- quantile(MG2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_mg2018 = hist(MG2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Minas Gerais no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_mg2018, ", desvio padrão:", DPLC_mg2018, ", coeficiente de variação:", CVLC_mg2018, ", primeiro quartil:", Q1LC_mg2018, ", segundo quartil:", Q2LC_mg2018, ", terceiro quartil:", Q3LC_mg2018)

# Pará

PA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PA') 
# Média:
mediaLC_pa2018 = mean(PA2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pa2018 = sd(PA2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pa2018 = cv(PA2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pa2018 <- quantile(PA2018$NU_NOTA_LC, probs = 0.25)
Q2LC_pa2018 <- quantile(PA2018$NU_NOTA_LC, probs = 0.50)
Q3LC_pa2018 <- quantile(PA2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pa2018 = hist(PA2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Pará no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_pa2018, ", desvio padrão:", DPLC_pa2018, ", coeficiente de variação:", CVLC_pa2018, ", primeiro quartil:", Q1LC_pa2018, ", segundo quartil:", Q2LC_pa2018, ", terceiro quartil:", Q3LC_pa2018)

# Paraíba

PB2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PB') 
# Média:
mediaLC_pb2018 = mean(PB2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pb2018 = sd(PB2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pb2018 = cv(PB2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pb2018 <- quantile(PB2018$NU_NOTA_LC, probs = 0.25)
Q2LC_pb2018 <- quantile(PB2018$NU_NOTA_LC, probs = 0.50)
Q3LC_pb2018 <- quantile(PB2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pb2018 = hist(PB2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Paraíba no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_pb2018, ", desvio padrão:", DPLC_pb2018, ", coeficiente de variação:", CVLC_pb2018, ", primeiro quartil:", Q1LC_pb2018, ", segundo quartil:", Q2LC_pb2018, ", terceiro quartil:", Q3LC_pb2018)

# Paraná

PR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PR') 
# Média:
mediaLC_pr2018 = mean(PR2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pr2018 = sd(PR2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pr2018 = cv(PR2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pr2018 <- quantile(PR2018$NU_NOTA_LC, probs = 0.25)
Q2LC_pr2018 <- quantile(PR2018$NU_NOTA_LC, probs = 0.50)
Q3LC_pr2018 <- quantile(PR2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pr2018 = hist(PR2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Paraná no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_pr2018, ", desvio padrão:", DPLC_pr2018, ", coeficiente de variação:", CVLC_pr2018, ", primeiro quartil:", Q1LC_pr2018, ", segundo quartil:", Q2LC_pr2018, ", terceiro quartil:", Q3LC_pr2018)

# Pernambuco

PE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PE') 
# Média:
mediaLC_pe2018 = mean(PE2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pe2018 = sd(PE2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pe2018 = cv(PE2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pe2018 <- quantile(PE2018$NU_NOTA_LC, probs = 0.25)
Q2LC_pe2018 <- quantile(PE2018$NU_NOTA_LC, probs = 0.50)
Q3LC_pe2018 <- quantile(PE2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pe2018 = hist(PE2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Pernambuco no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_pe2018, ", desvio padrão:", DPLC_pe2018, ", coeficiente de variação:", CVLC_pe2018, ", peimeiro quartil:", Q1LC_pe2018, ", segundo quartil:", Q2LC_pe2018, ", terceiro quartil:", Q3LC_pe2018)

# Piauí

PI2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PI') 
# Média:
mediaLC_pi2018 = mean(PI2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pi2018 = sd(PI2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pi2018 = cv(PI2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pi2018 <- quantile(PI2018$NU_NOTA_LC, probs = 0.25)
Q2LC_pi2018 <- quantile(PI2018$NU_NOTA_LC, probs = 0.50)
Q3LC_pi2018 <- quantile(PI2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pi2018 = hist(PI2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Piauí no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_pi2018, ", desvio padrão:", DPLC_pi2018, ", coeficiente de variação:", CVLC_pi2018, ", primeiro quartil:", Q1LC_pi2018, ", segundo quartil:", Q2LC_pi2018, ", terceiro quartil:", Q3LC_pi2018)

# Rio de Janeiro

RJ2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediaLC_rj2018 = mean(RJ2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rj2018 = sd(RJ2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rj2018 = cv(RJ2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rj2018 <- quantile(RJ2018$NU_NOTA_LC, probs = 0.25)
Q2LC_rj2018 <- quantile(RJ2018$NU_NOTA_LC, probs = 0.50)
Q3LC_rj2018 <- quantile(RJ2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rj2018 = hist(RJ2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Rio de Janeiro no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_rj2018, ", desvio padrão:", DPLC_rj2018, ", coeficiente de variação:", CVLC_rj2018, ", primeiro quartil:", Q1LC_rj2018, ", segundo quartil:", Q2LC_rj2018, ", terceiro quartil:", Q3LC_rj2018)

# Rio Grande do Norte

RN2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RN') 
# Média:
mediaLC_rn2018 = mean(RN2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rn2018 = sd(RN2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rn2018 = cv(RN2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rn2018 <- quantile(RN2018$NU_NOTA_LC, probs = 0.25)
Q2LC_rn2018 <- quantile(RN2018$NU_NOTA_LC, probs = 0.50)
Q3LC_rn2018 <- quantile(RN2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rn2018 = hist(RN2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Rio Grande do Norte no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_rn2018, ", desvio padrão:", DPLC_rn2018, ", coeficiente de variação:", CVLC_rn2018, ", primeiro quartil:", Q1LC_rn2018, ", segundo quartil:", Q2LC_rn2018, ", terceiro quartil:", Q3LC_rn2018)

# Rio Grande do Sul

RS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RS') 
# Média:
mediaLC_rs2018 = mean(RS2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rs2018 = sd(RS2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rs2018 = cv(RS2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rs2018 <- quantile(RS2018$NU_NOTA_LC, probs = 0.25)
Q2LC_rs2018 <- quantile(RS2018$NU_NOTA_LC, probs = 0.50)
Q3LC_rs2018 <- quantile(RS2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rs2018 = hist(RS2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Rio Grande do Sul no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_rs2018, ", desvio padrão:", DPLC_rs2018, ", coeficiente de variação:", CVLC_rs2018, ", primeiro quartil:", Q1LC_rs2018, ", segundo quartil:", Q2LC_rs2018, ", terceira quartil:", Q3LC_rs2018)

# Rondônia

RO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RO') 
# Média:
mediaLC_ro2018 = mean(RO2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ro2018 = sd(RO2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ro2018 = cv(RO2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ro2018 <- quantile(RO2018$NU_NOTA_LC, probs = 0.25)
Q2LC_ro2018 <- quantile(RO2018$NU_NOTA_LC, probs = 0.50)
Q3LC_ro2018 <- quantile(RO2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ro2018 = hist(RO2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Rondônia no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_ro2018, ", desvio padrão:", DPLC_ro2018, ", coeficiente de variação:", CVLC_ro2018, ", primeiro quartil:", Q1LC_ro2018, ", segundo quartil:", Q2LC_ro2018, ", terceiro quartil:", Q3LC_ro2018)

# Roraima

RR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RR') 
# Média:
mediaLC_rr2018 = mean(RR2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rr2018 = sd(RR2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rr2018 = cv(RR2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rr2018 <- quantile(RR2018$NU_NOTA_LC, probs = 0.25)
Q2LC_rr2018 <- quantile(RR2018$NU_NOTA_LC, probs = 0.50)
Q3LC_rr2018 <- quantile(RR2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rr2018 = hist(RR2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Roraima no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_rr2018, ", desvio padrão:", DPLC_rr2018, ", coeficiente de variação:", CVLC_rr2018, ", primeiro quartil:", Q1LC_rr2018, ", segundo quartil:", Q2LC_rr2018, ", terceiro quartil:", Q3LC_rr2018)

# Santa Catarina

SC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SC') 
# Média:
mediaLC_sc2018 = mean(SC2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_sc2018 = sd(SC2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_sc2018 = cv(SC2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_sc2018 <- quantile(SC2018$NU_NOTA_LC, probs = 0.25)
Q2LC_sc2018 <- quantile(SC2018$NU_NOTA_LC, probs = 0.50)
Q3LC_sc2018 <- quantile(SC2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_sc2018 = hist(SC2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Santa Catarina  no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_sc2018, ", desvio padrão:", DPLC_sc2018, ", coeficiente de variação:", CVLC_sc2018, ", primeiro quartil:", Q1LC_sc2018, ", segundo quartil:", Q2LC_sc2018, ", terceiro quartil:", Q3LC_sc2018)

# São Paulo

SP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SP') 
# Média:
mediaLC_sp2018 = mean(SP2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_sp2018 = sd(SP2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_sp2018 = cv(SP2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_sp2018 <- quantile(SP2018$NU_NOTA_LC, probs = 0.25)
Q2LC_sp2018 <- quantile(SP2018$NU_NOTA_LC, probs = 0.50)
Q3LC_sp2018 <- quantile(SP2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_sp2018 = hist(SP2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de São Paulo  no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_sp2018, ", desvio padrão:", DPLC_sp2018, ", coeficiente de variação:", CVLC_sp2018, ", primeiro quartil:", Q1LC_sp2018, ", segundo quartil:", Q2LC_sp2018, ", terceiro quartil:", Q3LC_sp2018)

# Sergipe

SE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SE') 
# Média:
mediaLC_se2018 = mean(SE2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_se2018 = sd(SE2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_se2018 = cv(SE2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_se2018 <- quantile(SE2018$NU_NOTA_LC, probs = 0.25)
Q2LC_se2018 <- quantile(SE2018$NU_NOTA_LC, probs = 0.50)
Q3LC_se2018 <- quantile(SE2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_se2018 = hist(SE2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Sergipe no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_se2018, ", desvio padrão:", DPLC_se2018, ", coeficiente de variação:", CVLC_se2018, ", primeiro quartil:", Q1LC_se2018, ", segundo quartil:", Q2LC_se2018, ", terceiro quartil:", Q3LC_se2018)

# Tocantins

TO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='TO') 
# Média:
mediaLC_to2018 = mean(TO2018$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_to2018 = sd(TO2018$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_to2018 = cv(TO2018$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_to2018 <- quantile(TO2018$NU_NOTA_LC, probs = 0.25)
Q2LC_to2018 <- quantile(TO2018$NU_NOTA_LC, probs = 0.50)
Q3LC_to2018 <- quantile(TO2018$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_to2018 = hist(TO2018$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Tocantins no Enem 2018",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota deLinguagens e Códigos do Enem 2018 --> média:", mediaLC_to2018, ", desvio padrão:", DPLC_to2018, ", coeficiente de variação:", CVLC_to2018, ", primeiro quartil:", Q1LC_to2018, ", segundo quartil:", Q2LC_to2018, ", terceiro quartil:", Q3LC_to2018)

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////

# Acre

AC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediaLC_ac2017 = mean(AC2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ac2017 = sd(AC2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ac2017 = cv(AC2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ac2017 <- quantile(AC2017$NU_NOTA_LC, probs = 0.25)
Q2LC_ac2017 <- quantile(AC2017$NU_NOTA_LC, probs = 0.50)
Q3LC_ac2017 <- quantile(AC2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ac2017 = hist(AC2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Acre no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_ac2017, ", desvio padrão:", DPLC_ac2017, ", coeficiente de variação:", CVLC_ac2017, ", primeiro quartil:", Q1LC_ac2017, ", segundo quartil:", Q2LC_ac2017, ", terceiro quartil:", Q3LC_ac2017)

# Alagoas

AL2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AL')
# Média:
mediaLC_al2017 = mean(AL2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_al2017 = sd(AL2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_al2017 = cv(AL2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_al2017 <- quantile(AL2017$NU_NOTA_LC, probs = 0.25)
Q2LC_al2017 <- quantile(AL2017$NU_NOTA_LC, probs = 0.50)
Q3LC_al2017 <- quantile(AL2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_al2017 = hist(AL2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Alagoas no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_al2017, ", desvio padrão:", DPLC_al2017, ", coeficiente de variação:", CVLC_al2017, ", primeiro quartil:", Q1LC_al2017, ", segundo quartil:", Q2LC_al2017, ", terceiro quartil:", Q3LC_al2017)

# Amapá

AP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AP') 
# Média:
mediaLC_ap2017 = mean(AP2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ap2017 = sd(AP2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ap2017 = cv(AP2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ap2017 <- quantile(AP2017$NU_NOTA_LC, probs = 0.25)
Q2LC_ap2017 <- quantile(AP2017$NU_NOTA_LC, probs = 0.50)
Q3LC_ap2017 <- quantile(AP2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ap2017 = hist(AP2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Amapá no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_ap2017, ", desvio padrão:", DPLC_ap2017, ", coeficiente de variação:", CVLC_ap2017, ", primeiro quartil:", Q1LC_ap2017, ", segundo quartil:", Q2LC_ap2017, ", terceiro quartil:", Q3LC_ap2017)

# Amazonas

AM2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AM') 
# Média:
mediaLC_am2017 = mean(AM2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_am2017 = sd(AM2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_am2017 = cv(AM2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_am2017 <- quantile(AM2017$NU_NOTA_LC, probs = 0.25)
Q2LC_am2017 <- quantile(AM2017$NU_NOTA_LC, probs = 0.50)
Q3LC_am2017 <- quantile(AM2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_am2017 = hist(AM2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Amazonas no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_am2017, ", desvio padrão:", DPLC_am2017, ", coeficiente de variação:", CVLC_am2017, ", primeiro quartil:", Q1LC_am2017, ", segundo quartil:", Q2LC_am2017, ", terceiro quartil:", Q3LC_am2017)

# Bahia

BA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='BA') 
# Média:
mediaLC_ba2017 = mean(BA2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ba2017 = sd(BA2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ba2017 = cv(BA2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ba2017 <- quantile(BA2017$NU_NOTA_LC, probs = 0.25)
Q2LC_ba2017 <- quantile(BA2017$NU_NOTA_LC, probs = 0.50)
Q3LC_ba2017 <- quantile(BA2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ba2017 = hist(BA2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Bahia no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_ba2017, ", desvio padrão:", DPLC_ba2017, ", coeficiente de variação:", CVLC_ba2017, ", primeiro quartil:", Q1LC_ba2017, ", segundo quartil:", Q2LC_ba2017, ", terceiro quartil:", Q3LC_ba2017)

# Ceará

CE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='CE') 
# Média:
mediaLC_ce2017 = mean(CE2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ce2017 = sd(CE2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ce2017 = cv(CE2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ce2017 <- quantile(CE2017$NU_NOTA_LC, probs = 0.25)
Q2LC_ce2017 <- quantile(CE2017$NU_NOTA_LC, probs = 0.50)
Q3LC_ce2017 <- quantile(CE2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ce2017 = hist(CE2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Ceará no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_ce2017, ", desvio padrão:", DPLC_ce2017, ", coeficiente de variação:", CVLC_ce2017, ", primeiro quartil:", Q1LC_ce2017, ", segundo quartil:", Q2LC_ce2017, ", terceiro quartil:", Q3LC_ce2017)

# Distrito Federal

DF2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='DF') 
# Média:
mediaLC_df2017 = mean(DF2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_df2017 = sd(DF2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_df2017 = cv(DF2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_df2017 <- quantile(DF2017$NU_NOTA_LC, probs = 0.25)
Q2LC_df2017 <- quantile(DF2017$NU_NOTA_LC, probs = 0.50)
Q3LC_df2017 <- quantile(DF2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_df2017 = hist(DF2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Distrito Federal no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_df2017, ", desvio padrão:", DPLC_df2017, ", coeficiente de variação:", CVLC_df2017, ", primeiro quartil:", Q1LC_df2017, ", segundo quartil:", Q2LC_df2017, ", terceiro quartil:", Q3LC_df2017)

# Espírito Santo

ES2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='ES') 
# Média:
mediaLC_es2017 = mean(ES2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_es2017 = sd(ES2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_es2017 = cv(ES2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_es2017 <- quantile(ES2017$NU_NOTA_LC, probs = 0.25)
Q2LC_es2017 <- quantile(ES2017$NU_NOTA_LC, probs = 0.50)
Q3LC_es2017 <- quantile(ES2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_es2017 = hist(ES2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Espírito Santo no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_es2017, ", desvio padrão:", DPLC_es2017, ", coeficiente de variação:", CVLC_es2017, ", primeiro quartil:", Q1LC_es2017, ", segundo quartil:", Q2LC_es2017, ", terceiro quartil:", Q3LC_es2017)

# Goiás

GO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='GO') 
# Média:
mediaLC_go2017 = mean(GO2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_go2017 = sd(GO2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_go2017 = cv(GO2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_go2017 <- quantile(GO2017$NU_NOTA_LC, probs = 0.25)
Q2LC_go2017 <- quantile(GO2017$NU_NOTA_LC, probs = 0.50)
Q3LC_go2017 <- quantile(GO2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_go2017 = hist(GO2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Goiás no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_go2017, ", desvio padrão:", DPLC_go2017, ", coeficiente de variação:", CVLC_go2017, ", primeiro quartil:", Q1LC_go2017, ", segundo quartil:", Q2LC_go2017, ", terceiro quartil:", Q3LC_go2017)

# Maranhão

MA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MA') 
# Média:
mediaLC_ma2017 = mean(MA2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ma2017 = sd(MA2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ma2017 = cv(MA2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ma2017 <- quantile(MA2017$NU_NOTA_LC, probs = 0.25)
Q2LC_ma2017 <- quantile(MA2017$NU_NOTA_LC, probs = 0.50)
Q3LC_ma2017 <- quantile(MA2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ma2017 = hist(MA2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Maranhão no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_ma2017, ", desvio padrão:", DPLC_ma2017, ", coeficiente de variação:", CVLC_ma2017, ", primeiro quartil:", Q1LC_ma2017, ", segundo quartil:", Q2LC_ma2017, ", terceiro quartil:", Q3LC_ma2017)

# Mato Grosso

MT2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MT') 
# Média:
mediaLC_MT2017 = mean(MT2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_MT2017 = sd(MT2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_MT2017 = cv(MT2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_MT2017 <- quantile(MT2017$NU_NOTA_LC, probs = 0.25)
Q2LC_MT2017 <- quantile(MT2017$NU_NOTA_LC, probs = 0.50)
Q3LC_MT2017 <- quantile(MT2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_MT2017 = hist(MT2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Mato Grosso no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_MT2017, ", desvio padrão:", DPLC_MT2017, ", coeficiente de variação:", CVLC_MT2017, ", primeiro quartil:", Q1LC_MT2017, ", segundo quartil:", Q2LC_MT2017, ", terceiro quartil:", Q3LC_MT2017)

# Mato Grosso do Sul

MS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MS') 
# Média:
mediaLC_ms2017 = mean(MS2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ms2017 = sd(MS2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ms2017 = cv(MS2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ms2017 <- quantile(MS2017$NU_NOTA_LC, probs = 0.25)
Q2LC_ms2017 <- quantile(MS2017$NU_NOTA_LC, probs = 0.50)
Q3LC_ms2017 <- quantile(MS2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ms2017 = hist(MS2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Mato Grosso do Sul no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_ms2017, ", desvio padrão:", DPLC_ms2017, ", coeficiente de variação:", CVLC_ms2017, ", primeiro quartil:", Q1LC_ms2017, ", segundo quartil:", Q2LC_ms2017, ", terceiro quartil:", Q3LC_ms2017)

# Minas Gerais

MG2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MG') 
# Média:
mediaLC_mg2017 = mean(MG2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_mg2017 = sd(MG2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_mg2017 = cv(MG2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_mg2017 <- quantile(MG2017$NU_NOTA_LC, probs = 0.25)
Q2LC_mg2017 <- quantile(MG2017$NU_NOTA_LC, probs = 0.50)
Q3LC_mg2017 <- quantile(MG2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_mg2017 = hist(MG2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Minas Gerais no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_mg2017, ", desvio padrão:", DPLC_mg2017, ", coeficiente de variação:", CVLC_mg2017, ", primeiro quartil:", Q1LC_mg2017, ", segundo quartil:", Q2LC_mg2017, ", terceiro quartil:", Q3LC_mg2017)

# Pará

PA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PA') 
# Média:
mediaLC_pa2017 = mean(PA2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pa2017 = sd(PA2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pa2017 = cv(PA2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pa2017 <- quantile(PA2017$NU_NOTA_LC, probs = 0.25)
Q2LC_pa2017 <- quantile(PA2017$NU_NOTA_LC, probs = 0.50)
Q3LC_pa2017 <- quantile(PA2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pa2017 = hist(PA2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Pará no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_pa2017, ", desvio padrão:", DPLC_pa2017, ", coeficiente de variação:", CVLC_pa2017, ", primeiro quartil:", Q1LC_pa2017, ", segundo quartil:", Q2LC_pa2017, ", terceiro quartil:", Q3LC_pa2017)

# Paraíba

PB2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PB') 
# Média:
mediaLC_pb2017 = mean(PB2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pb2017 = sd(PB2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pb2017 = cv(PB2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pb2017 <- quantile(PB2017$NU_NOTA_LC, probs = 0.25)
Q2LC_pb2017 <- quantile(PB2017$NU_NOTA_LC, probs = 0.50)
Q3LC_pb2017 <- quantile(PB2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pb2017 = hist(PB2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Paraíba no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_pb2017, ", desvio padrão:", DPLC_pb2017, ", coeficiente de variação:", CVLC_pb2017, ", primeiro quartil:", Q1LC_pb2017, ", segundo quartil:", Q2LC_pb2017, ", terceiro quartil:", Q3LC_pb2017)

# Paraná

PR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PR') 
# Média:
mediaLC_pr2017 = mean(PR2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pr2017 = sd(PR2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pr2017 = cv(PR2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pr2017 <- quantile(PR2017$NU_NOTA_LC, probs = 0.25)
Q2LC_pr2017 <- quantile(PR2017$NU_NOTA_LC, probs = 0.50)
Q3LC_pr2017 <- quantile(PR2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pr2017 = hist(PR2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Paraná no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_pr2017, ", desvio padrão:", DPLC_pr2017, ", coeficiente de variação:", CVLC_pr2017, ", primeiro quartil:", Q1LC_pr2017, ", segundo quartil:", Q2LC_pr2017, ", terceiro quartil:", Q3LC_pr2017)

# Pernambuco

PE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PE') 
# Média:
mediaLC_pe2017 = mean(PE2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pe2017 = sd(PE2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pe2017 = cv(PE2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pe2017 <- quantile(PE2017$NU_NOTA_LC, probs = 0.25)
Q2LC_pe2017 <- quantile(PE2017$NU_NOTA_LC, probs = 0.50)
Q3LC_pe2017 <- quantile(PE2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pe2017 = hist(PE2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Pernambuco no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_pe2017, ", desvio padrão:", DPLC_pe2017, ", coeficiente de variação:", CVLC_pe2017, ", peimeiro quartil:", Q1LC_pe2017, ", segundo quartil:", Q2LC_pe2017, ", terceiro quartil:", Q3LC_pe2017)

# Piauí

PI2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PI') 
# Média:
mediaLC_pi2017 = mean(PI2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_pi2017 = sd(PI2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_pi2017 = cv(PI2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_pi2017 <- quantile(PI2017$NU_NOTA_LC, probs = 0.25)
Q2LC_pi2017 <- quantile(PI2017$NU_NOTA_LC, probs = 0.50)
Q3LC_pi2017 <- quantile(PI2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_pi2017 = hist(PI2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Piauí no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_pi2017, ", desvio padrão:", DPLC_pi2017, ", coeficiente de variação:", CVLC_pi2017, ", primeiro quartil:", Q1LC_pi2017, ", segundo quartil:", Q2LC_pi2017, ", terceiro quartil:", Q3LC_pi2017)

# Rio de Janeiro

RJ2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediaLC_rj2017 = mean(RJ2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rj2017 = sd(RJ2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rj2017 = cv(RJ2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rj2017 <- quantile(RJ2017$NU_NOTA_LC, probs = 0.25)
Q2LC_rj2017 <- quantile(RJ2017$NU_NOTA_LC, probs = 0.50)
Q3LC_rj2017 <- quantile(RJ2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rj2017 = hist(RJ2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Rio de Janeiro no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_rj2017, ", desvio padrão:", DPLC_rj2017, ", coeficiente de variação:", CVLC_rj2017, ", primeiro quartil:", Q1LC_rj2017, ", segundo quartil:", Q2LC_rj2017, ", terceiro quartil:", Q3LC_rj2017)

# Rio Grande do Norte

RN2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RN') 
# Média:
mediaLC_rn2017 = mean(RN2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rn2017 = sd(RN2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rn2017 = cv(RN2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rn2017 <- quantile(RN2017$NU_NOTA_LC, probs = 0.25)
Q2LC_rn2017 <- quantile(RN2017$NU_NOTA_LC, probs = 0.50)
Q3LC_rn2017 <- quantile(RN2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rn2017 = hist(RN2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos da Rio Grande do Norte no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_rn2017, ", desvio padrão:", DPLC_rn2017, ", coeficiente de variação:", CVLC_rn2017, ", primeiro quartil:", Q1LC_rn2017, ", segundo quartil:", Q2LC_rn2017, ", terceiro quartil:", Q3LC_rn2017)

# Rio Grande do Sul

RS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RS') 
# Média:
mediaLC_rs2017 = mean(RS2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rs2017 = sd(RS2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rs2017 = cv(RS2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rs2017 <- quantile(RS2017$NU_NOTA_LC, probs = 0.25)
Q2LC_rs2017 <- quantile(RS2017$NU_NOTA_LC, probs = 0.50)
Q3LC_rs2017 <- quantile(RS2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rs2017 = hist(RS2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos do Rio Grande do Sul no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_rs2017, ", desvio padrão:", DPLC_rs2017, ", coeficiente de variação:", CVLC_rs2017, ", primeiro quartil:", Q1LC_rs2017, ", segundo quartil:", Q2LC_rs2017, ", terceira quartil:", Q3LC_rs2017)

# Rondônia

RO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RO') 
# Média:
mediaLC_ro2017 = mean(RO2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_ro2017 = sd(RO2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_ro2017 = cv(RO2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_ro2017 <- quantile(RO2017$NU_NOTA_LC, probs = 0.25)
Q2LC_ro2017 <- quantile(RO2017$NU_NOTA_LC, probs = 0.50)
Q3LC_ro2017 <- quantile(RO2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_ro2017 = hist(RO2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Rondônia no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_ro2017, ", desvio padrão:", DPLC_ro2017, ", coeficiente de variação:", CVLC_ro2017, ", primeiro quartil:", Q1LC_ro2017, ", segundo quartil:", Q2LC_ro2017, ", terceiro quartil:", Q3LC_ro2017)

# Roraima

RR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RR') 
# Média:
mediaLC_rr2017 = mean(RR2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_rr2017 = sd(RR2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_rr2017 = cv(RR2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_rr2017 <- quantile(RR2017$NU_NOTA_LC, probs = 0.25)
Q2LC_rr2017 <- quantile(RR2017$NU_NOTA_LC, probs = 0.50)
Q3LC_rr2017 <- quantile(RR2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_rr2017 = hist(RR2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Roraima no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_rr2017, ", desvio padrão:", DPLC_rr2017, ", coeficiente de variação:", CVLC_rr2017, ", primeiro quartil:", Q1LC_rr2017, ", segundo quartil:", Q2LC_rr2017, ", terceiro quartil:", Q3LC_rr2017)

# Santa Catarina

SC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SC') 
# Média:
mediaLC_sc2017 = mean(SC2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_sc2017 = sd(SC2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_sc2017 = cv(SC2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_sc2017 <- quantile(SC2017$NU_NOTA_LC, probs = 0.25)
Q2LC_sc2017 <- quantile(SC2017$NU_NOTA_LC, probs = 0.50)
Q3LC_sc2017 <- quantile(SC2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_sc2017 = hist(SC2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Santa Catarina  no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_sc2017, ", desvio padrão:", DPLC_sc2017, ", coeficiente de variação:", CVLC_sc2017, ", primeiro quartil:", Q1LC_sc2017, ", segundo quartil:", Q2LC_sc2017, ", terceiro quartil:", Q3LC_sc2017)

# São Paulo

SP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SP') 
# Média:
mediaLC_sp2017 = mean(SP2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_sp2017 = sd(SP2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_sp2017 = cv(SP2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_sp2017 <- quantile(SP2017$NU_NOTA_LC, probs = 0.25)
Q2LC_sp2017 <- quantile(SP2017$NU_NOTA_LC, probs = 0.50)
Q3LC_sp2017 <- quantile(SP2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_sp2017 = hist(SP2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de São Paulo  no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_sp2017, ", desvio padrão:", DPLC_sp2017, ", coeficiente de variação:", CVLC_sp2017, ", primeiro quartil:", Q1LC_sp2017, ", segundo quartil:", Q2LC_sp2017, ", terceiro quartil:", Q3LC_sp2017)

# Sergipe

SE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SE') 
# Média:
mediaLC_se2017 = mean(SE2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_se2017 = sd(SE2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_se2017 = cv(SE2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_se2017 <- quantile(SE2017$NU_NOTA_LC, probs = 0.25)
Q2LC_se2017 <- quantile(SE2017$NU_NOTA_LC, probs = 0.50)
Q3LC_se2017 <- quantile(SE2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_se2017 = hist(SE2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Sergipe no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_se2017, ", desvio padrão:", DPLC_se2017, ", coeficiente de variação:", CVLC_se2017, ", primeiro quartil:", Q1LC_se2017, ", segundo quartil:", Q2LC_se2017, ", terceiro quartil:", Q3LC_se2017)

# Tocantins

TO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='TO') 
# Média:
mediaLC_to2017 = mean(TO2017$NU_NOTA_LC) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPLC_to2017 = sd(TO2017$NU_NOTA_LC)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVLC_to2017 = cv(TO2017$NU_NOTA_LC)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1LC_to2017 <- quantile(TO2017$NU_NOTA_LC, probs = 0.25)
Q2LC_to2017 <- quantile(TO2017$NU_NOTA_LC, probs = 0.50)
Q3LC_to2017 <- quantile(TO2017$NU_NOTA_LC, probs = 0.75)
# Histograma: 
histogramaLC_to2017 = hist(TO2017$NU_NOTA_LC,  
                           main = "Nota deLinguagens e Códigos de Tocantins no Enem 2017",
                           xlab = "Nota deLinguagens e Códigos", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota deLinguagens e Códigos do Enem 2017 --> média:", mediaLC_to2017, ", desvio padrão:", DPLC_to2017, ", coeficiente de variação:", CVLC_to2017, ", primeiro quartil:", Q1LC_to2017, ", segundo quartil:", Q2LC_to2017, ", terceiro quartil:", Q3LC_to2017)

# ------------ NU_Nota_Redacao


#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
library(dplyr)

# Acre

AC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediaRedacao_ac2019 = mean(AC2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ac2019 = sd(AC2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ac2019 = cv(AC2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ac2019 <- quantile(AC2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ac2019 <- quantile(AC2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ac2019 <- quantile(AC2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ac2019 = hist(AC2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Acre no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Acre em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_ac2019, ", desvio padrão:", DPRedacao_ac2019, ", coeficiente de variação:", CVRedacao_ac2019, ", primeiro quartil:", Q1Redacao_ac2019, ", segundo quartil:", Q2Redacao_ac2019, ", terceiro quartil:", Q3Redacao_ac2019)

# Alagoas

AL2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AL')
# Média:
mediaRedacao_al2019 = mean(AL2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_al2019 = sd(AL2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_al2019 = cv(AL2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_al2019 <- quantile(AL2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_al2019 <- quantile(AL2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_al2019 <- quantile(AL2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_al2019 = hist(AL2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Alagoas no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_al2019, ", desvio padrão:", DPRedacao_al2019, ", coeficiente de variação:", CVRedacao_al2019, ", primeiro quartil:", Q1Redacao_al2019, ", segundo quartil:", Q2Redacao_al2019, ", terceiro quartil:", Q3Redacao_al2019)

# Amapá

AP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AP') 
# Média:
mediaRedacao_ap2019 = mean(AP2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ap2019 = sd(AP2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ap2019 = cv(AP2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ap2019 <- quantile(AP2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ap2019 <- quantile(AP2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ap2019 <- quantile(AP2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ap2019 = hist(AP2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Amapá no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Amapá em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_ap2019, ", desvio padrão:", DPRedacao_ap2019, ", coeficiente de variação:", CVRedacao_ap2019, ", primeiro quartil:", Q1Redacao_ap2019, ", segundo quartil:", Q2Redacao_ap2019, ", terceiro quartil:", Q3Redacao_ap2019)

# Amazonas

AM2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AM') 
# Média:
mediaRedacao_am2019 = mean(AM2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_am2019 = sd(AM2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_am2019 = cv(AM2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_am2019 <- quantile(AM2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_am2019 <- quantile(AM2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_am2019 <- quantile(AM2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_am2019 = hist(AM2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Amazonas no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_am2019, ", desvio padrão:", DPRedacao_am2019, ", coeficiente de variação:", CVRedacao_am2019, ", primeiro quartil:", Q1Redacao_am2019, ", segundo quartil:", Q2Redacao_am2019, ", terceiro quartil:", Q3Redacao_am2019)

# Bahia

BA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='BA') 
# Média:
mediaRedacao_ba2019 = mean(BA2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ba2019 = sd(BA2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ba2019 = cv(BA2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ba2019 <- quantile(BA2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ba2019 <- quantile(BA2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ba2019 <- quantile(BA2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ba2019 = hist(BA2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Bahia no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas da Bahia em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_ba2019, ", desvio padrão:", DPRedacao_ba2019, ", coeficiente de variação:", CVRedacao_ba2019, ", primeiro quartil:", Q1Redacao_ba2019, ", segundo quartil:", Q2Redacao_ba2019, ", terceiro quartil:", Q3Redacao_ba2019)

# Ceará

CE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='CE') 
# Média:
mediaRedacao_ce2019 = mean(CE2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ce2019 = sd(CE2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ce2019 = cv(CE2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ce2019 <- quantile(CE2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ce2019 <- quantile(CE2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ce2019 <- quantile(CE2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ce2019 = hist(CE2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Ceará no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Ceará em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_ce2019, ", desvio padrão:", DPRedacao_ce2019, ", coeficiente de variação:", CVRedacao_ce2019, ", primeiro quartil:", Q1Redacao_ce2019, ", segundo quartil:", Q2Redacao_ce2019, ", terceiro quartil:", Q3Redacao_ce2019)

# Distrito Federal

DF2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='DF') 
# Média:
mediaRedacao_df2019 = mean(DF2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_df2019 = sd(DF2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_df2019 = cv(DF2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_df2019 <- quantile(DF2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_df2019 <- quantile(DF2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_df2019 <- quantile(DF2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_df2019 = hist(DF2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Distrito Federal no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_df2019, ", desvio padrão:", DPRedacao_df2019, ", coeficiente de variação:", CVRedacao_df2019, ", primeiro quartil:", Q1Redacao_df2019, ", segundo quartil:", Q2Redacao_df2019, ", terceiro quartil:", Q3Redacao_df2019)

# Espírito Santo

ES2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='ES') 
# Média:
mediaRedacao_es2019 = mean(ES2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_es2019 = sd(ES2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_es2019 = cv(ES2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_es2019 <- quantile(ES2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_es2019 <- quantile(ES2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_es2019 <- quantile(ES2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_es2019 = hist(ES2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Espírito Santo no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_es2019, ", desvio padrão:", DPRedacao_es2019, ", coeficiente de variação:", CVRedacao_es2019, ", primeiro quartil:", Q1Redacao_es2019, ", segundo quartil:", Q2Redacao_es2019, ", terceiro quartil:", Q3Redacao_es2019)

# Goiás

GO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='GO') 
# Média:
mediaRedacao_go2019 = mean(GO2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_go2019 = sd(GO2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_go2019 = cv(GO2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_go2019 <- quantile(GO2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_go2019 <- quantile(GO2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_go2019 <- quantile(GO2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_go2019 = hist(GO2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Goiás no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Goiás em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_go2019, ", desvio padrão:", DPRedacao_go2019, ", coeficiente de variação:", CVRedacao_go2019, ", primeiro quartil:", Q1Redacao_go2019, ", segundo quartil:", Q2Redacao_go2019, ", terceiro quartil:", Q3Redacao_go2019)

# Maranhão

MA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MA') 
# Média:
mediaRedacao_ma2019 = mean(MA2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ma2019 = sd(MA2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ma2019 = cv(MA2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ma2019 <- quantile(MA2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ma2019 <- quantile(MA2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ma2019 <- quantile(MA2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ma2019 = hist(MA2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Maranhão no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_ma2019, ", desvio padrão:", DPRedacao_ma2019, ", coeficiente de variação:", CVRedacao_ma2019, ", primeiro quartil:", Q1Redacao_ma2019, ", segundo quartil:", Q2Redacao_ma2019, ", terceiro quartil:", Q3Redacao_ma2019)

# Mato Grosso

MT2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MT') 
# Média:
mediaRedacao_MT2019 = mean(MT2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_MT2019 = sd(MT2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_MT2019 = cv(MT2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_MT2019 <- quantile(MT2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_MT2019 <- quantile(MT2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_MT2019 <- quantile(MT2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_MT2019 = hist(MT2019$NU_NOTA_REDACAO,  
                                     main = "Nota de Redacao do Mato Grosso no Enem 2019",
                                     xlab = "Nota de Redacao", ylab = "Frequência", 
                                     col = c("violet"), 
                                     border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_MT2019, ", desvio padrão:", DPRedacao_MT2019, ", coeficiente de variação:", CVRedacao_MT2019, ", primeiro quartil:", Q1Redacao_MT2019, ", segundo quartil:", Q2Redacao_MT2019, ", terceiro quartil:", Q3Redacao_MT2019)

# Mato Grosso do Sul

MS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MS') 
# Média:
mediaRedacao_ms2019 = mean(MS2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ms2019 = sd(MS2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ms2019 = cv(MS2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ms2019 <- quantile(MS2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ms2019 <- quantile(MS2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ms2019 <- quantile(MS2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ms2019 = hist(MS2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Mato Grosso do Sul no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_ms2019, ", desvio padrão:", DPRedacao_ms2019, ", coeficiente de variação:", CVRedacao_ms2019, ", primeiro quartil:", Q1Redacao_ms2019, ", segundo quartil:", Q2Redacao_ms2019, ", terceiro quartil:", Q3Redacao_ms2019)

# Minas Gerais

MG2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MG') 
# Média:
mediaRedacao_mg2019 = mean(MG2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_mg2019 = sd(MG2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_mg2019 = cv(MG2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_mg2019 <- quantile(MG2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_mg2019 <- quantile(MG2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_mg2019 <- quantile(MG2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_mg2019 = hist(MG2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Minas Gerais no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_mg2019, ", desvio padrão:", DPRedacao_mg2019, ", coeficiente de variação:", CVRedacao_mg2019, ", primeiro quartil:", Q1Redacao_mg2019, ", segundo quartil:", Q2Redacao_mg2019, ", terceiro quartil:", Q3Redacao_mg2019)

# Pará

PA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PA') 
# Média:
mediaRedacao_pa2019 = mean(PA2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pa2019 = sd(PA2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pa2019 = cv(PA2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pa2019 <- quantile(PA2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pa2019 <- quantile(PA2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pa2019 <- quantile(PA2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pa2019 = hist(PA2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Pará no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Pará em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_pa2019, ", desvio padrão:", DPRedacao_pa2019, ", coeficiente de variação:", CVRedacao_pa2019, ", primeiro quartil:", Q1Redacao_pa2019, ", segundo quartil:", Q2Redacao_pa2019, ", terceiro quartil:", Q3Redacao_pa2019)

# Paraíba

PB2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PB') 
# Média:
mediaRedacao_pb2019 = mean(PB2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pb2019 = sd(PB2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pb2019 = cv(PB2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pb2019 <- quantile(PB2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pb2019 <- quantile(PB2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pb2019 <- quantile(PB2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pb2019 = hist(PB2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Paraíba no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_pb2019, ", desvio padrão:", DPRedacao_pb2019, ", coeficiente de variação:", CVRedacao_pb2019, ", primeiro quartil:", Q1Redacao_pb2019, ", segundo quartil:", Q2Redacao_pb2019, ", terceiro quartil:", Q3Redacao_pb2019)

# Paraná

PR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PR') 
# Média:
mediaRedacao_pr2019 = mean(PR2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pr2019 = sd(PR2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pr2019 = cv(PR2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pr2019 <- quantile(PR2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pr2019 <- quantile(PR2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pr2019 <- quantile(PR2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pr2019 = hist(PR2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Paraná no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Paraná em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_pr2019, ", desvio padrão:", DPRedacao_pr2019, ", coeficiente de variação:", CVRedacao_pr2019, ", primeiro quartil:", Q1Redacao_pr2019, ", segundo quartil:", Q2Redacao_pr2019, ", terceiro quartil:", Q3Redacao_pr2019)

# Pernambuco

PE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PE') 
# Média:
mediaRedacao_pe2019 = mean(PE2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pe2019 = sd(PE2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pe2019 = cv(PE2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pe2019 <- quantile(PE2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pe2019 <- quantile(PE2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pe2019 <- quantile(PE2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pe2019 = hist(PE2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Pernambuco no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_pe2019, ", desvio padrão:", DPRedacao_pe2019, ", coeficiente de variação:", CVRedacao_pe2019, ", peimeiro quartil:", Q1Redacao_pe2019, ", segundo quartil:", Q2Redacao_pe2019, ", terceiro quartil:", Q3Redacao_pe2019)

# Piauí

PI2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PI') 
# Média:
mediaRedacao_pi2019 = mean(PI2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pi2019 = sd(PI2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pi2019 = cv(PI2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pi2019 <- quantile(PI2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pi2019 <- quantile(PI2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pi2019 <- quantile(PI2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pi2019 = hist(PI2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Piauí no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Piauí em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_pi2019, ", desvio padrão:", DPRedacao_pi2019, ", coeficiente de variação:", CVRedacao_pi2019, ", primeiro quartil:", Q1Redacao_pi2019, ", segundo quartil:", Q2Redacao_pi2019, ", terceiro quartil:", Q3Redacao_pi2019)

# Rio de Janeiro

RJ2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediaRedacao_rj2019 = mean(RJ2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rj2019 = sd(RJ2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rj2019 = cv(RJ2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rj2019 <- quantile(RJ2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rj2019 <- quantile(RJ2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rj2019 <- quantile(RJ2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rj2019 = hist(RJ2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Rio de Janeiro no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_rj2019, ", desvio padrão:", DPRedacao_rj2019, ", coeficiente de variação:", CVRedacao_rj2019, ", primeiro quartil:", Q1Redacao_rj2019, ", segundo quartil:", Q2Redacao_rj2019, ", terceiro quartil:", Q3Redacao_rj2019)

# Rio Grande do Norte

RN2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RN') 
# Média:
mediaRedacao_rn2019 = mean(RN2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rn2019 = sd(RN2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rn2019 = cv(RN2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rn2019 <- quantile(RN2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rn2019 <- quantile(RN2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rn2019 <- quantile(RN2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rn2019 = hist(RN2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Rio Grande do Norte no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_rn2019, ", desvio padrão:", DPRedacao_rn2019, ", coeficiente de variação:", CVRedacao_rn2019, ", primeiro quartil:", Q1Redacao_rn2019, ", segundo quartil:", Q2Redacao_rn2019, ", terceiro quartil:", Q3Redacao_rn2019)

# Rio Grande do Sul

RS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RS') 
# Média:
mediaRedacao_rs2019 = mean(RS2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rs2019 = sd(RS2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rs2019 = cv(RS2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rs2019 <- quantile(RS2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rs2019 <- quantile(RS2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rs2019 <- quantile(RS2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rs2019 = hist(RS2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Rio Grande do Sul no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_rs2019, ", desvio padrão:", DPRedacao_rs2019, ", coeficiente de variação:", CVRedacao_rs2019, ", primeiro quartil:", Q1Redacao_rs2019, ", segundo quartil:", Q2Redacao_rs2019, ", terceira quartil:", Q3Redacao_rs2019)

# Rondônia

RO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RO') 
# Média:
mediaRedacao_ro2019 = mean(RO2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ro2019 = sd(RO2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ro2019 = cv(RO2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ro2019 <- quantile(RO2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ro2019 <- quantile(RO2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ro2019 <- quantile(RO2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ro2019 = hist(RO2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Rondônia no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_ro2019, ", desvio padrão:", DPRedacao_ro2019, ", coeficiente de variação:", CVRedacao_ro2019, ", primeiro quartil:", Q1Redacao_ro2019, ", segundo quartil:", Q2Redacao_ro2019, ", terceiro quartil:", Q3Redacao_ro2019)

# Roraima

RR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RR') 
# Média:
mediaRedacao_rr2019 = mean(RR2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rr2019 = sd(RR2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rr2019 = cv(RR2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rr2019 <- quantile(RR2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rr2019 <- quantile(RR2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rr2019 <- quantile(RR2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rr2019 = hist(RR2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Roraima no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Roraima em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_rr2019, ", desvio padrão:", DPRedacao_rr2019, ", coeficiente de variação:", CVRedacao_rr2019, ", primeiro quartil:", Q1Redacao_rr2019, ", segundo quartil:", Q2Redacao_rr2019, ", terceiro quartil:", Q3Redacao_rr2019)

# Santa Catarina

SC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SC') 
# Média:
mediaRedacao_sc2019 = mean(SC2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_sc2019 = sd(SC2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_sc2019 = cv(SC2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_sc2019 <- quantile(SC2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_sc2019 <- quantile(SC2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_sc2019 <- quantile(SC2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_sc2019 = hist(SC2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Santa Catarina  no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_sc2019, ", desvio padrão:", DPRedacao_sc2019, ", coeficiente de variação:", CVRedacao_sc2019, ", primeiro quartil:", Q1Redacao_sc2019, ", segundo quartil:", Q2Redacao_sc2019, ", terceiro quartil:", Q3Redacao_sc2019)

# São Paulo

SP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SP') 
# Média:
mediaRedacao_sp2019 = mean(SP2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_sp2019 = sd(SP2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_sp2019 = cv(SP2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_sp2019 <- quantile(SP2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_sp2019 <- quantile(SP2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_sp2019 <- quantile(SP2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_sp2019 = hist(SP2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de São Paulo  no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_sp2019, ", desvio padrão:", DPRedacao_sp2019, ", coeficiente de variação:", CVRedacao_sp2019, ", primeiro quartil:", Q1Redacao_sp2019, ", segundo quartil:", Q2Redacao_sp2019, ", terceiro quartil:", Q3Redacao_sp2019)

# Sergipe

SE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SE') 
# Média:
mediaRedacao_se2019 = mean(SE2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_se2019 = sd(SE2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_se2019 = cv(SE2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_se2019 <- quantile(SE2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_se2019 <- quantile(SE2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_se2019 <- quantile(SE2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_se2019 = hist(SE2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Sergipe no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_se2019, ", desvio padrão:", DPRedacao_se2019, ", coeficiente de variação:", CVRedacao_se2019, ", primeiro quartil:", Q1Redacao_se2019, ", segundo quartil:", Q2Redacao_se2019, ", terceiro quartil:", Q3Redacao_se2019)

# Tocantins

TO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='TO') 
# Média:
mediaRedacao_to2019 = mean(TO2019$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_to2019 = sd(TO2019$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_to2019 = cv(TO2019$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_to2019 <- quantile(TO2019$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_to2019 <- quantile(TO2019$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_to2019 <- quantile(TO2019$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_to2019 = hist(TO2019$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Tocantins no Enem 2019",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de Redacao do Enem 2019 --> média:", mediaRedacao_to2019, ", desvio padrão:", DPRedacao_to2019, ", coeficiente de variação:", CVRedacao_to2019, ", primeiro quartil:", Q1Redacao_to2019, ", segundo quartil:", Q2Redacao_to2019, ", terceiro quartil:", Q3Redacao_to2019)

#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////

# Acre

AC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediaRedacao_ac2018 = mean(AC2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ac2018 = sd(AC2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ac2018 = cv(AC2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ac2018 <- quantile(AC2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ac2018 <- quantile(AC2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ac2018 <- quantile(AC2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ac2018 = hist(AC2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Acre no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Acre em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_ac2018, ", desvio padrão:", DPRedacao_ac2018, ", coeficiente de variação:", CVRedacao_ac2018, ", primeiro quartil:", Q1Redacao_ac2018, ", segundo quartil:", Q2Redacao_ac2018, ", terceiro quartil:", Q3Redacao_ac2018)

# Alagoas

AL2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AL')
# Média:
mediaRedacao_al2018 = mean(AL2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_al2018 = sd(AL2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_al2018 = cv(AL2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_al2018 <- quantile(AL2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_al2018 <- quantile(AL2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_al2018 <- quantile(AL2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_al2018 = hist(AL2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Alagoas no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_al2018, ", desvio padrão:", DPRedacao_al2018, ", coeficiente de variação:", CVRedacao_al2018, ", primeiro quartil:", Q1Redacao_al2018, ", segundo quartil:", Q2Redacao_al2018, ", terceiro quartil:", Q3Redacao_al2018)

# Amapá

AP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AP') 
# Média:
mediaRedacao_ap2018 = mean(AP2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ap2018 = sd(AP2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ap2018 = cv(AP2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ap2018 <- quantile(AP2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ap2018 <- quantile(AP2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ap2018 <- quantile(AP2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ap2018 = hist(AP2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Amapá no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Amapá em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_ap2018, ", desvio padrão:", DPRedacao_ap2018, ", coeficiente de variação:", CVRedacao_ap2018, ", primeiro quartil:", Q1Redacao_ap2018, ", segundo quartil:", Q2Redacao_ap2018, ", terceiro quartil:", Q3Redacao_ap2018)

# Amazonas

AM2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AM') 
# Média:
mediaRedacao_am2018 = mean(AM2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_am2018 = sd(AM2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_am2018 = cv(AM2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_am2018 <- quantile(AM2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_am2018 <- quantile(AM2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_am2018 <- quantile(AM2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_am2018 = hist(AM2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Amazonas no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_am2018, ", desvio padrão:", DPRedacao_am2018, ", coeficiente de variação:", CVRedacao_am2018, ", primeiro quartil:", Q1Redacao_am2018, ", segundo quartil:", Q2Redacao_am2018, ", terceiro quartil:", Q3Redacao_am2018)

# Bahia

BA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='BA') 
# Média:
mediaRedacao_ba2018 = mean(BA2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ba2018 = sd(BA2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ba2018 = cv(BA2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ba2018 <- quantile(BA2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ba2018 <- quantile(BA2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ba2018 <- quantile(BA2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ba2018 = hist(BA2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Bahia no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas da Bahia em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_ba2018, ", desvio padrão:", DPRedacao_ba2018, ", coeficiente de variação:", CVRedacao_ba2018, ", primeiro quartil:", Q1Redacao_ba2018, ", segundo quartil:", Q2Redacao_ba2018, ", terceiro quartil:", Q3Redacao_ba2018)

# Ceará

CE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='CE') 
# Média:
mediaRedacao_ce2018 = mean(CE2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ce2018 = sd(CE2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ce2018 = cv(CE2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ce2018 <- quantile(CE2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ce2018 <- quantile(CE2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ce2018 <- quantile(CE2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ce2018 = hist(CE2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Ceará no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Ceará em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_ce2018, ", desvio padrão:", DPRedacao_ce2018, ", coeficiente de variação:", CVRedacao_ce2018, ", primeiro quartil:", Q1Redacao_ce2018, ", segundo quartil:", Q2Redacao_ce2018, ", terceiro quartil:", Q3Redacao_ce2018)

# Distrito Federal

DF2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='DF') 
# Média:
mediaRedacao_df2018 = mean(DF2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_df2018 = sd(DF2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_df2018 = cv(DF2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_df2018 <- quantile(DF2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_df2018 <- quantile(DF2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_df2018 <- quantile(DF2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_df2018 = hist(DF2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Distrito Federal no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_df2018, ", desvio padrão:", DPRedacao_df2018, ", coeficiente de variação:", CVRedacao_df2018, ", primeiro quartil:", Q1Redacao_df2018, ", segundo quartil:", Q2Redacao_df2018, ", terceiro quartil:", Q3Redacao_df2018)

# Espírito Santo

ES2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='ES') 
# Média:
mediaRedacao_es2018 = mean(ES2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_es2018 = sd(ES2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_es2018 = cv(ES2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_es2018 <- quantile(ES2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_es2018 <- quantile(ES2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_es2018 <- quantile(ES2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_es2018 = hist(ES2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Espírito Santo no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_es2018, ", desvio padrão:", DPRedacao_es2018, ", coeficiente de variação:", CVRedacao_es2018, ", primeiro quartil:", Q1Redacao_es2018, ", segundo quartil:", Q2Redacao_es2018, ", terceiro quartil:", Q3Redacao_es2018)

# Goiás

GO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='GO') 
# Média:
mediaRedacao_go2018 = mean(GO2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_go2018 = sd(GO2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_go2018 = cv(GO2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_go2018 <- quantile(GO2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_go2018 <- quantile(GO2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_go2018 <- quantile(GO2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_go2018 = hist(GO2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Goiás no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Goiás em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_go2018, ", desvio padrão:", DPRedacao_go2018, ", coeficiente de variação:", CVRedacao_go2018, ", primeiro quartil:", Q1Redacao_go2018, ", segundo quartil:", Q2Redacao_go2018, ", terceiro quartil:", Q3Redacao_go2018)

# Maranhão

MA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MA') 
# Média:
mediaRedacao_ma2018 = mean(MA2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ma2018 = sd(MA2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ma2018 = cv(MA2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ma2018 <- quantile(MA2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ma2018 <- quantile(MA2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ma2018 <- quantile(MA2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ma2018 = hist(MA2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Maranhão no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_ma2018, ", desvio padrão:", DPRedacao_ma2018, ", coeficiente de variação:", CVRedacao_ma2018, ", primeiro quartil:", Q1Redacao_ma2018, ", segundo quartil:", Q2Redacao_ma2018, ", terceiro quartil:", Q3Redacao_ma2018)

# Mato Grosso

MT2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MT') 
# Média:
mediaRedacao_MT2018 = mean(MT2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_MT2018 = sd(MT2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_MT2018 = cv(MT2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_MT2018 <- quantile(MT2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_MT2018 <- quantile(MT2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_MT2018 <- quantile(MT2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_MT2018 = hist(MT2018$NU_NOTA_REDACAO,  
                                     main = "Nota de Redacao do Mato Grosso no Enem 2018",
                                     xlab = "Nota de Redacao", ylab = "Frequência", 
                                     col = c("violet"), 
                                     border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_MT2018, ", desvio padrão:", DPRedacao_MT2018, ", coeficiente de variação:", CVRedacao_MT2018, ", primeiro quartil:", Q1Redacao_MT2018, ", segundo quartil:", Q2Redacao_MT2018, ", terceiro quartil:", Q3Redacao_MT2018)

# Mato Grosso do Sul

MS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MS') 
# Média:
mediaRedacao_ms2018 = mean(MS2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ms2018 = sd(MS2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ms2018 = cv(MS2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ms2018 <- quantile(MS2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ms2018 <- quantile(MS2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ms2018 <- quantile(MS2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ms2018 = hist(MS2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Mato Grosso do Sul no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_ms2018, ", desvio padrão:", DPRedacao_ms2018, ", coeficiente de variação:", CVRedacao_ms2018, ", primeiro quartil:", Q1Redacao_ms2018, ", segundo quartil:", Q2Redacao_ms2018, ", terceiro quartil:", Q3Redacao_ms2018)

# Minas Gerais

MG2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MG') 
# Média:
mediaRedacao_mg2018 = mean(MG2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_mg2018 = sd(MG2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_mg2018 = cv(MG2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_mg2018 <- quantile(MG2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_mg2018 <- quantile(MG2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_mg2018 <- quantile(MG2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_mg2018 = hist(MG2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Minas Gerais no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_mg2018, ", desvio padrão:", DPRedacao_mg2018, ", coeficiente de variação:", CVRedacao_mg2018, ", primeiro quartil:", Q1Redacao_mg2018, ", segundo quartil:", Q2Redacao_mg2018, ", terceiro quartil:", Q3Redacao_mg2018)

# Pará

PA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PA') 
# Média:
mediaRedacao_pa2018 = mean(PA2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pa2018 = sd(PA2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pa2018 = cv(PA2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pa2018 <- quantile(PA2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pa2018 <- quantile(PA2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pa2018 <- quantile(PA2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pa2018 = hist(PA2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Pará no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Pará em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_pa2018, ", desvio padrão:", DPRedacao_pa2018, ", coeficiente de variação:", CVRedacao_pa2018, ", primeiro quartil:", Q1Redacao_pa2018, ", segundo quartil:", Q2Redacao_pa2018, ", terceiro quartil:", Q3Redacao_pa2018)

# Paraíba

PB2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PB') 
# Média:
mediaRedacao_pb2018 = mean(PB2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pb2018 = sd(PB2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pb2018 = cv(PB2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pb2018 <- quantile(PB2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pb2018 <- quantile(PB2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pb2018 <- quantile(PB2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pb2018 = hist(PB2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Paraíba no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_pb2018, ", desvio padrão:", DPRedacao_pb2018, ", coeficiente de variação:", CVRedacao_pb2018, ", primeiro quartil:", Q1Redacao_pb2018, ", segundo quartil:", Q2Redacao_pb2018, ", terceiro quartil:", Q3Redacao_pb2018)

# Paraná

PR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PR') 
# Média:
mediaRedacao_pr2018 = mean(PR2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pr2018 = sd(PR2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pr2018 = cv(PR2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pr2018 <- quantile(PR2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pr2018 <- quantile(PR2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pr2018 <- quantile(PR2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pr2018 = hist(PR2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Paraná no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Paraná em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_pr2018, ", desvio padrão:", DPRedacao_pr2018, ", coeficiente de variação:", CVRedacao_pr2018, ", primeiro quartil:", Q1Redacao_pr2018, ", segundo quartil:", Q2Redacao_pr2018, ", terceiro quartil:", Q3Redacao_pr2018)

# Pernambuco

PE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PE') 
# Média:
mediaRedacao_pe2018 = mean(PE2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pe2018 = sd(PE2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pe2018 = cv(PE2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pe2018 <- quantile(PE2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pe2018 <- quantile(PE2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pe2018 <- quantile(PE2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pe2018 = hist(PE2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Pernambuco no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_pe2018, ", desvio padrão:", DPRedacao_pe2018, ", coeficiente de variação:", CVRedacao_pe2018, ", peimeiro quartil:", Q1Redacao_pe2018, ", segundo quartil:", Q2Redacao_pe2018, ", terceiro quartil:", Q3Redacao_pe2018)

# Piauí

PI2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PI') 
# Média:
mediaRedacao_pi2018 = mean(PI2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pi2018 = sd(PI2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pi2018 = cv(PI2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pi2018 <- quantile(PI2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pi2018 <- quantile(PI2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pi2018 <- quantile(PI2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pi2018 = hist(PI2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Piauí no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Piauí em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_pi2018, ", desvio padrão:", DPRedacao_pi2018, ", coeficiente de variação:", CVRedacao_pi2018, ", primeiro quartil:", Q1Redacao_pi2018, ", segundo quartil:", Q2Redacao_pi2018, ", terceiro quartil:", Q3Redacao_pi2018)

# Rio de Janeiro

RJ2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediaRedacao_rj2018 = mean(RJ2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rj2018 = sd(RJ2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rj2018 = cv(RJ2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rj2018 <- quantile(RJ2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rj2018 <- quantile(RJ2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rj2018 <- quantile(RJ2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rj2018 = hist(RJ2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Rio de Janeiro no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_rj2018, ", desvio padrão:", DPRedacao_rj2018, ", coeficiente de variação:", CVRedacao_rj2018, ", primeiro quartil:", Q1Redacao_rj2018, ", segundo quartil:", Q2Redacao_rj2018, ", terceiro quartil:", Q3Redacao_rj2018)

# Rio Grande do Norte

RN2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RN') 
# Média:
mediaRedacao_rn2018 = mean(RN2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rn2018 = sd(RN2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rn2018 = cv(RN2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rn2018 <- quantile(RN2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rn2018 <- quantile(RN2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rn2018 <- quantile(RN2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rn2018 = hist(RN2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Rio Grande do Norte no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_rn2018, ", desvio padrão:", DPRedacao_rn2018, ", coeficiente de variação:", CVRedacao_rn2018, ", primeiro quartil:", Q1Redacao_rn2018, ", segundo quartil:", Q2Redacao_rn2018, ", terceiro quartil:", Q3Redacao_rn2018)

# Rio Grande do Sul

RS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RS') 
# Média:
mediaRedacao_rs2018 = mean(RS2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rs2018 = sd(RS2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rs2018 = cv(RS2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rs2018 <- quantile(RS2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rs2018 <- quantile(RS2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rs2018 <- quantile(RS2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rs2018 = hist(RS2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Rio Grande do Sul no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_rs2018, ", desvio padrão:", DPRedacao_rs2018, ", coeficiente de variação:", CVRedacao_rs2018, ", primeiro quartil:", Q1Redacao_rs2018, ", segundo quartil:", Q2Redacao_rs2018, ", terceira quartil:", Q3Redacao_rs2018)

# Rondônia

RO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RO') 
# Média:
mediaRedacao_ro2018 = mean(RO2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ro2018 = sd(RO2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ro2018 = cv(RO2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ro2018 <- quantile(RO2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ro2018 <- quantile(RO2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ro2018 <- quantile(RO2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ro2018 = hist(RO2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Rondônia no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_ro2018, ", desvio padrão:", DPRedacao_ro2018, ", coeficiente de variação:", CVRedacao_ro2018, ", primeiro quartil:", Q1Redacao_ro2018, ", segundo quartil:", Q2Redacao_ro2018, ", terceiro quartil:", Q3Redacao_ro2018)

# Roraima

RR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RR') 
# Média:
mediaRedacao_rr2018 = mean(RR2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rr2018 = sd(RR2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rr2018 = cv(RR2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rr2018 <- quantile(RR2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rr2018 <- quantile(RR2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rr2018 <- quantile(RR2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rr2018 = hist(RR2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Roraima no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Roraima em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_rr2018, ", desvio padrão:", DPRedacao_rr2018, ", coeficiente de variação:", CVRedacao_rr2018, ", primeiro quartil:", Q1Redacao_rr2018, ", segundo quartil:", Q2Redacao_rr2018, ", terceiro quartil:", Q3Redacao_rr2018)

# Santa Catarina

SC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SC') 
# Média:
mediaRedacao_sc2018 = mean(SC2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_sc2018 = sd(SC2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_sc2018 = cv(SC2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_sc2018 <- quantile(SC2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_sc2018 <- quantile(SC2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_sc2018 <- quantile(SC2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_sc2018 = hist(SC2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Santa Catarina  no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_sc2018, ", desvio padrão:", DPRedacao_sc2018, ", coeficiente de variação:", CVRedacao_sc2018, ", primeiro quartil:", Q1Redacao_sc2018, ", segundo quartil:", Q2Redacao_sc2018, ", terceiro quartil:", Q3Redacao_sc2018)

# São Paulo

SP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SP') 
# Média:
mediaRedacao_sp2018 = mean(SP2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_sp2018 = sd(SP2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_sp2018 = cv(SP2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_sp2018 <- quantile(SP2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_sp2018 <- quantile(SP2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_sp2018 <- quantile(SP2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_sp2018 = hist(SP2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de São Paulo  no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_sp2018, ", desvio padrão:", DPRedacao_sp2018, ", coeficiente de variação:", CVRedacao_sp2018, ", primeiro quartil:", Q1Redacao_sp2018, ", segundo quartil:", Q2Redacao_sp2018, ", terceiro quartil:", Q3Redacao_sp2018)

# Sergipe

SE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SE') 
# Média:
mediaRedacao_se2018 = mean(SE2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_se2018 = sd(SE2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_se2018 = cv(SE2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_se2018 <- quantile(SE2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_se2018 <- quantile(SE2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_se2018 <- quantile(SE2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_se2018 = hist(SE2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Sergipe no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_se2018, ", desvio padrão:", DPRedacao_se2018, ", coeficiente de variação:", CVRedacao_se2018, ", primeiro quartil:", Q1Redacao_se2018, ", segundo quartil:", Q2Redacao_se2018, ", terceiro quartil:", Q3Redacao_se2018)

# Tocantins

TO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='TO') 
# Média:
mediaRedacao_to2018 = mean(TO2018$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_to2018 = sd(TO2018$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_to2018 = cv(TO2018$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_to2018 <- quantile(TO2018$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_to2018 <- quantile(TO2018$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_to2018 <- quantile(TO2018$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_to2018 = hist(TO2018$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Tocantins no Enem 2018",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de Redacao do Enem 2018 --> média:", mediaRedacao_to2018, ", desvio padrão:", DPRedacao_to2018, ", coeficiente de variação:", CVRedacao_to2018, ", primeiro quartil:", Q1Redacao_to2018, ", segundo quartil:", Q2Redacao_to2018, ", terceiro quartil:", Q3Redacao_to2018)

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////

# Acre

AC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediaRedacao_ac2017 = mean(AC2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ac2017 = sd(AC2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ac2017 = cv(AC2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ac2017 <- quantile(AC2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ac2017 <- quantile(AC2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ac2017 <- quantile(AC2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ac2017 = hist(AC2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Acre no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Acre em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_ac2017, ", desvio padrão:", DPRedacao_ac2017, ", coeficiente de variação:", CVRedacao_ac2017, ", primeiro quartil:", Q1Redacao_ac2017, ", segundo quartil:", Q2Redacao_ac2017, ", terceiro quartil:", Q3Redacao_ac2017)

# Alagoas

AL2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AL')
# Média:
mediaRedacao_al2017 = mean(AL2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_al2017 = sd(AL2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_al2017 = cv(AL2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_al2017 <- quantile(AL2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_al2017 <- quantile(AL2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_al2017 <- quantile(AL2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_al2017 = hist(AL2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Alagoas no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_al2017, ", desvio padrão:", DPRedacao_al2017, ", coeficiente de variação:", CVRedacao_al2017, ", primeiro quartil:", Q1Redacao_al2017, ", segundo quartil:", Q2Redacao_al2017, ", terceiro quartil:", Q3Redacao_al2017)

# Amapá

AP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AP') 
# Média:
mediaRedacao_ap2017 = mean(AP2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ap2017 = sd(AP2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ap2017 = cv(AP2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ap2017 <- quantile(AP2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ap2017 <- quantile(AP2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ap2017 <- quantile(AP2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ap2017 = hist(AP2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Amapá no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Amapá em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_ap2017, ", desvio padrão:", DPRedacao_ap2017, ", coeficiente de variação:", CVRedacao_ap2017, ", primeiro quartil:", Q1Redacao_ap2017, ", segundo quartil:", Q2Redacao_ap2017, ", terceiro quartil:", Q3Redacao_ap2017)

# Amazonas

AM2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AM') 
# Média:
mediaRedacao_am2017 = mean(AM2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_am2017 = sd(AM2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_am2017 = cv(AM2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_am2017 <- quantile(AM2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_am2017 <- quantile(AM2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_am2017 <- quantile(AM2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_am2017 = hist(AM2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Amazonas no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_am2017, ", desvio padrão:", DPRedacao_am2017, ", coeficiente de variação:", CVRedacao_am2017, ", primeiro quartil:", Q1Redacao_am2017, ", segundo quartil:", Q2Redacao_am2017, ", terceiro quartil:", Q3Redacao_am2017)

# Bahia

BA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='BA') 
# Média:
mediaRedacao_ba2017 = mean(BA2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ba2017 = sd(BA2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ba2017 = cv(BA2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ba2017 <- quantile(BA2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ba2017 <- quantile(BA2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ba2017 <- quantile(BA2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ba2017 = hist(BA2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Bahia no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas da Bahia em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_ba2017, ", desvio padrão:", DPRedacao_ba2017, ", coeficiente de variação:", CVRedacao_ba2017, ", primeiro quartil:", Q1Redacao_ba2017, ", segundo quartil:", Q2Redacao_ba2017, ", terceiro quartil:", Q3Redacao_ba2017)

# Ceará

CE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='CE') 
# Média:
mediaRedacao_ce2017 = mean(CE2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ce2017 = sd(CE2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ce2017 = cv(CE2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ce2017 <- quantile(CE2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ce2017 <- quantile(CE2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ce2017 <- quantile(CE2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ce2017 = hist(CE2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Ceará no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Ceará em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_ce2017, ", desvio padrão:", DPRedacao_ce2017, ", coeficiente de variação:", CVRedacao_ce2017, ", primeiro quartil:", Q1Redacao_ce2017, ", segundo quartil:", Q2Redacao_ce2017, ", terceiro quartil:", Q3Redacao_ce2017)

# Distrito Federal

DF2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='DF') 
# Média:
mediaRedacao_df2017 = mean(DF2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_df2017 = sd(DF2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_df2017 = cv(DF2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_df2017 <- quantile(DF2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_df2017 <- quantile(DF2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_df2017 <- quantile(DF2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_df2017 = hist(DF2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Distrito Federal no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_df2017, ", desvio padrão:", DPRedacao_df2017, ", coeficiente de variação:", CVRedacao_df2017, ", primeiro quartil:", Q1Redacao_df2017, ", segundo quartil:", Q2Redacao_df2017, ", terceiro quartil:", Q3Redacao_df2017)

# Espírito Santo

ES2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='ES') 
# Média:
mediaRedacao_es2017 = mean(ES2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_es2017 = sd(ES2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_es2017 = cv(ES2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_es2017 <- quantile(ES2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_es2017 <- quantile(ES2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_es2017 <- quantile(ES2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_es2017 = hist(ES2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Espírito Santo no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_es2017, ", desvio padrão:", DPRedacao_es2017, ", coeficiente de variação:", CVRedacao_es2017, ", primeiro quartil:", Q1Redacao_es2017, ", segundo quartil:", Q2Redacao_es2017, ", terceiro quartil:", Q3Redacao_es2017)

# Goiás

GO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='GO') 
# Média:
mediaRedacao_go2017 = mean(GO2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_go2017 = sd(GO2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_go2017 = cv(GO2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_go2017 <- quantile(GO2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_go2017 <- quantile(GO2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_go2017 <- quantile(GO2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_go2017 = hist(GO2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Goiás no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Goiás em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_go2017, ", desvio padrão:", DPRedacao_go2017, ", coeficiente de variação:", CVRedacao_go2017, ", primeiro quartil:", Q1Redacao_go2017, ", segundo quartil:", Q2Redacao_go2017, ", terceiro quartil:", Q3Redacao_go2017)

# Maranhão

MA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MA') 
# Média:
mediaRedacao_ma2017 = mean(MA2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ma2017 = sd(MA2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ma2017 = cv(MA2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ma2017 <- quantile(MA2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ma2017 <- quantile(MA2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ma2017 <- quantile(MA2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ma2017 = hist(MA2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Maranhão no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_ma2017, ", desvio padrão:", DPRedacao_ma2017, ", coeficiente de variação:", CVRedacao_ma2017, ", primeiro quartil:", Q1Redacao_ma2017, ", segundo quartil:", Q2Redacao_ma2017, ", terceiro quartil:", Q3Redacao_ma2017)

# Mato Grosso

MT2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MT') 
# Média:
mediaRedacao_MT2017 = mean(MT2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_MT2017 = sd(MT2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_MT2017 = cv(MT2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_MT2017 <- quantile(MT2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_MT2017 <- quantile(MT2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_MT2017 <- quantile(MT2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_MT2017 = hist(MT2017$NU_NOTA_REDACAO,  
                                     main = "Nota de Redacao do Mato Grosso no Enem 2017",
                                     xlab = "Nota de Redacao", ylab = "Frequência", 
                                     col = c("violet"), 
                                     border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_MT2017, ", desvio padrão:", DPRedacao_MT2017, ", coeficiente de variação:", CVRedacao_MT2017, ", primeiro quartil:", Q1Redacao_MT2017, ", segundo quartil:", Q2Redacao_MT2017, ", terceiro quartil:", Q3Redacao_MT2017)

# Mato Grosso do Sul

MS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MS') 
# Média:
mediaRedacao_ms2017 = mean(MS2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ms2017 = sd(MS2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ms2017 = cv(MS2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ms2017 <- quantile(MS2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ms2017 <- quantile(MS2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ms2017 <- quantile(MS2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ms2017 = hist(MS2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Mato Grosso do Sul no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_ms2017, ", desvio padrão:", DPRedacao_ms2017, ", coeficiente de variação:", CVRedacao_ms2017, ", primeiro quartil:", Q1Redacao_ms2017, ", segundo quartil:", Q2Redacao_ms2017, ", terceiro quartil:", Q3Redacao_ms2017)

# Minas Gerais

MG2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MG') 
# Média:
mediaRedacao_mg2017 = mean(MG2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_mg2017 = sd(MG2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_mg2017 = cv(MG2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_mg2017 <- quantile(MG2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_mg2017 <- quantile(MG2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_mg2017 <- quantile(MG2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_mg2017 = hist(MG2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Minas Gerais no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_mg2017, ", desvio padrão:", DPRedacao_mg2017, ", coeficiente de variação:", CVRedacao_mg2017, ", primeiro quartil:", Q1Redacao_mg2017, ", segundo quartil:", Q2Redacao_mg2017, ", terceiro quartil:", Q3Redacao_mg2017)

# Pará

PA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PA') 
# Média:
mediaRedacao_pa2017 = mean(PA2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pa2017 = sd(PA2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pa2017 = cv(PA2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pa2017 <- quantile(PA2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pa2017 <- quantile(PA2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pa2017 <- quantile(PA2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pa2017 = hist(PA2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Pará no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Pará em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_pa2017, ", desvio padrão:", DPRedacao_pa2017, ", coeficiente de variação:", CVRedacao_pa2017, ", primeiro quartil:", Q1Redacao_pa2017, ", segundo quartil:", Q2Redacao_pa2017, ", terceiro quartil:", Q3Redacao_pa2017)

# Paraíba

PB2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PB') 
# Média:
mediaRedacao_pb2017 = mean(PB2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pb2017 = sd(PB2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pb2017 = cv(PB2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pb2017 <- quantile(PB2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pb2017 <- quantile(PB2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pb2017 <- quantile(PB2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pb2017 = hist(PB2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Paraíba no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_pb2017, ", desvio padrão:", DPRedacao_pb2017, ", coeficiente de variação:", CVRedacao_pb2017, ", primeiro quartil:", Q1Redacao_pb2017, ", segundo quartil:", Q2Redacao_pb2017, ", terceiro quartil:", Q3Redacao_pb2017)

# Paraná

PR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PR') 
# Média:
mediaRedacao_pr2017 = mean(PR2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pr2017 = sd(PR2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pr2017 = cv(PR2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pr2017 <- quantile(PR2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pr2017 <- quantile(PR2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pr2017 <- quantile(PR2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pr2017 = hist(PR2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Paraná no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas do Paraná em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_pr2017, ", desvio padrão:", DPRedacao_pr2017, ", coeficiente de variação:", CVRedacao_pr2017, ", primeiro quartil:", Q1Redacao_pr2017, ", segundo quartil:", Q2Redacao_pr2017, ", terceiro quartil:", Q3Redacao_pr2017)

# Pernambuco

PE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PE') 
# Média:
mediaRedacao_pe2017 = mean(PE2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pe2017 = sd(PE2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pe2017 = cv(PE2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pe2017 <- quantile(PE2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pe2017 <- quantile(PE2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pe2017 <- quantile(PE2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pe2017 = hist(PE2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Pernambuco no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_pe2017, ", desvio padrão:", DPRedacao_pe2017, ", coeficiente de variação:", CVRedacao_pe2017, ", peimeiro quartil:", Q1Redacao_pe2017, ", segundo quartil:", Q2Redacao_pe2017, ", terceiro quartil:", Q3Redacao_pe2017)

# Piauí

PI2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PI') 
# Média:
mediaRedacao_pi2017 = mean(PI2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_pi2017 = sd(PI2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_pi2017 = cv(PI2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_pi2017 <- quantile(PI2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_pi2017 <- quantile(PI2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_pi2017 <- quantile(PI2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_pi2017 = hist(PI2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Piauí no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Piauí em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_pi2017, ", desvio padrão:", DPRedacao_pi2017, ", coeficiente de variação:", CVRedacao_pi2017, ", primeiro quartil:", Q1Redacao_pi2017, ", segundo quartil:", Q2Redacao_pi2017, ", terceiro quartil:", Q3Redacao_pi2017)

# Rio de Janeiro

RJ2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediaRedacao_rj2017 = mean(RJ2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rj2017 = sd(RJ2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rj2017 = cv(RJ2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rj2017 <- quantile(RJ2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rj2017 <- quantile(RJ2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rj2017 <- quantile(RJ2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rj2017 = hist(RJ2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Rio de Janeiro no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_rj2017, ", desvio padrão:", DPRedacao_rj2017, ", coeficiente de variação:", CVRedacao_rj2017, ", primeiro quartil:", Q1Redacao_rj2017, ", segundo quartil:", Q2Redacao_rj2017, ", terceiro quartil:", Q3Redacao_rj2017)

# Rio Grande do Norte

RN2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RN') 
# Média:
mediaRedacao_rn2017 = mean(RN2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rn2017 = sd(RN2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rn2017 = cv(RN2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rn2017 <- quantile(RN2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rn2017 <- quantile(RN2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rn2017 <- quantile(RN2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rn2017 = hist(RN2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao da Rio Grande do Norte no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_rn2017, ", desvio padrão:", DPRedacao_rn2017, ", coeficiente de variação:", CVRedacao_rn2017, ", primeiro quartil:", Q1Redacao_rn2017, ", segundo quartil:", Q2Redacao_rn2017, ", terceiro quartil:", Q3Redacao_rn2017)

# Rio Grande do Sul

RS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RS') 
# Média:
mediaRedacao_rs2017 = mean(RS2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rs2017 = sd(RS2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rs2017 = cv(RS2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rs2017 <- quantile(RS2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rs2017 <- quantile(RS2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rs2017 <- quantile(RS2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rs2017 = hist(RS2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao do Rio Grande do Sul no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_rs2017, ", desvio padrão:", DPRedacao_rs2017, ", coeficiente de variação:", CVRedacao_rs2017, ", primeiro quartil:", Q1Redacao_rs2017, ", segundo quartil:", Q2Redacao_rs2017, ", terceira quartil:", Q3Redacao_rs2017)

# Rondônia

RO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RO') 
# Média:
mediaRedacao_ro2017 = mean(RO2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_ro2017 = sd(RO2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_ro2017 = cv(RO2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_ro2017 <- quantile(RO2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_ro2017 <- quantile(RO2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_ro2017 <- quantile(RO2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_ro2017 = hist(RO2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Rondônia no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_ro2017, ", desvio padrão:", DPRedacao_ro2017, ", coeficiente de variação:", CVRedacao_ro2017, ", primeiro quartil:", Q1Redacao_ro2017, ", segundo quartil:", Q2Redacao_ro2017, ", terceiro quartil:", Q3Redacao_ro2017)

# Roraima

RR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RR') 
# Média:
mediaRedacao_rr2017 = mean(RR2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_rr2017 = sd(RR2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_rr2017 = cv(RR2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_rr2017 <- quantile(RR2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_rr2017 <- quantile(RR2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_rr2017 <- quantile(RR2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_rr2017 = hist(RR2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Roraima no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Roraima em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_rr2017, ", desvio padrão:", DPRedacao_rr2017, ", coeficiente de variação:", CVRedacao_rr2017, ", primeiro quartil:", Q1Redacao_rr2017, ", segundo quartil:", Q2Redacao_rr2017, ", terceiro quartil:", Q3Redacao_rr2017)

# Santa Catarina

SC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SC') 
# Média:
mediaRedacao_sc2017 = mean(SC2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_sc2017 = sd(SC2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_sc2017 = cv(SC2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_sc2017 <- quantile(SC2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_sc2017 <- quantile(SC2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_sc2017 <- quantile(SC2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_sc2017 = hist(SC2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Santa Catarina  no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_sc2017, ", desvio padrão:", DPRedacao_sc2017, ", coeficiente de variação:", CVRedacao_sc2017, ", primeiro quartil:", Q1Redacao_sc2017, ", segundo quartil:", Q2Redacao_sc2017, ", terceiro quartil:", Q3Redacao_sc2017)

# São Paulo

SP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SP') 
# Média:
mediaRedacao_sp2017 = mean(SP2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_sp2017 = sd(SP2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_sp2017 = cv(SP2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_sp2017 <- quantile(SP2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_sp2017 <- quantile(SP2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_sp2017 <- quantile(SP2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_sp2017 = hist(SP2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de São Paulo  no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_sp2017, ", desvio padrão:", DPRedacao_sp2017, ", coeficiente de variação:", CVRedacao_sp2017, ", primeiro quartil:", Q1Redacao_sp2017, ", segundo quartil:", Q2Redacao_sp2017, ", terceiro quartil:", Q3Redacao_sp2017)

# Sergipe

SE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SE') 
# Média:
mediaRedacao_se2017 = mean(SE2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_se2017 = sd(SE2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_se2017 = cv(SE2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_se2017 <- quantile(SE2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_se2017 <- quantile(SE2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_se2017 <- quantile(SE2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_se2017 = hist(SE2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Sergipe no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_se2017, ", desvio padrão:", DPRedacao_se2017, ", coeficiente de variação:", CVRedacao_se2017, ", primeiro quartil:", Q1Redacao_se2017, ", segundo quartil:", Q2Redacao_se2017, ", terceiro quartil:", Q3Redacao_se2017)

# Tocantins

TO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='TO') 
# Média:
mediaRedacao_to2017 = mean(TO2017$NU_NOTA_REDACAO) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPRedacao_to2017 = sd(TO2017$NU_NOTA_REDACAO)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVRedacao_to2017 = cv(TO2017$NU_NOTA_REDACAO)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1Redacao_to2017 <- quantile(TO2017$NU_NOTA_REDACAO, probs = 0.25)
Q2Redacao_to2017 <- quantile(TO2017$NU_NOTA_REDACAO, probs = 0.50)
Q3Redacao_to2017 <- quantile(TO2017$NU_NOTA_REDACAO, probs = 0.75)
# Histograma: 
histogramaRedacao_to2017 = hist(TO2017$NU_NOTA_REDACAO,  
                                main = "Nota de Redacao de Tocantins no Enem 2017",
                                xlab = "Nota de Redacao", ylab = "Frequência", 
                                col = c("violet"), 
                                border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de Redacao do Enem 2017 --> média:", mediaRedacao_to2017, ", desvio padrão:", DPRedacao_to2017, ", coeficiente de variação:", CVRedacao_to2017, ", primeiro quartil:", Q1Redacao_to2017, ", segundo quartil:", Q2Redacao_to2017, ", terceiro quartil:", Q3Redacao_to2017)
