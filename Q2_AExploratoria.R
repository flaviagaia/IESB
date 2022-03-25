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

#2- Faça uma análise da variável NU_Nota_MT, agora por Unidade da Federação da escola (SG_UF_RESIDENCIA).
#Calcule estatísticas de resumo, o histograma e coloque títulos em suas análises.

#/////////////////////////////////////////////////// 2019 /////////////////////////////////////////////////////////////
library(dplyr)

# Acre

AC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediamt_ac2019 = mean(AC2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ac2019 = sd(AC2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ac2019 = cv(AC2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ac2019 <- quantile(AC2019$NU_NOTA_MT, probs = 0.25)
Q2mt_ac2019 <- quantile(AC2019$NU_NOTA_MT, probs = 0.50)
Q3mt_ac2019 <- quantile(AC2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ac2019 = hist(AC2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Acre no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota de matemática do Enem 2019 --> média:", mediamt_ac2019, ", desvio padrão:", DPmt_ac2019, ", coeficiente de variação:", CVmt_ac2019, ", primeiro quartil:", Q1mt_ac2019, ", segundo quartil:", Q2mt_ac2019, ", terceiro quartil:", Q3mt_ac2019)

# Alagoas

AL2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AL')
# Média:
mediamt_al2019 = mean(AL2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_al2019 = sd(AL2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_al2019 = cv(AL2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_al2019 <- quantile(AL2019$NU_NOTA_MT, probs = 0.25)
Q2mt_al2019 <- quantile(AL2019$NU_NOTA_MT, probs = 0.50)
Q3mt_al2019 <- quantile(AL2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_al2019 = hist(AL2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Alagoas no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de matemática do Enem 2019 --> média:", mediamt_al2019, ", desvio padrão:", DPmt_al2019, ", coeficiente de variação:", CVmt_al2019, ", primeiro quartil:", Q1mt_al2019, ", segundo quartil:", Q2mt_al2019, ", terceiro quartil:", Q3mt_al2019)

# Amapá

AP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AP') 
# Média:
mediamt_ap2019 = mean(AP2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ap2019 = sd(AP2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ap2019 = cv(AP2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ap2019 <- quantile(AP2019$NU_NOTA_MT, probs = 0.25)
Q2mt_ap2019 <- quantile(AP2019$NU_NOTA_MT, probs = 0.50)
Q3mt_ap2019 <- quantile(AP2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ap2019 = hist(AP2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Amapá no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota de matemática do Enem 2019 --> média:", mediamt_ap2019, ", desvio padrão:", DPmt_ap2019, ", coeficiente de variação:", CVmt_ap2019, ", primeiro quartil:", Q1mt_ap2019, ", segundo quartil:", Q2mt_ap2019, ", terceiro quartil:", Q3mt_ap2019)

# Amazonas

AM2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='AM') 
# Média:
mediamt_am2019 = mean(AM2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_am2019 = sd(AM2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_am2019 = cv(AM2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_am2019 <- quantile(AM2019$NU_NOTA_MT, probs = 0.25)
Q2mt_am2019 <- quantile(AM2019$NU_NOTA_MT, probs = 0.50)
Q3mt_am2019 <- quantile(AM2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_am2019 = hist(AM2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Amazonas no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de matemática do Enem 2019 --> média:", mediamt_am2019, ", desvio padrão:", DPmt_am2019, ", coeficiente de variação:", CVmt_am2019, ", primeiro quartil:", Q1mt_am2019, ", segundo quartil:", Q2mt_am2019, ", terceiro quartil:", Q3mt_am2019)

# Bahia

BA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='BA') 
# Média:
mediamt_ba2019 = mean(BA2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ba2019 = sd(BA2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ba2019 = cv(BA2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ba2019 <- quantile(BA2019$NU_NOTA_MT, probs = 0.25)
Q2mt_ba2019 <- quantile(BA2019$NU_NOTA_MT, probs = 0.50)
Q3mt_ba2019 <- quantile(BA2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ba2019 = hist(BA2019$NU_NOTA_MT,  
                           main = "Nota de Matemática da Bahia no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota de matemática do Enem 2019 --> média:", mediamt_ba2019, ", desvio padrão:", DPmt_ba2019, ", coeficiente de variação:", CVmt_ba2019, ", primeiro quartil:", Q1mt_ba2019, ", segundo quartil:", Q2mt_ba2019, ", terceiro quartil:", Q3mt_ba2019)

# Ceará

CE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='CE') 
# Média:
mediamt_ce2019 = mean(CE2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ce2019 = sd(CE2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ce2019 = cv(CE2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ce2019 <- quantile(CE2019$NU_NOTA_MT, probs = 0.25)
Q2mt_ce2019 <- quantile(CE2019$NU_NOTA_MT, probs = 0.50)
Q3mt_ce2019 <- quantile(CE2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ce2019 = hist(CE2019$NU_NOTA_MT,  
                           main = "Nota de Matemática da Ceará no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota de matemática do Enem 2019 --> média:", mediamt_ce2019, ", desvio padrão:", DPmt_ce2019, ", coeficiente de variação:", CVmt_ce2019, ", primeiro quartil:", Q1mt_ce2019, ", segundo quartil:", Q2mt_ce2019, ", terceiro quartil:", Q3mt_ce2019)

# Distrito Federal

DF2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='DF') 
# Média:
mediamt_df2019 = mean(DF2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_df2019 = sd(DF2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_df2019 = cv(DF2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_df2019 <- quantile(DF2019$NU_NOTA_MT, probs = 0.25)
Q2mt_df2019 <- quantile(DF2019$NU_NOTA_MT, probs = 0.50)
Q3mt_df2019 <- quantile(DF2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_df2019 = hist(DF2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Distrito Federal no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de matemática do Enem 2019 --> média:", mediamt_df2019, ", desvio padrão:", DPmt_df2019, ", coeficiente de variação:", CVmt_df2019, ", primeiro quartil:", Q1mt_df2019, ", segundo quartil:", Q2mt_df2019, ", terceiro quartil:", Q3mt_df2019)

# Espírito Santo

ES2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='ES') 
# Média:
mediamt_es2019 = mean(ES2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_es2019 = sd(ES2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_es2019 = cv(ES2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_es2019 <- quantile(ES2019$NU_NOTA_MT, probs = 0.25)
Q2mt_es2019 <- quantile(ES2019$NU_NOTA_MT, probs = 0.50)
Q3mt_es2019 <- quantile(ES2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_es2019 = hist(ES2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Espírito Santo no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de matemática do Enem 2019 --> média:", mediamt_es2019, ", desvio padrão:", DPmt_es2019, ", coeficiente de variação:", CVmt_es2019, ", primeiro quartil:", Q1mt_es2019, ", segundo quartil:", Q2mt_es2019, ", terceiro quartil:", Q3mt_es2019)

# Goiás

GO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='GO') 
# Média:
mediamt_go2019 = mean(GO2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_go2019 = sd(GO2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_go2019 = cv(GO2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_go2019 <- quantile(GO2019$NU_NOTA_MT, probs = 0.25)
Q2mt_go2019 <- quantile(GO2019$NU_NOTA_MT, probs = 0.50)
Q3mt_go2019 <- quantile(GO2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_go2019 = hist(GO2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Goiás no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota de matemática do Enem 2019 --> média:", mediamt_go2019, ", desvio padrão:", DPmt_go2019, ", coeficiente de variação:", CVmt_go2019, ", primeiro quartil:", Q1mt_go2019, ", segundo quartil:", Q2mt_go2019, ", terceiro quartil:", Q3mt_go2019)

# Maranhão

MA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MA') 
# Média:
mediamt_ma2019 = mean(MA2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ma2019 = sd(MA2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ma2019 = cv(MA2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ma2019 <- quantile(MA2019$NU_NOTA_MT, probs = 0.25)
Q2mt_ma2019 <- quantile(MA2019$NU_NOTA_MT, probs = 0.50)
Q3mt_ma2019 <- quantile(MA2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ma2019 = hist(MA2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Maranhão no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de matemática do Enem 2019 --> média:", mediamt_ma2019, ", desvio padrão:", DPmt_ma2019, ", coeficiente de variação:", CVmt_ma2019, ", primeiro quartil:", Q1mt_ma2019, ", segundo quartil:", Q2mt_ma2019, ", terceiro quartil:", Q3mt_ma2019)

# Mato Grosso

MT2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MT') 
# Média:
mediamt_mt2019 = mean(MT2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_mt2019 = sd(MT2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_mt2019 = cv(MT2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_mt2019 <- quantile(MT2019$NU_NOTA_MT, probs = 0.25)
Q2mt_mt2019 <- quantile(MT2019$NU_NOTA_MT, probs = 0.50)
Q3mt_mt2019 <- quantile(MT2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_mt2019 = hist(MT2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Mato Grosso no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de matemática do Enem 2019 --> média:", mediamt_mt2019, ", desvio padrão:", DPmt_mt2019, ", coeficiente de variação:", CVmt_mt2019, ", primeiro quartil:", Q1mt_mt2019, ", segundo quartil:", Q2mt_mt2019, ", terceiro quartil:", Q3mt_mt2019)

# Mato Grosso do Sul

MS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MS') 
# Média:
mediamt_ms2019 = mean(MS2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ms2019 = sd(MS2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ms2019 = cv(MS2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ms2019 <- quantile(MS2019$NU_NOTA_MT, probs = 0.25)
Q2mt_ms2019 <- quantile(MS2019$NU_NOTA_MT, probs = 0.50)
Q3mt_ms2019 <- quantile(MS2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ms2019 = hist(MS2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Mato Grosso do Sul no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de matemática do Enem 2019 --> média:", mediamt_ms2019, ", desvio padrão:", DPmt_ms2019, ", coeficiente de variação:", CVmt_ms2019, ", primeiro quartil:", Q1mt_ms2019, ", segundo quartil:", Q2mt_ms2019, ", terceiro quartil:", Q3mt_ms2019)

# Minas Gerais

MG2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='MG') 
# Média:
mediamt_mg2019 = mean(MG2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_mg2019 = sd(MG2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_mg2019 = cv(MG2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_mg2019 <- quantile(MG2019$NU_NOTA_MT, probs = 0.25)
Q2mt_mg2019 <- quantile(MG2019$NU_NOTA_MT, probs = 0.50)
Q3mt_mg2019 <- quantile(MG2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_mg2019 = hist(MG2019$NU_NOTA_MT,  
                           main = "Nota de Matemática de Minas Gerais no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de matemática do Enem 2019 --> média:", mediamt_mg2019, ", desvio padrão:", DPmt_mg2019, ", coeficiente de variação:", CVmt_mg2019, ", primeiro quartil:", Q1mt_mg2019, ", segundo quartil:", Q2mt_mg2019, ", terceiro quartil:", Q3mt_mg2019)

# Pará

PA2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PA') 
# Média:
mediamt_pa2019 = mean(PA2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pa2019 = sd(PA2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pa2019 = cv(PA2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pa2019 <- quantile(PA2019$NU_NOTA_MT, probs = 0.25)
Q2mt_pa2019 <- quantile(PA2019$NU_NOTA_MT, probs = 0.50)
Q3mt_pa2019 <- quantile(PA2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pa2019 = hist(PA2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Pará no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota de matemática do Enem 2019 --> média:", mediamt_pa2019, ", desvio padrão:", DPmt_pa2019, ", coeficiente de variação:", CVmt_pa2019, ", primeiro quartil:", Q1mt_pa2019, ", segundo quartil:", Q2mt_pa2019, ", terceiro quartil:", Q3mt_pa2019)

# Paraíba

PB2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PB') 
# Média:
mediamt_pb2019 = mean(PB2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pb2019 = sd(PB2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pb2019 = cv(PB2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pb2019 <- quantile(PB2019$NU_NOTA_MT, probs = 0.25)
Q2mt_pb2019 <- quantile(PB2019$NU_NOTA_MT, probs = 0.50)
Q3mt_pb2019 <- quantile(PB2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pb2019 = hist(PB2019$NU_NOTA_MT,  
                           main = "Nota de Matemática da Paraíba no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de matemática do Enem 2019 --> média:", mediamt_pb2019, ", desvio padrão:", DPmt_pb2019, ", coeficiente de variação:", CVmt_pb2019, ", primeiro quartil:", Q1mt_pb2019, ", segundo quartil:", Q2mt_pb2019, ", terceiro quartil:", Q3mt_pb2019)

# Paraná

PR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PR') 
# Média:
mediamt_pr2019 = mean(PR2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pr2019 = sd(PR2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pr2019 = cv(PR2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pr2019 <- quantile(PR2019$NU_NOTA_MT, probs = 0.25)
Q2mt_pr2019 <- quantile(PR2019$NU_NOTA_MT, probs = 0.50)
Q3mt_pr2019 <- quantile(PR2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pr2019 = hist(PR2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Paraná no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota de matemática do Enem 2019 --> média:", mediamt_pr2019, ", desvio padrão:", DPmt_pr2019, ", coeficiente de variação:", CVmt_pr2019, ", primeiro quartil:", Q1mt_pr2019, ", segundo quartil:", Q2mt_pr2019, ", terceiro quartil:", Q3mt_pr2019)

# Pernambuco

PE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PE') 
# Média:
mediamt_pe2019 = mean(PE2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pe2019 = sd(PE2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pe2019 = cv(PE2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pe2019 <- quantile(PE2019$NU_NOTA_MT, probs = 0.25)
Q2mt_pe2019 <- quantile(PE2019$NU_NOTA_MT, probs = 0.50)
Q3mt_pe2019 <- quantile(PE2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pe2019 = hist(PE2019$NU_NOTA_MT,  
                           main = "Nota de Matemática de Pernambuco no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de matemática do Enem 2019 --> média:", mediamt_pe2019, ", desvio padrão:", DPmt_pe2019, ", coeficiente de variação:", CVmt_pe2019, ", peimeiro quartil:", Q1mt_pe2019, ", segundo quartil:", Q2mt_pe2019, ", terceiro quartil:", Q3mt_pe2019)

# Piauí

PI2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='PI') 
# Média:
mediamt_pi2019 = mean(PI2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pi2019 = sd(PI2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pi2019 = cv(PI2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pi2019 <- quantile(PI2019$NU_NOTA_MT, probs = 0.25)
Q2mt_pi2019 <- quantile(PI2019$NU_NOTA_MT, probs = 0.50)
Q3mt_pi2019 <- quantile(PI2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pi2019 = hist(PI2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Piauí no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota de matemática do Enem 2019 --> média:", mediamt_pi2019, ", desvio padrão:", DPmt_pi2019, ", coeficiente de variação:", CVmt_pi2019, ", primeiro quartil:", Q1mt_pi2019, ", segundo quartil:", Q2mt_pi2019, ", terceiro quartil:", Q3mt_pi2019)

# Rio de Janeiro

RJ2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediamt_rj2019 = mean(RJ2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rj2019 = sd(RJ2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rj2019 = cv(RJ2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rj2019 <- quantile(RJ2019$NU_NOTA_MT, probs = 0.25)
Q2mt_rj2019 <- quantile(RJ2019$NU_NOTA_MT, probs = 0.50)
Q3mt_rj2019 <- quantile(RJ2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rj2019 = hist(RJ2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Rio de Janeiro no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de matemática do Enem 2019 --> média:", mediamt_rj2019, ", desvio padrão:", DPmt_rj2019, ", coeficiente de variação:", CVmt_rj2019, ", primeiro quartil:", Q1mt_rj2019, ", segundo quartil:", Q2mt_rj2019, ", terceiro quartil:", Q3mt_rj2019)

# Rio Grande do Norte

RN2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RN') 
# Média:
mediamt_rn2019 = mean(RN2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rn2019 = sd(RN2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rn2019 = cv(RN2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rn2019 <- quantile(RN2019$NU_NOTA_MT, probs = 0.25)
Q2mt_rn2019 <- quantile(RN2019$NU_NOTA_MT, probs = 0.50)
Q3mt_rn2019 <- quantile(RN2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rn2019 = hist(RN2019$NU_NOTA_MT,  
                           main = "Nota de Matemática da Rio Grande do Norte no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de matemática do Enem 2019 --> média:", mediamt_rn2019, ", desvio padrão:", DPmt_rn2019, ", coeficiente de variação:", CVmt_rn2019, ", primeiro quartil:", Q1mt_rn2019, ", segundo quartil:", Q2mt_rn2019, ", terceiro quartil:", Q3mt_rn2019)

# Rio Grande do Sul

RS2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RS') 
# Média:
mediamt_rs2019 = mean(RS2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rs2019 = sd(RS2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rs2019 = cv(RS2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rs2019 <- quantile(RS2019$NU_NOTA_MT, probs = 0.25)
Q2mt_rs2019 <- quantile(RS2019$NU_NOTA_MT, probs = 0.50)
Q3mt_rs2019 <- quantile(RS2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rs2019 = hist(RS2019$NU_NOTA_MT,  
                           main = "Nota de Matemática do Rio Grande do Sul no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de matemática do Enem 2019 --> média:", mediamt_rs2019, ", desvio padrão:", DPmt_rs2019, ", coeficiente de variação:", CVmt_rs2019, ", primeiro quartil:", Q1mt_rs2019, ", segundo quartil:", Q2mt_rs2019, ", terceira quartil:", Q3mt_rs2019)

# Rondônia

RO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RO') 
# Média:
mediamt_ro2019 = mean(RO2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ro2019 = sd(RO2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ro2019 = cv(RO2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ro2019 <- quantile(RO2019$NU_NOTA_MT, probs = 0.25)
Q2mt_ro2019 <- quantile(RO2019$NU_NOTA_MT, probs = 0.50)
Q3mt_ro2019 <- quantile(RO2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ro2019 = hist(RO2019$NU_NOTA_MT,  
                           main = "Nota de Matemática de Rondônia no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de matemática do Enem 2019 --> média:", mediamt_ro2019, ", desvio padrão:", DPmt_ro2019, ", coeficiente de variação:", CVmt_ro2019, ", primeiro quartil:", Q1mt_ro2019, ", segundo quartil:", Q2mt_ro2019, ", terceiro quartil:", Q3mt_ro2019)

# Roraima

RR2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='RR') 
# Média:
mediamt_rr2019 = mean(RR2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rr2019 = sd(RR2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rr2019 = cv(RR2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rr2019 <- quantile(RR2019$NU_NOTA_MT, probs = 0.25)
Q2mt_rr2019 <- quantile(RR2019$NU_NOTA_MT, probs = 0.50)
Q3mt_rr2019 <- quantile(RR2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rr2019 = hist(RR2019$NU_NOTA_MT,  
                           main = "Nota de Matemática de Roraima no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota de matemática do Enem 2019 --> média:", mediamt_rr2019, ", desvio padrão:", DPmt_rr2019, ", coeficiente de variação:", CVmt_rr2019, ", primeiro quartil:", Q1mt_rr2019, ", segundo quartil:", Q2mt_rr2019, ", terceiro quartil:", Q3mt_rr2019)

# Santa Catarina

SC2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SC') 
# Média:
mediamt_sc2019 = mean(SC2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_sc2019 = sd(SC2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_sc2019 = cv(SC2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_sc2019 <- quantile(SC2019$NU_NOTA_MT, probs = 0.25)
Q2mt_sc2019 <- quantile(SC2019$NU_NOTA_MT, probs = 0.50)
Q3mt_sc2019 <- quantile(SC2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_sc2019 = hist(SC2019$NU_NOTA_MT,  
                           main = "Nota de Matemática de Santa Catarina  no Enem 2019",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de matemática do Enem 2019 --> média:", mediamt_sc2019, ", desvio padrão:", DPmt_sc2019, ", coeficiente de variação:", CVmt_sc2019, ", primeiro quartil:", Q1mt_sc2019, ", segundo quartil:", Q2mt_sc2019, ", terceiro quartil:", Q3mt_sc2019)

# São Paulo

SP2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SP') 
# Média:
mediamt_sp2019 = mean(SP2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_sp2019 = sd(SP2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_sp2019 = cv(SP2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_sp2019 <- quantile(SP2019$NU_NOTA_MT, probs = 0.25)
Q2mt_sp2019 <- quantile(SP2019$NU_NOTA_MT, probs = 0.50)
Q3mt_sp2019 <- quantile(SP2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_sp2019 = hist(SP2019$NU_NOTA_MT,  
                         main = "Nota de Matemática de São Paulo  no Enem 2019",
                         xlab = "Nota de Matemática", ylab = "Frequência", 
                         col = c("violet"), 
                         border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de matemática do Enem 2019 --> média:", mediamt_sp2019, ", desvio padrão:", DPmt_sp2019, ", coeficiente de variação:", CVmt_sp2019, ", primeiro quartil:", Q1mt_sp2019, ", segundo quartil:", Q2mt_sp2019, ", terceiro quartil:", Q3mt_sp2019)

# Sergipe

SE2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='SE') 
# Média:
mediamt_se2019 = mean(SE2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_se2019 = sd(SE2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_se2019 = cv(SE2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_se2019 <- quantile(SE2019$NU_NOTA_MT, probs = 0.25)
Q2mt_se2019 <- quantile(SE2019$NU_NOTA_MT, probs = 0.50)
Q3mt_se2019 <- quantile(SE2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_se2019 = hist(SE2019$NU_NOTA_MT,  
                         main = "Nota de Matemática de Sergipe no Enem 2019",
                         xlab = "Nota de Matemática", ylab = "Frequência", 
                         col = c("violet"), 
                         border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de matemática do Enem 2019 --> média:", mediamt_se2019, ", desvio padrão:", DPmt_se2019, ", coeficiente de variação:", CVmt_se2019, ", primeiro quartil:", Q1mt_se2019, ", segundo quartil:", Q2mt_se2019, ", terceiro quartil:", Q3mt_se2019)

# Tocantins

TO2019=filter(ENEM_2019, SG_UF_RESIDENCIA=='TO') 
# Média:
mediamt_to2019 = mean(TO2019$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_to2019 = sd(TO2019$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_to2019 = cv(TO2019$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_to2019 <- quantile(TO2019$NU_NOTA_MT, probs = 0.25)
Q2mt_to2019 <- quantile(TO2019$NU_NOTA_MT, probs = 0.50)
Q3mt_to2019 <- quantile(TO2019$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_to2019 = hist(TO2019$NU_NOTA_MT,  
                         main = "Nota de Matemática de Tocantins no Enem 2019",
                         xlab = "Nota de Matemática", ylab = "Frequência", 
                         col = c("violet"), 
                         border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de matemática do Enem 2019 --> média:", mediamt_to2019, ", desvio padrão:", DPmt_to2019, ", coeficiente de variação:", CVmt_to2019, ", primeiro quartil:", Q1mt_to2019, ", segundo quartil:", Q2mt_to2019, ", terceiro quartil:", Q3mt_to2019)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de matemática nos estados em 2019? Qual o estado com melhor nota? Qual o estado com pior nota?")
cat("Em 2019, o estado com melhor nota em matemática foi O ..., tendo uma média de ... pontos e o estado com a pior nota em matemática foi O ..., tendo uma média de ... pontos")	

#/////////////////////////////////////////////////// 2018 /////////////////////////////////////////////////////////////

# Acre

AC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediamt_ac2018 = mean(AC2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ac2018 = sd(AC2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ac2018 = cv(AC2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ac2018 <- quantile(AC2018$NU_NOTA_MT, probs = 0.25)
Q2mt_ac2018 <- quantile(AC2018$NU_NOTA_MT, probs = 0.50)
Q3mt_ac2018 <- quantile(AC2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ac2018 = hist(AC2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Acre no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota de matemática do Enem 2018 --> média:", mediamt_ac2018, ", desvio padrão:", DPmt_ac2018, ", coeficiente de variação:", CVmt_ac2018, ", primeiro quartil:", Q1mt_ac2018, ", segundo quartil:", Q2mt_ac2018, ", terceiro quartil:", Q3mt_ac2018)

# Alagoas

AL2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AL')
# Média:
mediamt_al2018 = mean(AL2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_al2018 = sd(AL2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_al2018 = cv(AL2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_al2018 <- quantile(AL2018$NU_NOTA_MT, probs = 0.25)
Q2mt_al2018 <- quantile(AL2018$NU_NOTA_MT, probs = 0.50)
Q3mt_al2018 <- quantile(AL2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_al2018 = hist(AL2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Alagoas no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de matemática do Enem 2018 --> média:", mediamt_al2018, ", desvio padrão:", DPmt_al2018, ", coeficiente de variação:", CVmt_al2018, ", primeiro quartil:", Q1mt_al2018, ", segundo quartil:", Q2mt_al2018, ", terceiro quartil:", Q3mt_al2018)

# Amapá

AP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AP') 
# Média:
mediamt_ap2018 = mean(AP2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ap2018 = sd(AP2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ap2018 = cv(AP2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ap2018 <- quantile(AP2018$NU_NOTA_MT, probs = 0.25)
Q2mt_ap2018 <- quantile(AP2018$NU_NOTA_MT, probs = 0.50)
Q3mt_ap2018 <- quantile(AP2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ap2018 = hist(AP2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Amapá no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota de matemática do Enem 2018 --> média:", mediamt_ap2018, ", desvio padrão:", DPmt_ap2018, ", coeficiente de variação:", CVmt_ap2018, ", primeiro quartil:", Q1mt_ap2018, ", segundo quartil:", Q2mt_ap2018, ", terceiro quartil:", Q3mt_ap2018)

# Amazonas

AM2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='AM') 
# Média:
mediamt_am2018 = mean(AM2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_am2018 = sd(AM2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_am2018 = cv(AM2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_am2018 <- quantile(AM2018$NU_NOTA_MT, probs = 0.25)
Q2mt_am2018 <- quantile(AM2018$NU_NOTA_MT, probs = 0.50)
Q3mt_am2018 <- quantile(AM2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_am2018 = hist(AM2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Amazonas no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de matemática do Enem 2018 --> média:", mediamt_am2018, ", desvio padrão:", DPmt_am2018, ", coeficiente de variação:", CVmt_am2018, ", primeiro quartil:", Q1mt_am2018, ", segundo quartil:", Q2mt_am2018, ", terceiro quartil:", Q3mt_am2018)

# Bahia

BA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='BA') 
# Média:
mediamt_ba2018 = mean(BA2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ba2018 = sd(BA2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ba2018 = cv(BA2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ba2018 <- quantile(BA2018$NU_NOTA_MT, probs = 0.25)
Q2mt_ba2018 <- quantile(BA2018$NU_NOTA_MT, probs = 0.50)
Q3mt_ba2018 <- quantile(BA2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ba2018 = hist(BA2018$NU_NOTA_MT,  
                           main = "Nota de Matemática da Bahia no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota de matemática do Enem 2018 --> média:", mediamt_ba2018, ", desvio padrão:", DPmt_ba2018, ", coeficiente de variação:", CVmt_ba2018, ", primeiro quartil:", Q1mt_ba2018, ", segundo quartil:", Q2mt_ba2018, ", terceiro quartil:", Q3mt_ba2018)

# Ceará

CE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='CE') 
# Média:
mediamt_ce2018 = mean(CE2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ce2018 = sd(CE2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ce2018 = cv(CE2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ce2018 <- quantile(CE2018$NU_NOTA_MT, probs = 0.25)
Q2mt_ce2018 <- quantile(CE2018$NU_NOTA_MT, probs = 0.50)
Q3mt_ce2018 <- quantile(CE2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ce2018 = hist(CE2018$NU_NOTA_MT,  
                           main = "Nota de Matemática da Ceará no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota de matemática do Enem 2018 --> média:", mediamt_ce2018, ", desvio padrão:", DPmt_ce2018, ", coeficiente de variação:", CVmt_ce2018, ", primeiro quartil:", Q1mt_ce2018, ", segundo quartil:", Q2mt_ce2018, ", terceiro quartil:", Q3mt_ce2018)

# Distrito Federal

DF2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='DF') 
# Média:
mediamt_df2018 = mean(DF2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_df2018 = sd(DF2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_df2018 = cv(DF2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_df2018 <- quantile(DF2018$NU_NOTA_MT, probs = 0.25)
Q2mt_df2018 <- quantile(DF2018$NU_NOTA_MT, probs = 0.50)
Q3mt_df2018 <- quantile(DF2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_df2018 = hist(DF2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Distrito Federal no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de matemática do Enem 2018 --> média:", mediamt_df2018, ", desvio padrão:", DPmt_df2018, ", coeficiente de variação:", CVmt_df2018, ", primeiro quartil:", Q1mt_df2018, ", segundo quartil:", Q2mt_df2018, ", terceiro quartil:", Q3mt_df2018)

# Espírito Santo

ES2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='ES') 
# Média:
mediamt_es2018 = mean(ES2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_es2018 = sd(ES2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_es2018 = cv(ES2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_es2018 <- quantile(ES2018$NU_NOTA_MT, probs = 0.25)
Q2mt_es2018 <- quantile(ES2018$NU_NOTA_MT, probs = 0.50)
Q3mt_es2018 <- quantile(ES2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_es2018 = hist(ES2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Espírito Santo no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de matemática do Enem 2018 --> média:", mediamt_es2018, ", desvio padrão:", DPmt_es2018, ", coeficiente de variação:", CVmt_es2018, ", primeiro quartil:", Q1mt_es2018, ", segundo quartil:", Q2mt_es2018, ", terceiro quartil:", Q3mt_es2018)

# Goiás

GO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='GO') 
# Média:
mediamt_go2018 = mean(GO2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_go2018 = sd(GO2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_go2018 = cv(GO2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_go2018 <- quantile(GO2018$NU_NOTA_MT, probs = 0.25)
Q2mt_go2018 <- quantile(GO2018$NU_NOTA_MT, probs = 0.50)
Q3mt_go2018 <- quantile(GO2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_go2018 = hist(GO2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Goiás no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota de matemática do Enem 2018 --> média:", mediamt_go2018, ", desvio padrão:", DPmt_go2018, ", coeficiente de variação:", CVmt_go2018, ", primeiro quartil:", Q1mt_go2018, ", segundo quartil:", Q2mt_go2018, ", terceiro quartil:", Q3mt_go2018)

# Maranhão

MA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MA') 
# Média:
mediamt_ma2018 = mean(MA2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ma2018 = sd(MA2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ma2018 = cv(MA2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ma2018 <- quantile(MA2018$NU_NOTA_MT, probs = 0.25)
Q2mt_ma2018 <- quantile(MA2018$NU_NOTA_MT, probs = 0.50)
Q3mt_ma2018 <- quantile(MA2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ma2018 = hist(MA2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Maranhão no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de matemática do Enem 2018 --> média:", mediamt_ma2018, ", desvio padrão:", DPmt_ma2018, ", coeficiente de variação:", CVmt_ma2018, ", primeiro quartil:", Q1mt_ma2018, ", segundo quartil:", Q2mt_ma2018, ", terceiro quartil:", Q3mt_ma2018)

# Mato Grosso

MT2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MT') 
# Média:
mediamt_mt2018 = mean(MT2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_mt2018 = sd(MT2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_mt2018 = cv(MT2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_mt2018 <- quantile(MT2018$NU_NOTA_MT, probs = 0.25)
Q2mt_mt2018 <- quantile(MT2018$NU_NOTA_MT, probs = 0.50)
Q3mt_mt2018 <- quantile(MT2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_mt2018 = hist(MT2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Mato Grosso no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de matemática do Enem 2018 --> média:", mediamt_mt2018, ", desvio padrão:", DPmt_mt2018, ", coeficiente de variação:", CVmt_mt2018, ", primeiro quartil:", Q1mt_mt2018, ", segundo quartil:", Q2mt_mt2018, ", terceiro quartil:", Q3mt_mt2018)

# Mato Grosso do Sul

MS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MS') 
# Média:
mediamt_ms2018 = mean(MS2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ms2018 = sd(MS2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ms2018 = cv(MS2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ms2018 <- quantile(MS2018$NU_NOTA_MT, probs = 0.25)
Q2mt_ms2018 <- quantile(MS2018$NU_NOTA_MT, probs = 0.50)
Q3mt_ms2018 <- quantile(MS2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ms2018 = hist(MS2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Mato Grosso do Sul no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de matemática do Enem 2018 --> média:", mediamt_ms2018, ", desvio padrão:", DPmt_ms2018, ", coeficiente de variação:", CVmt_ms2018, ", primeiro quartil:", Q1mt_ms2018, ", segundo quartil:", Q2mt_ms2018, ", terceiro quartil:", Q3mt_ms2018)

# Minas Gerais

MG2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='MG') 
# Média:
mediamt_mg2018 = mean(MG2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_mg2018 = sd(MG2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_mg2018 = cv(MG2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_mg2018 <- quantile(MG2018$NU_NOTA_MT, probs = 0.25)
Q2mt_mg2018 <- quantile(MG2018$NU_NOTA_MT, probs = 0.50)
Q3mt_mg2018 <- quantile(MG2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_mg2018 = hist(MG2018$NU_NOTA_MT,  
                           main = "Nota de Matemática de Minas Gerais no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de matemática do Enem 2018 --> média:", mediamt_mg2018, ", desvio padrão:", DPmt_mg2018, ", coeficiente de variação:", CVmt_mg2018, ", primeiro quartil:", Q1mt_mg2018, ", segundo quartil:", Q2mt_mg2018, ", terceiro quartil:", Q3mt_mg2018)

# Pará

PA2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PA') 
# Média:
mediamt_pa2018 = mean(PA2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pa2018 = sd(PA2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pa2018 = cv(PA2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pa2018 <- quantile(PA2018$NU_NOTA_MT, probs = 0.25)
Q2mt_pa2018 <- quantile(PA2018$NU_NOTA_MT, probs = 0.50)
Q3mt_pa2018 <- quantile(PA2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pa2018 = hist(PA2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Pará no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota de matemática do Enem 2018 --> média:", mediamt_pa2018, ", desvio padrão:", DPmt_pa2018, ", coeficiente de variação:", CVmt_pa2018, ", primeiro quartil:", Q1mt_pa2018, ", segundo quartil:", Q2mt_pa2018, ", terceiro quartil:", Q3mt_pa2018)

# Paraíba

PB2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PB') 
# Média:
mediamt_pb2018 = mean(PB2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pb2018 = sd(PB2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pb2018 = cv(PB2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pb2018 <- quantile(PB2018$NU_NOTA_MT, probs = 0.25)
Q2mt_pb2018 <- quantile(PB2018$NU_NOTA_MT, probs = 0.50)
Q3mt_pb2018 <- quantile(PB2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pb2018 = hist(PB2018$NU_NOTA_MT,  
                           main = "Nota de Matemática da Paraíba no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de matemática do Enem 2018 --> média:", mediamt_pb2018, ", desvio padrão:", DPmt_pb2018, ", coeficiente de variação:", CVmt_pb2018, ", primeiro quartil:", Q1mt_pb2018, ", segundo quartil:", Q2mt_pb2018, ", terceiro quartil:", Q3mt_pb2018)

# Paraná

PR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PR') 
# Média:
mediamt_pr2018 = mean(PR2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pr2018 = sd(PR2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pr2018 = cv(PR2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pr2018 <- quantile(PR2018$NU_NOTA_MT, probs = 0.25)
Q2mt_pr2018 <- quantile(PR2018$NU_NOTA_MT, probs = 0.50)
Q3mt_pr2018 <- quantile(PR2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pr2018 = hist(PR2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Paraná no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota de matemática do Enem 2018 --> média:", mediamt_pr2018, ", desvio padrão:", DPmt_pr2018, ", coeficiente de variação:", CVmt_pr2018, ", primeiro quartil:", Q1mt_pr2018, ", segundo quartil:", Q2mt_pr2018, ", terceiro quartil:", Q3mt_pr2018)

# Pernambuco

PE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PE') 
# Média:
mediamt_pe2018 = mean(PE2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pe2018 = sd(PE2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pe2018 = cv(PE2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pe2018 <- quantile(PE2018$NU_NOTA_MT, probs = 0.25)
Q2mt_pe2018 <- quantile(PE2018$NU_NOTA_MT, probs = 0.50)
Q3mt_pe2018 <- quantile(PE2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pe2018 = hist(PE2018$NU_NOTA_MT,  
                           main = "Nota de Matemática de Pernambuco no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de matemática do Enem 2018 --> média:", mediamt_pe2018, ", desvio padrão:", DPmt_pe2018, ", coeficiente de variação:", CVmt_pe2018, ", peimeiro quartil:", Q1mt_pe2018, ", segundo quartil:", Q2mt_pe2018, ", terceiro quartil:", Q3mt_pe2018)

# Piauí

PI2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='PI') 
# Média:
mediamt_pi2018 = mean(PI2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pi2018 = sd(PI2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pi2018 = cv(PI2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pi2018 <- quantile(PI2018$NU_NOTA_MT, probs = 0.25)
Q2mt_pi2018 <- quantile(PI2018$NU_NOTA_MT, probs = 0.50)
Q3mt_pi2018 <- quantile(PI2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pi2018 = hist(PI2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Piauí no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota de matemática do Enem 2018 --> média:", mediamt_pi2018, ", desvio padrão:", DPmt_pi2018, ", coeficiente de variação:", CVmt_pi2018, ", primeiro quartil:", Q1mt_pi2018, ", segundo quartil:", Q2mt_pi2018, ", terceiro quartil:", Q3mt_pi2018)

# Rio de Janeiro

RJ2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediamt_rj2018 = mean(RJ2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rj2018 = sd(RJ2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rj2018 = cv(RJ2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rj2018 <- quantile(RJ2018$NU_NOTA_MT, probs = 0.25)
Q2mt_rj2018 <- quantile(RJ2018$NU_NOTA_MT, probs = 0.50)
Q3mt_rj2018 <- quantile(RJ2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rj2018 = hist(RJ2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Rio de Janeiro no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de matemática do Enem 2018 --> média:", mediamt_rj2018, ", desvio padrão:", DPmt_rj2018, ", coeficiente de variação:", CVmt_rj2018, ", primeiro quartil:", Q1mt_rj2018, ", segundo quartil:", Q2mt_rj2018, ", terceiro quartil:", Q3mt_rj2018)

# Rio Grande do Norte

RN2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RN') 
# Média:
mediamt_rn2018 = mean(RN2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rn2018 = sd(RN2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rn2018 = cv(RN2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rn2018 <- quantile(RN2018$NU_NOTA_MT, probs = 0.25)
Q2mt_rn2018 <- quantile(RN2018$NU_NOTA_MT, probs = 0.50)
Q3mt_rn2018 <- quantile(RN2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rn2018 = hist(RN2018$NU_NOTA_MT,  
                           main = "Nota de Matemática da Rio Grande do Norte no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de matemática do Enem 2018 --> média:", mediamt_rn2018, ", desvio padrão:", DPmt_rn2018, ", coeficiente de variação:", CVmt_rn2018, ", primeiro quartil:", Q1mt_rn2018, ", segundo quartil:", Q2mt_rn2018, ", terceiro quartil:", Q3mt_rn2018)

# Rio Grande do Sul

RS2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RS') 
# Média:
mediamt_rs2018 = mean(RS2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rs2018 = sd(RS2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rs2018 = cv(RS2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rs2018 <- quantile(RS2018$NU_NOTA_MT, probs = 0.25)
Q2mt_rs2018 <- quantile(RS2018$NU_NOTA_MT, probs = 0.50)
Q3mt_rs2018 <- quantile(RS2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rs2018 = hist(RS2018$NU_NOTA_MT,  
                           main = "Nota de Matemática do Rio Grande do Sul no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de matemática do Enem 2018 --> média:", mediamt_rs2018, ", desvio padrão:", DPmt_rs2018, ", coeficiente de variação:", CVmt_rs2018, ", primeiro quartil:", Q1mt_rs2018, ", segundo quartil:", Q2mt_rs2018, ", terceira quartil:", Q3mt_rs2018)

# Rondônia

RO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RO') 
# Média:
mediamt_ro2018 = mean(RO2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ro2018 = sd(RO2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ro2018 = cv(RO2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ro2018 <- quantile(RO2018$NU_NOTA_MT, probs = 0.25)
Q2mt_ro2018 <- quantile(RO2018$NU_NOTA_MT, probs = 0.50)
Q3mt_ro2018 <- quantile(RO2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ro2018 = hist(RO2018$NU_NOTA_MT,  
                           main = "Nota de Matemática de Rondônia no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de matemática do Enem 2018 --> média:", mediamt_ro2018, ", desvio padrão:", DPmt_ro2018, ", coeficiente de variação:", CVmt_ro2018, ", primeiro quartil:", Q1mt_ro2018, ", segundo quartil:", Q2mt_ro2018, ", terceiro quartil:", Q3mt_ro2018)

# Roraima

RR2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='RR') 
# Média:
mediamt_rr2018 = mean(RR2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rr2018 = sd(RR2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rr2018 = cv(RR2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rr2018 <- quantile(RR2018$NU_NOTA_MT, probs = 0.25)
Q2mt_rr2018 <- quantile(RR2018$NU_NOTA_MT, probs = 0.50)
Q3mt_rr2018 <- quantile(RR2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rr2018 = hist(RR2018$NU_NOTA_MT,  
                           main = "Nota de Matemática de Roraima no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota de matemática do Enem 2018 --> média:", mediamt_rr2018, ", desvio padrão:", DPmt_rr2018, ", coeficiente de variação:", CVmt_rr2018, ", primeiro quartil:", Q1mt_rr2018, ", segundo quartil:", Q2mt_rr2018, ", terceiro quartil:", Q3mt_rr2018)

# Santa Catarina

SC2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SC') 
# Média:
mediamt_sc2018 = mean(SC2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_sc2018 = sd(SC2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_sc2018 = cv(SC2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_sc2018 <- quantile(SC2018$NU_NOTA_MT, probs = 0.25)
Q2mt_sc2018 <- quantile(SC2018$NU_NOTA_MT, probs = 0.50)
Q3mt_sc2018 <- quantile(SC2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_sc2018 = hist(SC2018$NU_NOTA_MT,  
                           main = "Nota de Matemática de Santa Catarina  no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de matemática do Enem 2018 --> média:", mediamt_sc2018, ", desvio padrão:", DPmt_sc2018, ", coeficiente de variação:", CVmt_sc2018, ", primeiro quartil:", Q1mt_sc2018, ", segundo quartil:", Q2mt_sc2018, ", terceiro quartil:", Q3mt_sc2018)

# São Paulo

SP2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SP') 
# Média:
mediamt_sp2018 = mean(SP2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_sp2018 = sd(SP2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_sp2018 = cv(SP2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_sp2018 <- quantile(SP2018$NU_NOTA_MT, probs = 0.25)
Q2mt_sp2018 <- quantile(SP2018$NU_NOTA_MT, probs = 0.50)
Q3mt_sp2018 <- quantile(SP2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_sp2018 = hist(SP2018$NU_NOTA_MT,  
                           main = "Nota de Matemática de São Paulo  no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de matemática do Enem 2018 --> média:", mediamt_sp2018, ", desvio padrão:", DPmt_sp2018, ", coeficiente de variação:", CVmt_sp2018, ", primeiro quartil:", Q1mt_sp2018, ", segundo quartil:", Q2mt_sp2018, ", terceiro quartil:", Q3mt_sp2018)

# Sergipe

SE2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='SE') 
# Média:
mediamt_se2018 = mean(SE2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_se2018 = sd(SE2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_se2018 = cv(SE2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_se2018 <- quantile(SE2018$NU_NOTA_MT, probs = 0.25)
Q2mt_se2018 <- quantile(SE2018$NU_NOTA_MT, probs = 0.50)
Q3mt_se2018 <- quantile(SE2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_se2018 = hist(SE2018$NU_NOTA_MT,  
                           main = "Nota de Matemática de Sergipe no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de matemática do Enem 2018 --> média:", mediamt_se2018, ", desvio padrão:", DPmt_se2018, ", coeficiente de variação:", CVmt_se2018, ", primeiro quartil:", Q1mt_se2018, ", segundo quartil:", Q2mt_se2018, ", terceiro quartil:", Q3mt_se2018)

# Tocantins

TO2018=filter(ENEM_2018, SG_UF_RESIDENCIA=='TO') 
# Média:
mediamt_to2018 = mean(TO2018$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_to2018 = sd(TO2018$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_to2018 = cv(TO2018$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_to2018 <- quantile(TO2018$NU_NOTA_MT, probs = 0.25)
Q2mt_to2018 <- quantile(TO2018$NU_NOTA_MT, probs = 0.50)
Q3mt_to2018 <- quantile(TO2018$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_to2018 = hist(TO2018$NU_NOTA_MT,  
                           main = "Nota de Matemática de Tocantins no Enem 2018",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de matemática do Enem 2018 --> média:", mediamt_to2018, ", desvio padrão:", DPmt_to2018, ", coeficiente de variação:", CVmt_to2018, ", primeiro quartil:", Q1mt_to2018, ", segundo quartil:", Q2mt_to2018, ", terceiro quartil:", Q3mt_to2018)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de matemática nos estados em 2018? Qual o estado com melhor nota? Qual o estado com pior nota?")
cat("Em 2018, o estado com melhor nota em matemática foi O ..., tendo uma média de ... pontos e o estado com a pior nota em matemática foi O ..., tendo uma média de ... pontos")	

#/////////////////////////////////////////////////// 2017 /////////////////////////////////////////////////////////////

# Acre

AC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AC') # Filtro
# Média:
mediamt_ac2017 = mean(AC2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ac2017 = sd(AC2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ac2017 = cv(AC2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ac2017 <- quantile(AC2017$NU_NOTA_MT, probs = 0.25)
Q2mt_ac2017 <- quantile(AC2017$NU_NOTA_MT, probs = 0.50)
Q3mt_ac2017 <- quantile(AC2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ac2017 = hist(AC2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Acre no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Acre em relação a nota de matemática do Enem 2017 --> média:", mediamt_ac2017, ", desvio padrão:", DPmt_ac2017, ", coeficiente de variação:", CVmt_ac2017, ", primeiro quartil:", Q1mt_ac2017, ", segundo quartil:", Q2mt_ac2017, ", terceiro quartil:", Q3mt_ac2017)

# Alagoas

AL2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AL')
# Média:
mediamt_al2017 = mean(AL2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_al2017 = sd(AL2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_al2017 = cv(AL2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_al2017 <- quantile(AL2017$NU_NOTA_MT, probs = 0.25)
Q2mt_al2017 <- quantile(AL2017$NU_NOTA_MT, probs = 0.50)
Q3mt_al2017 <- quantile(AL2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_al2017 = hist(AL2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Alagoas no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Alagoas em relação a nota de matemática do Enem 2017 --> média:", mediamt_al2017, ", desvio padrão:", DPmt_al2017, ", coeficiente de variação:", CVmt_al2017, ", primeiro quartil:", Q1mt_al2017, ", segundo quartil:", Q2mt_al2017, ", terceiro quartil:", Q3mt_al2017)

# Amapá

AP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AP') 
# Média:
mediamt_ap2017 = mean(AP2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ap2017 = sd(AP2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ap2017 = cv(AP2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ap2017 <- quantile(AP2017$NU_NOTA_MT, probs = 0.25)
Q2mt_ap2017 <- quantile(AP2017$NU_NOTA_MT, probs = 0.50)
Q3mt_ap2017 <- quantile(AP2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ap2017 = hist(AP2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Amapá no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amapá em relação a nota de matemática do Enem 2017 --> média:", mediamt_ap2017, ", desvio padrão:", DPmt_ap2017, ", coeficiente de variação:", CVmt_ap2017, ", primeiro quartil:", Q1mt_ap2017, ", segundo quartil:", Q2mt_ap2017, ", terceiro quartil:", Q3mt_ap2017)

# Amazonas

AM2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='AM') 
# Média:
mediamt_am2017 = mean(AM2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_am2017 = sd(AM2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_am2017 = cv(AM2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_am2017 <- quantile(AM2017$NU_NOTA_MT, probs = 0.25)
Q2mt_am2017 <- quantile(AM2017$NU_NOTA_MT, probs = 0.50)
Q3mt_am2017 <- quantile(AM2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_am2017 = hist(AM2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Amazonas no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Amazonas em relação a nota de matemática do Enem 2017 --> média:", mediamt_am2017, ", desvio padrão:", DPmt_am2017, ", coeficiente de variação:", CVmt_am2017, ", primeiro quartil:", Q1mt_am2017, ", segundo quartil:", Q2mt_am2017, ", terceiro quartil:", Q3mt_am2017)

# Bahia

BA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='BA') 
# Média:
mediamt_ba2017 = mean(BA2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ba2017 = sd(BA2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ba2017 = cv(BA2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ba2017 <- quantile(BA2017$NU_NOTA_MT, probs = 0.25)
Q2mt_ba2017 <- quantile(BA2017$NU_NOTA_MT, probs = 0.50)
Q3mt_ba2017 <- quantile(BA2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ba2017 = hist(BA2017$NU_NOTA_MT,  
                           main = "Nota de Matemática da Bahia no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Bahia em relação a nota de matemática do Enem 2017 --> média:", mediamt_ba2017, ", desvio padrão:", DPmt_ba2017, ", coeficiente de variação:", CVmt_ba2017, ", primeiro quartil:", Q1mt_ba2017, ", segundo quartil:", Q2mt_ba2017, ", terceiro quartil:", Q3mt_ba2017)

# Ceará

CE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='CE') 
# Média:
mediamt_ce2017 = mean(CE2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ce2017 = sd(CE2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ce2017 = cv(CE2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ce2017 <- quantile(CE2017$NU_NOTA_MT, probs = 0.25)
Q2mt_ce2017 <- quantile(CE2017$NU_NOTA_MT, probs = 0.50)
Q3mt_ce2017 <- quantile(CE2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ce2017 = hist(CE2017$NU_NOTA_MT,  
                           main = "Nota de Matemática da Ceará no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Ceará em relação a nota de matemática do Enem 2017 --> média:", mediamt_ce2017, ", desvio padrão:", DPmt_ce2017, ", coeficiente de variação:", CVmt_ce2017, ", primeiro quartil:", Q1mt_ce2017, ", segundo quartil:", Q2mt_ce2017, ", terceiro quartil:", Q3mt_ce2017)

# Distrito Federal

DF2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='DF') 
# Média:
mediamt_df2017 = mean(DF2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_df2017 = sd(DF2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_df2017 = cv(DF2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_df2017 <- quantile(DF2017$NU_NOTA_MT, probs = 0.25)
Q2mt_df2017 <- quantile(DF2017$NU_NOTA_MT, probs = 0.50)
Q3mt_df2017 <- quantile(DF2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_df2017 = hist(DF2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Distrito Federal no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Distrito Federal em relação a nota de matemática do Enem 2017 --> média:", mediamt_df2017, ", desvio padrão:", DPmt_df2017, ", coeficiente de variação:", CVmt_df2017, ", primeiro quartil:", Q1mt_df2017, ", segundo quartil:", Q2mt_df2017, ", terceiro quartil:", Q3mt_df2017)

# Espírito Santo

ES2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='ES') 
# Média:
mediamt_es2017 = mean(ES2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_es2017 = sd(ES2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_es2017 = cv(ES2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_es2017 <- quantile(ES2017$NU_NOTA_MT, probs = 0.25)
Q2mt_es2017 <- quantile(ES2017$NU_NOTA_MT, probs = 0.50)
Q3mt_es2017 <- quantile(ES2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_es2017 = hist(ES2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Espírito Santo no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Espírito Santo em relação a nota de matemática do Enem 2017 --> média:", mediamt_es2017, ", desvio padrão:", DPmt_es2017, ", coeficiente de variação:", CVmt_es2017, ", primeiro quartil:", Q1mt_es2017, ", segundo quartil:", Q2mt_es2017, ", terceiro quartil:", Q3mt_es2017)

# Goiás

GO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='GO') 
# Média:
mediamt_go2017 = mean(GO2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_go2017 = sd(GO2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_go2017 = cv(GO2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_go2017 <- quantile(GO2017$NU_NOTA_MT, probs = 0.25)
Q2mt_go2017 <- quantile(GO2017$NU_NOTA_MT, probs = 0.50)
Q3mt_go2017 <- quantile(GO2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_go2017 = hist(GO2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Goiás no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Goiás em relação a nota de matemática do Enem 2017 --> média:", mediamt_go2017, ", desvio padrão:", DPmt_go2017, ", coeficiente de variação:", CVmt_go2017, ", primeiro quartil:", Q1mt_go2017, ", segundo quartil:", Q2mt_go2017, ", terceiro quartil:", Q3mt_go2017)

# Maranhão

MA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MA') 
# Média:
mediamt_ma2017 = mean(MA2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ma2017 = sd(MA2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ma2017 = cv(MA2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ma2017 <- quantile(MA2017$NU_NOTA_MT, probs = 0.25)
Q2mt_ma2017 <- quantile(MA2017$NU_NOTA_MT, probs = 0.50)
Q3mt_ma2017 <- quantile(MA2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ma2017 = hist(MA2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Maranhão no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Maranhão em relação a nota de matemática do Enem 2017 --> média:", mediamt_ma2017, ", desvio padrão:", DPmt_ma2017, ", coeficiente de variação:", CVmt_ma2017, ", primeiro quartil:", Q1mt_ma2017, ", segundo quartil:", Q2mt_ma2017, ", terceiro quartil:", Q3mt_ma2017)

# Mato Grosso

MT2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MT') 
# Média:
mediamt_mt2017 = mean(MT2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_mt2017 = sd(MT2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_mt2017 = cv(MT2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_mt2017 <- quantile(MT2017$NU_NOTA_MT, probs = 0.25)
Q2mt_mt2017 <- quantile(MT2017$NU_NOTA_MT, probs = 0.50)
Q3mt_mt2017 <- quantile(MT2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_mt2017 = hist(MT2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Mato Grosso no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso em relação a nota de matemática do Enem 2017 --> média:", mediamt_mt2017, ", desvio padrão:", DPmt_mt2017, ", coeficiente de variação:", CVmt_mt2017, ", primeiro quartil:", Q1mt_mt2017, ", segundo quartil:", Q2mt_mt2017, ", terceiro quartil:", Q3mt_mt2017)

# Mato Grosso do Sul

MS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MS') 
# Média:
mediamt_ms2017 = mean(MS2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ms2017 = sd(MS2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ms2017 = cv(MS2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ms2017 <- quantile(MS2017$NU_NOTA_MT, probs = 0.25)
Q2mt_ms2017 <- quantile(MS2017$NU_NOTA_MT, probs = 0.50)
Q3mt_ms2017 <- quantile(MS2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ms2017 = hist(MS2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Mato Grosso do Sul no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Mato Grosso do Sul em relação a nota de matemática do Enem 2017 --> média:", mediamt_ms2017, ", desvio padrão:", DPmt_ms2017, ", coeficiente de variação:", CVmt_ms2017, ", primeiro quartil:", Q1mt_ms2017, ", segundo quartil:", Q2mt_ms2017, ", terceiro quartil:", Q3mt_ms2017)

# Minas Gerais

MG2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='MG') 
# Média:
mediamt_mg2017 = mean(MG2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_mg2017 = sd(MG2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_mg2017 = cv(MG2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_mg2017 <- quantile(MG2017$NU_NOTA_MT, probs = 0.25)
Q2mt_mg2017 <- quantile(MG2017$NU_NOTA_MT, probs = 0.50)
Q3mt_mg2017 <- quantile(MG2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_mg2017 = hist(MG2017$NU_NOTA_MT,  
                           main = "Nota de Matemática de Minas Gerais no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Minas Gerais em relação a nota de matemática do Enem 2017 --> média:", mediamt_mg2017, ", desvio padrão:", DPmt_mg2017, ", coeficiente de variação:", CVmt_mg2017, ", primeiro quartil:", Q1mt_mg2017, ", segundo quartil:", Q2mt_mg2017, ", terceiro quartil:", Q3mt_mg2017)

# Pará

PA2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PA') 
# Média:
mediamt_pa2017 = mean(PA2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pa2017 = sd(PA2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pa2017 = cv(PA2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pa2017 <- quantile(PA2017$NU_NOTA_MT, probs = 0.25)
Q2mt_pa2017 <- quantile(PA2017$NU_NOTA_MT, probs = 0.50)
Q3mt_pa2017 <- quantile(PA2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pa2017 = hist(PA2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Pará no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Pará em relação a nota de matemática do Enem 2017 --> média:", mediamt_pa2017, ", desvio padrão:", DPmt_pa2017, ", coeficiente de variação:", CVmt_pa2017, ", primeiro quartil:", Q1mt_pa2017, ", segundo quartil:", Q2mt_pa2017, ", terceiro quartil:", Q3mt_pa2017)

# Paraíba

PB2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PB') 
# Média:
mediamt_pb2017 = mean(PB2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pb2017 = sd(PB2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pb2017 = cv(PB2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pb2017 <- quantile(PB2017$NU_NOTA_MT, probs = 0.25)
Q2mt_pb2017 <- quantile(PB2017$NU_NOTA_MT, probs = 0.50)
Q3mt_pb2017 <- quantile(PB2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pb2017 = hist(PB2017$NU_NOTA_MT,  
                           main = "Nota de Matemática da Paraíba no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas da Paraíba em relação a nota de matemática do Enem 2017 --> média:", mediamt_pb2017, ", desvio padrão:", DPmt_pb2017, ", coeficiente de variação:", CVmt_pb2017, ", primeiro quartil:", Q1mt_pb2017, ", segundo quartil:", Q2mt_pb2017, ", terceiro quartil:", Q3mt_pb2017)

# Paraná

PR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PR') 
# Média:
mediamt_pr2017 = mean(PR2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pr2017 = sd(PR2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pr2017 = cv(PR2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pr2017 <- quantile(PR2017$NU_NOTA_MT, probs = 0.25)
Q2mt_pr2017 <- quantile(PR2017$NU_NOTA_MT, probs = 0.50)
Q3mt_pr2017 <- quantile(PR2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pr2017 = hist(PR2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Paraná no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas do Paraná em relação a nota de matemática do Enem 2017 --> média:", mediamt_pr2017, ", desvio padrão:", DPmt_pr2017, ", coeficiente de variação:", CVmt_pr2017, ", primeiro quartil:", Q1mt_pr2017, ", segundo quartil:", Q2mt_pr2017, ", terceiro quartil:", Q3mt_pr2017)

# Pernambuco

PE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PE') 
# Média:
mediamt_pe2017 = mean(PE2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pe2017 = sd(PE2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pe2017 = cv(PE2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pe2017 <- quantile(PE2017$NU_NOTA_MT, probs = 0.25)
Q2mt_pe2017 <- quantile(PE2017$NU_NOTA_MT, probs = 0.50)
Q3mt_pe2017 <- quantile(PE2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pe2017 = hist(PE2017$NU_NOTA_MT,  
                           main = "Nota de Matemática de Pernambuco no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Pernambuco em relação a nota de matemática do Enem 2017 --> média:", mediamt_pe2017, ", desvio padrão:", DPmt_pe2017, ", coeficiente de variação:", CVmt_pe2017, ", peimeiro quartil:", Q1mt_pe2017, ", segundo quartil:", Q2mt_pe2017, ", terceiro quartil:", Q3mt_pe2017)

# Piauí

PI2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='PI') 
# Média:
mediamt_pi2017 = mean(PI2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_pi2017 = sd(PI2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_pi2017 = cv(PI2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_pi2017 <- quantile(PI2017$NU_NOTA_MT, probs = 0.25)
Q2mt_pi2017 <- quantile(PI2017$NU_NOTA_MT, probs = 0.50)
Q3mt_pi2017 <- quantile(PI2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_pi2017 = hist(PI2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Piauí no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Piauí em relação a nota de matemática do Enem 2017 --> média:", mediamt_pi2017, ", desvio padrão:", DPmt_pi2017, ", coeficiente de variação:", CVmt_pi2017, ", primeiro quartil:", Q1mt_pi2017, ", segundo quartil:", Q2mt_pi2017, ", terceiro quartil:", Q3mt_pi2017)

# Rio de Janeiro

RJ2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RJ') 
# Média:
mediamt_rj2017 = mean(RJ2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rj2017 = sd(RJ2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rj2017 = cv(RJ2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rj2017 <- quantile(RJ2017$NU_NOTA_MT, probs = 0.25)
Q2mt_rj2017 <- quantile(RJ2017$NU_NOTA_MT, probs = 0.50)
Q3mt_rj2017 <- quantile(RJ2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rj2017 = hist(RJ2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Rio de Janeiro no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio de Janeiro em relação a nota de matemática do Enem 2017 --> média:", mediamt_rj2017, ", desvio padrão:", DPmt_rj2017, ", coeficiente de variação:", CVmt_rj2017, ", primeiro quartil:", Q1mt_rj2017, ", segundo quartil:", Q2mt_rj2017, ", terceiro quartil:", Q3mt_rj2017)

# Rio Grande do Norte

RN2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RN') 
# Média:
mediamt_rn2017 = mean(RN2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rn2017 = sd(RN2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rn2017 = cv(RN2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rn2017 <- quantile(RN2017$NU_NOTA_MT, probs = 0.25)
Q2mt_rn2017 <- quantile(RN2017$NU_NOTA_MT, probs = 0.50)
Q3mt_rn2017 <- quantile(RN2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rn2017 = hist(RN2017$NU_NOTA_MT,  
                           main = "Nota de Matemática da Rio Grande do Norte no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Norte em relação a nota de matemática do Enem 2017 --> média:", mediamt_rn2017, ", desvio padrão:", DPmt_rn2017, ", coeficiente de variação:", CVmt_rn2017, ", primeiro quartil:", Q1mt_rn2017, ", segundo quartil:", Q2mt_rn2017, ", terceiro quartil:", Q3mt_rn2017)

# Rio Grande do Sul

RS2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RS') 
# Média:
mediamt_rs2017 = mean(RS2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rs2017 = sd(RS2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rs2017 = cv(RS2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rs2017 <- quantile(RS2017$NU_NOTA_MT, probs = 0.25)
Q2mt_rs2017 <- quantile(RS2017$NU_NOTA_MT, probs = 0.50)
Q3mt_rs2017 <- quantile(RS2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rs2017 = hist(RS2017$NU_NOTA_MT,  
                           main = "Nota de Matemática do Rio Grande do Sul no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rio Grande do Sul em relação a nota de matemática do Enem 2017 --> média:", mediamt_rs2017, ", desvio padrão:", DPmt_rs2017, ", coeficiente de variação:", CVmt_rs2017, ", primeiro quartil:", Q1mt_rs2017, ", segundo quartil:", Q2mt_rs2017, ", terceira quartil:", Q3mt_rs2017)

# Rondônia

RO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RO') 
# Média:
mediamt_ro2017 = mean(RO2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_ro2017 = sd(RO2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_ro2017 = cv(RO2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_ro2017 <- quantile(RO2017$NU_NOTA_MT, probs = 0.25)
Q2mt_ro2017 <- quantile(RO2017$NU_NOTA_MT, probs = 0.50)
Q3mt_ro2017 <- quantile(RO2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_ro2017 = hist(RO2017$NU_NOTA_MT,  
                           main = "Nota de Matemática de Rondônia no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Rondonia em relação a nota de matemática do Enem 2017 --> média:", mediamt_ro2017, ", desvio padrão:", DPmt_ro2017, ", coeficiente de variação:", CVmt_ro2017, ", primeiro quartil:", Q1mt_ro2017, ", segundo quartil:", Q2mt_ro2017, ", terceiro quartil:", Q3mt_ro2017)

# Roraima

RR2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='RR') 
# Média:
mediamt_rr2017 = mean(RR2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_rr2017 = sd(RR2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_rr2017 = cv(RR2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_rr2017 <- quantile(RR2017$NU_NOTA_MT, probs = 0.25)
Q2mt_rr2017 <- quantile(RR2017$NU_NOTA_MT, probs = 0.50)
Q3mt_rr2017 <- quantile(RR2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_rr2017 = hist(RR2017$NU_NOTA_MT,  
                           main = "Nota de Matemática de Roraima no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Roraima em relação a nota de matemática do Enem 2017 --> média:", mediamt_rr2017, ", desvio padrão:", DPmt_rr2017, ", coeficiente de variação:", CVmt_rr2017, ", primeiro quartil:", Q1mt_rr2017, ", segundo quartil:", Q2mt_rr2017, ", terceiro quartil:", Q3mt_rr2017)

# Santa Catarina

SC2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SC') 
# Média:
mediamt_sc2017 = mean(SC2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_sc2017 = sd(SC2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_sc2017 = cv(SC2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_sc2017 <- quantile(SC2017$NU_NOTA_MT, probs = 0.25)
Q2mt_sc2017 <- quantile(SC2017$NU_NOTA_MT, probs = 0.50)
Q3mt_sc2017 <- quantile(SC2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_sc2017 = hist(SC2017$NU_NOTA_MT,  
                           main = "Nota de Matemática de Santa Catarina  no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Santa Catarina em relação a nota de matemática do Enem 2017 --> média:", mediamt_sc2017, ", desvio padrão:", DPmt_sc2017, ", coeficiente de variação:", CVmt_sc2017, ", primeiro quartil:", Q1mt_sc2017, ", segundo quartil:", Q2mt_sc2017, ", terceiro quartil:", Q3mt_sc2017)

# São Paulo

SP2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SP') 
# Média:
mediamt_sp2017 = mean(SP2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_sp2017 = sd(SP2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_sp2017 = cv(SP2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_sp2017 <- quantile(SP2017$NU_NOTA_MT, probs = 0.25)
Q2mt_sp2017 <- quantile(SP2017$NU_NOTA_MT, probs = 0.50)
Q3mt_sp2017 <- quantile(SP2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_sp2017 = hist(SP2017$NU_NOTA_MT,  
                           main = "Nota de Matemática de São Paulo  no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de São Paulo em relação a nota de matemática do Enem 2017 --> média:", mediamt_sp2017, ", desvio padrão:", DPmt_sp2017, ", coeficiente de variação:", CVmt_sp2017, ", primeiro quartil:", Q1mt_sp2017, ", segundo quartil:", Q2mt_sp2017, ", terceiro quartil:", Q3mt_sp2017)

# Sergipe

SE2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='SE') 
# Média:
mediamt_se2017 = mean(SE2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_se2017 = sd(SE2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_se2017 = cv(SE2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_se2017 <- quantile(SE2017$NU_NOTA_MT, probs = 0.25)
Q2mt_se2017 <- quantile(SE2017$NU_NOTA_MT, probs = 0.50)
Q3mt_se2017 <- quantile(SE2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_se2017 = hist(SE2017$NU_NOTA_MT,  
                           main = "Nota de Matemática de Sergipe no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Sergipe em relação a nota de matemática do Enem 2017 --> média:", mediamt_se2017, ", desvio padrão:", DPmt_se2017, ", coeficiente de variação:", CVmt_se2017, ", primeiro quartil:", Q1mt_se2017, ", segundo quartil:", Q2mt_se2017, ", terceiro quartil:", Q3mt_se2017)

# Tocantins

TO2017=filter(ENEM_2017, SG_UF_RESIDENCIA=='TO') 
# Média:
mediamt_to2017 = mean(TO2017$NU_NOTA_MT) 
# Desvio padrão: determina o grau de dispersão dos dados a partir da média.
DPmt_to2017 = sd(TO2017$NU_NOTA_MT)
# Coeficiente de variação: descreve a variação nos dados em relação à média
cv<- function(x){coef<-sd(x)/mean(x)*100
return(coef)}

CVmt_to2017 = cv(TO2017$NU_NOTA_MT)
# Quartis: dividem uma amostra de dados ordenados em quatro partes iguais
Q1mt_to2017 <- quantile(TO2017$NU_NOTA_MT, probs = 0.25)
Q2mt_to2017 <- quantile(TO2017$NU_NOTA_MT, probs = 0.50)
Q3mt_to2017 <- quantile(TO2017$NU_NOTA_MT, probs = 0.75)
# Histograma: 
histogramamt_to2017 = hist(TO2017$NU_NOTA_MT,  
                           main = "Nota de Matemática de Tocantins no Enem 2017",
                           xlab = "Nota de Matemática", ylab = "Frequência", 
                           col = c("violet"), 
                           border = FALSE)

cat("Estatísticas de Tocantins em relação a nota de matemática do Enem 2017 --> média:", mediamt_to2017, ", desvio padrão:", DPmt_to2017, ", coeficiente de variação:", CVmt_to2017, ", primeiro quartil:", Q1mt_to2017, ", segundo quartil:", Q2mt_to2017, ", terceiro quartil:", Q3mt_to2017)

# Interpretação:
cat("O que você pode afirmar dos candidatos em relação a nota de matemática nos estados em 2017? Qual o estado com melhor nota? Qual o estado com pior nota?")
cat("Em 2017, o estado com melhor nota em matemática foi O ..., tendo uma média de ... pontos e o estado com a pior nota em matemática foi O ..., tendo uma média de ... pontos")	

# Gráfico que mostra as médias de cada estado:

library(ggplot2)
ggplot(ENEM_2017, aes(x=sg,y=mediamt_ap2017))+
  geom_line(aes(col='MÉDIA AP'))+
  geom_line(aes(x=mediamt_to2017, col='MÉDIA TO'))+
  geom_line(aes(x=mediamt_al2017, col='MÉDIA AL'))+
  theme_bw()+
  labs(x="ESTADOS",
       y="Média da nota",
       color=NULL)+
  theme(legend.position = "top")