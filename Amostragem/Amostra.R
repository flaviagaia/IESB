# Pacotes.
## install.packages('SunterSampling')
## install.packages('TeachingSampling')
## install.packages('sampling')
## install.packages('pps')
## install.packages('samplingVarEst')
## install.packages('samplingbook')

library(SunterSampling)
library(TeachingSampling)
library(sampling)
library(pps)
library(samplingVarEst)
library(samplingbook)
setwd("C:\\Users\\natal\\OneDrive\\Documentos\\IESB\\Graduacao\\2021_02 - HMDC065Amostragem\\Aula 20210914")
data=read.table("Amostra.txt",header=T)
attach(data)

#Tamanho da população.
N=50
#Tamanho da amostra.
n=25

#Amostragem Aleatória Simples
#Seleção
sam=S.SI(N,n)
#Visualizar a amostra
amostra=data[sam,]
  #Estimar o total populacional (ou média) e variância
  E.SI(N,n,amostra$y)
  sum(data$y)
  
#Amostragem Estratificada Proporcional
#Vetor com o tamanho populacional de cada estrato
Nh=as.numeric(table(Estrato))
#Vetor com o tamanho amostral de cada estrato
nh=c(15,10)
#Seleção
sam=S.STSI(as.factor(Estrato),Nh,nh)
#Visualizar a amostra
amostra=data[sam,]
 #Estimar o total populacional (ou média) e variância
 E.STSI(as.factor(amostra$Estrato),Nh,nh,amostra$y)
 sum(data$y)
 
 #https://www.youtube.com/watch?v=sX48c5M8EWM

#Amostragem Sistemática
#Valor
IS=N/n
#Seleção
sam=S.SY(N,IS)
#Visualizar a amostra
amostra=data[sam,]
 #Estimar o total populacional (ou média) e variância
 E.SY(N,IS,amostra$y)
 sum(data$y)
 
#Amostragem Bernoulli
#Valor da constante pi pré-fixado
prob=0.6
#Seleção
sam=S.BE(N,prob)
#Visualizar a amostra
amostra=data[sam,]
 #Estimar o total populacional (ou média) e variância
 E.BE(amostra$y,prob)
 sum(data$y)
 
 