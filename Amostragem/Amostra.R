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

#Tamanho da popula��o.
N=50
#Tamanho da amostra.
n=25

#Amostragem Aleat�ria Simples
#Sele��o
sam=S.SI(N,n)
#Visualizar a amostra
amostra=data[sam,]
  #Estimar o total populacional (ou m�dia) e vari�ncia
  E.SI(N,n,amostra$y)
  sum(data$y)
  
#Amostragem Estratificada Proporcional
#Vetor com o tamanho populacional de cada estrato
Nh=as.numeric(table(Estrato))
#Vetor com o tamanho amostral de cada estrato
nh=c(15,10)
#Sele��o
sam=S.STSI(as.factor(Estrato),Nh,nh)
#Visualizar a amostra
amostra=data[sam,]
 #Estimar o total populacional (ou m�dia) e vari�ncia
 E.STSI(as.factor(amostra$Estrato),Nh,nh,amostra$y)
 sum(data$y)
 
 #https://www.youtube.com/watch?v=sX48c5M8EWM

#Amostragem Sistem�tica
#Valor
IS=N/n
#Sele��o
sam=S.SY(N,IS)
#Visualizar a amostra
amostra=data[sam,]
 #Estimar o total populacional (ou m�dia) e vari�ncia
 E.SY(N,IS,amostra$y)
 sum(data$y)
 
#Amostragem Bernoulli
#Valor da constante pi pr�-fixado
prob=0.6
#Sele��o
sam=S.BE(N,prob)
#Visualizar a amostra
amostra=data[sam,]
 #Estimar o total populacional (ou m�dia) e vari�ncia
 E.BE(amostra$y,prob)
 sum(data$y)
 
 