# Gráficos para Interpretação dos resultados de Análise exploratória de dados:

# Histograma: 
# histogramamt_brQ2= hist(ENEM_2017_2019_MATEMATICA$NU_NOTA_MT,  
#                            main = "Nota de Matemática no Enem 2017",
#                            xlab = "Nota de Matemática", ylab = "Frequência", 
#                            col = c("violet"), 
#                            border = FALSE)

#Gráfico de Barras  Q2
library(ggplot2)
ggplot(mediamt, aes(y=`NOTAMT`,x=`ANO`, fill = `UF`)) +
  geom_bar(stat = "identity", color = "tomato") 


#Gráfico de Barras  Q3 BRASIL
library(ggplot2)
ggplot(media_notas_br2019, aes(y=`NOTA`,x=`MATERIA`)) +
  geom_bar(stat = "identity", color = "tomato") 

#Gráfico de Barras  Q3 BRASIL
library(ggplot2)
ggplot(media_notas_br2018, aes(y=`NOTA`,x=`MATERIA`)) +
  geom_bar(stat = "identity", color = "tomato") 

#Gráfico de Barras  Q3  BRASIL
ggplot(media_notas_br2017, aes(y=`NOTA`,x=`MATERIA`)) +
  geom_bar(stat = "identity", color = "tomato") 

#Gráfico de Barras melhor e pior nota UF
library(ggplot2)
ggplot(mediauf, aes(y=`NOTACN`,x=`UF`)) +
  geom_bar(stat = "identity", color = "tomato") 

head(mediauf)