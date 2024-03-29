################################## https://rpubs.com/Mateusecb/IC_BAYES e https://rstudio-pubs-static.s3.amazonaws.com/823986_727f74e5538d4ae7a4119c81bca4e7d8.html 
#################### https://rstudio-pubs-static.s3.amazonaws.com/946019_8df19c2974554dd89671ddc926b846b3.html ###########################

# Pacotes que utilizei:

library(knitr) # colocar figuras/imagens na apresenta��o
library(readxl) # leitura dos dados em excel
#install.packages("flextable")
library(flextable) # Construir Tabelas
#install.packages("officer")
library(officer) # Pacote para utilizar algumas op��es extras do pacote flextable
library(DT) # pacote para construir tabelas din�micas
library(readr)

# Um algoritmo de classifica��o para filtrar spam em mensagens SMS da telefonia celular

# Este conjunto de dados, sms_spam.xlsx, inclui o texto de mensagens SMS juntamente com um r�tulo que indica se a mensagem � indesejada.
# As mensagens indesejadas s�o rotuladas como spam, enquanto mensagens leg�timas s�o marcadas como ham.
sms_raw <- read_excel("C:/Users/FGGP/Downloads/sms_spam.xlsx")
str(sms_raw)

sms_raw$type <- factor(sms_raw$type) # converte o tipo (ham/spam) para fator

options(digits = 2)
prop.table(table(sms_raw$type)) * 100 # O percentual de spam e ham nas mensagens

# Prepara��o dos dados:

library(tm)

sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)

inspect(sms_corpus[1:3])

corpus_limpo <- tm_map(sms_corpus, tolower)
corpus_limpo <- tm_map(corpus_limpo, removeNumbers)
corpus_limpo <- tm_map(corpus_limpo, removeWords, stopwords())
corpus_limpo <- tm_map(corpus_limpo, removePunctuation)
corpus_limpo <- tm_map(corpus_limpo, stripWhitespace)

# A fun��o DocumentTermMatrix() cria uma estrutura de dados denominada matriz esparsa a partir de um corpus, onde as linhas da matriz indicam documentos (ou seja, mensagens SMS) e as colunas indicam termos (ou seja, palavras).
#Cada c�lula na matriz armazena um n�mero indicando uma contagem das vezes que a palavra indicada pela coluna aparece no documento indicado pela linha.

sms_dtm <- DocumentTermMatrix(corpus_limpo)

# Separa��o dos dados de treinamento e teste

sms_raw_treinamento <- sms_raw[1:4169,]
sms_raw_teste  <- sms_raw[4170:5559,]

sms_dtm_treinamento <- sms_dtm[1:4169,]
sms_dtm_teste  <- sms_dtm[4170:5559,]

sms_corpus_treinamento <- corpus_limpo[1:4169]
sms_corpus_teste  <- corpus_limpo[4170:5559]

options(digits = 2)
prop.table(table(sms_raw_treinamento$type)) * 100

prop.table(table(sms_raw_teste$type)) * 100

# As propor��es entre spam e ham s�o praticamente id�nticas nos dois conjuntos de dados.

# Word clouds: Uma word cloud (nuvem de palavras) � uma forma bastante comum de descrever visualmente a frequ�ncia com que as palavra aparecem em um texto. 
# A nuvem � composta de palavras espalhadas aleatoriamente.
# As palavras que aparecem com mais frequ�ncia no texto s�o mostradas em uma fonte maior, enquanto termos menos comuns s�o mostrados em fontes menores.
# Esse tipo de figura cresceu em popularidade recentemente, uma vez que fornece uma maneira de observar temas de tend�ncias em s�tios de m�dia social e em discursos.

library(wordcloud)

wordcloud(sms_corpus_treinamento, min.freq = 40, random.order = FALSE) # nuvem de todos as palavras.

# Vamos comparar os textos classificados como spam e ham. Iniciemos com os spams.

spam <- subset(sms_raw_treinamento, type == "spam")
wordcloud(spam$text, max.words = 40, scale = c(4, 0.5))

# Agora, os textos classificados como ham.

ham <- subset(sms_raw_treinamento, type == "ham")
wordcloud(ham$text, max.words = 40, scale = c(4, 0.5))

# Eliminando as palavras que aparecem em menos de cinco mensagens SMS, ou seja, menos de cerca de 0,1% dos dados utilzados para o treinamento do modelo.

meus_termos <- findFreqTerms(sms_dtm_treinamento, 5)
sms_treinamento <- DocumentTermMatrix(sms_corpus_treinamento, list(dictionary = meus_termos))
sms_teste  <- DocumentTermMatrix(sms_corpus_teste, list(dictionary = meus_termos))

# define uma fun��o converte_contagens() para converter contagens em fatores e aplica a fun��o a cada coluna das matrizes.

converte_contagens <- function(contagem) {
  contagem <- ifelse(contagem > 0, 1, 0)
  contagem <- factor(contagem, levels = c(0, 1), labels = c("Nao", "Sim"))
  return(contagem)
}
sms_treinamento <- apply(sms_treinamento, MARGIN = 2, converte_contagens)
sms_teste  <- apply(sms_teste, MARGIN = 2, converte_contagens)

# Treinamento do modelo

library(e1071)

sms_classificador <- naiveBayes(sms_treinamento, sms_raw_treinamento$type)
sms_teste_pred <- predict(sms_classificador, sms_teste)

# Para comparar os valores previstos com os valores reais, usaremos a fun��o CrossTable() no pacote gmodels.

library(gmodels)
CrossTable(sms_teste_pred, sms_raw_teste$type,
           prop.chisq = FALSE, prop.t = FALSE, 
           dnn = c('predito', 'real'))

# O c�digo a seguir utiliza o par�mtro laplace = 1:

require(e1071)
sms_classificador2 <- naiveBayes(sms_treinamento, sms_raw_treinamento$type,
                                 laplace = 1)
sms_teste_pred2 <- predict(sms_classificador2, sms_teste)
CrossTable(sms_teste_pred2, sms_raw_teste$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predito', 'real'))



