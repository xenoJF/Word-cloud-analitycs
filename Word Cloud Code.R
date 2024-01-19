library(tm);library(NLP); library(SnowballC);
library(ggwordcloud);library(RColorBrewer);library(tidyverse)

# Inicio ------------------------------------------------------------------
setwd('./Insumo')
Speech<- "Dia nacional del trabajo.txt"  

text<- readLines(Speech,encoding = 'UTF-8')
text<- iconv(text,from ='UTF-8',to= 'latin1') 
# cambio de encoding para procesar (texto en español por eso se cambia a latin1)
# para que R pueda procesar el texto 

corpus<- Corpus(VectorSource(text)) 

# Aplicar procesamiento
mi_corpus<- tm_map(corpus,tolower) # convertir todo a minusculas
mi_corpus<- tm_map(mi_corpus,stripWhitespace) # elimino espacios en blanco
mi_corpus <- tm_map(mi_corpus,removePunctuation) # elimino Signos de puntuación
mi_corpus <- tm_map(mi_corpus,removeNumbers) # Quito numeros

# Remover palabras genericas
stopwords('spanish')
mi_corpus<- tm_map(mi_corpus,removeWords,stopwords('spanish'))

# si quiero remover palabras propias
# mi_corpus<- tm_map(mi_corpus,removeWords,c('Hola','Adiós'))

# Cuantas veces aparece cada palabra --------------------------------------
words<- TermDocumentMatrix(mi_corpus)
words<- as.matrix(words) # Fila : palabra Col: Parrafos

# cuantas vece aparecio una palabra 
rowSums(words)
View(sort(rowSums(words),decreasing = T))
# Crear data.frame
Words2<- sort(rowSums(words),decreasing = T)
df_words<- data.frame(word=names(Words2),freq=Words2)
# grafico
barplot(df_words[1:10,]$freq,
        names.arg = df_words[1:10,]$word,
        col="cornflowerblue",
        main = "Often Words",
        ylab = "Frequency")

# Subset de palabras mas frecuentes
sub_df<- subset(df_words, df_words$freq>=3)

# Creación de la nube
# 1. Categoria de color
sub_df$colors<- cut(sub_df$freq,
                    breaks =c(0,sort(unique(sub_df$freq))),
                    labels =unique(sub_df$freq))

# R por defecto arroja intervalos abiertos 
# por la izquierda y cerrado a la derecha
# Categorias diferentes con el comando unique()

set.seed(2021)
sub_df$Angle <- sample(c(0,60,90),nrow(sub_df),replace =T) 
# seleccion de angulo de palabra
# La primera palabra ponerle angulo de 0°
sub_df$Angle[1]<-0
names(sub_df)
################################# Acomodar palabras

set.seed(2021)

sub_df %>% ggplot(aes(label=word,
                      size=freq,
                      color=colors,
                      angle= Angle)) + geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  theme_minimal()
