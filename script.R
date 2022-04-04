#Script para análisis de texto - Audiencias e Iniciativas DDFF

#paquetes####

library(tidyverse)
library(tidytext)
library(readr)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.tidy)
library(tokenizers)
library(corpus)
library(SnowballC)
library(udpipe)
library(stopwords)


#variables####

otras_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")

discursoindirecto <- c("dijo", "dije", "diría", "digamos",
                       "fue", "fui",
                       "llegué", "llegó",
                       "entonces", "final", "fin",
                       "creo", "pienso",
                       "oye", "cierto")

palabrasdemas <- c(stopwords_es, otras_stopwords,
                   "hoy día", "hoy dia", "no cierto", "hoy en día")

#importar####

audiencias <- readtext("entrada/audiencias/*.txt",
                       docvarsfrom = "filenames",
                       docvarnames = c("nombre", "bloque", "codigo", "expertiz",
                                       "sexo","procedencia","iniciativa"),
                       dvsep = "_",
                       encoding = "UTF-8")

#crear corpus####

corpus_audiencias <- corpus(audiencias, text_field = "text")
#lista de etiquetas
doc_id <- paste(audiencias$nombre,
                sep = "-")
docnames(corpus_audiencias) <-doc_id

#análisis de palabras clave####

audiencias %>%
  filter(codigo != c("c203","c301","c205")) %>%   #elige grupo de referencia
  corpus(text_field = "text") -> corpus_temp
docnames(corpus_temp) <-doc_id
corpus_temp %>%
  tokens(remove_punct = TRUE,
         remove_numbers = FALSE) %>%
  tokens_remove(pattern = phrase(palabrasdemas), valuetype = 'fixed') %>%
                #descartar palabras comunes
  tokens_wordstem(language = "spanish") %>% #lematiza (palabras a raíz)
  tokens_tolower() %>%
   tokens_ngrams(n = 2,                  #crea frases de n palabras
                concatenator = " ") %>%
  tokens_group(groups = expertiz) %>%   #elige criterio de agrupación
  dfm() %>%
  textstat_keyness(target = c("escol"), #elige grupo objetivo
                   measure = "lr") %>%
  textplot_keyness(color = c("red2","gray"), #color de barras
                   labelcolor = "gray30", #color de texto
                   labelsize = 4, #tamaño de etiquetas
                   n = 20, #numero de palabras
                   margin = 0.1)

#búsqueda de palabras clave####

multiple <- expr(match("derech.\\s(\\w+\\s)+trabaj."))
corpus_audiencias %>%
  filter(codigo != c("c203","c301","c205")) -> toks   #elige grupo de referencia
  kwic(toks, pattern = multiple)
