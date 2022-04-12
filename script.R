#Script para análisis de texto - Audiencias e Iniciativas DDFF

#paquetes####

library(tidyverse)
library(tidytext)
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
library(readxl)

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

corpus(audiencias,)

#análisis de palabras clave####

audiencias %>%
  filter(codigo != c("c203","c301","c205")) %>%   #elige grupo de referencia
  corpus(text_field = "text", docid_field = "nombre") %>%
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

#multiple <- expr(match("derech.\\s(\\w+\\s)+trabaj."))
#corpus_audiencias %>%
#  filter(codigo != c("c203","c301","c205")) -> toks   #elige grupo de referencia
#  kwic(toks, pattern = multiple)


#Separamos texto IPN
ipn_data <- read_excel("entrada/iniciativas_ddff.xlsx")

ipn_data <- ipn_data %>%
  mutate(text = texto_completo) %>%
  separate(text,
           into = c("nada","text"),
           sep = "PROBLEMA A SOLUCIONAR:") %>%
  separate(text,
           into = c("problema", "text"),
           sep = "SITUACIÓN IDEAL:") %>%
  separate(text,
           into = c("ideal", "text"),
           sep = "QUÉ DEBE CONTEMPLAR LA NUEVA CONSTITUCIÓN:") %>%
  separate(text,
           into = c("contemplar", "text"),
           sep = "¿CON QUÉ ARGUMENTOS TÚ O TU ORGANIZACIÓN RESPALDAN ESTA PROPUESTA\\?") %>%
  separate(text,
           into = c("argumentos", "text"),
           sep = "PROPUESTA DE ARTICULADO") %>%
  separate(text,
           into = c("articulado", "proponentes"),
           sep = "BREVE RESEÑA SOBRE QUIÉN O QUIÉNES PROPONEN Y LA HISTORIA DE LA ELABORACIÓN DE LA INICIATIVA")

#y separamos de nuevo en unidades de texto

problema <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","problema"))
ideal <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","ideal"))
contemplar <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","contemplar"))
argumentos <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","argumentos"))
articulado <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","articulado"))
proponentes <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","proponentes"))

#y recreamos la variable de texto completo

str_remove(ipn_data$texto_completo, c("PROBLEMA A SOLUCIONAR:",
                                      "SITUACIÓN IDEAL:",
                                      "QUÉ DEBE CONTEMPLAR LA NUEVA CONSTITUCIÓN:",
                                      "¿CON QUÉ ARGUMENTOS TÚ O TU ORGANIZACIÓN RESPALDAN ESTA PROPUESTA\\?",
                                      "PROPUESTA DE ARTICULADO",
                                      "BREVE RESEÑA SOBRE QUIÉN O QUIÉNES PROPONEN Y LA HISTORIA DE LA ELABORACIÓN DE LA INICIATIVA")) -> ipn_data$texto_completo

completo <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","texto_completo"))

#ahora vemos si resulta convertirlos en corpus

corpus_problema <- corpus(problema,text_field = "problema")
