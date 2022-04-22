#Script para análisis de texto - Audiencias e Iniciativas DDFF

#paquetes####

library(devtools)
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
                       "oye", "cierto",
                       "hoy dia", "hoy día", "hoy en día", "hoy en dia",
                       "muchas gracias","muchísimas gracias")

palabrasdemas <- c(stopwords_es, otras_stopwords,
                   "hoy día", "hoy dia", "no cierto", "hoy en día",
                   "muchas gracias","muchísimas gracias")

#importar####

audiencias <- readtext("entrada/audiencias/*.txt",
                       docvarsfrom = "filenames",
                       docvarnames = c("nombre", "bloque", "codigo", "expertiz",
                                       "sexo","procedencia","iniciativa"),
                       dvsep = "_",
                       encoding = "UTF-8")

audiencias$codigo <- str_remove(audiencias$codigo, "c")

#crear corpus####

corpus_audiencias <- corpus(audiencias, text_field = "text",
                            docid_field = "nombre")

#Iniciativas####

#Separamos texto IPN
ipn_data <- read_excel("entrada/iniciativas_ddff.xlsx")

ipn_data$codigo <- as.character(ipn_data$codigo)

ipn_data <- ipn_data %>%
  mutate(text = texto) %>%
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

ipn_data <- ipn_data %>%
  mutate(nombre = paste0("i_",nombre))

ipn_data <- rename(ipn_data, text = texto)

#y separamos de nuevo en unidades de texto

problema <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","problema"))
ideal <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","ideal"))
contemplar <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","contemplar"))
argumentos <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","argumentos"))
articulado <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","articulado"))
proponentes <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","proponentes"))

#y recreamos la variable de texto completo

ipn_data$text <- paste(problema$problema, ideal$ideal, contemplar$contemplar,
                       argumentos$argumentos,articulado$articulado,proponentes$proponentes)

iniciativas <- select(ipn_data, c("nombre","autoria","apoyos","solicito_ap","tuvo_ap","codigo","text"))

#fusionar bases de datos####

audiencias <- mutate(audiencias,tipo = "ap")
iniciativas <- mutate(iniciativas, tipo = "ipn")

# dividir iniciativas por apoyos

iniciativas %>%
  mutate(quince = case_when(apoyos > 14999 ~ "si", apoyos <= 14999 ~ "no")) -> iniciativas

audiencias %>%
  mutate(quince = "si") -> audiencias

#crear variable de bloques para iniciativas

iniciativas %>%
  mutate(bloque = case_when(codigo < 200 ~ "b1",
                            codigo < 300 & codigo >= 200 ~ "b2",
                            codigo < 400 & codigo >= 300 ~ "b3",
                            codigo >= 400 ~ "b4")) -> iniciativas

bind_rows(audiencias, iniciativas) -> todo

#ahora vemos si resulta convertirlos en corpus
##yo sé que se puede hacer con una función, pero pa qué

corpus_problema <- corpus(problema,text_field = "problema")
corpus_ideal <- corpus(ideal,text_field = "ideal")
corpus_contemplar <- corpus(contemplar,text_field = "contemplar")
corpus_argumentos <- corpus(argumentos,text_field = "argumentos")
corpus_articulado <- corpus(articulado,text_field = "articulado")
corpus_proponentes <- corpus(proponentes,text_field = "proponentes")
corpus_iniciativas <- corpus(iniciativas,text_field = "text")
corpus_todo <- corpus(todo, text_field = "text", docid_field = "nombre")

df_todo <- as.tibble(todo)

saveRDS(df_todo, "entrada/df_completa.RDS")

#descriptivos####

audiencias %>%
  count(codigo) -> cuenta_aud

audiencias %>%
  filter(nombre == "cimundis") -> discapacidad

#Palabras clave mezcladas

todo %>%
  mutate(tipo = case_when(tipo == "ipn" ~ "Iniciativas",
                          tipo == "ap" ~ "Audiencias")) %>%
  filter(quince %in% "si") %>%   #elige grupo de referencia
  filter(codigo %in% c("206")) %>%
  corpus(text_field = "text", docid_field = "nombre") %>%
  tokens(remove_punct = T,
         remove_numbers = F) %>%
  tokens_remove(pattern = stopwords_es, valuetype = 'fixed') %>%
  #descartar palabras comunes
  tokens_wordstem(language = "spanish") %>% #lematiza (palabras a raíz)
  tokens_tolower() %>%
  tokens_ngrams(n = 2,                  #crea frases de n palabras
                concatenator = " ") %>%
  tokens_group(groups = tipo) %>%   #elige criterio de agrupación
  dfm() %>%
  textstat_keyness(target = c("Iniciativas"), #elige grupo objetivo
                   measure = "lr") %>%
  textplot_keyness(color = c("gold3","gray"), #color de barras
                   labelcolor = "gray30", #color de texto
                   labelsize = 3, #tamaño de etiquetas
                   n = 20, #número de palabras
                   ) +
  scale_x_continuous(limits = c(-30,30)) +
  labs(title = "Frases clave",
       subtitle = "Derecho a la Seguridad Social",
       #x = "Desigualdad relativa (χ²)",
       x = "Tasa de Verosimilitud (-2LL)",
       y = "") +
  geom_vline(xintercept = c(-10,10), linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        legend.position = "bottom")

kwic####

todo %>%
  filter(codigo == "208") %>%
  corpus(text_field = "text", docid_field = "nombre") %>%
  kwic("establecimiento", window = 9) -> educacional

#redes####

todo %>%
  filter(quince %in% "si") %>%   #elige grupo de referencia
  #filter(tipo %in% "ap") %>%
  filter(codigo %in% c("206")) %>%
  corpus(text_field = "text", docid_field = "nombre") %>%
  tokens(remove_punct = T,
         remove_numbers = F) %>%
  tokens_remove(pattern = stopwords_es, valuetype = 'fixed') %>%
  #descartar palabras comunes
  tokens_wordstem(language = "spanish") %>% #lematiza (palabras a raíz)
  tokens_tolower() %>%
  tokens_ngrams(n = 2,                  #crea frases de n palabras
                concatenator = " ") %>%
  fcm(context = "window", tri = F) -> fcmat
feat <- names(topfeatures(fcmat, 66))
fcm_select(fcmat, pattern = feat) %>%
  textplot_network(min_freq = .5) +
  labs(title = "Red de Co-locaciones",
       subtitle = "Derecho a la Seguridad Social",
       x = "",
       y = "") +
  theme_minimal()

#arbol####
#referencia:
#https://quanteda.io/articles/pkgdown/replication/digital-humanities.html#11-clustering

devtools::install_github("quanteda/quanteda.corpora", force = T)
library(quanteda.corpora)

ddff_dfm <- tokens(corpus_todo, remove_punct = T) %>%
  tokens_wordstem("es") %>%
  tokens_remove(stopwords_es) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 3)

ddff_dist <- dfm_weight(ddff_dfm, scheme = "prop") %>%
  textstat_dist(method = "euclidean") %>%
  as.dist()

ddff_cluster <- hclust(ddff_dist)

ddff_cluster$labels <- docnames(ddff_dfm)

plot(ddff_cluster, xlab = "", sub = "",
     main = "Euclidean Distance on Normalized Token Frequency")

