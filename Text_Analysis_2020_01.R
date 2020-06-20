#install.packages("pdftools")
#install.packages("tibble")
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("readr")
#install.packages("wordcloud")
#install.packages("ggplot2")
#install.packages("widyr")
#install.packages("ggraph")
#install.packages("igraph")
library("pdftools")
library("tibble")
library("dplyr")
library("tidytext")
library("readr")
library("wordcloud")
library("ggplot2")
library("tidyr")
library("widyr")
library("ggraph")
library("igraph")

# Leitura do PDF
text <- pdf_text("/Users/jasmine/Desktop/biblia.pdf")
text <- unlist(strsplit(text,"[.]"))

#Carregar Lexicon de Sentimentos
affin_pt <- read_delim("/Users/jasmine/Desktop/affin_pt.csv",
                       ";",escape_double = FALSE, trim_ws = TRUE)

#Carregar Stop Words
stopwords_pt <- read_delim("/Users/jasmine/Desktop/stopwords.csv",
                           ";",escape_double = FALSE, trim_ws = TRUE)
#Adicionar palavra nova às Stop Words
stopwords_pt <- add_row(stopwords_pt, word = "disse")

#Transformação do texto em tabela por linhas
text <- tibble(sentence = text)

#Criar uma coluna com o número da linha
text <- mutate(text, linenumber = row_number())

#Desaninhar tokens
tokens <- unnest_tokens(text, word,sentence)

#Contar tokens - análise de frequência
tokens_count <- tokens %>%
  count(word, sort = TRUE)

#Remover Stop Words
tokens_count <- tokens_count %>%
  anti_join(stopwords_pt)

#Word Cloud
wordcloud(tokens_count$word, tokens_count$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Gerar Paleta de cores
numRegister <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(numRegister)

#Gerar gráfico
tokens_count %>%
  mutate(word = reorder(word,n)) %>%
  head(numRegister) %>%
  ggplot(aes(word,n,fill=factor(word)))+
  scale_fill_manual(values = mycolors) +
  geom_col()+
  xlab(NULL)+
  coord_flip()

#Gerar Bigramas
bigrams <- text %>%
  unnest_tokens(bigram, sentence, token = "ngrams", n = 2) %>%
  separate(bigram,c("word1","word2"),sep = " ") %>%
  na.omit() %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word2 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(bigram,word1,word2,sep = " ") %>%
  count(bigram, sort = TRUE)

#Gráfico de Bigramas
numRegister <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(numRegister)
bigrams %>%
  mutate(bigram = reorder(bigram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(bigram,n,fill=factor(bigram)))+
  guides(fill=FALSE)+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#Gerar Trigramas
trigrams <- text %>%
  unnest_tokens(trigram, sentence, token = "ngrams", n = 3) %>%
  separate(trigram,c("word1","word2","word3"),sep = " ") %>%
  na.omit() %>% 
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word3 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(trigram,word1,word2,word3,sep = " ") %>%
  count(trigram, sort = TRUE)

#Gráfico de Trigramas
numRegister <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(numRegister)
trigrams %>%
  mutate(trigram = reorder(trigram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(trigram,n,fill=factor(trigram)))+
  guides(fill=FALSE)+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#Cálculo da Correlação de Pares
word_cors <- tokens %>%
  group_by(word) %>%
  filter(n()>3) %>%
  pairwise_cor(word,linenumber,sort = TRUE)


#Gráfico de Correlação de Pares com filtro
word_cors %>%
  filter(item1 %in% c("cidade","bárbaros")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2,correlation))+
    geom_bar(stat = "identity")+
    facet_wrap(~ item1, scales="free")+
    coord_flip()



#Grafo da correlação de pares
a <- grid::arrow(type="closed",length = unit(.15,"inches"))

word_cors %>%
  filter(correlation != Inf) %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
    geom_edge_link(aes(edge_alpha = correlation), 
                 show.legend = FALSE,
                 arrow = a,
                 end_cap = circle(.14,"inches"))+
    geom_node_point(color="#0000AA",size=2)+
    geom_node_text(aes(label=name), vjust = 1, hjust = 1)+
    theme_void()


#Análise de Sentimentos
#Cálculo dos sentimentos
affin <- tokens %>%
  inner_join(affin_pt) %>%
  count(index= linenumber %/% 1000, sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment = positivo - negativo) 
  #summarise(soma = sum(sentiment))
  #mutate(sentiment = sign(sentiment)*log(abs(sentiment)+1))

#Gráfico dos Sentimentos
ggplot(affin, aes(index,sentiment))+
  geom_col(show.legend = TRUE)
