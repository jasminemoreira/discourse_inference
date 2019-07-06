# install.packages("Rcpp", dependencies = TRUE)
# install.packages("pkgconfig", dependencies = TRUE)
# install.packages("pdftools")
# install.packages("tibble")
# install.packages("tidytext")
# install.packages("readr")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("igraph")
# install.packages("ggraph")
# install.packages("widyr")

library(pdftools)
library(tibble)
library(dplyr)
library(tidytext)
library(readr)
library(tm)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)

stopwords_pt <- read_delim("C:\\Users\\Aluno\\Desktop\\stopwords.csv",
                           ";",escape_double = FALSE, trim_ws = TRUE)

affin_pt <- read_delim("C:\\Users\\Aluno\\Desktop\\affin_pt.csv",
                           ";",escape_double = FALSE, trim_ws = TRUE)


text <- paste(pdf_text("C:\\Users\\Aluno\\Desktop\\minhabase\\conae\\doc_referencia_conae_2018.pdf")," ")
text <- unlist(strsplit(text,"[.]"))
text <- tibble(sentence = text) 

text$sentence <- text$sentence %>%
  removePunctuation() %>%
  stripWhitespace() %>%
  removeNumbers()

tokens <- text %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word,sentence) %>%
  anti_join(stopwords_pt)

tokens_count <- tokens %>%
  count(word, sort = TRUE)

wordcloud(tokens_count$word, tokens_count$n,
          max.words = 50, scale = c(2,0.5),
          colors = brewer.pal(10, "Spectral"))

tokens_count %>%
  mutate(word = reorder(word,n)) %>%
  head(9) %>%
  ggplot(aes(word,n,fill=factor(word)))+
    scale_fill_brewer(palette="Purples")+
    geom_col()+
    xlab(NULL)+
    coord_flip()


bigrams <- text %>%
  unnest_tokens(bigram, sentence, token = "ngrams", n = 2) %>%
  separate(bigram,c("word1","word2"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word2 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(bigram,word1,word2,sep = " ") %>%
  count(bigram, sort = TRUE)

bigrams_graph <- bigrams %>%
  separate(bigram,c("word1","word2"), sep = " ") %>%
  filter(n>5) %>%
  graph_from_data_frame()

set.seed(2019)
a <- grid::arrow(type = "closed", 
                 length = unit(.15,"inches"))
ggraph(bigrams_graph, layout = "fr")+
  geom_edge_link(aes(edge_alpha = n), 
                 show.legend = FALSE,
                 arrow = a,
                 end_cap = circle(.07,"inches"))+
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name), vjust = 1, hjust = 1)+
  theme_void()

trigrams <- text %>%
  unnest_tokens(trigram, sentence, token = "ngrams", n = 3) %>%
  separate(trigram,c("word1","word2","word3"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word3 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(trigram,word1,word2,word3,sep = " ") %>%
  count(trigram, sort = TRUE)

trigrams_graph <- trigrams %>%
  separate(trigram,c("word1","word2","word3"), sep = " ") %>%
  filter(n>3) %>%
  graph_from_data_frame()

ggraph(trigrams_graph, layout = "fr")+
  geom_edge_link(aes(edge_alpha = n), 
                 show.legend = FALSE,
                 arrow = a,
                 end_cap = circle(.07,"inches"))+
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name), vjust = 1, hjust = 1)+
  theme_void()

word_cors <- tokens %>%
  group_by(word) %>%
  filter(n()>3) %>%
  pairwise_cor(word,linenumber,sort = TRUE)
  
word_cors %>%
  filter(correlation != Inf) %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), 
                 show.legend = FALSE,
                 arrow = a,
                 end_cap = circle(.07,"inches"))+
  geom_node_point(color="#CC00AA",size=5)+
  geom_node_text(aes(label=name), vjust = 1, hjust = 1)+
  theme_void()

affin <- tokens %>%
  inner_join(affin_pt) %>%
  count(index=linenumber %% 80, sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment = positivo - negativo) #%>%
  #mutate(sentiment = sign(sentiment)*log(abs(sentiment)+1))

ggplot(affin, aes(index,sentiment))+
  geom_col(show.legend = TRUE)
