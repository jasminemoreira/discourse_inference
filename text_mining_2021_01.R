#install.packages("installr")
#library(installr)
#updateR()

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
# install.packages("topicmodels")
# install.packages("ldatuning")
# install.packages("purrr")

library(pdftools)
library(tibble)
library(tidytext)
library(dplyr)
library(ggplot2)
library(readr)
library(wordcloud)
library(tidyr)
library(widyr)
library(tm)
library(topicmodels)

#Carregar stop words
stopwords_pt <- read_delim("C:\\Users\\jasmi\\OneDrive\\Área de Trabalho\\PLN RV\\stopwords.csv",
                           ";",escape_double = FALSE, trim_ws = TRUE)

affin_pt <- read_delim("C:\\Users\\jasmi\\OneDrive\\Área de Trabalho\\PLN RV\\affin_pt.csv",
                       ";",escape_double = FALSE, trim_ws = TRUE)


#Carregar PDF e converter para texto
text <- pdf_text("C:\\Users\\jasmi\\OneDrive\\Área de Trabalho\\PLN RV\\senhor_aneis_1.pdf")
text <- unlist(strsplit(text,"[.]"))
text <- tibble(sentence = text)

text$sentence <- text$sentence %>%
  removePunctuation() %>%
  stripWhitespace() %>%
  removeNumbers()

#Tokenizar texto (separar em palavras) e remover stop words
tokens <- text %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word,sentence) %>%
  anti_join(stopwords_pt)

#Sumarização (contagem das palavras)
tokens_count <- tokens %>%
  count(word, sort = TRUE)

#Análise gráfica de frequência
tokens_count %>%
  mutate(word = reorder(word,n)) %>%
  head(9) %>%
  ggplot(aes(word,n,fill=factor(word)))+
  geom_col()+
  coord_flip()

#Nuvem de palavras
wordcloud(tokens_count$word, tokens_count$n,
          max.words = 100, scale = c(2,0.5),
          colors = brewer.pal(9, "Set1"), 
          random.order = FALSE)

#Análise de bigramas
bigrams <- text %>%
  unnest_tokens(bigram, sentence, token = "ngrams", n = 2) %>%
  separate(bigram,c("word1","word2"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word2 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(bigram,word1,word2,sep = " ") %>%
  count(bigram, sort = TRUE)

bigrams %>%
  mutate(bigram = reorder(bigram,n)) %>%
  head(9) %>%
  ggplot(aes(bigram,n,fill=factor(bigram)))+
  geom_col()+
  coord_flip()

#Análise de trigramas
trigrams <- text %>%
  unnest_tokens(trigram, sentence, token = "ngrams", n = 3) %>%
  separate(trigram,c("word1","word2","word3"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word3 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(trigram,word1,word2,word3,sep = " ") %>%
  count(trigram, sort = TRUE)

trigrams %>%
  mutate(trigram = reorder(trigram,n)) %>%
  head(9) %>%
  ggplot(aes(trigram,n,fill=factor(trigram)))+
  geom_col()+
  coord_flip()

#Análise de quadrigramas
fourgrams <- text %>%
  unnest_tokens(fourgram, sentence, token = "ngrams", n = 4) %>%
  separate(fourgram,c("word1","word2","word3","word4"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word4 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(fourgram,word1,word2,word3,word4,sep = " ") %>%
  count(fourgram, sort = TRUE)

fourgrams %>%
  mutate(fourgram = reorder(fourgram,n)) %>%
  head(9) %>%
  ggplot(aes(fourgram,n,fill=factor(fourgram)))+
  geom_col()+
  coord_flip()

#Análise de pentagramas
fivegrams <- text %>%
  unnest_tokens(fivegram, sentence, token = "ngrams", n = 5) %>%
  separate(fivegram,c("word1","word2","word3","word4","word5"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word5 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(fivegram,word1,word2,word3,word4,word5,sep = " ") %>%
  count(fivegram, sort = TRUE)

fivegrams %>%
  mutate(fivegram = reorder(fivegram,n)) %>%
  head(9) %>%
  ggplot(aes(fivegram,n,fill=factor(fivegram)))+
  geom_col()+
  coord_flip()

#Análise de correlação de palavras
word_cors <- tokens %>%
  group_by(word) %>%
  filter(n()>15) %>%
  pairwise_cor(word,linenumber,sort = TRUE) %>%
  rename(word = item1) %>%
  inner_join(tokens_count, by="word") %>%
  mutate(linenumber = row_number()) %>%
  filter(linenumber %% 2 != 0 ) %>%
  unite(pair,word,item2,sep = " - ") 

word_cors %>%
  mutate(pair = reorder(pair,correlation)) %>%
  head(9) %>%
  ggplot(aes(pair,correlation,fill=factor(n)))+
  geom_col()+
  guides(fill=guide_legend(title="frequency"))+
  coord_flip()

#Análise de polaridade (sentimento)
affin <- tokens %>%
  inner_join(affin_pt) %>%
  count(index=floor(linenumber/400), sentiment) %>%  
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment = positivo - negativo) #%>%
  #mutate(sentiment = sign(sentiment)*log10(abs(sentiment)+1))

ggplot(affin, aes(index,sentiment))+
  geom_col(show.legend = TRUE)


#Análise de tópicos (Alocação Latente de Dirichlet - LDA)
dtm <- tokens %>%
  count(linenumber,word, sort=TRUE) %>%
  cast_dtm(linenumber,word,n)

corpus_lda <- LDA(dtm, k = 7, control = list(seed = 1234))
get_terms(corpus_lda, 10)

corpus_topics <- tidy(corpus_lda,matrix="beta")
corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  arrange(topic, -beta) %>%
  do(head(., n = 15)) %>%
  ungroup() %>%
  mutate(term=reorder(term,beta)) %>%
  mutate(order = row_number())

corpus_top_terms %>%
  ggplot(aes(order, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  xlab("Termos") +
  ylab("Beta") +
  scale_x_continuous(
    breaks = corpus_top_terms$order,
    labels = corpus_top_terms$term,
    expand = c(0,0),
    trans = "reverse"
  )+
  coord_flip()
