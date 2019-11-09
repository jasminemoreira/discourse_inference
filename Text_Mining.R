# install.packages("installr")
# install.packages('stringr')
# library(installr)
# updateR()

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
#install.packages("topicmodels")
#install.packages("ldatuning")


library(topicmodels)
library(ldatuning)
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
library(stringr)

stopwords_pt <- read_delim("C:\\Users\\Aluno\\Desktop\\stopwords.csv",
                           ";",escape_double = FALSE, trim_ws = TRUE)

stopwords_pt <- add_row(stopwords_pt, word = "disse")
stopwords_pt <- add_row(stopwords_pt, word = "pode")
stopwords_pt <- add_row(stopwords_pt, word = "poderia")
stopwords_pt <- add_row(stopwords_pt, word = "fazer")

text <- paste(pdf_text("C:\\Users\\Aluno\\Desktop\\bicent.pdf")," ")
text <- unlist(strsplit(text,"[.]"))
text <- tibble(sentence = text) 
text$sentence <- str_replace(text$sentence,"tam-\nbém", "também")

tokens <- text %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word,sentence) %>%
  anti_join(stopwords_pt)

tokens_count <- tokens %>%
  count(word, sort = TRUE)

wordcloud(tokens_count$word, tokens_count$n,
          max.words = 50, scale = c(2,0.5),
          colors = brewer.pal(10, "Spectral"))

numRegister <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(numRegister)
tokens_count %>%
  mutate(word = reorder(word,n)) %>%
  head(numRegister) %>%
  ggplot(aes(word,n,fill=factor(word)))+
  scale_fill_manual(values = mycolors) +
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

numRegister <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(numRegister)
bigrams %>%
  mutate(bigram = reorder(bigram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(bigram,n,fill=factor(bigram)))+
  scale_fill_manual(values = mycolors) +
  geom_col()+
  xlab(NULL)+
  coord_flip()


trigrams <- text %>%
  unnest_tokens(trigram, sentence, token = "ngrams", n = 3) %>%
  separate(trigram,c("word1","word2","word3"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
  filter(!word3 %in% as.vector(t(stopwords_pt$word))) %>% 
  unite(trigram,word1,word2,word3,sep = " ") %>%
  count(trigram, sort = TRUE)

numRegister <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(numRegister)
trigrams %>%
  mutate(bigram = reorder(trigram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(bigram,n,fill=factor(trigram)))+
  scale_fill_manual(values = mycolors) +
  geom_col()+
  xlab(NULL)+
  coord_flip()

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




#Análise de Tópicos
dtm <- tokens %>%
  count(linenumber,word, sort=TRUE) %>%
  cast_dtm(linenumber,word,n) 

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)


# Latent Dirichlet Allocation - separa os textos por assuntos
corpus_lda <- dtm %>%
  LDA(k=10,control=list(seed=1234))

get_terms(corpus_lda, 15)

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
  facet_wrap(~ topic, scales = "free", nrow = 3) +
  xlab("Termos") +
  ylab("Beta") +
  scale_x_continuous(
    breaks = corpus_top_terms$order,
    labels = corpus_top_terms$term,
    expand = c(0,0),
    trans = "reverse"
  )+
  coord_flip()



