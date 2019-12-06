#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec  4 14:21:09 2019

@author: jasmine
"""

import nltk
#nltk.download()
from nltk.book import *
from nltk import word_tokenize
from nltk.collocations import BigramCollocationFinder
import matplotlib.pyplot as plt
import numpy as np
from nltk.tokenize import RegexpTokenizer
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
from gensim import corpora, models
import gensim


text1
text1.concordance("monstrous")
text1.similar("monstrous")
text1.common_contexts(["monstrous", "curious"])
text4.dispersion_plot(["citizens", "democracy", "freedom", "duties", "America"])

len(text3)

sorted(set(text3))

100 * len(set(text3)) / len(text3)

text4.count("America")
text4.count("America")/len(text4) * 100

def lexical_diversity(text):
    return len(set(text)) / len(text) * 100

lexical_diversity(text4)

sent1 = ['Call', 'me', 'Ishmael', '.']

lexical_diversity(sent1)

text4[0:1]


saying = ['After', 'all', 'is', 'said', 'and', 'done','more', 'is', 'said', 'than', 'done']

tokens = set(saying)
tokens = sorted(tokens)
tokens
tokens[-2:]


fdist1 = FreqDist(text1)
fdist1

V = set(text1)
long_words = [w for w in V if len(w) > 15]
sorted(long_words)

fdist5 = FreqDist(text5)
sorted([w for w in set(text5) if len(w) > 7 and fdist5[w] > 7])

text4.collocation_list()


def plot_bar_x(labels,values):
    # this is for plotting purpose
    index = np.arange(len(labels))
    plt.bar(index, values)
    plt.xlabel('Ngram', fontsize=10)
    plt.ylabel('No of Ocurrences', fontsize=10)
    plt.xticks(index, labels, fontsize=10, rotation=70)
    plt.title('NGrams Frequency')
    plt.show()

text = "obama says that obama says that the war is happening"
finder = BigramCollocationFinder.from_words(word_tokenize(text))
bigram = []
bfreq = []
for k,v in finder.ngram_fd.items():
    bigram.append(k[0]+" "+k[1])
    bfreq.append(v)
    print(k,v)
plot_bar_x(bigram,bfreq)

from nltk.collocations import TrigramCollocationFinder
trigram = []
tfreq = []  
finder = TrigramCollocationFinder.from_words(word_tokenize(text))
for k,v in finder.ngram_fd.items():
    trigram.append(k[0]+" "+k[1]+" "+k[2])
    tfreq.append(v)
    print(k,v)
plot_bar_x(trigram,tfreq)

from nltk.collocations import QuadgramCollocationFinder    
finder = QuadgramCollocationFinder.from_words(word_tokenize(text))
quadgram = []
qfreq = []  
for k,v in finder.ngram_fd.items():
    quadgram.append(k[0]+" "+k[1]+" "+k[2]+" "+k[3])
    qfreq.append(v)
    print(k,v)
plot_bar_x(quadgram,qfreq)


#Análise de Tópicos

tokenizer = RegexpTokenizer(r'\w+')
# create English stop words list
en_stop = set(stopwords.words('english')) 
# Create p_stemmer of class PorterStemmer
p_stemmer = PorterStemmer()
    
# create sample documents
doc_a = "Brocolli is good to eat. My brother likes to eat good brocolli, but not my mother."
doc_b = "My mother spends a lot of time driving my brother around to baseball practice."
doc_c = "Some health experts suggest that driving may cause increased tension and blood pressure."
doc_d = "I often feel pressure to perform well at school, but my mother never seems to drive my brother to do better."
doc_e = "Health professionals say that brocolli is good for your health." 

# compile sample documents into a list
doc_set = [doc_a, doc_b, doc_c, doc_d, doc_e]

# list for tokenized documents in loop
texts = []

# loop through document list
for i in doc_set:
    
    # clean and tokenize document string
    raw = i.lower()
    tokens = tokenizer.tokenize(raw)
    # remove stop words from tokens
    stopped_tokens = [i for i in tokens if not i in en_stop]  
    # stem tokens
    stemmed_tokens = [p_stemmer.stem(i) for i in stopped_tokens]  
    # add tokens to list
    texts.append(stemmed_tokens)

# turn our tokenized documents into a id <-> term dictionary
dictionary = corpora.Dictionary(texts)
    
# convert tokenized documents into a document-term matrix
corpus = [dictionary.doc2bow(text) for text in texts]

def plot_bar_x(labels,values):
    # this is for plotting purpose
    index = np.arange(len(labels))
    plt.bar(index, values)
    plt.xlabel('Word', fontsize=10)
    plt.ylabel('Beta', fontsize=10)
    plt.xticks(index, labels, fontsize=10, rotation=70)
    plt.title('Topic Analysis')
    plt.show()

# generate LDA model
ntopics = 2
ldamodel = gensim.models.ldamodel.LdaModel(corpus, num_topics=ntopics, id2word = dictionary, passes=20)

print(ldamodel.print_topics(num_topics=ntopics, num_words=4))
topics = ldamodel.show_topics( num_words=5,formatted=False)

for n,t in topics:
    words = [i[0] for i in t]
    betas = [i[1] for i in t]
    plot_bar_x(words,betas)

