#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul  3 20:06:23 2020

@author: Jasmine Moreira
"""

pt_stop = set()

import csv
for row in csv.reader(open('/Users/jasmine/Desktop/stopwords.csv',encoding='utf-8'), delimiter=','):
    pt_stop.add(row[0])
    
    
import PyPDF2
pdfFileObject = open('/Users/jasmine/Desktop/republica.pdf', 'rb')
pdfReader = PyPDF2.PdfFileReader(pdfFileObject)
count = pdfReader.numPages
doc = ""
for i in range(count):
    page = pdfReader.getPage(i)
    doc += page.extractText()

doc_set = [doc]

tokens = []

from nltk.tokenize import RegexpTokenizer
tokenizer = RegexpTokenizer(r'\w+')

# loop through document list
for document in doc_set:
    # clean and tokenize document string
    raw = document.lower()
    tokens_i = tokenizer.tokenize(raw)
    stopped_tokens = [token for token in tokens_i if not token in pt_stop]
    tokens.append(stopped_tokens)

from nltk import FreqDist
dic = FreqDist(tokens[0])
dic.plot(50, cumulative=False)


from nltk.util import ngrams
bigrams = ngrams(tokens[0],2)
trigrams = ngrams(tokens[0],3)
fourgrams = ngrams(tokens[0],4)
fivegrams = ngrams(tokens[0],5)

# print(*map(' '.join, bigrams), sep=', ')

from collections import Counter
Counter(bigrams).most_common(10)
Counter(trigrams).most_common(10)
Counter(fourgrams).most_common(10)
Counter(fivegrams).most_common(10)


import gensim
# turn our tokenized documents into a id <-> term dictionary
dictionary = gensim.corpora.Dictionary(tokens)

# convert tokenized documents into a document-term matrix
corpus = [dictionary.doc2bow(doc) for doc in tokens]

# generate LDA model
ntopics = 6
ldamodel = gensim.models.ldamodel.LdaModel(corpus, num_topics=ntopics, id2word = dictionary, passes=20)

print(ldamodel.print_topics(num_topics=ntopics, num_words=4))
topics = ldamodel.show_topics( num_words=10,formatted=False)


import matplotlib.pyplot as plt
import numpy as np
def plot_bar_x(labels,values):
    # this is for plotting purpose
    index = np.arange(len(labels))
    plt.bar(index, values)
    plt.xlabel('Word', fontsize=10)
    plt.ylabel('Beta', fontsize=10)
    plt.xticks(index, labels, fontsize=10, rotation=70)
    plt.title('Topic Analysis')
    plt.show()

for n,t in topics:
    words = [i[0] for i in t]
    betas = [i[1] for i in t]
    plot_bar_x(words,betas)
