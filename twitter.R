library(twitteR)
library(tm)           #Text Mining
library(wordcloud)    #Wordcloud
library(RColorBrewer)

consumer_key <- "xxxxxxxxx"
consumer_secret <- "xxxxxxxx"
access_token <- "xxxxxx-xxxxxx"
access_secret <- "xxxxxxxx"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

ts <- searchTwitter('Gugu', 
                    since = "2019-11-21",
                    until = "2019-11-22",
                    lang="pt",  
                    n=4000)

ts <- strip_retweets(ts)
df <- twListToDF(ts) # dataframe


vs <- VectorSource(df$text)
CorpusObj<- Corpus(vs)
CorpusObj <- tm_map(CorpusObj, stripWhitespace)
remove.carrigae <- function(x) gsub("[\r\n]", "", x)
CorpusObj <- tm_map(CorpusObj, remove.carrigae)
removeURL <- function(x) gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", x)
CorpusObj <- tm_map(CorpusObj, removeURL)
removeSPE <- function(x) gsub("[^\u0020-\uC3BF]", " ", x)
CorpusObj <- tm_map(CorpusObj, removeSPE)
removeSPE <- function(x) gsub("[\u007F-\u009F]", " ", x)
CorpusObj <- tm_map(CorpusObj, removeSPE)
CorpusObj <- tm_map(CorpusObj, tolower)
CorpusObj <- tm_map(CorpusObj, removePunctuation) 
CorpusObj <- tm_map(CorpusObj, removeNumbers)
CorpusObj <- tm_map(CorpusObj, removeWords, stopwords("portuguese"))
#CorpusObj <- tm_map(CorpusObj, stemDocument)

#CorpusObj <- tm_map(CorpusObj, function(x) iconv(enc2utf8(x), sub = "byte"))
# writeCorpus(tm_map(CorpusObj, PlainTextDocument))
td_mtx <- TermDocumentMatrix(CorpusObj, control = list(minWordLength = 3))
v <- sort(rowSums(as.matrix(td_mtx)), decreasing=TRUE)
df <- data.frame(word=names(v), freq=v)
pal2 <- brewer.pal(8,"Dark2")

#png("wordcloud_packages.png", width=12,height=8, units='in', res=500)
wordcloud(df$word, df$freq, min.freq=1,max.words=200, random.order=FALSE, rot.per=.15, colors=pal2, scale=c(2,1))
#dev.off()

# Dendrograma
tdm <- TermDocumentMatrix(CorpusObj)
tdm <- removeSparseTerms(tdm, sparse = 1)
df <- as.data.frame(inspect(tdm))
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)
#rect.hclust(fit.ward2, k=3) 

