# if required install.packages(c("wordcloud", "tm", "ggplot2", "RCurl"))

library("wordcloud")
library("tm")
library("ggplot2")
library("RCurl")

x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/tweets.csv")
twts <- read.csv(text = x)

str(twts)
# it's a data.frame

text <- twts$text  # pull out the text... 
str(text)   # check the structure
# factor - no I dont' think that's quite what we want...

text <- as.character(twts$text)
str(text)
# so that worked and looks better 

# check a few of the entries...
text[1]
text[2]
text[10]


# defaults are not great - quite slow and probably too many words to be useful. 
# best to add a max.words modifier....
wordcloud(text, max.words = 35)
wordcloud(text, 
          max.words = 35,
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))
# layout is random... 
# set.seed(500) 

# some interesting words here
# my name: brennanpcardiff
# some people I communicate with on twitter: amcunningham, drbillyo, drnostromo
# words like amp, just, the, think, well, day, now, nice, great, new

# good to do some processing....

# convert into a Corpus - a structure for organising text...
text.c <- Corpus(VectorSource(text))
text.c.p <- tm_map(text.c, content_transformer(tolower))
# remove stopwords
text.c.p <- tm_map(text.c.p, removeWords, stopwords("english"))
# get a list of Englisth stopwords....
stopwords(kind = "en")
length(stopwords(kind = "en"))

wordcloud(text.c.p, max.words = 35, colors=brewer.pal(8, "Dark2"))

# so some words removed but leaving others... including smiley face. 
# remove some Punctuation
text.c.p <- tm_map(text.c.p, removePunctuation)

wordcloud(text.c.p, max.words = 35, colors=brewer.pal(8, "Dark2"))

# better... rstats has appeared - good to see. 
# not sure how useful day, the, can, well, like, http are....
# remove more words
text.c.p <- tm_map(text.c.p, removeWords, c("new", "the", "today", "can",
                                            "just", "day", "amp", "http",
                                            "good", "great", "like", "nice",
                                            "well", "brennanpcardiff", "will", "now", "httpt",
                                            "get", "one")) 
set.seed(501)
wordcloud(text.c.p, 
          max.words = 30,
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))



# for more on word frequencies and a graph....... 

# create Document Term Matrix
dtm <- DocumentTermMatrix(text.c.p)
inspect(dtm)
dim(dtm)
 
# Plotting Word Frequencies
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(freq, 14)

# make the data frame for ggplot
wf <- data.frame(word = names(freq), freq = freq)
head(wf,20)

wf.freq <- subset(wf, freq > 35) # data frame with abundant words - 23 words

p <-  ggplot(wf.freq, aes(word, freq)) + 
             geom_bar(stat="identity") +
             xlab("Frequent words") +   # label x-axis
             ylab("Frequency") +    # label y-axis
             ggtitle("Word Frequencies in my tweets") +
             theme_bw() +
             theme(axis.text.x = element_text(angle=45, hjust=1)) 

p  # show the object...

# make another wordle with the frequencies... gives the same plot...
set.seed(501)  # gives a reproducible plot...
wordcloud(names(freq), freq, max.words =30,
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))


