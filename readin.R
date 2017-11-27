library(tm)
library(readr)
library(quanteda)
library(tidytext)
library(stringr)
library(dplyr)
library(data.table)

## Define some functions

cleanFeatures <- function(x_dfm){
    temp <- dfm_select(
        x_dfm
        , pattern = "^.*â.*"
        , selection = "remove"
        , valuetype = "regex"
    )
    
    temp <- dfm_select(
        temp
        , pattern = "^.*â.*"
        , selection = "remove"
        , valuetype = "regex"
    )
    
    temp <- dfm_select(
        temp
        , pattern = "^.*[0-9].*\\b"
        , selection = "remove"
        , valuetype = "regex"
    )
    
    temp <- dfm_select(
        temp
        , pattern = "^.*[^a-z'. \\_àèìòùáéíóúâêîôûãñõäëïöüåæœçðø¿¡ß-].*\\b"
        , selection = "remove"
        , valuetype = "regex"
    )
    
    temp <- dfm_select(
        temp
        , pattern = profanity
        , selection = "remove"
    )
    
    return(temp)
}


splitNGram <- function(x_DT){
    # A Data.Table will be updated globally
    # Not locally to the function as you would expect from a DataFrame
    x_DT[, c("phrase", "predict") := tstrsplit(ngram, ' (?=[^ ]+$)', perl=TRUE)]
    setcolorder(x_DT,
                c("ngram","phrase","predict","docfreq"))
}

en_US.twitter.txt

con <- file("..\\..\\Data\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt", "rb")
con2 <- file("..\\..\\Data\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt", "rb")
con3 <- file("..\\..\\Data\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt", "rb")
data_blogs <- read_lines(con)
data_news <- read_lines(con2)
data_twitter <- read_lines(con3)
close(con)
close(con2)
close(con3)

con <- file("../../Data/obsceneEnglish.txt", "rb")
profanity <- read_lines(con)
close(con)
save(profanity, file = "..\\..\\Data\\profanity.RData")

head(data_blogs)
length(data_blogs)
head(data_twitter)

## Convert to Corpus
blogsCorpus <- corpus(data_blogs)
newsCorpus <- corpus(data_news)
twitterCorpus <- corpus(data_twitter)
rm(data_blogs)
rm(data_news)
rm(data_twitter)


summary(blogsCorpus)
summary(newsCorpus)
summary(twitterCorpus)
texts(newsCorpus)[1:20]


splitSet602020 <- function(n){
    set.seed(20171112)
    # assign each document to training hold out of test set
    sample(rep(1:3, diff(floor(n * c(0, 0.6, 0.8, 1)))))
}

nBlogs <- ndoc(blogsCorpus)
nNews <- ndoc(newsCorpus)
nTweets <- ndoc(twitterCorpus)
ss <- splitSet602020(nBlogs)
ssNews <- splitSet602020(nNews)
ssTweets <- splitSet602020(nTweets)

blogsTrain <- corpus_subset(blogsCorpus, subset = (ss==1))
newsTrain <- corpus_subset(newsCorpus, subset = (ssNews==1))
tweetsTrain <- corpus_subset(twitterCorpus, subset = (ssTweets == 1))

summary(newsTrain)
rm(blogsCorpus)
rm(newsCorpus)
rm(twitterCorpus)

blogsHoldOut <- corpus_subset(blogsCorpus, subset = (ss == 2))
blogsTest <- corpus_subset(blogsCorpus, subset = (ss==3))



ndoc(blogsTrain)
ndoc(blogsHoldOut)
ndoc(blogsTest)
ndoc(tweetsTrain)

writeLines(as.character(mycorpus), con="mycorpus.txt")
write_lines(as.character(blogsTest), path = "..\\..\\Data\\blogsTest.txt")

rm(blogsHoldOut)
rm(blogsTest)

blogsSentences <- corpus_reshape(blogsTrain, to = "sentences")
newsSentences <- corpus_reshape(newsTrain, to = "sentences")
tweetsSentences <- corpus_reshape(tweetsTrain, to = "sentences")
blogsSentences[[1]]
View(blogsSentences[c(1:20),])
View(newsSentences[c(1:20),])
texts(blogsSentences)[1:20]
texts(newsSentences)[1:20]
texts(tweetsSentences)[1:20]
summary(blogsSentences)
summary(newsSentences)
head(blogsSentences)

docvars(blogsSentences, "source") <- "blogs"
docvars(newsSentences, "source") <- "news"
docvars(tweetsSentences, "source") <- "tweets"
summary(blogsSentences)
summary(newsSentences)
summary(tweetsSentences)

TrainSentences <- blogsSentences + newsSentences + tweetsSentences
corpusTrain <- blogsTrain + newsTrain
summary(corpusTrain)
texts(corpusTrain)[1:20]
rm(blogsTrain)
rm(corpusTrain)
rm(tweetsTrain)

options(width = 200)
kwic(corpusTrain, "terror")

corpus_reshape(corpusTrain, to = "documents")
summary(corpusTrain)


# Make a document frequency matrix

trainDfm <- dfm(
    corpusTrain
    , tolower = TRUE
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , remove = stopwords("english")
    , verbose = quanteda_options("verbose")
    )
trainDfm[1:10, 1:10]
texts(corpusTrain)[c(1,2)] # compare this to the original text

topfeatures(trainDfm, 100)  # top words

trainDfm <- dfm_select(
    trainDfm
    , pattern = "^.*â.*"
    , selection = c("remove")
    , valuetype = c("regex")
)

trainDfm <- dfm_select(
    trainDfm
    , pattern = "^[0-9].+\\b"
    , selection = c("remove")
    , valuetype = c("regex")
)

trainDfm <- dfm_select(
    trainDfm
    , pattern = "^.*[^a-z'.\\-àèìòùáéíóúâêîôûãñõäëïöüåæœçðø¿¡ß].*\\b"
    , selection = c("remove")
    , valuetype = c("regex")
)

trainDfm
topfeatures(trainDfm, 500)  # top words

set.seed(2017)
textplot_wordcloud(trainDfm, min.freq = 25675, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

trainDfm <- dfm(
    trainDfm
    , remove = "^â.+\\b"
    , valuetype = c("regex")
    , verbose = quanteda_options("verbose")
)
trainDfm <- removeFeatures(trainDfm)
trainDfm <- dfm(
    trainDfm
    , removeFeatures() = "^[0-9].+\\b"
    , valuetype = c("regex")
    , verbose = quanteda_options("verbose")
)

## quick selection or removal
selectDfm <- dfm_select(
    trainDfm
    , pattern = "^.*â.*"
    , selection = c("keep")
    , valuetype = c("regex")
)

selectDfm <- dfm_select(
    trainDfm
    , pattern = "^[0-9].+\\b"
    , selection = c("keep")
    , valuetype = c("regex")
)

selectDfm <- dfm_select(
    trainDfm
    , pattern = "^.*[^a-z'.\\-àèìòùáéíóúâêîôûãñõäëïöüåæœçðø¿¡ß].*\\b"
    , selection = c("keep")
    , valuetype = c("regex")
)
selectDfm <- dfm_select(
    selectDfm
    , pattern = "^.*['//-].*\\b"
    , selection = c("remove")
    , valuetype = c("regex")
)

selectDfm <- dfm(
    trainDfm
    , select = "^â.+\\b"
    , valuetype = c("regex")
    , verbose = quanteda_options("verbose")
    )

selectDfm <- dfm(
    trainDfm
    , select = "^â€.+\\b"
    , valuetype = c("regex")
    , verbose = quanteda_options("verbose")
)

selectDfm <- dfm(
    trainDfm
    , select = "^[0-9].+\\b"
    , valuetype = c("regex")
    , verbose = quanteda_options("verbose")
)

selectDfm <- dfm(
    trainDfm
    , select = "^â.*[^a-z].*\\b"
    , valuetype = c("regex")
    , verbose = quanteda_options("verbose")
)

selectDfm
topfeatures(selectDfm, 500)  # top words

#     , select = "^.*\\W+.*\\b" gave me hyphens - and appostrophies '

#> topfeatures(trainDfm, 20)  # top words
#      .     the       ,     and      to       a      of      in       "       i    that 
#2468948 2299848 2245785 1186680 1179957 1065286  989000  761766  678225  559931  484493 
#     is     for      it      on    with     was      as     you      at 
# 429477  428879  374540  324807  324495  305079  246482  236450  230309 


## TF-IDF Term Frequency - Inverse Document Frequency
train_tfidf <- tfidf(trainDfm, scheme_tf = "prop")
topfeatures(train_tfidf,100)
topfeatures(trainDfm,100)

### Ngrams

quingramTrain <- dfm(
    TrainSentences
    , tolower = TRUE
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , stem = FALSE
    , ngrams = 5
    , remove_twitter = TRUE
    , remove_url = TRUE
    , concatenator = " "
    , verbose = TRUE)

trigramTrain <- dfm(
    TrainSentences
    , tolower = TRUE
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , stem = FALSE
    , ngrams = 3
    # add this to all ngrams:
    #, concatenator = " "
    , verbose = TRUE)

bigramTrain <- dfm(
    TrainSentences
    , tolower = TRUE
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , stem = FALSE
    , ngrams = 2
    # add this to all ngrams:
    , concatenator = " "
    , verbose = TRUE)

unigramTrain <- dfm(
    TrainSentences
    , tolower = TRUE
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , stem = FALSE
    , ngrams = 1
    # add this to all ngrams:
    , concatenator = " "
    , verbose = TRUE)

trigramTrain
bigramTrain
unigramTrain
topfeatures(quingramTrain, 50)
topfeatures(trigramTrain, 1000)  # top words
topfeatures(bigramTrain, 200)  # top words
topfeatures(unigramTrain, 100)
featnames(trigramTrain)[1:20]
docfreq(trigramTrain)[1:20]
summary(trigramTrain)
summary(bigramTrain)
trigramTrain[1:5, 1:5]
bigramTrain[1:5, 1:5]
unigramTrain[1:5, 1:5]

## Trim 
## quingramTrain for instance is 4.4 Gb
quingramTrain <- dfm_trim(quingramTrain, min_docfreq = 4)

### Clean Ngrams

trigramTrain <- dfm_select(
    trigramTrain
    , pattern = "^.*â.*"
    , selection = "remove"
    , valuetype = "regex"
)

trigramTrain <- dfm_select(
    trigramTrain
    , pattern = "^.*â.*"
    , selection = "remove"
    , valuetype = "regex"
)

trigramTrain <- dfm_select(
    trigramTrain
    , pattern = "^.*[0-9].*\\b"
    , selection = "remove"
    , valuetype = "regex"
)

trigramTrain <- dfm_select(
    trigramTrain
    , pattern = "^.*[^a-z'.\\_àèìòùáéíóúâêîôûãñõäëïöüåæœçðø¿¡ß-].*\\b"
    , selection = "remove"
    , valuetype = "regex"
)

## Testing cleaning

tempTrigram <- dfm_select(
    trigramTrain
    , pattern = "^.*â.*"
    , selection = "keep"
    , valuetype = "regex"
)

tempTrigram <- dfm_select(
    trigramTrain
    , pattern = "^.*[0-9].*\\b"
    , selection = c("keep")
    , valuetype = c("regex")
)

tempTrigram <- dfm_select(
    trigramTrain
    , pattern = "^.*[^a-z'.\\_àèìòùáéíóúâêîôûãñõäëïöüåæœçðø¿¡ß-].*\\b"
    , selection = c("keep")
    , valuetype = c("regex")
)

tempTrigram <- dfm_select(
    bigramTrain
    , pattern = "^.*[^a-z'. \\_àèìòùáéíóúâêîôûãñõäëïöüåæœçðø¿¡ß-].*\\b"
    , selection = c("keep")
    , valuetype = c("regex")
)

tempTrigram <- dfm_trim(tempTrigram, min_count = 1, min_docfreq = 1)
tempTrigram <- dfm_trim(tempTrigram, min_docfreq = 1)
topfeatures(tempTrigram, 100)
tempTrigram
rm(tempTrigram)

# Function to bring together all the cleaning steps
quingramTrain <- cleanFeatures(quingramTrain)
bigramTrain <- cleanFeatures(bigramTrain)
unigramTrain <- cleanFeatures(unigramTrain)

## Really important conversion
quingramDT <- data.table(
    ngram = featnames(quingramTrain), 
    docfreq = docfreq(quingramTrain),
    keep.rownames = F, 
    stringsAsFactors = F
)
trigramDT <- data.table(
    ngram = featnames(trigramTrain), 
    docfreq = docfreq(trigramTrain),
    keep.rownames = F, 
    stringsAsFactors = F
                        )
bigramDT <- data.table(
    ngram = featnames(bigramTrain), 
    docfreq = docfreq(bigramTrain),
    keep.rownames = F, 
    stringsAsFactors = F
)
unigramDT <- data.table(
    ngram = featnames(unigramTrain), 
    docfreq = docfreq(unigramTrain),
    keep.rownames = F, 
    stringsAsFactors = F
)

quingramDT
trigramDT
bigramDT
unigramDT


quingramDT[ngram %like% "beer"]

splitNGram(quingramDT)
splitNGram(trigramDT)
splitNGram(bigramDT)
#splitNGram not working
trigramDT[, c("phrase", "predict") := tstrsplit(ngram, ' (?=[^ ]+$)', perl=TRUE)]
setcolorder(trigramDT,
            c("ngram","phrase","predict","docfreq"))

## Next time make ngrams without "_"
trigramDT[, c("ngram", "phrase") := c(gsub("_", " ", ngram), gsub("_", " ", phrase))]

save(quingramDT, file = "../../Data/quingramDT.RData")
save(trigramDT, file = "../../Data/trigramDT.RData")
save(bigramDT, file = "../../Data/bigramDT.RData")
save(unigramDT, file = "../../Data/unigramDT.RData")
load(file = "../../Data/trigramDT.RData")
load(file = "../../Data/bigramDT.RData")
load(file = "../../Data/unigramDT.RData")

# ngrams take up a lot of memory
rm(quingramTrain)
rm(trigramTrain)
rm(bigramTrain)
rm(unigramTrain)

unigramDT[ngram == "love"]

set.seed(2017)
babyTri <- dfm_sample(trigramTrain, size = 30, margin = "documents")
# remove the many features with a zero count after the sample
babyTri <- dfm_trim(babyTri, min_count = 1, min_docfreq = 1)

babyTri[1:5, 1:5]


babyDT <- head(trigramDT, 20)

babyDT <- head(trigram)
dfm_select(babyDT, "fort", selection = "keep")

#Split trigram into two
## best one
babyDT[, c("phrase", "predict") := tstrsplit(trigram, '_(?=[^_]*$)', perl=TRUE)]

#Split trigram into one
#babyDT[, c("word1", "word2", "predict") := tstrsplit(trigram, "_", fixed=TRUE)]

babyDT <- rbind(babyDT, list("love_you_too",20, "love_you", "too"))
setkey(babyDT, phrase)

tempbabyDT <- babyDT[,.(docfreq_phrase = sum(docfreq)),by=phrase]
setkey(tempbabyDT, phrase)

# inner join to get phrase frequency
# A[B, bb:=i.b, on='a'] is efficient join
babyDT[tempbabyDT, phraseFreq :=i.docfreq_phrase, on = 'phrase']
#DT[, V1 := round(exp(V1),2)]
babyDT[, prop := docfreq/phraseFreq]
setcolorder(babyDT, c("trigram", "phrase", "predict", "docfreq", "phraseFreq", "prop"))
babyDT[, phrase := gsub("_", " ", phrase)]

babyDT

save(babyDT, file = "../../Data/babyDT.RData")
load(file = "../../Data/babyDT.RData")

## predict based on input text
txt <- "love you"
babyDT[phrase == txt][order(-prop)]
babyDT[phrase == txt][order(-prop)][, predict]
babyDT[phrase == txt][order(-prop)][, .(predict)]



trigramDT[, c("word1", "word2", "predict") := tstrsplit(trigram, "_", fixed=TRUE)]
tstrsplit(trigramDT, "_", names = c("word1", "word2", "predict"))


tstrsplit(x, ..., fill=NA, type.convert=FALSE, keep, names=FALSE)
change to
dt[, c("PX", "PY") := tstrsplit(PREFIX, "_", fixed=TRUE)]
### TF-IDF
