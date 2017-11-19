library(tm)
library(readr)
library(quanteda)
library(tidytext)
library(stringr)
library(dplyr)


txt <- c(text1 = "This is $10 in 999 different ways.\n up and down; left and right!", 
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
txt <- c(text1 = "honey was an essential forest resourceâ€¦as a sweetener",
         text2 = "1,000 dollars",
         text3 = "I have first sentence. Then I have a second sentence. Big sentence third")
myDfm <- dfm(txt)
myDfm <- dfm(
    txt
    , tolower = TRUE
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , remove = stopwords("english")
    , verbose = quanteda_options("verbose")
)
myDfm
topfeatures(myDfm)

dfm_select(
    myDfm
    , pattern = "^.*â.*"
    , selection = c("keep")
    , valuetype = c("regex")
)
dfm_select(
    myDfm
    , pattern = "^.*â.*"
    , selection = c("remove")
    , valuetype = c("regex")
)


dfm("Banking industry", stem = TRUE, ngrams = 2, verbose = FALSE)
dfm(myDfm, stem = TRUE, ngrams = 2, verbose = FALSE)
ngramDfm <- dfm(
    txt
    , tolower = TRUE
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , remove = stopwords("english")
    , stem = FALSE
    , ngrams = 1:3
    , verbose = TRUE)
topfeatures(ngramDfm,100)

ngramDfm <- dfm_select(
    ngramDfm
    , pattern = "^.*[^a-z'.\\_-àèìòùáéíóúâêîôûãñõäëïöüåæœçðø¿¡ß].*\\b"
    #, pattern = "^.*[^a-zâ_].*\\b"
    , selection = c("remove")
    , valuetype = c("regex")
)

ngramDfm <- txt %>%
    dfm(
    tolower = TRUE
    , what = "sentence"
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , remove = stopwords("english")
    , stem = FALSE
    , ngrams = 1:3
    , verbose = TRUE)


ngramDfm <- dfm_select(
    ngramDfm
    , pattern = "^.*\.*$"
    , selection = c("keep")
    , valuetype = c("regex")
)



topfeatures(ngramDfm,100)



ngramDfm <- txt %>%
    tokens(what = "sentence") %>%
    dfm(
        tolower = TRUE
        , remove_numbers = TRUE
        , remove_punct = TRUE
        , remove = stopwords("english")
        , stem = FALSE
        , ngrams = 1:3
        , verbose = TRUE
        )


blogsSentences <- corpus_reshape(blogsTrain, to = "sentences")
txtCorpus <- corpus(txt)
txtSentences <- corpus_reshape(txtCorpus, to = "sentences")
ngramDfm <- dfm(
    txtSentences
    , tolower = TRUE
    , remove_numbers = TRUE
    , remove_punct = TRUE
    , remove = stopwords("english")
    , stem = FALSE
    , ngrams = 1:3
    , verbose = TRUE)



#### tf-idf
mydfm <- data_dfm_lbgexample
mydfm <- as.dfm(data_dfm_lbgexample)
head(mydfm[, 5:10])
head(tfidf(mydfm)[, 5:10])
docfreq(mydfm)[5:15]
head(tf(mydfm)[, 5:10])
