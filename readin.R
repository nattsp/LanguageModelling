library(tm)
library(readr)
library(quanteda)
library(tidytext)
library(stringr)
library(dplyr)


con <- file("..\\..\\Data\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt", "rb")
con2 <- file("..\\..\\Data\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt", "rb")
data_blogs <- read_lines(con)
data_news <- read_lines(con2)
close(con)
close(con2)

head(data_blogs)
length(data_blogs)

## Convert to Corpus
blogsCorpus <- corpus(data_blogs)
newsCorpus <- corpus(data_news)
rm(data_blogs)
rm(data_news)


summary(blogsCorpus)
summary(newsCorpus)
texts(newsCorpus)[1:20]


splitSet602020 <- function(n){
    set.seed(20171112)
    # assign each document to training hold out of test set
    sample(rep(1:3, diff(floor(n * c(0, 0.6, 0.8, 1)))))
}

nBlogs <- ndoc(blogsCorpus)
nNews <- ndoc(newsCorpus)
ss <- splitSet602020(nBlogs)
ssNews <- splitSet602020(nNews)

blogsTrain <- corpus_subset(blogsCorpus, subset = (ss==1))
newsTrain <- corpus_subset(newsCorpus, subset = (ssNews==1))

summary(newsTrain)
rm(newsCorpus)

blogsHoldOut <- corpus_subset(blogsCorpus, subset = (ss == 2))
blogsTest <- corpus_subset(blogsCorpus, subset = (ss==3))



ndoc(blogsTrain)
ndoc(blogsHoldOut)
ndoc(blogsTest)

writeLines(as.character(mycorpus), con="mycorpus.txt")
write_lines(as.character(blogsTest), path = "..\\..\\Data\\blogsTest.txt")

rm(blogsHoldOut)
rm(blogsTest)

blogsSentences <- corpus_reshape(blogsTrain, to = "sentences")
newsSentences <- corpus_reshape(newsTrain, to = "sentences")
blogsSentences[[1]]
View(blogsSentences[c(1:20),])
View(newsSentences[c(1:20),])
texts(blogsSentences)[1:20]
texts(newsSentences)[1:20]
summary(blogsSentences)
summary(newsSentences)
head(blogsSentences)

docvars(blogsSentences, "source") <- "blogs"
docvars(newsSentences, "source") <- "news"
summary(blogsSentences)
summary(newsSentences)

#corpusTrain <- blogsSentences + newsSentences
corpusTrain <- blogsTrain + newsTrain
summary(corpusTrain)
texts(corpusTrain)[1:20]

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






## Next Remove profane words from the corpus

profane <- read_lines("..\\..\\Data\\obsceneEnglish.txt")

(replaceText <- content_transformer(function(x,from)gsub(from, "", x)))
for(i in 1:length(profane)){
    blogsSentences <- tm_map(blogsSentences, replaceText, profane[i], "")
}

blogsSentences <- Corpus(blogsSentences)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
blogsSentences <- tm_map(blogsSentences, toSpace, "ball sack")

testCorpus <- c("In the years thereafter, most of the Oil fields and platforms were named after pagan gods.", "We love you Mr. Brown.", "Chad has been awesome with the kids and holding down the fort while I work later than usual! The kids have been busy together playing Skylander on the XBox together, after Kyan cashed in his $$$ from his piggy bank. He wanted that game so bad and used his gift card from his birthday he has been saving and the money to get it (he never taps into that thing either, that is how we know he wanted it so bad). We made him count all of his money to make sure that he had enough! It was very cute to watch his reaction when he realized he did! He also does a very good job of letting Lola feel like she is playing too, by letting her switch out the characters! She loves it almost as much as him.")
testCorpus <- Corpus(testCorpus)
class(testCorpus)
