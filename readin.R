library(tm)
library(quanteda)
library(tidytext)
library(stringr)
library(readr)
library(dplyr)


con <- file("..\\..\\Data\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt", "rb")
data_blogs <- read_lines(con)
close(con)

head(data_blogs)
length(data_blogs)

## Convert to Corpus
blogsCorpus <- corpus(data_blogs)
rm(data_blogs)

summary(blogsCorpus)


splitSet602020 <- function(n){
    set.seed(20171112)
    # assign each document to training hold out of test set
    sample(rep(1:3, diff(floor(n * c(0, 0.6, 0.8, 1)))))
}

nBlogs <- ndoc(blogsCorpus)
ss <- splitSet602020(nBlogs)

blogsTrain <- corpus_subset(blogsCorpus, subset = (ss==1))
blogsHoldOut <- corpus_subset(blogsCorpus, subset = (ss == 2))
blogsTest <- corpus_subset(blogsCorpus, subset = (ss==3))

ndoc(blogsTrain)
ndoc(blogsHoldOut)
ndoc(blogsTest)

writeLines(as.character(mycorpus), con="mycorpus.txt")
write_lines(as.character(blogsTest), path = "..\\..\\Data\\blogsTest.txt")


blogsSentences <- corpus_reshape(blogsTrain, to = "sentences")
blogsSentences[[1]]
View(blogsSentences[c(1:20),])
summary(blogsSentences)
head(blogsSentences)

## Next Remove profane words from the corpus
