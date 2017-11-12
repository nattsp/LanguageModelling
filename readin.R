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


blogsCorpus <- corpus(data_blogs)
rm(data_blogs)

summary(blogsCorpus)

#corpus_sample()

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
