# Predict
library(quanteda)

load(file = "../../Data/trigramDT.RData")
load(file = "../../Data/bigramDT.RData")
load(file = "../../Data/unigramDT.RData")
con <- file("../../Data/obsceneEnglish.txt", "rb")
profanity <- read_lines(con)
close(con)

txt = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
txt = "#greatday this is a tweet fuck"

## Convert to Corpus
txtCorpus <- corpus(txt)

txtTokens <- tokens(txtCorpus
                    , remove_numbers = TRUE
                    , remove_punct = TRUE
                    , remove_separators = TRUE
                    , remove_twitter = TRUE)
head(txtTokens)


txtTokens <- tokens_select(txtTokens
                           , profanity
                           , selection = "remove"
                           , verbose = quanteda_options("verbose"))

wordCount <- ntoken(txtTokens)

txtBi <- tokens_ngrams(txtTokens, n = 2, concatenator = " ")
txtBi <- tail(txtBi[[1]],1)
txtUni <- tail(txtTokens[[1]], 1)

