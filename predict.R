# Predict
library(quanteda)

# Load the ngram data.tables that will be used for prediciton
load(file = "../../Data/trigramDT.RData")
load(file = "../../Data/bigramDT.RData")
load(file = "../../Data/unigramDT.RData")

load(file = "../../Data/profanity.RData")

setkey(trigramDT, ngram, phrase)
setkey(bigramDT, ngram, phrase)

# Test input text
txt = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
txt = "#greatday this is a tweet fuck"

## Convert to ngrams using the last words in the sentence
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

triPredict <- trigramDT[phrase == txtBi][order(-docfreq)][1:5]
freqBi <- bigramDT[ngram == txtBi][, docfreq]
triPredict[, score := docfreq/freqBi]
triPredict[, c("ngram", "phrase", "docfreq") := NULL]

biPredict <- bigramDT[phrase == txtUni][order(-docfreq)][1:5]
freqUni <- unigramDT[ngram == txtUni][, docfreq]
biPredict[, score := 0.4 * docfreq/freqUni]
biPredict[, c("ngram", "phrase", "docfreq") := NULL]

uniPredict <- unigramDT[order(-docfreq)][1:5]


allWords <- unigramDT[, sum(docfreq)]

uniPredict[, score := 0.4 * 0.4 * docfreq/allWords]
uniPredict[, docfreq := NULL]


l = list(triPredict, biPredict, uniPredict)
predictWord = as.data.table(rbindlist(l))
predictWord[, .(predict, max(score)), by = .(predict)]
predictWord[1:5, ]


# Join using A[B, bb:=i.b, on='a']
#tempDT <- copy(trigramDT)
tempDT[bigramDT, score := docfreq / bigramDT.docfreq, on]