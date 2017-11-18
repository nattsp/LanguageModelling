txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!", 
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
txt <- c(text1 = "honey was an essential forest resourceâ€¦as a sweetener",
         text2 = "1,000 dollars")
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


