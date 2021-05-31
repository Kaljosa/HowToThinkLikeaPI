# This script is based on instruction from the following two places 
# (a hybrid of the two procedures) 
# https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
#
# STEP 1: Retrieving the data and uploading the packages
#
# install packages for colors and wordclouds 
# install.packages("RColorBrewer")
# install.packages("wordcloud")
# install.packages("tidyverse")
#
# load packages for colors and wordclouds 
library(RColorBrewer)
library(wordcloud)

# prepare text
# copy "comments" into an Excel file
# save file in \wordcloud directory (QXX.xlsx file)
# read comments into dataframe
# install.packages("readxl")
library(readxl)
comments <- read_excel("C:/Users/u0078315/Dropbox (Personal)/Successful Life Scientist/Interviews/wordclouds/source files/Q6_10.xlsx")
# load the tidyverse package to use pipes
library(tidyverse)
#remove empty rows
comments <- comments %>% filter(!is.na(Comments))
#Create a vector containing only the text
text <- comments$Comments

# install.packages("udpipe")
library(udpipe)
# udpipe can annotate the entire text file, find tokens, lemmas
anno <- udpipe(text, "english")

anno[, c("doc_id", "sentence_id", "token", "lemma", "upos")]


# select all the lemmas from the annotated text that is not punctuation
#lemmas <- anno %>% filter(!upos == "PUNCT") %>% select(lemma)

# select nouns, verbs, adjectives and adverbs only
lemmas <- anno %>% filter(upos == c("NOUN", "VERB", "ADJ", "ADV")) %>% select(lemma)


# install and load the tm package for loading text as corpus and cleaning it up
# install.packages("tm")
library(tm)

# Load the data as a corpus
docs <- Corpus(VectorSource(lemmas))
#
# Inspect the content of the document
inspect(docs)

#
# Cleaning the text in general
#
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove punctuation
docs <- tm_map(docs, removePunctuation)
# remove "na" and "\n"
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "na")
docs <- tm_map(docs, toSpace, "\n")
# Remove common English stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

## OPTIONAL
# replace words that were not lemmatized correctly
 toChange <- content_transformer(function (x , pattern ) gsub(pattern, "vision", x))
 docs <- tm_map(docs, toChange, "visio")
# Remove your own stop words
# specify your stopwords as a character vector
 mywords <- c("fear", "lot")
 docs <- tm_map(docs, removeWords, mywords)
## END OF OPTIONAL PART
#
# Build a term-document matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
#
# STEP 4: Generate the word cloud
#
# wordcloud package
library(wordcloud)
# 
# install.packages("ggsci") # ggsci color palette generator 
# install.packages("scales") # to show generated color palette
library(ggsci) 
library("scales")

# Nature Publishing Group colors
# mypal = pal_npg(palette = c("nrc"), alpha = 1)(6)
# show_col(mypal)

# NEJM colors
# mypal = pal_nejm(palette = c("default"), alpha = 1)(6)
# show_col(mypal)

# AAAS colors set alpha and how many colors to generate
mypal = pal_aaas(palette = c("default"), alpha = 0.8)(6)
# show_col(mypal)

set.seed(1234) 
# setting the seed is good if you want to generate another plot 
# with different colors but the same placement of
# the words - skip if there is something 
# funky about the plot to get a new arrangement of the words 

wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=30, random.order=FALSE, rot.per=0,
          colors= mypal,use.r.layout=FALSE,
          fixed.asp=TRUE)

