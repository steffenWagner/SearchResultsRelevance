# Erste Analyse
rm(list = ls(all = TRUE))

library(readr)
library(tm)

# Import
dat <- read_csv("data/train.csv") 
str(dat)

# Regressoren
# Prüfe ob Suchbegriff in description und/order title enthalten ist
howManyXInY <- function(x, y, sep = " "){
  mapply(function(terms, yInt) sum(sapply(terms, function(word) grepl(word, yInt))),
         terms = strsplit(x, split = sep),
         yInt = y,
         SIMPLIFY = TRUE,
         USE.NAMES = FALSE)
}


sa

pply(strsplit(x, split = " "), function(terms) sum(sapply(terms, function(word) grepl(word, y))))


y


# Erstelle zunächst einfaches ordinales Logit 



# Aufbereitung der descriptions
dat$product_description <- tolower(dat$product_description)
mapply(function(strsplit(dat$product_description[1:10], split = " ")

                
                
                

# Betrachtung: product_title
# Keep only unique documents and convert them to lowercase
corpus <- Corpus(VectorSource(tolower(unique(dat$product_title))))
# Remove punctuation from the documents
corpus <- tm_map(corpus, removePunctuation)
# Remove english stopwords, such as "the" and "a"
corpus <- tm_map(corpus, removeWords, stopwords("english"))

doc_terms <- DocumentTermMatrix(corpus)
doc_terms <- as.data.frame(as.matrix(doc_terms))

str(doc_terms)


boxplot(nchar(dat$query) ~ dat$median_relevance)


# Identifikation Corpus spezifischer Stopwords
# - title corpus
# - description corpus


strsplit(dat$product_description, split




