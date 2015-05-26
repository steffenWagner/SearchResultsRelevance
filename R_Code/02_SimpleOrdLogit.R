# Erste Analyse
rm(list = ls(all = TRUE))

library(readr)
library(tm)
library(ordinal)
library(nnet)
library(Metrics)
library(mgcv)

# Import
dat <- read_csv("data/train.csv") 

# Aufbereitung
source(file = "source/01_prepData.R")
dat <- prepData(dat)

datList <- split(dat, f = dat$query_prep)
# x <- datList[[1]]
countOccurence <- function (x) {
  # Aufbereitung der Description (bereits über prepData)
  corpus <- Corpus(VectorSource(x$product_title))
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  tdm <- weightTfIdf(TermDocumentMatrix(corpus))
#   tdm <- (TermDocumentMatrix(corpus))
  
  system.time(
    termList <- apply(tdm, 2, function(x) x[x >0, drop = FALSE])
  )
  x$nOccur <- mapply(function(query, terms) sum(terms[unlist(strsplit(query, split = " "))], na.rm = TRUE),
         query = x$query_prep,
         terms = termList)
  return(x)
}

system.time(
  dat2 <- do.call(rbind, lapply(datList, countOccurence))
)

cor(dat2$nOccur, as.numeric(dat2$median_relevance))

olm2 <- gam(as.numeric(median_relevance) ~ queryKlass_prep + s(log(nOccur + 1), k =3), 
            data = dat2, 
            subset = dat2$relevance_variance < 5 ,
            family = ocat(link="identity", R=4))
summary(olm2)
table(apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))
table(as.numeric(dat2$median_relevance), 
      apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))

ScoreQuadraticWeightedKappa(as.numeric(dat2$median_relevance), 
                            apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))

boxplot(unlist(predict(olm2, newdata = dat2, type = "terms")) ~ dat2$median_relevance, outline = TRUE)
plot(olm2)




boxplot(nOccur ~ median_relevance, data = dat2, outline = FALSE)

hist(sapply(termList, NROW), breaks = 100)

boxplot(sapply(termList, min) ~ dat$median_relevance, outline = FALSE)


mapply(function(terms, query) 
termList[[2]]
dat$query_prep[[2]]


tdm <- as.matrix(weightTfIdf(tdm))
tdm[, 3]


dat[70,]


Heaps_plot(dtm)
Zipf_plot(dtm)
Terms(dtm)[1:100]

dtm[1:10,]
weightTfIdf(dtm)
Terms(dtm)
as.matrix(dtm)[1:200, 1:100]






system.time(
  dat$queryScore <- mapply(function(description, query) tm_term_score(description, 
                                                                      unlist(strsplit(query, split = " "))),
                           description = corpus,
                           query = dat$query_prep)
)
dat$queryScoreKlass <- cut(log(dat$queryScore + 1), breaks = c(0:3, Inf), include.lowest = TRUE)
table(dat$queryScoreKlass, dat$median_relevance, useNA = "a")
assocplot(table(dat$queryScoreKlass, dat$median_relevance))

boxplot(dat$queryScore ~ dat$median_relevance, outline = FALSE)



  
dimnames(dtm)$Terms[1:100]





dat$product_description_prep 


# Verteilung der Anteile an Suchbegriffen in Titel
with(dat, plot(ecdf((nTermInTitle_query_prep/nTerm_query_prep))))
with(dat, table(nTermInTitle_query_prep/nTerm_query_prep, inTitle_query_prep | eqTitle_query_prep))
boxplot(I(nTermInTitle_query_prep/nTerm_query_prep) ~ median_relevance, data = dat)

boxplot(I(nTermInTitle_query_prep) ~ median_relevance, data = dat)


# Erstelle neue Variable
dat$ratioTermsInTitle_prep <- with(dat, cut(nTermInTitle_query_prep/nTerm_query_prep, 
                                            breaks = c(0, 0.3, 0.5, 0.6, 0.8, 1),
                                            right = FALSE,
                                            include.lowest = TRUE))

# Erstelle Master Klassierung
dat$queryKlass_prep <- ifelse(dat$eqTitle_query_prep, "queryEqTitle",
                              ifelse(dat$inTitle_query_prep, "queryInTitle", 
                                     dat$ratioTermsInTitle_prep))
assocplot(table(dat$median_relevance, dat$queryKlass_prep)                              )




# Regressoren
# Prüfe ob Suchbegriff in description und/order title enthalten ist

dat$queryInTitle <- howManyXInY(dat$query, dat$product_title, sep = sep)
dat$queryInDescription <- howManyXInY(dat$query, dat$product_description, sep = sep)
dat$nTermsQuery <- sapply(strsplit(dat$query, split = sep), NROW)
dat$exactQueryInTite <- mapply(function(query, text) grepl(query, text),
       query = dat$query, text = dat$product_title)
dat$exactQueryInDescription <- mapply(function(query, text) grepl(query, text),
                               query = dat$query, text = dat$product_description)


table(dat$exactQueryInTite)
table(dat$exactQueryInDescription)

hist(dat$queryInTitle, breaks = 30)
hist(dat$queryInDescription, breaks = 30)
hist(dat$nTermsQuery, breaks = 30)

round(prop.table(table(dat$median_relevance, 
                       dat$queryInDescription), 1)
      , 2)

# Deskriptive Grafiken 
assocplot(table(dat$queryInTitle, dat$median_relevance))
assocplot(table(dat$queryInDescription, dat$median_relevance))
assocplot(table(dat$noDescr, dat$median_relevance))

# Vor der Modellierung müssen die Variablen klassiert werden
dat$queryInTitle <- factor(dat$queryInTitle)
dat$queryInDescription <- factor(dat$queryInDescription)

# Einfaches ordinales Logit
olm1 <- clm(median_relevance ~ queryKlass_prep + nOccur, 
            data = dat2, subset = dat2$relevance_variance < 0.5)
summary(olm1)

olm2 <- gam(as.numeric(median_relevance) ~ queryKlass_prep + s(nOccur, k =3), 
            data = dat2, subset = dat2$relevance_variance < 0.5,
            family = ocat(link="identity", R=4))
summary(olm2)
table(apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))
table(as.numeric(dat2$median_relevance), 
      apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))

ScoreQuadraticWeightedKappa(as.numeric(dat2$median_relevance), 
                            apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))

boxplot(unlist(predict(olm2, newdata = dat2, type = "prob")) ~ dat2$median_relevance, outline = FALSE)




boxplot(unlist(predict(olm2)) ~ dat$median_relevance, outline = FALSE)
plot(ecdf(unlist(predict(olm2))))


# Betrachtung der In-Sample-Güte
table(dat$median_relevance, 
      unlist(predict(olm2, newdata = dat, type = "class")))
# Welche Güte liefert das Nullmodell:
ScoreQuadraticWeightedKappa(as.numeric(dat$median_relevance), 
                            as.numeric(unlist(predict(olm1, newdata = dat, type = "class"))))


# Teste multinomiales Logit
multnom1 <- multinom(median_relevance ~ queryInDescription , data = dat)
table(dat$median_relevance, predict(multnom1))

# Teste neuronale Netze
nnet1 <- nnet(median_relevance ~ queryInDescription
              + exactQueryInDescription
              + exactQueryInTite
              + queryInTitle
              + noDescr, data = dat, size = 2)
summary(nnet1)
table(predict(nnet1, type = "class"))
# Welche Güte liefert das Nullmodell:
ScoreQuadraticWeightedKappa(as.numeric(dat$median_relevance), 
                            as.numeric(unlist(predict(nnet1, type = "class"))))


# Welche Güte liefert das Nullmodell:
ScoreQuadraticWeightedKappa(as.numeric(dat$median_relevance), 
                            as.numeric(unlist(predict(olm1, type = "class"))))





