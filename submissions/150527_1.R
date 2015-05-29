# Erste Analyse
rm(list = ls(all.names = TRUE))

library(readr)
library(tm)
library(ordinal)
library(nnet)
library(Metrics)
library(mgcv)

# Import
dat <- read_csv("data/train.csv") 
test <- read_csv("data/test.csv")


# Aufbereitung
source(file = "source/01_prepData.R")
dat <- prepData(dat)
test <- prepData(test)


datList <- split(dat, f = dat$query_prep)
testList <- split(test, f = test$query_prep)
# x <- datList[[1]]
countOccurence <- function (x) {
  # Aufbereitung der Description (bereits über prepData)
  corpus <- Corpus(VectorSource(x$product_title))
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
#   tdm <- weightTfIdf(TermDocumentMatrix(corpus))
   tdm <- (TermDocumentMatrix(corpus))
  
  system.time(
    termList <- apply(tdm, 2, function(x) x[x >0, drop = FALSE])
  )
  x$nOccur <- mapply(function(query, terms) sum(terms[unlist(strsplit(query, split = " "))], na.rm = TRUE),
         query = x$query_prep,
         terms = termList)
  x$nOccurDemeaned <- x$nOccur - mean(x$nOccur)
  return(x)
}

system.time(
  dat2 <- do.call(rbind, lapply(datList, countOccurence))
)
system.time(
  test2 <- do.call(rbind, lapply(testList, countOccurence))
)
test2$idRE <- factor(test2$query)


cor(dat2$nOccur, as.numeric(dat2$median_relevance))
cor(dat2$nOccurDemeaned, as.numeric(dat2$median_relevance))

boxplot(nOccur ~ median_relevance , data = dat2, outline = FALSE)
boxplot(nOccurDemeaned ~ median_relevance , data = dat2, outline = FALSE)

dat2$idRE <- factor(dat2$query)
olm2 <- gam(as.numeric(median_relevance) ~ queryKlass_prep
            + s(log(nOccur + 1), k = 4)
            + s(nOccurDemeaned, k = 4)
            + s(idRE, bs = "re"), 
            data = dat2, 
            subset = dat2$relevance_variance < 5 ,
            family = ocat(link = "identity", R = 4))
summary(olm2)
table(apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))
table(as.numeric(dat2$median_relevance), 
      apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))

ScoreQuadraticWeightedKappa(as.numeric(dat2$median_relevance), 
                            apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))

boxplot(unlist(predict(olm2, newdata = dat2, type = "terms")) ~ dat2$median_relevance, outline = FALSE)
plot(olm2)

## Prognose für Test
test2$prediction <- apply(predict(olm2, newdata = test2, type = "response"), 1, which.max)

write.csv(test2, file = "submissions/150527_1_full.csv", row.names = FALSE)
write.csv(test2[ ,c("id", "prediction")], file = "submissions/150527_1.csv", row.names = FALSE)
table(test2$pred)



