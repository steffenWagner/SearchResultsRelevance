# Erste Analyse
rm(list = ls(all.names = TRUE))

library(readr)
library(tm)
library(dplyr)
library(stringr)
library(mgcv)
library(Metrics)

# Import
train <- read_csv("data/train.csv") 
test <- read_csv("data/test.csv")

# Aufbereitung der Text-Elemente
source("source/01_prepData.R")
for(col in c("query", "product_title", "product_description")){
  train[[col]] <- gsub("\\n", " ", train[[col]])
  test[[col]] <- gsub("\\n", " ", test[[col]])
}
rm(col)

# Beschneide Description auf 25 Terme
train$product_description <- maxTerms(train$product_description, 25)
test$product_description <- maxTerms(test$product_description, 25)

train <- prepText(x = train)
test <- prepText(x = test)
rm(prepText)

# Erzeuge Dummy-Variable Train ja nein
ratioTrain <- 0.75
set.seed(19750510)
train <- train %>% 
  group_by(query) %>% 
  mutate(train = runif(n()) <= ratioTrain) %>% 
  ungroup

# Entwicklung Abstandsmaß:
n <- 1
query <- train$query_prep[n]
text <- train$product_title_prep[n]
text <- train$product_description_prep[n]

lapply(unlist(strsplit(query, split = " ")),
                   function(x) x == unlist(strsplit(text, split = " ")))

meanTermDist <- function (query, text) {
  meanTermDistIntern <- function (query, text) {
    mean(unlist(lapply(unlist(strsplit(query, split = " ")),
                       function(x) min(which(x == unlist(strsplit(text, split = " ")))))))
  }
  mapply(meanTermDistIntern,
         query = query,
         text = text,
         SIMPLIFY = TRUE,
         USE.NAMES = FALSE)
}

distTitle <- meanTermDist(train$query_prep, train$product_title_prep)
which(is.infinite(distTitle))

# # Reduziere Terms in Train auf Terms in Test
# source("source/02_trimTrainTextToTestText.R")
# train$product_title_prep <- trimTrainTextToTestText(train = train, test = test, col = "product_title_prep")
# train$product_description_prep <- trimTrainTextToTestText(train = train, test = test, col = "product_description_prep")
# rm(trimTrainTextToTestText)


# Jaccard-Koeffizient
train$jaccTitle <- jaccard(train$query_prep, train$product_title_prep, weighting = weightBin)
train$jaccDescr <- jaccard(train$query, train$product_description, weighting = weightTf)

test$jaccTitle <- jaccard(test$query_prep, test$product_title_prep, weighting = weightBin)
test$jaccDescr <- jaccard(test$query, test$product_description, weighting = weightTf)

# Erzeugung query-spezifischer Regressoren
trainList <- split(train, f = train$query)
testList <- split(test, f = test$query)[names(trainList)]

# Definiere Funktion zur Erstellung einer dtm-Matrix
dtm <- function(dat, columnName) {
  dtm <- as.matrix(
    DocumentTermMatrix(Corpus(VectorSource(dat[[columnName]])),
                       control = list(weighting = weightBin)))
  # weights <- log(colSums(dtm) + 1 ) # Keine Verbesserung
  weights <- 1
  return(t(t(dtm) * weights))
}

# Schätze pls für jeden Query
source(file = "source/03_pls.R")
plsCoefTitle <- plsCoef(trainList, colTitle = "product_title_prep", dtm = dtm)
plsCoefDescr <- plsCoef(trainList, colTitle = "product_description_prep", dtm = dtm)
rm(plsCoef)

# Setze PLS-Koeffizienten mit abs(coef) < 

strCoef <- function (coef, coefMax) {
  threshold <- apply(coef, 3, function(x) sort(abs(x), decreasing = TRUE)[min(coefMax, dim(coef)[1])])
  for(layer in seq_len(dim(coef)[3])){
    coef[ (coef[ ,1, layer]) < threshold[layer],1, layer] <- 0
  }
  
  coef
}

plsCoefTitle <- lapply(plsCoefTitle, strCoef, coefMax = 5)
plsCoefDescr <- lapply(plsCoefDescr, strCoef, coefMax = 5)

# predPlsScore
trainList <- predPLS(datList = trainList, 
                     coefList = plsCoefTitle, 
                     colTitle = "product_title_prep",
                     dtm = dtm)
trainList <- predPLS(datList = trainList, 
                     coefList = plsCoefDescr, 
                     colTitle = "product_description_prep",
                     dtm = dtm)
testList <- predPLS(datList = testList, 
                     coefList = plsCoefTitle, 
                     colTitle = "product_title_prep",
                     dtm = dtm)
testList <- predPLS(datList = testList, 
                     coefList = plsCoefDescr, 
                     colTitle = "product_description_prep",
                     dtm = dtm)
rm(predPLS)


# Aufheben der Listenstruktur und Modellierung des ganzen Datensatzes
train <- do.call(rbind, trainList)
test <- do.call(rbind, testList)
rm(trainList, testList)

# Erstellen der neuen Regressoren
train <- newRegressors(x = train, 
                       colQuery = c("query", "query_prep"),
                       colTitle = c("product_title", "product_title_prep"))
test <- newRegressors(x = test,
                      colQuery = c("query", "query_prep"),
                      colTitle = c("product_title", "product_title_prep"))
rm(newRegressors)


# Modellschätzung
system.time(
  olm2 <- gam(as.numeric(median_relevance) ~ 
               
              queryKlass_prep
              # + s(jaccTitle, k = 4)
              + s(jaccDescr, k = 4, by = noDescr)              
              + s(product_title_prep_predPLS_1, k = 4)
              + s(product_title_prep_predPLS_2, k = 4)
              + s(product_description_prep_predPLS_1, k = 4)
              + s(product_description_prep_predPLS_2, k = 4)
              + s(idRE, bs = "re"), 
              data = train[train$train, ], 
              family = ocat(link = "identity", R = 4))
)
summary(olm2)
plot(olm2, pages = 1)

# In-Sample:
table(as.numeric(apply(predict(olm2, newdata = train[train$train, ], type = "response"), 1, which.max)))
table(as.numeric(train$median_relevance[train$train]), 
      as.numeric(apply(predict(olm2, newdata = train[train$train, ], type = "response"), 1, which.max)))
ScoreQuadraticWeightedKappa(as.numeric(train$median_relevance[train$train]), 
                            as.numeric(apply(predict(olm2, newdata = train[train$train, ], type = "response"), 1, which.max)))


# Out-of-Sample:
table(as.numeric(apply(predict(olm2, newdata = train[!train$train, ], type = "response"), 1, which.max)))
table(as.numeric(train$median_relevance[!train$train]), 
      as.numeric(apply(predict(olm2, newdata = train[!train$train, ], type = "response"), 1, which.max)))
ScoreQuadraticWeightedKappa(as.numeric(train$median_relevance[!train$train]), 
                            as.numeric(apply(predict(olm2, newdata = train[!train$train, ], type = "response"), 1, which.max)))

# Optimierung oos-Thresholds:
scoreProb <- predict(olm2, newdata = train[!train$train, ], type = "link")
range(scoreProb)
row.names(scoreProb) <- NULL

cutOffPoints <- function(cuts){
  -ScoreQuadraticWeightedKappa(as.numeric(train$median_relevance[!train$train]), 
                              unlist(sapply(scoreProb, 
                                          function(x) sum(x > c(-Inf, cuts)), simplify = FALSE)))
}
startParam <- olm2$family$getTheta(TRUE)
startParam <- as.numeric(quantile(scoreProb, probs = c(0.25, 0.5, 0.85)))
olm2$family$getTheta(TRUE)
optRes <- optim(olm2$family$getTheta(TRUE), cutOffPoints, control = list(maxit = 10000))
optRes

classPred <- function(scoreProb, cuts){
  unlist(sapply(scoreProb, function(x) sum(x > c(-Inf, cuts)), simplify = FALSE))
}

table(classPred(scoreProb, optRes$par),
      train$median_relevance[!train$train])
ScoreQuadraticWeightedKappa(classPred(scoreProb, optRes$par),
                            train$median_relevance[!train$train])

# Out-Of-Sample wird spannend
test$predScore <- as.numeric(predict(olm2, newdata = test, type = "link"))
test$prediction <- classPred(test$predScore, cuts = optRes$par)
table(test$prediction)
write.csv(test[ ,c("id", "prediction")], file = "submissions/150611_5.csv", row.names = FALSE)




