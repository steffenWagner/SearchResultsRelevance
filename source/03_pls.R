library(pls)

plsCoef <- function (trainList, 
                     colTitle, 
                     dtm = function(dat, columnName) as.matrix(
                       DocumentTermMatrix(Corpus(VectorSource(dat[[columnName]])),
                                          control = list(weighting = weightBin)))
                     ) {
  plsTrainIntern <- function (train, 
                              colTitle, 
                              dtm = dtm
                              ) {
    
    
    dtmTrain <- dtm(train, colTitle)
    trainData <- list(median_relevance = train$median_relevance,
                      train = train$train,
                      dtm = dtmTrain)
    class(trainData) <- "data.frame"
    # testData <- list(median_relevance = sample(1:4, size = NROW(dtmTest), replace = TRUE),
    #                   dtm = dtmTest)
    # class(testData) <- "data.frame"
    
    plsRes <- plsr(median_relevance ~ dtm, 
                   data = trainData[trainData$train, ], 
                   ncomp = 2, 
                   method = "oscorespls")
    # plot(train$median_relevance, dtmTrain %*% coef(plsRes))
    return(plsRes$coefficients)
    
  }
  lapply(trainList, plsTrainIntern, colTitle = colTitle, dtm = dtm)
}


predPLS <- function (datList, coefList, colTitle, dtm) {
  
  predPLSintern <- function (dat, coef, colTitle, dtm) {
    dtmMat <- dtm(dat, columnName = colTitle)
    terms <- row.names(coef)
    dtmMat <- dtmMat[ , colnames(dtmMat) %in% terms]
    missCol <- terms[!terms %in% colnames(dtmMat)]
    if(NROW(missCol)){
      dtmMat <- cbind(dtmMat,
                      matrix(0, 
                             ncol = length(missCol), 
                             nrow = NROW(dtmMat), 
                             dimnames = list(1:NROW(dtmMat), missCol)))
    }
    
    dtmMat <- dtmMat[ , terms]
    nComp <- dim(coef)[3]
    
    for(i in seq_len(nComp)){
      dat[[paste(colTitle, "predPLS", i, sep = "_")]] <- dtmMat %*% coef[ , , i]  
      NAs <- is.na(dat[[paste(colTitle, "predPLS", i, sep = "_")]])
      if(any(NAs)){
        dat[[paste(colTitle, "predPLS", i, sep = "_")]][NAs] <- 0
      }
    }
    return(dat)
  }
  
  mapply(predPLSintern, 
         dat = datList, 
         coef = coefList,
         MoreArgs = list(colTitle = colTitle,
                         dtm = dtm),
         SIMPLIFY = FALSE,
         USE.NAMES = TRUE)
}