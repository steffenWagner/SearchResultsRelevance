library(tm)
library(dplyr)
library(stringr)

trimTrainTextToTestText <- function (train, test, col) {
  train <- train %>% ungroup %>% mutate(.idIntern = row_number())
  
  # Aufbereiten der Text-Elemente
  # Welche Wörter kommen im Testdatensatz vor?
  trainList <- split(train, f = train$query)
  testList <- split(test, f = test$query)
  
  
  
  # Hilfsfunktion
  trainTermsToTestTerms <- function (train, test, col) {
    testWords <- unlist(unique(strsplit(test[[col]], split = " ")))
    trainWords <- unlist(unique(strsplit(train[[col]], split = " ")))
    
    # Welche trainWords sind nicht in Test-Words
    removeList <- trainWords[!trainWords %in% testWords]
    removeList <- removeList[removeList != ""]
    # Entferne entsprechende Wörter aus den Traingstexten
    newCol <- sapply(train[[col]], function(x) {
      for(word in removeList){
        x <- gsub(word, "", x, fixed = TRUE)
      }
      return(x)
    }, USE.NAMES = FALSE)
    return(data.frame(train, ".newCol" = newCol))
  }
  
  # Prüfe, ob Sortierung von Train- und Test-List identisch sind
  if(!isTRUE(all.equal(names(trainList), names(testList)))) stop("Sortierung der Listen ist unterschiedlich")
  
  system.time(
    reducedTrainTerms <- mapply(trainTermsToTestTerms, 
                                train = trainList, 
                                test = testList,
                                MoreArgs = list(col = col),
                                SIMPLIFY = FALSE)
  )        
                      
  trainNeu <- do.call(rbind, reducedTrainTerms)
  trainNeu <- trainNeu[order(trainNeu$".idIntern"), ]
  
  # strip-White-Spaces  
  newCol <- str_trim(stripWhitespace(as.character(trainNeu$".newCol")))
    
  return(newCol)
}
