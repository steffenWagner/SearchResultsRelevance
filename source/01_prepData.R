# Funktionen zur Datenaufbereitung
prepData <- function(dat, sep = " "){

  # Hilfsfunktionen
  # StopWord_Removal etc.
  prepText <- function(x){(
    removeWords(
#       stemDocument(
      removeNumbers(
        removePunctuation(x)), 
      words = stopwords(kind = "en")))
  }
  

  # Beginn der Aufbereitung
  
  # Umwandeln der abhängigen in ordinalen Faktor
  dat$median_relevance <- factor(dat$median_relevance, ordered = TRUE)
 
  # preparation der text-Elemente
  colToLower <- c("query", "product_title", "product_description")
  for(col in colToLower){
    dat[[col]] <- tolower(dat[[col]])
  }
  
  # Erstellen von aufbereiteten Spalten
  for(col in colToLower){
    dat[[paste(col, "prep", sep = "_")]] <- prepText(dat[[col]])
  }
  
  # entspricht der query dem Titel
  colQuery <- c("query", "query_prep")
  colTitle <- c("product_title", "product_title_prep")
  
  datQueryEqTitle <- do.call(data.frame, mapply(function(query, title) dat[[query]] == dat[[title]], 
                               query = colQuery, 
                               title = colTitle,
                               SIMPLIFY = FALSE))
  names(datQueryEqTitle) <- paste("eqTitle", names(datQueryEqTitle), sep = "_")
  
  # ist der query im title
  datQueryInTitle <- do.call(data.frame, 
                             mapply(function(queryOut, titleOut) 
                               mapply(function(queryIn, titleIn) grepl(queryIn, titleIn),
                                      queryIn = dat[[queryOut]], 
                                      titleIn = dat[[titleOut]]),
                                    queryOut = colQuery,
                                    titleOut = colTitle,
                                    SIMPLIFY = FALSE))
  names(datQueryInTitle) <- paste("inTitle", names(datQueryInTitle), sep = "_")
  
  
  # wieviele Suchbegriffe kommen im Title vor?
  # Prüft wie viele der query-terms in einem anderen Textfeld vorkommen 
  howManyXInY <- function(x, y, sep = sep){
    mapply(function(terms, yInt) sum(sapply(terms, function(word) grepl(word, yInt))),
           terms = strsplit(x, split = sep),
           yInt = y,
           SIMPLIFY = TRUE,
           USE.NAMES = FALSE)
  }
  
  datHowManyQueryInTitle <- do.call(data.frame, 
                                    mapply(howManyXInY, 
                                           x = dat[colQuery], 
                                           y = dat[colTitle],
                                           MoreArgs = list(sep = sep),
                                           SIMPLIFY = FALSE))
  names(datHowManyQueryInTitle) <- paste("nTermInTitle", names(datHowManyQueryInTitle), sep = "_")
  
  # Wie viele begriffe kammen vor?
  cols <- c(colQuery, colTitle)
  datNTerms <- do.call(data.frame, 
                       lapply(dat[cols], function(x) sapply(strsplit(x, split = sep), NROW)))
  names(datNTerms) <- paste("nTerm_", names(datNTerms), sep = "")
  
  
  # Fasse Returnobjekt zusammen
  dat <- data.frame(dat, datQueryEqTitle, datQueryInTitle, datHowManyQueryInTitle, datNTerms)
  
  # Erstelle neue Variable
  dat$ratioTermsInTitle_prep <- with(dat, cut(nTermInTitle_query_prep/nTerm_query_prep, 
                                              breaks = c(0, 0.3, 0.5, 0.6, 0.8, 1),
                                              right = FALSE,
                                              include.lowest = TRUE))
  
  # Erstelle Master Klassierung für query <-> title Bezug
  dat$queryKlass_prep <- ifelse(dat$eqTitle_query_prep, "queryEqTitle",
                                ifelse(dat$inTitle_query_prep, "queryInTitle", 
                                       dat$ratioTermsInTitle_prep))
  
  # Erstelle Master Klassierung für query <-> title Bezug
  dat$queryKlass <- ifelse(dat$eqTitle_query, "queryEqTitle",
                                ifelse(dat$inTitle_query, "queryInTitle", 
                                       dat$ratioTermsInTitle))
  
  dat$noDescr <- dat$product_description == ""
  
  return(dat)
}