# Funktionen zur Datenaufbereitung
prepText <- function (x, textCol = c("query", "product_title", "product_description")) {
  
  stemOwn <- function (x) {
    stemIntern <- function (x) {
      x <- str_trim(stripWhitespace(
        removePunctuation(gsub("-|[0-9]", "", tolower(x)))))
      paste(sapply(unlist(strsplit(x,
                        split = " ")), wordStem),
        collapse = " ")
    }
    sapply(x, stemIntern, USE.NAMES = FALSE)
  }
  
  prepTextIntern <- function(text){
    text <- str_to_lower(gsub("-", " ", text))
    text <- removePunctuation(text, preserve_intra_word_dashes = FALSE)
    text <- stemDocument(
      str_trim(
        stripWhitespace(
          removeNumbers(
            removeWords(text, stopwords("english"))))))
    return(str_trim(text))
  }
  
  for(col in textCol){
    x[[paste(col, "_prep", sep ="")]] <- stemOwn(x[[col]])
  }
  return(x)
}

newRegressors <- function(x, sep = " "){

 
  # entspricht der query dem Titel
  colQuery <- c("query", "query_prep")
  colTitle <- c("product_title", "product_title_prep")
  
  datQueryEqTitle <- do.call(data.frame, mapply(function(query, title) x[[query]] == x[[title]], 
                               query = colQuery, 
                               title = colTitle,
                               SIMPLIFY = FALSE))
  names(datQueryEqTitle) <- paste("eqTitle", names(datQueryEqTitle), sep = "_")
  
  # ist der query im title
  datQueryInTitle <- do.call(data.frame, 
                             mapply(function(queryOut, titleOut) 
                               mapply(function(queryIn, titleIn) grepl(queryIn, titleIn),
                                      queryIn = x[[queryOut]], 
                                      titleIn = x[[titleOut]]),
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
                                           x = x[colQuery], 
                                           y = x[colTitle],
                                           MoreArgs = list(sep = sep),
                                           SIMPLIFY = FALSE))
  names(datHowManyQueryInTitle) <- paste("nTermInTitle", names(datHowManyQueryInTitle), sep = "_")
  
  # Wie viele begriffe kammen vor?
  cols <- c(colQuery, colTitle)
  datNTerms <- do.call(data.frame, 
                       lapply(x[cols], function(x) sapply(strsplit(x, split = sep), NROW)))
  names(datNTerms) <- paste("nTerm_", names(datNTerms), sep = "")
  
  
  # Fasse Returnobjekt zusammen
  x <- data.frame(x, datQueryEqTitle, datQueryInTitle, datHowManyQueryInTitle, datNTerms)
  
  # Erstelle neue Variable
  x$ratioTermsInTitle_prep <- with(x, cut(nTermInTitle_query_prep/nTerm_query_prep, 
                                              breaks = c(0, 0.3, 0.5, 0.6, 0.8, 1),
                                              right = FALSE,
                                              include.lowest = TRUE))
  
  # Erstelle Master Klassierung für query <-> title Bezug
  x$queryKlass_prep <- ifelse(x$eqTitle_query_prep, "queryEqTitle",
                                ifelse(x$inTitle_query_prep, "queryInTitle", 
                                       x$ratioTermsInTitle_prep))
  
  # Erstelle Master Klassierung für query <-> title Bezug
  x$queryTitleKlass <- ifelse(x$eqTitle_query, "queryEqTitle",
                                ifelse(x$inTitle_query, "queryInTitle", 
                                       x$ratioTermsInTitle))
  
  x$noDescr <- x$product_description == ""
  x$idRE <- factor(x$query)
  
  
  return(x)
}