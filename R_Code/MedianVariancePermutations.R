# Erste Analyse
rm(list = ls(all.names = TRUE))

library(dplyr)
library(nnet)
library(readr)

# Import
train <- read_csv("data/train.csv") 

# Welche Median-Varianz-Kombinationen kommen vor?
dat <- train %>% 
  select(median_relevance, relevance_variance) %>% 
  distinct %>% 
  arrange(median_relevance, relevance_variance) %>% 
  mutate(id = row_number())
View(dat)

# Welche möglichen Median-Varianz-Kombinationen gibt es?
elements <- 1:4   # Antwortkategorien
nResults <- 3:6   # Anzahl results

# Erstelle Liste mit allen möglichen Kombinationen
permList <- unlist(sapply(nResults, 
                          function(n) combn(rep(elements, n), n, simplify = FALSE),
                          simplify = FALSE, USE.NAMES = TRUE),
                   recursive = FALSE)
# Reduziere auf 'unique' Sets
permList <- unique(lapply(permList, sort))
# Berechne für jedes Element den Median und 'relevance_variance'
sdKaggle <- function(x) round(sqrt(var(x)*(NROW(x) - 1)/NROW(x)), 3)
sdKaggle(c(4,4,4,3,1)) # Bsp aus dem Kaggle-Forum

datTheo <- do.call(rbind,
                   sapply(permList, function(x) data.frame(sample = paste(x, collapse = ""),
                                                           median_relevance = median(x),
                                                           relevance_variance = sdKaggle(x),
                                                           n = NROW(x),
                                                           stringsAsFactors = FALSE),
                          simplify = FALSE))

# Füge die beiden Datensätze zusammen:
mapTab <- dat %>% inner_join(datTheo, by = c("median_relevance", "relevance_variance"))

# Einträge mit Varianz = 0 werden auf 3 Results reduziert
mapTab <- mapTab %>% filter(n == 3 | relevance_variance > 0)

# Betrachtung der Mapping-Tabelle

# rel. Häufigkeit der passenden Ergebnisse pro Median/Variance-Kombination
# in etwas mehr als 60% der Fälle in der Mapping-Tabelle ist ein
# eindeutiges Mapping möglich
table(table(mapTab$id))

# Wie sehen die Mehrfachtreffer aus?
View(mapTab %>% group_by(id) %>% 
       mutate(freq = n()) %>% 
       filter(freq > 1) %>% 
       ungroup %>% 
       arrange(median_relevance, relevance_variance, n))

# Welcher Anteil an Kombinationen im Trainingsdatensatz entfällt auf eindeutige Kombinationen?
train %>%  
  group_by(median_relevance, relevance_variance) %>% 
  summarize(Freq = n()) %>% 
  left_join(mapTab %>% 
              group_by(id) %>% 
              mutate(freq = n()) %>% 
              filter(freq == 1) %>% 
              ungroup, by = c("median_relevance", "relevance_variance")) %>% 
  group_by(is.na(sample)) %>% 
  summarize(n = sum(Freq))
# In 53% der Fälle ist ein eindeutiges Mapping möglich.


# Für das Mapping der Result-Sets an den 'train' Datensatz verwende ich die folgenden Regeln:
# 1. Bei Eindeutigkeit: eindeutiges Mapping möglich
# 2. Bei Mehrdeutigkeit: kürzestes Resultset, da lt. Kaggle-Beschreibung am wahrscheinlichsten
# 3. Bei Mehrdeutigkeit und mehreren kürzesten Results: zufällige Auswahl
# 4. Bei keinem exakten Mapping: Zurückgreifen auf die naheliegendste Varianz
# Die Funktion 'nnet::which.is.max' besitzt die gewünschte Funktionalität

set.seed(1)
train$sample <- mapply(function(med, var) {
                        mapTab <- mapTab[mapTab$median_relevance == med, 
                                         c("relevance_variance", "n", "sample"), drop = FALSE]
                        if(any(mapTab$relevance_variance == var)){
                          mapTab <- mapTab[mapTab$relevance_variance == var, ]
                          mapTab$sample[which.is.max(-mapTab$n)]
                        } else{
                          mapTab <- mapTab[order(mapTab$relevance_variance), ]
                          distances <- -abs(mapTab$relevance_variance - var)
                          mapTab$sample[which.is.max(distances)]
                        }
                      },
                      med = train$median_relevance,
                      var = train$relevance_variance, 
                      SIMPLIFY = TRUE)
train$nSample <- nchar(train$sample)
table(train$nSample)


# Validiere Berechnung
plot(train$relevance_variance, 
     sapply(strsplit(train$sample, split = ""),
       function(x) sdKaggle(as.integer(x))),
     col = train$median_relevance,
     xlab = "Original Varianz", ylab = "Simulierte Varianz")
abline(0, 1, col = "red")
# Ausreichend genau

# Jetzt gilt es den ursprünglichen Datensatz entsprechend des in 'sample' enthaltenen
# Resultsets aufzublasen:
train <- train %>% ungroup %>% mutate(id = row_number())
trainFull <- train[rep(train$id, times = train$nSample), ]
# Prüfung:
NROW(trainFull)==sum(train$nSample)
trainFull$result <- as.integer(unlist(strsplit(train$sample, split="")))


# Um jetzt eine Stichprobe auf 'result'-Ebene zu ziehen, kann dplyr:sample_n
# verwendet werden.
trainSample <- trainFull %>% 
  group_by(id) %>% 
  sample_n(1)

# trainSample enthält jetzt genauso viele Zeilen wie der ursprüngliche 'train'-Datensatz
vgl <- trainSample %>% select(id, result) %>% 
  full_join(train %>% select(id, median_relevance),
            by = "id")
vgl
# Allerdings weichen 'median_relevance' und 'result', die neue Abhängige, 
# wie gewünscht voneinander ab:
smoothScatter(vgl$median_relevance, vgl$result)
prop.table(table(vgl$median_relevance, vgl$result))


