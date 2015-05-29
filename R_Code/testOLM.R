system.time(
  olm2 <- gam(as.numeric(median_relevance) ~ queryKlass_prep
              + s(idRE, bs = "re"), 
              data = dat, 
              family = ocat(link = "identity", R = 4))
)
summary(olm2)
table(apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))
table(as.numeric(dat2$median_relevance), 
      apply(predict(olm2, newdata = dat2, type = "response"), 1, which.max))

ScoreQuadraticWeightedKappa(as.numeric(dat$median_relevance), 
                            apply(predict(olm2, newdata = dat, type = "response"), 1, which.max))

0.5665935