install.packages("corpus.JSS.papers", repos = "http://datacube.wu.ac.at/", type = "source")
library(corpus.JSS.papers)
data("JSS_papers", package = "corpus.JSS.papers")


# sudo apt-get install libgsl0-dev
pkg <- c("OAIHarvester", "tm", "topicmodels", "XML", "slam", "bitops", "ggplot2", "mapproj", "stringr", "maps",
         "grid", "gridExtra", "RColorBrewer", "igraph", "colorspace", "scales", "stringr", "reshape2", "data.table",
         "ldatuning", "tidyr")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
  lapply(new.pkg, require, character.only = TRUE)
} else{
  lapply(pkg, require, character.only = TRUE)
}

data("AssociatedPress", package = "topicmodels")
associated.dtm <- AssociatedPress
llistopic.dtm <- associated.dtm

term_tfidf <- tapply(llistopic.dtm$v/slam::row_sums(llistopic.dtm)[llistopic.dtm$i], llistopic.dtm$j, mean) *
  log2(tm::nDocs(llistopic.dtm)/slam::col_sums(llistopic.dtm > 0))
summary(term_tfidf)

llisreduced.dtm <- llistopic.dtm[,term_tfidf >= summary(term_tfidf)["Median"]]
summary(slam::col_sums(llisreduced.dtm))

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

k <- 25
burnin <- 1000
iter <- 1000
keep <- 50
fitted <- topicmodels::LDA(llisreduced.dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )
## assuming that burnin is a multiple of keep
logLiks <- fitted@logLiks[-c(1:(burnin/keep))]
install.packages("Rmpfr")
library(Rmpfr)
## This returns the harmomnic mean for k = 25 topics.
harmonicMean(logLiks)



dtm1 <- llisreduced.dtm[101:120, ]
system.time(result <- FindTopicsNumber(
  dtm1,
  topics = seq(from = 2, to = 100, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 5L,
  verbose = TRUE
))
result1 <- as.data.table(result)
result1[, lapply(.SD, which.min), .SDcols = c(names(result[,c(-1, -2, -5)]))]
result1[, lapply(.SD, which.max), .SDcols = c(names(result[,c(-1, -3, -4)]))]
FindTopicsNumber_plot(result)

seqk <- seq(2, 25, 1)
burnin <- 10000
iter <- 50000
keep <- 120
system.time(fitted_many3 <- lapply(seqk, function(k) topicmodels::LDA(dtm1, k = k, method = "Gibbs", 
                                                                      control = list(burnin = burnin,iter = iter, 
                                                                                     keep = keep))))

logLiks_many3 <- lapply(fitted_many3, function(L)  L@logLiks[-c(1:(burnin/keep))])
hm_many3 <- sapply(logLiks_many3, function(h) harmonicMean(h))
paste("The optimal number of topics is", seqk[which.max(hm_many3)])

