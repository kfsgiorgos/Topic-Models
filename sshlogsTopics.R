pkg <- c("readr", "lubridate", "chron", "DT", "data.table", "plyr", "dplyr", "tidytext", "ldatuning", "tidyr", "Rmpfr",
         "topicmodels", "tm", "splitstackshape", "LDAvis", "stringi", "servr")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
  lapply(new.pkg, require, character.only = TRUE)
} else{
  lapply(pkg, require, character.only = TRUE)
}


testlogs <- as.data.table(base::scan("/home/kfsgiorgos/Downloads/ssh.log", sep = "\n", what = "character"))
temp <- splitstackshape::cSplit(testlogs, splitCols = "V1", sep = "\t", direction = "wide", drop = TRUE)
str(temp)
temp[, unique(V1_08)]
temp[, `:=` (V1_11 = NULL,V1_12 = NULL, V1_13 = NULL, V1_14 = NULL, V1_15 = NULL)]
temp[V1_03 == "192.168.202.68", unique(V1_07)]
temp[, V1_10:= as.character(V1_10)]
temp[, V1_10:=plyr::revalue(temp[, V1_10], c("SSH-1.99-Cisco-1.25" = "SSH1.99Cisco1.25", 
                               "SSH-2.0-OpenSSH_5.8p1 Debian-1ubuntu3" = "SSH_2OpenSSH_5.8p1Debian1ubuntu3",
                               "SSH-2.0-OpenSSH_5.8p1 Debian-7ubuntu1" = "SSH-2-OpenSSH_5.8p1Debian7ubuntu1",
                               "SSH-2.0-OpenSSH_4.3" = "SSH-2-OpenSSH4.3", "SSH-2.0-OpenSSH_4.5" = "SSH-2-OpenSSH4.5",
                               "SSH-2.0-OpenSSH_5.1p1 Debian-5" = "SSH-2-OpenSSH5.1p1Debian5", "SSH-1.99-OpenSSH_4.5" = "SSH-1.99-OpenSSH4.5",
                               "SSH-2.0-OpenSSH_5.3p1 Debian-3ubuntu6" = "SSH-2-OpenSSH5.3p1Debian-3ubuntu6",
                               "SSH-2.0-Cisco-1.25" = "SSH-2-Cisco-1.25"))]
temp[, unique(V1_09)]
str(temp)
e <- setorder(temp[, .N, by = V1_03], -N)
doc1 <- temp[V1_03 == "192.168.202.110"]
doc2 <- temp[V1_03 == "192.168.202.140"]
doc3 <- temp[V1_03 == "192.168.204.45"]
doc4 <- temp[V1_03 %in% c("192.168.202.79", "192.168.202.138", "192.168.202.109")]
doc4[, V1_03:=plyr::revalue(doc4[, V1_03], c("192.168.202.138" = "192.168.202.79",
                                             "192.168.202.109" = "192.168.202.79"))]
doc5 <- temp[V1_03 == "192.168.202.112"]
doc6 <- temp[V1_03 == "192.168.202.108"]
termfreq1 <- doc1[, .N, by = V1_09]
termfreq2 <- doc2[, .N, by = V1_09]
termfreq3 <- doc3[, .N, by = V1_09]
termfreq4 <- doc4[, .N, by = V1_09]
termfreq5 <- doc5[, .N, by = V1_09]
termfreq6 <- doc6[, .N, by = V1_09]
termfreq1[, V1_09:=plyr::revalue(termfreq1[, V1_09], c("-" = "fail ssh access"))]
termfreq2[, V1_09:=plyr::revalue(termfreq2[, V1_09], c("-" = "fail ssh access"))]
termfreq3[, V1_09:=plyr::revalue(termfreq3[, V1_09], c("-" = "fail ssh access"))]
termfreq4[, V1_09:=plyr::revalue(termfreq4[, V1_09], c("-" = "fail ssh access"))]
termfreq5[, V1_09:=plyr::revalue(termfreq5[, V1_09], c("-" = "fail ssh access"))]
termfreq6[, V1_09:=plyr::revalue(termfreq6[, V1_09], c("-" = "fail ssh access"))]
termfreq1[, document:=1]
termfreq2[, document:=2]
termfreq3[, document:=3]
termfreq4[, document:=4]
termfreq5[, document:=5]
termfreq6[, document:=6]

list1 <- list()
list2 <- list()
for (i in 1:6){
  w <- as.vector(as.character(levels(corpusip$term)))
  w1 <- as.vector(eval(parse(text = paste0("termfreq", i)))[, as.character(V1_09)])
  list1[[i]] <- data.table(V1_09 = setdiff(w, w1), N = 0, document = i)
}
for (i in 1:6){
  list2[[i]] <- rbindlist(list(eval(parse(text = paste0("termfreq", i))), list1[[i]]))
}
corpusip <- rbindlist(list2)

setnames(corpusip, c("V1_09", "N"), c("term", "count"))
setcolorder(corpusip, c("document", "term", "count"))

dtmip <- corpusip %>%
  cast_dtm(document, term, count)

system.time(ipresult <- FindTopicsNumber(
  dtmip,
  topics = seq(from = 2, to = 18, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 6L,
  verbose = TRUE
))

result_1 <- as.data.table(ipresult)
result_1[, lapply(.SD, which.min), .SDcols = c(names(result_1[,c(-1, -2, -5)]))]
result_1[, lapply(.SD, which.max), .SDcols = c(names(result_1[,c(-1, -3, -4)]))]
FindTopicsNumber_plot(ipresult)

seqk <- seq(2, 7, 1)
burnin <- 1000
iter <- 3000
keep <- 100
system.time(llis.model <- topicmodels::LDA(dtmip, 3, method = "Gibbs", control = list(burnin = burnin,iter = iter, 
                                                                                                             keep = keep)))
llis.topics <- topicmodels::topics(llis.model, 1)
llis.terms <- as.data.frame(topicmodels::terms(llis.model, 20), stringsAsFactors = FALSE)
doctopics.df <- as.data.table(llis.topics)

theta <- as.data.table(topicmodels::posterior(llis.model)$topics)
head(theta)

doctopics.df[, DocumentId:= as.integer(rownames(doctopics.df))]
setnames(doctopics.df, "llis.topics", "Topic")
setcolorder(doctopics.df, c("DocumentId", "Topic"))
str(doctopics.df)
##################################################################
######################## Topic Labels ############################
##################################################################

#  tidyr::gather(llis.terms, Topic)
t <- as.data.table(llis.terms)
topicTerms <- data.table::melt.data.table(t, measure.vars = c("Topic 1", "Topic 2", "Topic 3"))
topicTerms[, Rank:= rep(1:15)]
topTerms <- copy(topicTerms[Rank<4])
topTerms[, variable:= as.character(variable)]
topTerms[, Topic:= as.numeric(stringr::word(variable, 2))]
str(topTerms)



topicLabel <- topTerms[, .(Label = list(value), Topic = variable), by = variable]
topicLabel[, variable:=NULL]
topicLabel[, Label := lapply(Label, function(x) paste0(x[1], " @nd ", x[2], " @nd ", x[3]))]
phi <- as.data.table(topicmodels::posterior(llis.model)$terms)
vocab <- names(phi)
docLength <- as.vector(corpusip[, .N, by = document][, N])

freqterms <- as.vector(corpusip[, sum(count), by = term][,V1])

json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                               vocab = vocab,
                               doc.length = docLength,
                               term.frequency = freqterms,
                               R = length(vocab))


