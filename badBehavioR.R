
Sys.setenv(NOAWT = TRUE) 
library("calibrate")
library("Snowball")
library("SnowballC")
library("Rstem")
library("tm")
library("lsa")
library("topicmodels")
library("lda")
library("ape")
library("rgl")
library("RWeka")
library("Rgraphviz")
library("igraph")
library("rjson")
library("gsubfn")
library("sqldf")

#startYear = 1958
#endYear = 1996
#duration = endYear - startYear

home = "/home/kingfish"
homePath = paste(home, "/greenspanC", sep="")

wd <- "home/kingfish/greenspanC/"

setwd(paste(homePath, sep=""))
text <- system.file("texts", "txt", package="tm");
corpus <- Corpus(DirSource())
corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
print("removing stopwords")
corpus <- tm_map(corpus, removeWords, stopwords("english"))
print("removing punctuation")
corpus <- tm_map(corpus, removePunctuation)
print("removing numbers")
corpus <- tm_map(corpus, removeNumbers)
print("complete stems from originals")
corpus_orig <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
print("stemming")
corpus <- tm_map(corpus, stemDocument, language="english")

BiGramTokenizer <- function(x) RWeka::NGramTokenizer(x, Weka_control(min = 2, max = 2))
#dtm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), tokenize=yourTokenizer, stopwords = TRUE))

dtm = TermDocumentMatrix(corpus, control = list(weighting = weightTf, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, minWordLength = 4))

dtm <- removeSparseTerms(dtm, .98)
rownames(dtm) = stemCompletion(rownames(dtm), corpus_orig)
#ldaG <- LDA(tdm, 10, method = "VEM", control = NULL, model = NULL)

K <- 10
SEED <- 2013
greenspan_TM <-
  list(VEM = LDA(dtm, k = K, control = list(seed = SEED)),
       VEM_fixed = LDA(dtm, k = K,
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(dtm, k = K, 
                       control = list(seed = SEED, burnin = 1000,
                                      thin = 100, iter = 1000)))

sapply(greenspan_TM[1:2], slot, "alpha")

sapply(greenspan_TM, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))))

Topic <- topics(greenspan_TM[["VEM"]], 1)

Terms <- terms(greenspan_TM[["VEM"]], 20)

topic_sequence <- sequence(20)
Terms[,1]
#findAssocs(dtm, "risk", .4)


as.matrix(dfm)
dfm <- removeSparseTerms(dfm, .99)
dfm_distro <- dist(dfm, method="euclidian") # euclidian, manhattan, maximum, canberra, binary, minkowski
#dfm_distro <- dist(dfm)
#dfm_ward <- hclust(dfm_distro, method="ward")
dfm_complete <- hclust(dfm_distro, method="complete")
#dfm_complete <- hclust(dfm_distro, method="mcquitty")
#dfm_single <-hclust(dfm_distro, method="single")
#dfm_mcquitty <- hclust(dfm_distro, method="mcquitty")
#dfm_median <-hclust(dfm_distro, method="median") 
#dfm_centroid <-hclust(dfm_distro, method="centroid")
op = par(bg="#DDE3CA")
plot(dfm_complete, col="#487AA1", col.main="#45ADA8", col.lab="#7C8071",
     col.axis="#F38630", lwd=1, lty=1, sub='', hang=-1, axes=FALSE,
     main = "Cluster Dendrogram of Proppian Narreme Matrix", 
     xlab="Tale Name", ylab = "DNM Distance")
par(op)
nplot(dfm_complete, hang=1, axes = TRUE, ann=TRUE, main = "Cluster Dendrogram of Proppian Narreme Matrix", 
     xlab="Tale Name", ylab = "DNM Distance")
phyl <- as.phylo(hclust(dfm_distro))
plot(phyl, edge.col=c("blue", "green", "red")[c(TRUE, FALSE) + 1 + (phyl$edge.length > 20)])
as.matrix(dfm)


#dtm <- DocumentTermMatrix(corpus)
dtm <- dfm
# Lots of simple, easy terms to work with
colnames(dtm)
rownames(dtm)
dense.dtm <- as.matrix(dtm)

# Example Usage: Latent Semantic Analysis
#
##############################################################################

# Throw out rare terms
dense.dtm <- dense.dtm[, which(colSums(dense.dtm) > 5)]

# PCA fails
#text.pca <- princomp(dense.dtm)
# Seems like two columns may be nearly perfectly correlated

# Use SVD instead to achive LSA effects

# Replace raw counts with binary counts
# Not really sufficient in such short document
# TODO: Try TF-IDF
dense.dtm <- ifelse(dense.dtm >= 10, 1, 0)

text.svd <- svd(dense.dtm)

# Plot results
plot(text.svd$v[, 1:2])
plot(svd(text.svd$v))
dtm$dimnames
dat<- text.svd
x <- dat$v
y <- dat$d
z <- dat$u
dtm$dimnames

x <- dtm$i
y <- dtm$v
z <- dtm$j
with(dat,plot3d(x,y,z))
plot(z)
with(dat,text3d(x,y,z,colnames(dtm)))

