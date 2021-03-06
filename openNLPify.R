library(XML)
library(tm)
library(RCurl)
library(openNLP)
#see tm::writecorpus() in gnaReme repository
url <- "http://clover.slavic.pitt.edu/sam/propp/have_a_little_byte/magicgeese.xml"
#url <- "http://www.maleclabs.com/Propp/Corpus.xml"
tale <- xmlTreeParse(getURL(url), useInternal = T)
tale
initsit <- xmlValue(getNodeSet(tale, "//Corpus//Folktale//Move//Preparation//InitialSituation")[[1]]);
initsit
return <- xmlValue(getNodeSet(tale, "//Corpus//Folktale//Move//Return")[[1]])
return
# sentDetect(initsit, language = "en", model = NULL)
taggedSegment <- tagPOS(initsit, language = "en", model = NULL, tagdict = NULL)
taggedSegment
