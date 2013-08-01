#####################################################
#castRo                                          
#####################################################
#scrape Castro database
# http://lanic.utexas.edu/la/cb/cuba/castro.html
#####################################################
library(XML) #htmlTreeParse
library(tm)
library(RCurl)
library(RWeka)
setwd("/home/kingfish/castRoCorpus")
startYear = 1961
endYear= 1984
duration = endYear - startYear
baseurl <- "http://lanic.utexas.edu/project/castro/db/"
trim <- function(x) { gsub("\\s", "", x) }
monthMax = 12
dayMax = 31
duration = endYear - startYear
for (y in 0:duration) { 
  for (month in 1:monthMax) {
    for (day in 1:dayMax) {    
      year = startYear + y
      yearStr = as.character(year)
      monthStr = as.character(month)
      if (month < 10) { monthStr = trim(paste("0", monthStr)) }
      dayStr = trim(as.character(day))
      if (day <10) { dayStr = trim(paste("0", dayStr)) }
      if (month == 12) { month = 1 }
      if (day == 31) { day = 1 }
      if (year == (duration + startYear)) { year = 9999999 }
      url = paste(baseurl, yearStr, "/", yearStr, monthStr, dayStr, ".html")
      url <- gsub("\\s","", url)
      if (url.exists(url)) {      
        doc <- htmlTreeParse(url, useInternalNodes=TRUE)
        print(doc)
        pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
        text <- try(unlist(xpathApply(text, "//html", xmlValue, namespaces = xmlNamespaceDefinitions(doc, simplify = TRUE), resolveNamespaces = TRUE)))
        if (!(class(text) == "try-error" || class(text) == "NULL")) {
        text <- gsub("i/END/", "", text)
        text <- unlist(gsub(pattern, "\\1", text))
        filename = as.character(gsub("\\s", "", c("castro", yearStr, monthStr, dayStr, ".txt")))
        print (url)
        print (text)   
        write(t(text), file=as.character(trim(paste('FidelCastroRuz', yearStr, monthStr, dayStr, ".txt"))))
        # openNLPicize each new text as it comes in, scrubbing it along the way
        } else print(doc) 
        print("bubble trouble")
      }
    }
  }
}
