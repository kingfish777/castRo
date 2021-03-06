#####################################################
#gReenspanCorpus                                          
#####################################################
#scrape gReenspan database
# http://fraser.stlouisfed.org/docs/historic/greenspan/Greenspan_ddddmmdd.pdf
#####################################################
library(XML) #htmlTreeParse
library(tm)
library(openNLP)
library(RCurl)
library(RWeka)
setwd("/home/kingfish/gReenspanCorpus")
homePath = "/home/kingfish/gReenspanCorpus"
startYear = 1987
endYear= 2005
duration = endYear - startYear
baseurl <- "http://fraser.stlouisfed.org/docs/historical/greenspan/Greenspan_"
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
      url = paste(baseurl, yearStr, monthStr, dayStr, ".pdf", sep="")
      print(url)
      if (url.exists(url)) {      
        dest = as.character(paste("greenspan_", yearStr, monthStr, dayStr, ".pdf", sep=""))
        url = as.character(url)
        pdf <- readPDF(PdftotextOptions="-layout")
        print("found one!")
        uri=url
        dat <- download.file(url, dest, mode = "w")
        txt_file_name <- sub(".pdf", ".txt", dest)
        try(msg<-system(paste("pdftotext ", dest, txt_file_name, sep=" ")))
        if (msg > 'NULL') {
        print(msg) }
        if (msg == "stderror")
        {
          system(paste("rm ", dest, sep=""));
          print("WE GOT ONE!")          
        }
        } else  
        print("bubble trouble")
      }
    }
}
