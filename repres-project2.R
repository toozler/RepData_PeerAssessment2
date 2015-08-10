# Reproductible Research
# Project 2


# setting work dir
setwd("~/Documents/DataScience/5. Reproducible Research/RepData_PeerAssessment2")

# loading libraries
library(dplyr)
library(ggplot2)
library(reshape2)

# downloading data and unzipping
getnoaa <- function() { 
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "data.bz2", method = "curl")
}

# loading data
noaa <- read.csv(bzfile("data.bz2"))
names(noaa) <- tolower(names(noaa))

# subsetting events 
noaa$year <- as.numeric(format(as.Date(noaa$bgn_date, format="%m/%d/%Y %H:%M:%S"), "%Y"))
table(noaa$year)

noaa <- noaa[noaa$year >= 1996, ]


# fetch, sort and return total number of events of a variable (sum)
# getevents(variable name, data = noaa, arrangeby="value|evrate")
getevents <- function(evtypename, data = noaa, arrangeby = "value") {
      result <- eval(substitute(evtypename),data,parent.frame())
      result <- aggregate(result,list(noaa$evtype), sum) 
      names(result) <- c("evtype","value")
      
      # calculate frequency
      frequencies <- data.frame(table(noaa$evtype))
      names(frequencies) <- c("evtype","evtotalcount")
      result <- merge(result, frequencies, by="evtype")
      result$evrate <- result[,2] / result$evtotalcount
      
      # arrange and rename
      if (arrangeby == "value") { result <- arrange(result, desc(value)) }
      else { result <- arrange(result, desc(evrate)) }
      names(result) <- c("evtype",substitute(evtypename),"evcount","evrate")
      result
}

# dmg mult
getdollar <- function(amount,m) {
   
    if(m == "B") { mexp <- 9 }
    else if(m == "M") { mexp <- 6 }
    else if(m == "K") { mexp <- 3 }
    else if(m == "H") { mexp <- 2 }
    else if(m == "?") { mexp <- 1 }
    else if(is.na(m)) { mexp <- 1 }
    else { mexp <- 1 }
    
    amount^mexp
    
}
noaa$cropdmgdolar <- mapply(getdollar,noaa$cropdmg, noaa$cropdmgexp)
noaa$propdmgdolar <- mapply(getdollar,noaa$propdmg, noaa$propdmgexp)


# health




# plotting

ggplot(getevents(fatalities,arrangeby="value")[1:10,], aes(x=reorder(evtype,fatalities), y=fatalities, fill="tomato")) +
    geom_bar(stat="identity", alpha=0.8) +
    coord_flip() + theme(legend.position="none") +
    labs(y = "Number of Fatalities", x = "Event Type")

# economical

economy <- merge(select(getevents(propdmg),evtype,propdmg),select(getevents(cropdmg),evtype,cropdmg),by="evtype")
economy$totaldmg <- economy$propdmg + economy$cropdmg
economy <- arrange(economy, desc(totaldmg))

# economy separate
select(getevents(propdmg),evtype,propdmg)
select(getevents(cropdmg),evtype,cropdmg)








# wordcloud
library(wordcloud)
library(tm)
wordcloud(tolower(harmfull$evtype)[1:15],harmfull$fatalities,c(5,.8),4,100,TRUE,,.15,,)


# harmful events deprecated

# totalfat <- aggregate(noaa$fatalities, list(noaa$evtype), sum) 
# totalinj <- aggregate(noaa$injuries, list(noaa$evtype), sum)
#harmfull <- arrange(merge(totalfat,totalinj,by="Group.1"), desc(x.x), desc(x.y))
#names(harmfull) <- c("evtype","fatalities","injuries")
#harmfull$weighted <- with(harmfull, (fatalities*0.4)+(injuries*0.6) )
#harmfull <- arrange(harmfull, desc(weighted))

