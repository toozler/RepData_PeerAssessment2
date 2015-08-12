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
    if (!file.exists("data.bz2")) { 
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "data.bz2", method = "curl")
    }
}
getnoaa()

# loading data (------------------------------------------- new)
noaa <- read.csv(bzfile("data.bz2"))[, c("EVTYPE","BGN_DATE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
names(noaa) <- tolower(names(noaa))

# subsetting events 
noaa$year <- as.numeric(format(as.Date(noaa$bgn_date, format="%m/%d/%Y %H:%M:%S"), "%Y"))
noaa <- noaa[noaa$year >= 1996, ]


# filter out unknown/misc evtypes (------------------------------------------- new)
# started with 516 after subseting to 1996, check via >>> length(unique(noaa$evtype))

# eventnames <- toupper(c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Frost/Freeze","Funnel Cloud","Freezing Fog","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane (Typhoon)","Ice Storm","Lake-Effect Snow","Lakeshore Flood","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Surge/Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather"))

noaa$evtype <- toupper(noaa$evtype) #match capitalization
noaa$evtype <- gsub("  "," ",noaa$evtype) #remove double spaces (leaves 434)
noaa$evtype <- gsub("/"," ",noaa$evtype) #replace slashes for spaces (leaves 432)
noaa$evtype <- sub("^\\s+|\\s+$","",noaa$evtype) #removes leading and trailing spaces (leaves 424)

#individual fixes after looking at frequencies[frequencies$Freq > 40, ]
noaa$evtype <- sub("THUNDERSTORM WIND","TSTM WIND",noaa$evtype)
noaa$evtype <- sub("WILD FOREST FIRE","WILDFIRE",noaa$evtype)
noaa$evtype <- sub("STRONG WINDS","STRONG WIND",noaa$evtype)
noaa$evtype <- sub("RIP CURRENTS","RIP CURRENT",noaa$evtype)
noaa$evtype <- sub("MARINE TSTM WIND","MARINE THUNDERSTORM WIND",noaa$evtype)
noaa$evtype <- sub("WINTER WEATHER MIX","WINTER WEATHER",noaa$evtype)
noaa$evtype <- sub("COASTAL FLOODING","COASTAL FLOOD",noaa$evtype)
noaa$evtype <- sub("HURRICANE","HURRICANE/TYPHOON",noaa$evtype) 
noaa$evtype <- sub("HURRICANE/TYPHOON TYPHOON","HURRICANE/TYPHOON",noaa$evtype) 
noaa$evtype <- sub("URBAN SML STREAM FLD","FLOOD",noaa$evtype)
noaa$evtype <- sub("LANDSLIDE","AVALANCHE",noaa$evtype) # (leaves 413)

# check number of events again >> length(unique(noaa$evtype))

frequencies <- data.frame(table(noaa$evtype))
names(frequencies) <- c("evtype","evtotalcount")

# the individual fixes must go here


noaa <- merge(noaa, frequencies, by="evtype")



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


# filter out unknown/misc evtypes based on frequency:

frequencies <- data.frame(table(noaa$evtype))
names(frequencies) <- c("evtype","evtotalcount")
noaa <- merge(noaa, frequencies, by="evtype")
noaa <- noaa[noaa$evtotalcount >= 2, ]






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

