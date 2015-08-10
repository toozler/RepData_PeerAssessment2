# Evaluation of NOAA Storm Database events from 1950 to 2011

## Synopsis

The Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern. This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

## Data Processing

### Loading required libraries

The following libraries are used in this analysis:


```r
library(dplyr, quietly=T, verbose=F, warn.conflicts=F)
library(ggplot2)
library(reshape2)
library(gridExtra)
```


### Obtaining and loading the dataset

The dataset is here downloaded from the source and loaded into the *noaa* data frame. Variable names are replaced by lowercase to comply with tidy data practices.


```r
#getnoaa <- function() { 
#    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
#                  destfile = "data.bz2", method = "curl")
#}
#getnoaa()

noaa <- read.csv(bzfile("data.bz2"))
names(noaa) <- tolower(names(noaa))
```

### Subset by Year

As the events in the database start in the year 1950 and end in November 2011 and in the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. 


```r
noaa$year <- as.numeric(format(as.Date(noaa$bgn_date, format="%m/%d/%Y %H:%M:%S"),"%Y"))
```

The variables describing the multiplier for the PROPDMG and CROPDMG variables, PROPDMGEXP and CROPDMGEXP, respectivelly, include unknown values not mentioned on the data documentation for the years prior to 1996:


```r
filter(noaa, year >= 1995) %>% select(propdmgexp) %>% unique()
```

```
##       propdmgexp
## 1               
## 5              B
## 12             M
## 13             K
## 15             m
## 728            +
## 2317           0
## 2360           5
## 4965           6
## 5069           ?
## 5322           4
## 5534           2
## 5900           3
## 11846          7
## 13301          H
## 18934          -
## 21156          1
## 21383          8
```

```r
filter(noaa, year >= 1995) %>% select(cropdmgexp) %>% unique()
```

```
##       cropdmgexp
## 1               
## 5              M
## 15             m
## 111            K
## 648            B
## 2169           ?
## 2280           0
## 3571           k
## 15484          2
```

The data from 1996 onwards only indicates known multipliers (“K” for thousands, “M” for millions, and “B” for billions.):

```r
filter(noaa, year >= 1996) %>% select(propdmgexp) %>% unique()
```

```
##        propdmgexp
## 1               K
## 6                
## 72              M
## 49321           B
## 639593          0
```

```r
filter(noaa, year >= 1996) %>% select(cropdmgexp) %>% unique()
```

```
##        cropdmgexp
## 1               K
## 2                
## 184             M
## 332770          B
```

The dataset will be subsetted to include only events recorded from 1996 to 2011, a period chosen based on the quality of event records, as previously described: 


```r
noaa <- noaa[noaa$year >= 1996, ]
```


### Obtaining variables of interest via getevents() function

The custom *getevents()* function fetches and sorts in descending order (from higher to lower) the values of a given variable of interest, such as fatalities or injuries counts.


```r
getevents <- function(evtypename, data = noaa, arrangeby = "value") {
      result <- eval(substitute(evtypename),data,parent.frame())
      result <- aggregate(result,list(noaa$evtype), sum) 
      names(result) <- c("evtype","value")
      
      # calculate rates
      frequencies <- data.frame(table(noaa$evtype))
      names(frequencies) <- c("evtype","evtotalcount")
      result <- merge(result, frequencies, by="evtype")
      result$evrate <- result[,2] / result$evtotalcount
      
      # arrange based on "arrangeby" argument and rename variables
      if (arrangeby == "value") { result <- arrange(result, desc(value)) }
      else { result <- arrange(result, desc(evrate)) }
      names(result) <- c("evtype",substitute(evtypename),"evcount","evrate")
      result
}
```

In addition to the absolute count, the *getevents()* function also calculates the the givens variable rate based on the frequency of each event type. When the *arrangeby* argument is defined as *evrate*, the event rate is calculated by the total amount of values (e.g. total amount of fatalities due to flooding) per number of occurences of an event type (e.g. total number of floods).


### Calculating US Dollar amounts for crop and property damage via getdollar() function

The property and crop damage values presented on the PROPDMG and CROPDMG are calculated based on the respective -EXP variable via the *getdollar()* function. Two new variables are added to the dataset, indicating the damage value is US dollars.


```r
getdollar <- function(amount,m) {
    if(m == "B") { mexp <- 9 }
    else if(m == "M") { mexp <- 6 }
    else if(m == "K") { mexp <- 3 }
    else if(m == "H") { mexp <- 2 }
    else if(m == "?") { mexp <- 0 }
    else if(is.na(m)) { mexp <- 0 }
    else { mexp <- 0 }
    amount * 10^mexp
}
noaa$cropdmgdolar <- mapply(getdollar,noaa$cropdmg, noaa$cropdmgexp)
noaa$propdmgdolar <- mapply(getdollar,noaa$propdmg, noaa$propdmgexp)
```



## Results

### Which types of events are most harmful with respect to population health?

The effect on the human population health is estimated here by the number of fatalities and number of injuries. For those two specific variables, the top 20 event types with that have impacted a larger population are as follows:


```r
select(getevents(fatalities),evtype,fatalities)[1:20,]
```

```
##                     evtype fatalities
## 1           EXCESSIVE HEAT       1797
## 2                  TORNADO       1511
## 3              FLASH FLOOD        887
## 4                LIGHTNING        651
## 5                    FLOOD        414
## 6              RIP CURRENT        340
## 7                TSTM WIND        241
## 8                     HEAT        237
## 9                HIGH WIND        235
## 10               AVALANCHE        223
## 11            RIP CURRENTS        202
## 12            WINTER STORM        191
## 13       THUNDERSTORM WIND        130
## 14 EXTREME COLD/WIND CHILL        125
## 15            EXTREME COLD        113
## 16              HEAVY SNOW        107
## 17             STRONG WIND        103
## 18         COLD/WIND CHILL         95
## 19              HEAVY RAIN         94
## 20               HIGH SURF         87
```


```r
select(getevents(injuries),evtype,injuries)[1:20,]
```

```
##               evtype injuries
## 1            TORNADO    20667
## 2              FLOOD     6758
## 3     EXCESSIVE HEAT     6391
## 4          LIGHTNING     4141
## 5          TSTM WIND     3629
## 6        FLASH FLOOD     1674
## 7  THUNDERSTORM WIND     1400
## 8       WINTER STORM     1292
## 9  HURRICANE/TYPHOON     1275
## 10              HEAT     1222
## 11         HIGH WIND     1083
## 12          WILDFIRE      911
## 13              HAIL      713
## 14               FOG      712
## 15        HEAVY SNOW      698
## 16  WILD/FOREST FIRE      545
## 17          BLIZZARD      385
## 18        DUST STORM      376
## 19    WINTER WEATHER      343
## 20    TROPICAL STORM      338
```


```r
plotfat <- ggplot(getevents(fatalities,arrangeby="value")[1:20,], aes(x=reorder(evtype,fatalities), y=fatalities)) +
    geom_bar(stat="identity", fill="tomato", alpha=.8) +
    coord_flip() + theme(legend.position="none") +
    labs(y = "Number of Fatalities", x = "Event Type")

plotinj <- ggplot(getevents(injuries,arrangeby="value")[1:20,], aes(x=reorder(evtype,injuries), y=injuries)) +
    geom_bar(stat="identity", fill="steelblue2", alpha=.8) +
    coord_flip() + theme(legend.position="none") +
    labs(y = "Number of Injuries", x = "")

grid.arrange(plotfat, plotinj, ncol = 2, top="Natural Event's Effect on Human Population Health in the U.S. between 1996 and 2011")
```

![plot of chunk plot1](figure/plot1-1.png) 

Alternatively, one may evaluate the harmfulness of an event type by the rate of fatalities or injuries it may cause. Here are the top ranking harmful event types ranked by the rate of fatalities and injuries they have caused, limited to events with at least 10 occurences and with a average rate of at least 0.3 for fatalities and 0.5 injuries per event record.


```r
ratefat <- filter(getevents(fatalities,arrangeby="evrate"), evcount > 10) %>% select(evtype,evrate) %>% filter(evrate > 0.3)
ratefat
```

```
##              evtype    evrate
## 1           TSUNAMI 1.6500000
## 2    EXCESSIVE HEAT 1.0851449
## 3       RIP CURRENT 0.7870370
## 4 HURRICANE/TYPHOON 0.7272727
## 5      RIP CURRENTS 0.6688742
## 6         AVALANCHE 0.5899471
## 7              COLD 0.4411765
## 8         HURRICANE 0.3588235
## 9              HEAT 0.3310056
```


```r
rateinj <- filter(getevents(injuries,arrangeby="evrate"), evcount > 10) %>% select(evtype,evrate) %>% filter(evrate > 0.5)
rateinj
```

```
##               evtype     evrate
## 1  HURRICANE/TYPHOON 14.4886364
## 2              GLAZE 10.0952381
## 3            TSUNAMI  6.4500000
## 4     EXCESSIVE HEAT  3.8592995
## 5          BLACK ICE  1.7142857
## 6               HEAT  1.7067039
## 7                FOG  1.3383459
## 8       RIP CURRENTS  0.9735099
## 9         WINTRY MIX  0.9390244
## 10  FREEZING DRIZZLE  0.9285714
## 11         ICY ROADS  0.9166667
## 12        DUST STORM  0.9016787
## 13           TORNADO  0.8925888
## 14        HEAVY SURF  0.5194805
```


```r
plotprop <- ggplot(ratefat, aes(x=reorder(evtype,evrate), y=evrate)) +
    geom_bar(stat="identity", fill="tomato3", alpha=.8) +
    coord_flip() + theme(legend.position="none") +
    labs(y = "Fatality Rate per \nEvent Type", x = "") 

plotcrop <- ggplot(rateinj, aes(x=reorder(evtype,evrate), y=evrate)) +
    geom_bar(stat="identity", fill="steelblue4", alpha=.8) +
    coord_flip() + theme(legend.position="none") +
    labs(y = "Injury Rate per \nEvent Type", x = "")

grid.arrange(plotprop, plotcrop, ncol = 2, top="Natural Event's Effect on Human Population Health in the U.S. between 1996 and 2011")
```

![plot of chunk plot2](figure/plot2-1.png) 

The analysis of these rates compared to the absolute total fatalities and injuries (previous graph) indicates that a tsunamis have a higher fatality rate than excessive heat, which ranked first in number of fatalities. In other words, althougt not having caused the most total deaths, tsunamis are the deadliest event on the dataset: each recorded tsunami (33 tsunamis total) caused the death of 1.65 people (20 fatalities total).


### Which types of events have the greatest economic consequences?

The economic impact is estimated here by property and crop damages, in US dollars.


```r
select(getevents(propdmgdolar),evtype,propdmgdolar)[1:20,]
```

```
##               evtype propdmgdolar
## 1              FLOOD 143944833550
## 2  HURRICANE/TYPHOON  69305840000
## 3        STORM SURGE  43193536000
## 4            TORNADO  24616945710
## 5        FLASH FLOOD  15222203910
## 6               HAIL  14595143420
## 7          HURRICANE  11812819010
## 8     TROPICAL STORM   7642475550
## 9          HIGH WIND   5247860360
## 10          WILDFIRE   4758667000
## 11  STORM SURGE/TIDE   4641188000
## 12         TSTM WIND   4478026440
## 13         ICE STORM   3642248810
## 14 THUNDERSTORM WIND   3382654440
## 15  WILD/FOREST FIRE   3001782500
## 16      WINTER STORM   1532743250
## 17           DROUGHT   1046101000
## 18         LIGHTNING    743077080
## 19        HEAVY SNOW    634417540
## 20           TYPHOON    600230000
```


```r
select(getevents(cropdmgdolar),evtype,cropdmgdolar)[1:20,]
```

```
##               evtype cropdmgdolar
## 1            DROUGHT  13367566000
## 2              FLOOD   4974778400
## 3          HURRICANE   2741410000
## 4  HURRICANE/TYPHOON   2607872800
## 5               HAIL   2476029450
## 6        FLASH FLOOD   1334901700
## 7       EXTREME COLD   1288973000
## 8       FROST/FREEZE   1094086000
## 9         HEAVY RAIN    728169800
## 10    TROPICAL STORM    677711000
## 11         HIGH WIND    633561300
## 12         TSTM WIND    553915350
## 13    EXCESSIVE HEAT    492402000
## 14 THUNDERSTORM WIND    398331000
## 15          WILDFIRE    295472800
## 16           TORNADO    283425010
## 17            FREEZE    146225000
## 18  WILD/FOREST FIRE    106782330
## 19        HEAVY SNOW     71122100
## 20       STRONG WIND     64953500
```

Economic impact:


```r
plotprop <- ggplot(getevents(propdmgdolar,arrangeby="value")[1:20,], aes(x=reorder(evtype,propdmgdolar), y=propdmgdolar)) +
    geom_bar(stat="identity", fill="slategray3", alpha=.8) +
    coord_flip() + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = "Property Damage \n Cost in US Dollars", x = "") + scale_y_sqrt()

plotcrop <- ggplot(getevents(cropdmgdolar,arrangeby="value")[1:20,], aes(x=reorder(evtype,cropdmgdolar), y=cropdmgdolar)) +
    geom_bar(stat="identity", fill="olivedrab4", alpha=.8) +
    coord_flip() + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = "Crop Damage \n Cost in US Dollars", x = "") + scale_y_sqrt()

grid.arrange(plotprop, plotcrop, ncol = 2, top="Natural Event's Economical Impact in the U.S. between 1996 and 2011 \n (square root scale)")
```

![plot of chunk plot3](figure/plot3-1.png) 

