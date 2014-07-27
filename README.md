
### Evaluation of the Effects of Natural Diasters on Humans and its Monetary Costs

### Synopsis

Using the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database (<http://www.ncdc.noaa.gov/oa/climate/severeweather/extremes.html>) it is shown here the following:

* Across the United States, **tornadoes** are most harmful with respect to population health. Harmful is in terms of number of fatalities and injuried combined.

* Across the United States, **flooding** the greatest economic consequences. The economic consequence are interms of damage to property and crops is estimated to be nearly $326B.

The result of this analysis will also show that during the period from January 1, 1990 to November 30, 2011 both Alabama and Texas had nearly the same amount of huam impact (fatalities and injuries).  During the same time period Alabama had the maximum economic impact (property and crop damage) while Missouri had the second most economic impact.

### Data Processing 
The storm data is in a comma delimted values (csv) with an header.  This file is first loaded into a data frame (df).

```{r, cache=TRUE,echo=TRUE}
df <- read.csv(bzfile("./repData-data-StormData.csv.bz2"), stringsAsFactors=FALSE,header=TRUE)
```

The strom data file contains `r nrow(df)` records and the attributes (column names) of these records are listed below:

```{r, echo=TRUE}
colnames(df)
```

Our report here focusses on various events (sepcified in the EVTYPE column), costs (specified in columns PROPDMG, PROPDMGEXP (unit for PROPDMG), CROPDMG, and CROPDMGEXP (unit for CROPDMG)), and harmful factors (specified in columns FATALITIES and INJURIES).  Our analysis will also present information on costs and harmful factors for various states. Given this, we will first obtain a subset of the data (as a added bonus it will speedup the rest of the computation).

```{r, cache=TRUE,echo=TRUE}
dfsmall <- df[,c("STATE", "BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", 
                 "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
dfsmall$EVTYPE <- toupper(dfsmall$EVTYPE) ## change all to uppercase
```

The columns PROPDMGEXP and CROPDMGEXP contains several single letters that should correspond to unit.  We are intersted only in letters (upper or lower) h,k,m, and b that indicates the units to be hundreds, thousands, millions, and billions. We will replace letters in columns PROPDMGEXP and CROPDMGEXP with integers corresponding to their units.

```{r cache=TRUE,echo=TRUE}
##
## Credit for this code below goes to a forum contributor
##
pattern <- c("^$|[-?+]", "[hH]", "[kK]", "[mM]", "[bB]")
replacement <-c("0", "2", "3", "6", "9")
for (i in 1:length(pattern)) {
  dfsmall$PROPDMGEXP <- sub(pattern[i],replacement[i], dfsmall$PROPDMGEXP)
  dfsmall$CROPDMGEXP <- sub(pattern[i],replacement[i], dfsmall$CROPDMGEXP)
}
dfsmall$PROPDMGEXP <- as.numeric(dfsmall$PROPDMGEXP)
dfsmall$CROPDMGEXP <- as.numeric(dfsmall$CROPDMGEXP)
```

The following code segment is will calculate the total of FATALITIES and INJURIES.  Also we will use the units to multiply with the damage costs for property and crops.  This will help us to determine the total costs.

```{r, cache=TRUE,echo=TRUE}
temp <- sapply(1:nrow(dfsmall), function(i) {
  x1 <- dfsmall[i,c('PROPDMG')]*10^dfsmall[i,c('PROPDMGEXP')]
  x2 <- dfsmall[i,c('CROPDMG')]*10^dfsmall[i,c('CROPDMGEXP')]
  x3 <- x1 + x2
  x4 <- dfsmall[i,c('FATALITIES')] + dfsmall[i,c('INJURIES')]
  return(c(x1,x2,x3,x4))
})
t1 <- as.data.frame(t(temp))
colnames(t1) <- c("PROPCASH","CROPCASH","TOTALCASH","HARMED")
dfsmall <- cbind(dfsmall,t1)
head(dfsmall)
```

We are going to focus on the following 8 events as given in the manual of the Federal Emergency Management Administration (<http://www.fema.gov/media-library-data/20130726-1549-20490-4629/natural_hazards_1.pdf>). These events are:

* FLOOD
* TORNADOES
* HURRICANES
* THUNDERSTROM & LIGHTNING
* WINTER STROM & EXTREME COLD
* EXCESSIVE HEAT
* WILD FIRES
* HIGH WINDS

Given that we will focus on the above events, we will map several events given in the column EVTYPE to one of the above (e.g SNOW to WINTER STROM & EXTREME COLD). Note that the column EVTYPE cotains events that we have not mapped to the above (e.g HIGH WINDS).

```{r, cache=TRUE,echo=TRUE}
dfsmall[grep(" FLD|HIGH WATER|RAPIDLY RISING WATER|TYPHOON|TSUNAMI|STORM SURGE|COASTAL STORM|FLOOD|RAIN|HAIL|SHOWER|FLOOODING",
             dfsmall$EVTYPE),c("EVTYPE")] <- "FLOOD"
dfsmall[grep("TORNAD|TORNDAO",dfsmall$EVTYPE),c("EVTYPE")] <- "TORNADOES"
dfsmall[grep("HURRICA",dfsmall$EVTYPE),c("EVTYPE")] <- "HURRICANES"
dfsmall[grep("THUNDERTORM|THUNDEERSTORM|THUNERSTORM|THUNDERESTORM|TROPICAL STORM|THUNDERSTORM|THUNDESTORM|THUNDERSTROM|THUNDER STORM|TUNDERSTORM|THUNDERTSORM|LIGHTN",
             dfsmall$EVTYPE),c("EVTYPE")] <- "THUNDERSTORMS & LIGHTNING"
dfsmall[grep("WINTRY MIX|MIXED PRECIP|LOW TEMPERATURE|WIND CHILL|WINDCHILL|HEAVY PRECIPATATION|SNOW|COLD|SLEET|FREEZ|BLIZZA|ICE|ICY|WINTER|BLOWING SNO|HEAVY MIX|EXTREME WIND CHILL",
             dfsmall$EVTYPE),c("EVTYPE")] <- "WINTER STORM & EXTREME COLD"
dfsmall[grep("UNSEASONABLY WARM|HEAT",dfsmall$EVTYPE),c("EVTYPE")] <- "EXCESSIVE HEAT"
dfsmall[grep("FIRE",dfsmall$EVTYPE),c("EVTYPE")] <- "WILD FIRES"
dfsmall[grep("WIND",dfsmall$EVTYPE),c("EVTYPE")] <- "HIGH WINDS"
```

### Results

For each of the events we can now calculate the total of the fields "HARMED" (which is sum of FATALITIES and INJURIES). Next we will select the events wherein HARMED is greater than 100. From the table below it it clear that tornadoes causes the greatest harm to humans in the United States.

```{r, cache=TRUE,echo=TRUE}
library("plyr")
r1 <- ddply(dfsmall,.(EVTYPE),colwise(sum, c("HARMED")))
r1 <- r1[r1$HARMED > 0,]  ## Select rows where the number of HARMED is > 100
r1[order(-r1$HARMED),]
```

For each of the events we can now calculate the total of the fields "TOTALCASH" (which is sum of PROPCASH and CROPCASH). Next we will select the events wherein TOTALCASH is greater than 10 million dollars. From the table below it is clear that flooding causes the greatest economic impact among all natural hazards totalling a whopping $326B.

```{r, cache=TRUE,echo=TRUE}
library(scales)
r2 <- ddply(dfsmall,.(EVTYPE),colwise(sum, c("TOTALCASH")))
r2 <- r2[r2$TOTALCASH > 10*10^6,]  ## Select rows where the TOTALCASH is > 10M
r3 <- r2[order(-r2$TOTALCASH),]
r3$TOTALCASH <- dollar(r3$TOTALCASH)
r3
```

### Tornadoes in the United States

The following plot will show pictorially the human impact of tornadoes in each of the states in the United States starting from January 1, 1990 to November 30, 2011. The barplot shows that Alabama and Texas had nearly the same amount of human impact due to tornadoes.

<figure>
```{r, echo=TRUE, cache=TRUE}
adate <- "01/01/1990 0:00:00"
r4 <- dfsmall[grep("TORNADOES",dfsmall$EVTYPE),]
r4 <- subset(r4,r4$BGN_DATE > adate)
r5 <- ddply(r4,.(STATE),colwise(sum, c("HARMED")))
bp <- barplot(r5$HARMED,las=2,names=r5$STATE,cex.names=0.5,cex.axis=0.5,
              horiz=FALSE,space=5.0,main="Human Impacts of Tornadoes (1/1/1990 - 11/30/2011)",
              xlab="States in USA",ylab="Fatalities + Injuries")
```
<figcaption>
Histograph of human impact from tornadoes from 1/1/1990 - 11/30/2011.
</figcaption>
</figure>

The following plot will show pictorially the economic impact of tornadoes in each of the states in the United States starting from January 1, 1990 to November 30, 2011. The barplot shows that Alabama and Missouri had top two severe economic impacts due to tornadoes.

<figure>
```{r, echo=TRUE, cache=TRUE}
adate <- "01/01/1990 0:00:00"
r4 <- dfsmall[grep("TORNADOES",dfsmall$EVTYPE),]
r4 <- subset(r4,r4$BGN_DATE > adate)
r5 <- ddply(r4,.(STATE),colwise(sum, c("TOTALCASH")))
bp <- barplot(round(r5$TOTALCASH/10^6,2),las=2,names=r5$STATE,cex.names=0.5,cex.axis=0.5,
              horiz=FALSE,space=5.0,main="Economic Impacts of Tornadoes (1/1/1990 - 11/30/2011)",
              xlab="States in USA",ylab="Crop and Property Damage Costs in Millions")
```
<figcaption>
Histograph of economic impact from tornadoes from 1/1/1990 - 11/30/2011.
</figcaption>
</figure>

### Conclusions

The data set given for this project is very interesting but of poor quaity.  It was suprising that Alabama was most affected by tornadoes than other states.

  
  
