library("knitr")
library("dplyr")
library("lubridate")
library("ggplot2")

# Read the dataset as dataframe
stormDf <- read.csv("repdata_data_StormData.csv.bz2")    #Cache it!

# Put dates in good format and select the year
stormDf$BGN_DATE <- as.Date(stormDf$BGN_DATE, format = "%m/%d/%Y %H:%M:%S")
stormDf$YEAR <- year(stormDf$BGN_DATE)

relevStormDf <- select(stormDf, YEAR, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

#########
# PREPROCESS THE DATA

#There are not so many events prior to the 90s
png("Figures/cut_years.png", height = 480, width = 480, units = "px")
barplot(table(relevStormDf$YEAR), 
        main = "Number of weather events per year", 
        xlab = "Year", ylab = "Frequency", 
        col = "red")
dev.off()

relevStormDf <- filter(relevStormDf, relevStormDf$YEAR > 1990)

# Let's work out the event type factors
relevStormDf$EVTYPE <- as.factor(toupper(relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(trimws(relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("\\ {2, }", "\\ ", relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("(\\ *)[/&\\-](\\ *)", "/", relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("[/\\.]$", "", relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("TIDES", "TIDE", relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("FLOOODING", "FLOODING", relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("CLOUDS", "CLOUD", relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("HVY", "HEAVY", relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("TORNDAO", "TORNADO", relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("TORNADO(E*)S", "TORNADO", relevStormDf$EVTYPE))
relevStormDf$EVTYPE <- as.factor(gsub("W(I*)ND(S*)", "WIND", relevStormDf$EVTYPE))


# In this two columns we have abbreviated multipliers H (Hundred), K (Thousand), M (Million) and B (Billion).
relevStormDf$PROPDMGEXP <- gsub("[Hh]", "2", relevStormDf$PROPDMGEXP)
relevStormDf$PROPDMGEXP <- gsub("[Kk]", "3", relevStormDf$PROPDMGEXP)
relevStormDf$PROPDMGEXP <- gsub("[Mm]", "6", relevStormDf$PROPDMGEXP)
relevStormDf$PROPDMGEXP <- gsub("[Bb]", "9", relevStormDf$PROPDMGEXP)
relevStormDf$PROPDMGEXP <- gsub("\\+|\\-|\\?\\ ", "0",  relevStormDf$PROPDMGEXP)
relevStormDf$PROPDMGEXP <- as.numeric(relevStormDf$PROPDMGEXP)
relevStormDf$PROPDMGEXP[is.na(relevStormDf$PROPDMGEXP)] <- 0

relevStormDf$CROPDMGEXP <- gsub("[Hh]", "2", relevStormDf$CROPDMGEXP)
relevStormDf$CROPDMGEXP <- gsub("[Kk]", "3", relevStormDf$CROPDMGEXP)
relevStormDf$CROPDMGEXP <- gsub("[Mm]", "6", relevStormDf$CROPDMGEXP)
relevStormDf$CROPDMGEXP <- gsub("[Bb]", "9", relevStormDf$CROPDMGEXP)
relevStormDf$CROPDMGEXP <- gsub("\\+|\\-|\\?\\ ", "0", relevStormDf$CROPDMGEXP)
relevStormDf$CROPDMGEXP <- as.numeric(relevStormDf$CROPDMGEXP)
relevStormDf$CROPDMGEXP[is.na(relevStormDf$CROPDMGEXP)] <- 0

relevStormDf <- mutate(relevStormDf, PROPDMG = PROPDMG * (10 ^ PROPDMGEXP), CROPDMG = CROPDMG * (10 ^ CROPDMGEXP)) 
relevStormDf <- select(relevStormDf, -c(PROPDMGEXP, CROPDMGEXP))

#########
# HOW MANY PEOPLE ARE IMPACTED BY THE NATURAL EVENTS?
casualties <- summarise(group_by(relevStormDf, EVTYPE), 
                     totalFATALITIES = sum(FATALITIES), 
                     totalINJURIES = sum(INJURIES))
casualties <- mutate(casualties, 
                  IMPACT = totalFATALITIES + totalINJURIES)
mostDangerousEvents <- arrange(casualties, desc(IMPACT))[1:10, c(1, 4)]

# Make barplot of IMPACT
g <- ggplot(data = mostDangerousEvents, aes(EVTYPE))
g <- g + geom_bar(aes(weight=IMPACT),
                  colour = "black", fill = "green")
g <- g + coord_flip()
g <- g + labs(title = "Total amount of people injured or killed by hazardous weather events",
              x = "Event Type", y = "Impacted people")
# print(g)

ggsave("Figures/impact.png", plot=g, device = "png",
       height = 20, width = 20, units = "cm")

#######
# ECONOMICAL DAMAGE

ECON_DATA <- summarise(group_by(relevStormDf, EVTYPE), 
                         TOTAL_PROPDMG = sum(PROPDMG), 
                         TOTAL_CROPDMG = sum(CROPDMG))
ECON_DATA <- mutate(ECON_DATA, ECON_IMPACT = (TOTAL_PROPDMG + TOTAL_CROPDMG)/10e+9)

worstEconomicLosses <- arrange(ECON_DATA, desc(ECON_IMPACT))[1:10, c(1,4)]

# Make barplot of IMPACT
g <- ggplot(data = worstEconomicLosses, aes(EVTYPE))
g <- g + geom_bar(aes(weight=ECON_IMPACT),
                  colour = "black", fill = "blue")
g <- g + coord_flip()
g <- g + labs(title = "Total economic loss due to bad weather events",
              x = "Event Type", y = "Total amount of Property and Crop Damage in Billion USD")
# print(g)

ggsave("Figures/loss.png", plot=g, device = "png",
       height = 20, width = 20, units = "cm")