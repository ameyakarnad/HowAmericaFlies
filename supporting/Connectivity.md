Visualise Airline Connectivity!!
================

These maps were created for our visualisation and were not included in the final report due to intergration issues

``` r
library(tidyverse)
air_data <- read_csv(file = "D:/Columbia/Fall 2018/EDAV/Ameya Work final/Airline_Data_v31/01_year_data.csv",
                     col_names = TRUE)

library(maps)
library(geosphere)
library(dplyr)

myairlines <- air_data %>%
  mutate(Flights = 1)
airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE)

selected <- myairlines %>%
  select(Reporting_Airline, Origin, OriginStateName, Dest, DestStateName, Flights)
selected <- selected %>%
  subset(OriginStateName != "Alaska" & OriginStateName != "Hawaii" & OriginStateName != "Puerto Rico" & 
           OriginStateName != "U.S. Virgin Islands" & OriginStateName != "U.S. Pacific Trust Territories and Possessions") %>%
  subset(DestStateName != "Alaska" & DestStateName != "Hawaii" & DestStateName != "Puerto Rico" &
DestStateName != "U.S. Virgin Islands" & DestStateName != "U.S. Pacific Trust Territories and Possessions")


pal <- colorRampPalette(c("gray25", "gray100"))
colors <- pal(100)
map("usa", col="gray10",fill=TRUE, bg="white",lwd=0.05)

fsub <- selected[selected$Reporting_Airline == "WN",]
grp <- fsub %>% group_by(Origin,Dest) %>% summarise(freq = sum(Flights))
grp$Origin <- as.character(grp$Origin)
grp$Dest <- as.character(grp$Dest)
maxcnt <- max(grp$freq)
airports <- na.omit(airports)
for (j in 1:nrow(grp)) {
    air1 <- airports[airports$iata == grp[j,]$Origin,]
    air2 <- airports[airports$iata == grp[j,]$Dest,]
    if (nrow(air1)!=0 & nrow(air2)!=0) {
    inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, 
                            addStartEnd=TRUE)
     inter
    colindex <- round( (grp[j,]$freq / maxcnt) * length(colors) )
    points(x=air1[1,]$long, y=air1[1,]$lat, pch=19, 
       col="white")
    points(x=air2[1,]$long, y=air2[1,]$lat, pch=19, 
       col="white")
    lines(inter, col=colors[colindex], lwd=0.8)
    }
} 

title(main = "Routes Operated by Southwestern Airlines", sub = "Limited routes, Mostly concentrated 
      in Eastern USA",
      cex.main = 2,   font.main= 3, col.main= "black",
      cex.sub = 0.75, font.sub = 30, col.sub = "black",
      col.lab ="darkblue"
      )
```

![](Connectivity_files/figure-markdown_github/unnamed-chunk-1-1.png)

-   South Western Airlines mainly connect the South western parts of the United States (Califoria, Arizona) to the other cities in the United states.
-   Contraditing its name, it also operates routes from/to New York, Florida and Texas
-   It has the highest number of flights, but operates on less number of routes.

``` r
pal <- colorRampPalette(c("gray25", "gray100"))
colors <- pal(100)
map("usa", col="gray10",fill=TRUE, bg="white",lwd=0.05)

fsub <- selected[selected$Reporting_Airline == "DL",]
grp <- fsub %>% group_by(Origin,Dest) %>% summarise(freq = sum(Flights))
grp$Origin <- as.character(grp$Origin)
grp$Dest <- as.character(grp$Dest)
maxcnt <- max(grp$freq)
airports <- na.omit(airports)
for (j in 1:nrow(grp)) {
    air1 <- airports[airports$iata == grp[j,]$Origin,]
    air2 <- airports[airports$iata == grp[j,]$Dest,]
    if (nrow(air1)!=0 & nrow(air2)!=0) {
    inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat),
                            n=100, addStartEnd=TRUE)
     inter
    colindex <- round( (grp[j,]$freq / maxcnt) * length(colors) )
    points(x=air1[1,]$long, y=air1[1,]$lat, pch=19, 
       col="white")
    points(x=air2[1,]$long, y=air2[1,]$lat, pch=19, 
       col="white")
    lines(inter, col=colors[colindex], lwd=0.8)
    }
} 

title(main = "Routes Operated by Delta Airlines", sub = "Headquatered at Atlanta",
      cex.main = 2,   font.main= 3, col.main= "black",
      cex.sub = 0.75, font.sub = 30, col.sub = "black",
      col.lab ="darkblue"
      )
```

![](Connectivity_files/figure-markdown_github/unnamed-chunk-2-1.png)

-   Delta Airlines is headquartered at Atlanta. The number of routes that are concentrated at Atlanta show the Dominance of Delta Airlines in the city
-   Mainlu operates in the Eastern parts of the United States

``` r
pal <- colorRampPalette(c("gray25", "gray100"))
colors <- pal(100)
map("usa", col="gray10",fill=TRUE, bg="white",lwd=0.05)

fsub <- selected[selected$Reporting_Airline == "OO",]
grp <- fsub %>% group_by(Origin,Dest) %>% summarise(freq = sum(Flights))
grp$Origin <- as.character(grp$Origin)
grp$Dest <- as.character(grp$Dest)
maxcnt <- max(grp$freq)
airports <- na.omit(airports)
for (j in 1:nrow(grp)) {
    air1 <- airports[airports$iata == grp[j,]$Origin,]
    air2 <- airports[airports$iata == grp[j,]$Dest,]
    if (nrow(air1)!=0 & nrow(air2)!=0) {
    inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat),
                            n=100, addStartEnd=TRUE)
     inter
    colindex <- round( (grp[j,]$freq / maxcnt) * length(colors) )
    points(x=air1[1,]$long, y=air1[1,]$lat, pch=19, 
       col="white")
    points(x=air2[1,]$long, y=air2[1,]$lat, pch=19, 
       col="white")
    lines(inter, col=colors[colindex], lwd=0.8)
    }
} 

title(main = "Routes Operated by Skywest Airlines", sub = "Large number of routes, Caters to 
      smaller airports",
      cex.main = 2,   font.main= 3, col.main= "black",
      cex.sub = 0.75, font.sub = 30, col.sub = "black",
      col.lab ="darkblue"
      )
```

![](Connectivity_files/figure-markdown_github/unnamed-chunk-3-1.png)

-   Skywest Airline is a small city airline. It mostly connects the metropolitian cities to the tier 2 and tier 3 cities.
-   It has the most number of unique routes, a comparision between this and other graphs shows exactly that

``` r
pal <- colorRampPalette(c("gray25", "gray100"))
colors <- pal(100)
map("usa", col="gray10",fill=TRUE, bg="white",lwd=0.05)

fsub <- selected[selected$Reporting_Airline == "9E",]
grp <- fsub %>% group_by(Origin,Dest) %>% summarise(freq = sum(Flights))
grp$Origin <- as.character(grp$Origin)
grp$Dest <- as.character(grp$Dest)
maxcnt <- max(grp$freq)
airports <- na.omit(airports)
for (j in 1:nrow(grp)) {
    air1 <- airports[airports$iata == grp[j,]$Origin,]
    air2 <- airports[airports$iata == grp[j,]$Dest,]
    if (nrow(air1)!=0 & nrow(air2)!=0) {
    inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat),
                            n=100, addStartEnd=TRUE)
     inter
    colindex <- round( (grp[j,]$freq / maxcnt) * length(colors) )
    points(x=air1[1,]$long, y=air1[1,]$lat, pch=19, 
       col="white")
    points(x=air2[1,]$long, y=air2[1,]$lat, pch=19, 
       col="white")
    lines(inter, col=colors[colindex], lwd=0.8)
    }
} 

title(main = "Routes Operated by Endeavor Air", sub = "Dominates the Eastern United States",
      cex.main = 2,   font.main= 3, col.main= "black",
      cex.sub = 0.75, font.sub = 30, col.sub = "black",
      col.lab ="darkblue"
      )
```

![](Connectivity_files/figure-markdown_github/unnamed-chunk-4-1.png)

-   Endeavor Air operates exclusively on the Eastern Part of the United States connecting most of the airports there
