library(lubridate)
library(leaflet)
library(sf)
library(dplyr)
library(rdwd)
library(ggplot2)

# Reads and cleans the data
dat <- read.csv("Clean_Stream_Database_2015_2022_DAR - DATABASE.csv")
dat$Date <- mdy(dat$Date)
dat$Count <- as.numeric(dat$Count)
length(sort(unique(dat$Species)))
dat$Species <- sub("Andinocara rivulatus", "Andinoacara rivulatus", dat$Species)
length(sort(unique(dat$Species)))

# condenses dat into only wanted columns and removes NA
sdat <- dat[, c(1, 2, 3, 8, 9, 13, 15)]
names(sdat) <- c("date", "lat", "lon", "stream", "location", "species", "count")
sdat <- na.omit(sdat)

# stashes the unique names
ndat <- sort(unique(sdat$species))

# condenses sdat into only unique locations
ldat <- sdat[match(unique(sdat$location), sdat$location), ]

# maps only the locations
leaflet(data = ldat[1:length(ldat$date), ]) %>%
addProviderTiles(providers$Esri.WorldImagery) %>%
addMarkers(~lon, ~lat, popup = ~as.character(location),
           label = ~as.character(location))

# loops through the data frame and sums species collected by site
cdat <- data.frame()

for (i in ndat) {
     temp <- sdat[sdat$species == i, ]
     temp_out <- aggregate(count ~ species + location + lon + lat,
                           data = temp, FUN = sum)
    cdat <- rbind(cdat, temp_out)
}

# combines the gensp and count data
genspcount <- paste(cdat$species, cdat$count, sep = ": ")
cdat <- data.frame(cdat, genspcount)

# converts data frame to sf object for mapping
cdat <- st_as_sf(cdat, coords = c("lon", "lat"))

maplab <- paste(sep = "<br/>", cdat$genspcount)

# maps sf object
leaflet(data = cdat[1:length(cdat$species), ]) %>%
addProviderTiles(providers$Esri.WorldImagery) %>%
addMarkers(label = ~location,
           popup = genspcount)

ggplot(data = sdat[sdat$species == "Amatitlania nigrofasciata",], aes(x = date, y = count, col = species)) +
    geom_bar(stat = "identity")

ggplot(data = sdat[sdat$species == "Amphilophus labiatus",], aes(x = date, y = count, col = species)) +
    geom_bar(stat = "identity")

ggplot(data = sdat[sdat$species == "Ancistrus temminckii",], aes(x = date, y = count, col = species)) +
    geom_bar(stat = "identity")


all_counts <- aggregate(count ~ date,
                        data = sdat, FUN = sum)

class(all_counts$count)

ggplot(data = all_counts, aes(x = date, y = count)) +
    geom_bar(stat = "identity")
