library(ggplot2)
library(readr)
library(rgdal)
library(ggmap)
library(leaflet)
library(tidyverse)
library(tidycensus)
library(maptools)
library(data.table)
library(tigris)


census_api_key("ab278550a1784132f981997c48014f5614b9404b")


v15 <- load_variables(2015, "acs5", cache = TRUE)
ages <- get_acs(geography = "county", variables = "B01002_001", year = 2014)




cancercounties <- fread("./cancercounties.csv")
counties <- readOGR('./countyshapes/cb_2016_us_county_20m.shp', 
                    layer = 'cb_2016_us_county_20m',
                    stringsAsFactors = FALSE)

counties <- counties()

counties.df <- as(counties, "data.frame")
counties.df

cancercounties$GEOID = as.character(cancercounties$GEOID)


colnames(cancercounties)[2] = "GEOID"
colnames(cancercounties)[4] = "cases"


cc = cancercounties[, c("GEOID", "cases")]


cc[setDT(ages), age := i.estimate, on = "GEOID"]

df = merge(counties, cc, by = "GEOID")

counties$GEOID[counties$GEOID[[2]] == 6]


class(counties$GEOID[1])

leaflet(df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", as.numeric(cases))(as.numeric(cases)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

leaflet(df) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", as.numeric(age))(as.numeric(age)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
