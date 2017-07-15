library(ggplot2)
library(readr)
library(leaflet)
library(tidyverse)
library(tidycensus)
library(data.table)
library(tigris)
library(readxl)

census_api_key("ab278550a1784132f981997c48014f5614b9404b")


v15 <- load_variables(2015, "acs5", cache = TRUE)
ages <- get_acs(geography = "county", variables = "B01002_001", year = 2014)
ages$GEOID = as.integer(ages$GEOID)


setwd("~/DSA/RDA/Shiny/Project")
cancercounties <- fread("./cancercounties.csv")
cancercounties$GEOID = as.integer(cancercounties$GEOID)
colnames(cancercounties)[2] = "GEOID"
colnames(cancercounties)[4] = "cases"
cancercounties$cases = as.numeric(gsub("#", "", cancercounties$cases))
cc = cancercounties[, c("GEOID", "cases")]

pol <- read_excel("~/DSA/RDA/Shiny/Project/ctyfactbook2015.xlsx")
colnames(pol)[3] = "GEOID"
colnames(pol)[11] = "PM2W"
colnames(pol)[10] = "PM10"
colnames(pol)[9] = "o3"
colnames(pol)
pol$GEOID = as.integer(pol$GEOID)

counties <- counties(cb = TRUE, resolution = "20m")
counties$GEOID = as.integer(counties$GEOID)
cc[setDT(ages), age := i.estimate, on = "GEOID"]

cc$GEOID = as.integer(cc$GEOID)

df2 <- full_join(pol, cc, by = "GEOID")
df <- geo_join(counties, df2, by="GEOID")

cor(df$cases, as.numeric(df$PM2W), use = "pairwise.complete.obs")
cor(df$cases, as.numeric(df$PM10), use = "pairwise.complete.obs")
cor(df$cases, as.numeric(df$age), use = "pairwise.complete.obs")
chisq.test(df$cases, as.numeric(df$PM10))
chisq.test(df$cases, as.numeric(df$age))

popup <- paste0("County: ", df$County, "<br>", "State: ", df$State, "<br>", "Cancer Incidence Rate (per 100,0
                00): ", cc$cases)

leaflet(df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", as.numeric(cases))(as.numeric(cases)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = popup)

leaflet(df) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", as.numeric(age))(as.numeric(age)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
leaflet(df) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", as.numeric(o3))(as.numeric(o3)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
