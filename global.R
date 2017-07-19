library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidycensus)
library(data.table)
library(tigris)
library(readxl)
library(DT)
library(ggplot2)
library(psych)
library(leaflet)
library(plotly)

#setwd("~/DSA/RDA/Shiny/Project")

#census_api_key("ab278550a1784132f981997c48014f5614b9404b")

load('./.RData')

#census <- get_acs(geography = "county", variables = c("B01002_001", "B06011_001"), year = 2014)
census$GEOID = as.integer(census$GEOID)

cancercounties <- fread("./cancercounties.csv")
cancercounties$GEOID = as.integer(cancercounties$GEOID)
colnames(cancercounties)[2] = "GEOID"
colnames(cancercounties)[4] = "Cases"
cancercounties$Cases = as.numeric(gsub("#", "", cancercounties$Cases))
cc = cancercounties[, c("GEOID", "Cases")]

aqi <- fread("./annual_aqi_by_county_2015.csv")

aqi = aqi %>%
  mutate(NAME = paste(County, State, sep=" County, ")) %>% 
  select(NAME, AQI = 'Median AQI')

ob <- read_excel("./OB_PREV_ALL_STATES.xlsx")

ob = ob %>% 
  select(GEOID = 'FIPS Codes', Obesity = `age-adjusted percent__9`) %>% 
  mutate_all(as.numeric)

vars = census %>% 
  spread(variable, estimate) %>% 
  select(-moe) %>% 
  group_by(GEOID) %>% 
  summarise_all(funs(first(na.omit(.)))) %>% 
  rename(age = B01002_001, income = B06011_001)


vars = left_join(vars, aqi, by = 'NAME')
vars = left_join(vars, cc, by = 'GEOID')
vars = left_join(vars, ob, by = 'GEOID')
vars = vars %>% 
  separate(NAME, c('County', 'State'), sep=', ' )

vars = vars[,c(1,2,3,7,4,6,5,8)]

#counties <- counties(cb = TRUE, resolution = "20m")
counties$GEOID = as.integer(counties$GEOID)

fullmap = geo_join(counties, vars, by = "GEOID")

choice <- colnames(vars)[(4:8)]
states <- unique(vars$State)

popupFull <- paste0(fullmap$County,"<br>","State: ", fullmap$State,"<br>", 
                    "Cancer Incidence Rate (per 100,000): ", fullmap$Cases)


col1 <- subset(fullmap, select = Cases)[[1]]

cors = map_df(as.list(vars[5:8]),function (x) {cor.test(vars$Cases, x)$estimate[[1]]})
        
desc <- describe(vars[4:8])[c(2,4,5,8,9,10)]
desc <- mutate(desc, Variables = rownames(desc))
desc <- desc[,c(7, 1:6)]

