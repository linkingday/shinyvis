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

function(input, output, session){
  

  output$map <- renderLeaflet({
    leaflet(fullmap) %>% addProviderTiles('CartoDB.Positron') %>%
      setView(lng=-97.5833, lat=38.8333, zoom = 4,options=(animate=FALSE)) #%>% 
      # addLegend("bottomright", pal = colorBin("YlOrRd", col1, n=8), values = col1, opacity = 1,
      #           title = "Quantile Subset")
  })
  
  # pal <- reactive({
  #   colorBin("YlOrRd", vars[as.character(input$selected)][[1]], 6)
  # })
  
  observeEvent(input$selected, {
    
    pal <- colorQuantile("YlOrRd", domain = vars[as.character(input$selected)][[1]])
    
    col <- subset(fullmap, select = input$selected)[[1]]

    leafletProxy("map", data=fullmap) %>%
      clearShapes() %>% clearControls() %>% 
      addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(col),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = paste0(fullmap$County,"<br>","State: ", fullmap$State,"<br>", 
                                 "Cancer Incidence Rate (per 100,000): ", fullmap$Cases)) %>% 
      addLegend("bottomright", pal = colorBin("YlOrRd", col, n=8), values = col, opacity = 1,
                title = "Quantile Subset")
    
    smp <- geo_join(counties, filter(vars, State == input$statels),
                    by="GEOID", how='inner')
    
    cols <- subset(smp, select = input$selected)[[1]]
    
    
    leafletProxy("mapstate", data=smp) %>%
      clearShapes() %>%
      addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", cols)(cols),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = paste0(smp$County,"<br>", 
                                 "Cancer Incidence Rate (per 100,000): ", smp$Cases)) %>%
      fitBounds(~min(fortify(smp)$long,na.rm=T), ~min(fortify(smp)$lat,na.rm=T), 
                ~max(fortify(smp)$long,na.rm=T), ~max(fortify(smp)$lat,na.rm=T))
      
    
      })

  stateinit <- geo_join(counties, filter(vars, State == 'Alabama'),
                   by="GEOID", how='inner')
  

  output$mapstate <- renderLeaflet({
    leaflet(stateinit) %>% #addProviderTiles('CartoDB.Positron') %>%
      addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  })

  observeEvent(input$statels, {
    
    pal <- colorQuantile("YlOrRd", domain = vars[as.character(input$selected)][[1]])

    smp <- geo_join(counties, statev(),
                                      by="GEOID", how='inner')

    cols <- subset(smp, select = input$selected)[[1]]

    leafletProxy("mapstate", data=smp) %>%
      clearShapes() %>%
      addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(cols),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      fitBounds(~min(fortify(smp)$long,na.rm=T), ~min(fortify(smp)$lat,na.rm=T), 
                ~max(fortify(smp)$long,na.rm=T), ~max(fortify(smp)$lat,na.rm=T))
  })
  
  output$alltab <- renderTable(desc)
  
  output$cortest <- renderTable(cors)
  
  desc <- describe(vars[4:8])[c(2,4,5,8,9,10)]
  desc <- mutate(desc, Variables = rownames(desc))
  desc <- desc[,c(7, 1:6)]
  
  
  statetable <- reactive({
    describe((vars %>% filter(State == input$statels))[4:8])[c(2,4,5,8,9,10)]
  })
  
  output$statetab <- renderTable(statetable())
  
  pick <- reactive({
    vars[as.character(input$selected)]
  })
  
  output$dens <- renderPlotly(
    ggplotly(ggplot(pick(), aes(x=pick()[1])) +
      geom_density(fill='red') + xlab(input$selected) + 
        ggtitle(paste0("Density of ",as.character(input$selected))))
  )
  
  output$boxp <- renderPlotly(
    ggplotly(ggplot(pick(), aes(x='', y=pick()[1])) +
      geom_boxplot(outlier.shape = NA) + ylab(input$selected) + xlab('U.S.') +
      ggtitle(paste0('Distribution of ',as.character(input$selected)))))
  
  statev <- reactive({
    vars %>% filter(State == input$statels)
  })
  
  stateAllV <- reactive({
    vars %>% filter(State == input$statels) %>% select(input$selected)
  })
  
  # stv <- vars %>% filter(State == 'New York') %>% select('Cases')
  # ggplotly(ggplot(stv, aes(Cases)) +
  #            geom_density())
  
  output$sdens <- renderPlotly(
    ggplotly(ggplot(statev(), aes_string(input$selected)) +
      geom_density(fill='red') + xlab(input$selected) + 
        ggtitle(paste0("Density of ",as.character(input$selected), ' in ',
                       as.character(input$statels)))))
  
  output$sboxp <- renderPlotly(
    ggplotly(ggplot(stateAllV(), aes(x = '', y = stateAllV()[1])) +
               geom_boxplot()+ ylab(input$selected) + xlab(input$statels) +
      ggtitle(paste0('Distribution of ', as.character(input$selected),' in ', 
                     as.character(input$statels)))))
}
#vars %>% filter(State == "New York") %>% ggplot(aes(x='', y=Cases)) + geom_boxplot()
