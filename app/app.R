#
# My Shinny App Fernando Garcia
# Data and Programming II

library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(viridis)
library(htmlwidgets)
library(htmltools)
library(tigris)
library(reshape2)
library(shiny)
library(shinythemes)
library(rsconnect)
library(plotly)
library(scales)

rsconnect::setAccountInfo(name='nanojgarcia', 
                          token='3D27515E9473E538F46606D5C46EA075', 
                          secret='C9pgrbt7XyTYjYnS3rhH1W89XhFro9VxZMgpDBZ/')

#rsconnect::deployApp('path/to/your/app')

rm(list = ls())

covid <- read.csv("COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv")

#Cleaning the COVID data
covid <- covid %>%
    filter(ZIP.Code != "Unknown") %>%
    mutate(WeekEnd = mdy(Week.End)) %>%
    select(ZIP.Code, Cases...Weekly, Cases...Cumulative, Case.Rate...Weekly,
           Case.Rate...Cumulative, Deaths...Cumulative, Deaths...Cumulative,
           Death.Rate...Cumulative, WeekEnd)

#Change colnames
colnames(covid) = gsub("[...]", "", colnames(covid))

zip <- st_read("Boundaries - ZIP Codes.geojson") %>%
    st_transform(., crs = 4326)

#Join Zip Codes with Covid Data
covid_zip <- left_join(zip, covid, c("zip" = "ZIPCode"))

#Vaccination Data
vaccine <- read.csv("COVID-19_Vaccinations_by_ZIP_Code.csv")

vaccine <- vaccine %>%
    dplyr::mutate(date = mdy(Date),
                  doses_per_population = Total.Doses...Cumulative / Population) %>%
    dplyr::select(Zip.Code, date, doses_per_population, Total.Doses...Daily, 
                  Total.Doses...Cumulative, Population)

colnames(vaccine) = gsub("[...]", "", colnames(vaccine))

vaccine_zip <- left_join(zip, vaccine, c("zip" = "ZipCode"))

#Shiny app

ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("COVID-19 in the City of Chicago by Zip Codes"),
    fluidRow(
        column(4,
    dateInput("date",
              "Choose a week:",
              daysofweekdisabled = c(0,1,2,3,4,5),
              value = max(covid_zip$WeekEnd),
              min = min(covid_zip$WeekEnd),
              max = max(vaccine_zip$date)
                ),
    selectInput("variable",
                "Variable:",
                c("Cumulative cases" = "CasesCumulative",
                  "Weekly cases" = "CasesWeekly",
                  "Cumulative rate cases" = "CaseRateCumulative",
                  "Weekly rate cases" = "CaseRateWeekly",
                  "Cumulative deaths" = "DeathsCumulative",
                  "Cumulative death rate" = "DeathRateCumulative",
                  "Cumulative Doses per population" = "doses_per_population" )),
    checkboxInput("street",
                  "Include major streets",
                  FALSE)),
    column(6,
        plotlyOutput("covid")
              )
    )
)

server <- function(input, output) {
    major_streets <- read_sf("Major_Streets.shp") %>%
        st_transform(., crs = 4326)
    
    data <- reactive({
        if(input$variable == "CasesCumulative" ||
           input$variable == "CasesWeekly" ||
           input$variable == "CaseRateCumulative" ||
           input$variable == "CaseRateWeekly" ||
           input$variable == "DeathsCumulative" ||
           input$variable == "DeathRateCumulative"){
            filter(covid_zip, WeekEnd == input$date) %>%
                select(input$variable)}
        else{
            
            filter(vaccine_zip, date == input$date) %>%
                select(input$variable)
        }
            
    })
    
    output$covid <- renderPlotly({
        p = ggplot() +
            geom_sf(data = data(), aes_string(fill = input$variable)) +
            scale_y_continuous(labels = comma) +
            labs(title = input$covid) +
            scale_fill_distiller(palette = "Spectral") +
            labs(fill = "") +
            theme_minimal()
        
        if(input$street){
            p  = p + geom_sf(data = major_streets)  
        }
        ggplotly(p)
    })
}

shinyApp(ui = ui, server = server)

#https://nanojgarcia.shinyapps.io/Covid19/