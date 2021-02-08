#Title: "ps2 - Data II"

#Author: "Fernando Garcia"

#Date: "2/5/2021"

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

rm(list = ls())
setwd("C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/data_skills_2_r_hw2")


#Download data for COVID cases, tests, deaths and vaccination and ZIP code
covid <- read.csv("COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv")

vaccine <- read.csv("COVID-19_Vaccinations_by_ZIP_Code.csv")

zip <- st_read("Boundaries - ZIP Codes.geojson") %>%
  st_transform(., crs = 4326)
  

#Cleaning the COVID data
covid <- covid %>%
  filter(ZIP.Code != "Unknown") %>%
  mutate(WeekEnd = mdy(Week.End)) %>%
  select(ZIP.Code, Cases...Weekly, Cases...Cumulative, Case.Rate...Weekly,
         Case.Rate...Cumulative, Deaths...Cumulative, Deaths...Cumulative,
         Death.Rate...Weekly, Death.Rate...Cumulative, WeekEnd)

#Change colnames
colnames(covid) = gsub("[...]", "", colnames(covid))

covid_zip <- left_join(zip, covid, c("zip" = "ZIPCode"))

#First Choropleths: Cumulative cases per 100,000 in Chicago by Zip Code
covid_zip %>%
  filter(WeekEnd == max(WeekEnd)) %>%
  ggplot() +
  geom_sf(aes(fill = CaseRateCumulative)) +
  scale_fill_distiller(palette = "Spectral") +
  labs(fill = "Rate") +
  theme_void() +
  ggtitle(label = "Cumulative COVID cases in Chicago",
          subtitle = "Rate per 100,000 by Zip Code (01-30-2021)")


#"Cleaning" Vaccine data
vaccine <- vaccine %>%
  dplyr::mutate(date = mdy(Date),
         doses_per_population = Total.Doses...Cumulative / Population) %>%
  dplyr::select(Zip.Code, date, doses_per_population, Total.Doses...Daily, 
         Total.Doses...Cumulative, Population)

colnames(vaccine) = gsub("[...]", "", colnames(vaccine))
  
vaccine_zip <- left_join(zip, vaccine, c("zip" = "ZipCode"))

#Second Choropleth: Cumulative dosis per population in Chicago by Zip Code
vaccine_zip %>%
  filter(date == max(date)) %>%
  ggplot() +
  geom_sf(aes(fill = doses_per_population)) +
  scale_fill_distiller(palette = "Spectral") +
  labs(fill = "Rate") +
  theme_void() +
  ggtitle(label = "Cumulative vaccination in Chicago",
          subtitle = "Rate per population by Zip Code (02-05-2021)")

#The question I'm trying to answer with these clorophlets is: 
#are the regions more affected by COVID-19 the first to be vaccinated?
#The answer suggested by these plots is no. Vaccination appears to be concentrated 
#in the Loop, and the North Side, while the most affected areas are the South West
#and the West. Nevertheless, we don't know what actually explains these patterns.
