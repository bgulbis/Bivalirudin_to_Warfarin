library(dplyr)
library(lubridate)
library(stringr)

warf <- read.csv("Warfarin.csv", colClasses="character") %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              date = mdy(Discharge.Date))

sample <- filter(warf, date >= mdy("1/1/2014"),
              date <= mdy("3/31/2014"))

pie <- str_c(sample$pie.id, collapse=";")

sample2 <- filter(warf, date >= mdy("1/1/2013"),
                 date <= mdy("3/31/2013"))

pie2 <- str_c(sample2$pie.id, collapse=";")


bival <- read.csv("Bivalirudin 2014.csv", colClasses="character") %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              date.time = mdy_hms(Clinical.Event.End.Date.Time),
              nurse.unit = factor(Nurse.Unit.of.Clinical.Event))
              
argat <- read.csv("Argatroban 2014.csv", colClasses="character") %>% 
    transmute(pie.id = PowerInsight.Encounter.Id,
              date.time = mdy_hms(Clinical.Event.End.Date.Time),
              nurse.unit = factor(Nurse.Unit.of.Clinical.Event))

bival.pts <- group_by(bival, pie.id) %>%
    summarize(num = n())

argat.pts <- group_by(argat, pie.id) %>%
    summarize(num = n())

bival2 <- read.csv("Bivalirudin 2013.csv", colClasses="character") %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              date.time = mdy_hms(Clinical.Event.End.Date.Time),
              nurse.unit = factor(Nurse.Unit.of.Clinical.Event))

argat2 <- read.csv("Argatroban 2013.csv", colClasses="character") %>% 
    transmute(pie.id = PowerInsight.Encounter.Id,
              date.time = mdy_hms(Clinical.Event.End.Date.Time),
              nurse.unit = factor(Nurse.Unit.of.Clinical.Event))

bival.pts2 <- group_by(bival2, pie.id) %>%
    summarize(num = n())

argat.pts2 <- group_by(argat2, pie.id) %>%
    summarize(num = n())