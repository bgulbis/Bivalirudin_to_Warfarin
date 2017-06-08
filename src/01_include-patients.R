## inclusion.R
##
## Identify those patients meeting inclusion criteria
##

# source("library.R")
library(tidyverse)
library(lubridate)
library(edwr)

## join the files together, tidy the variables
## remove patients < 18 year old or if admit date is before 7/1/2012
pts.identified <- list.files("data/raw", pattern="^identified", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              encounter.type = factor(Encounter.Type),
              age = as.numeric(Age..Years..Visit.),
              admit.date = mdy(Admit.Date, tz = "US/Central")) %>%
    filter(age >= 18,
           admit.date >= mdy("7/1/2012"))
# order.mnemonic = factor(Order.Catalog.Mnemonic),
# order.mpp = factor(MPP..which.generated.order.))

edw.pie <- concat_encounters(pts.identified$pie.id)

## create list of id's to use for query 2
