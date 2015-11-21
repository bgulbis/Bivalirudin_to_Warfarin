## inclusion.R
##
## Identify those patients meeting inclusion criteria
##

source("library.R")

## join the files together, tidy the variables
## remove patients < 18 year old or if admit date is before 7/1/2012
pts.identified <- list.files("Screen", pattern="^identified", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              encounter.type = factor(Encounter.Type),
              age = as.numeric(Age..Years..Visit.),
              admit.date = mdy(Admit.Date)) %>%
    filter(age >= 18,
           admit.date >= mdy("7/1/2012"))
              # order.mnemonic = factor(Order.Catalog.Mnemonic),
              # order.mpp = factor(MPP..which.generated.order.))

## create list of id's to use for query 2
# edw.pie <- (str_c(pts.identified$pie.id, collapse=";"))

## split the patients up into groups of 100
edw.pie <- split(pts.identified$pie.id, ceiling(seq_along(pts.identified$pie.id)/50))
## combine the id's in each group into a string, separated by semi-colon
edw.pie <- lapply(edw.pie, str_c, collapse=";")

print(edw.pie)