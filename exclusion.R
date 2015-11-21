## exclusion.R
##
## Check patients for exclusion criteria
##

source("inclusion.R")
library(R.utils)
library(zoo)
# library(tools)
# library(dplyr)
# library(lubridate)
# library(stringr)

## exclusion criteria
## number of hours for warfarin and bivalirudin overlap
ec.overlap <- 72

## number of days prior to bivalirudin initiation with INR < 1.5
ec.inr <- 1

## function to check if a file is already gzipped and will zip if not
gzip_files <- function(x) {
    if(isGzipped(x) == FALSE) {
        gzip(x)
    }
}

## compress medication data files
list.files("Data", full.names=TRUE) %>%
    lapply(gzip_files)

## read in medication data and tidy variables
raw.meds <- list.files("Data", pattern="^medications", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              admin.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              med = str_to_lower(Clinical.Event),
              dose = as.numeric(Clinical.Event.Result),
              dose.units = factor(Clinical.Event.Result.Units),
              rate = as.numeric(Infusion.Rate),
              rate.units = factor(Infusion.Rate.Unit),
              route = factor(Route.of.Administration...Short))

## remove patients not receiving warfarin or receiving less than 3 doses
tmp.warf.start <- filter(raw.meds, str_detect(med, "warfarin")) %>%
    group_by(pie.id) %>%
    summarize(warf.start = min(admin.datetime),
              warf.doses = n()) %>% 
    filter(warf.doses >= 3)
    
## calculate transition interval

## find patients who received both bivalirudin and warfarin
tmp.bival <- filter(raw.meds, str_detect(med, "bivalirudin")) %>%
    inner_join(tmp.warf.start, by="pie.id") %>%
    group_by(pie.id) %>%
    mutate(warf.diff = as.numeric(difftime(admin.datetime, warf.start, units="hours"))) 

## limit to those with overlapping bivalirudin and warfarin
tmp.overlap <- mutate(tmp.bival, before = ifelse(warf.diff >= -24 & warf.diff <= 0, TRUE, FALSE),
                      after = ifelse(warf.diff > 0 & warf.diff <= 24, TRUE, FALSE)) %>%
    filter(before == TRUE | after == TRUE) %>%
    summarize(before = sum(before),
              after = sum(after)) %>%
    filter(before >= 1,
           after >=1) %>%
    select(pie.id) %>%
    distinct

## find bivalirudin start
## - first row of data for the patient
## - the previous data charted is > 24 hours
## - dose and rate
tmp.bival.start <- filter(tmp.bival, pie.id %in% tmp.overlap$pie.id,
                          admin.datetime <= warf.start) %>%
    group_by(pie.id) %>%
    mutate(time.prev = as.numeric(difftime(admin.datetime, lag(admin.datetime), units="hours")),
           zero.prev = ifelse(lag(dose) == 0 & lag(rate) == 0, TRUE, FALSE)) %>% 
    filter((dose > 0 | rate > 0) & (is.na(time.prev) | time.prev > 24)) %>%
    summarize(bival.start = max(admin.datetime))
## alternate: med == "bivalirudin"

## consider stopped if: 
## - it's the last row of data for the patient
## - the next data charted is > 24 hours
## - dose and rate are both 0 and the next data charted is > 12 hours or the next
## row is also 0 and data charted after that is > 12 hours or 3 consecutive rows of 0
tmp.bival.stop <- filter(tmp.bival, pie.id %in% tmp.overlap$pie.id,
                         admin.datetime >= warf.start) %>%
    group_by(pie.id) %>%
    mutate(time.next = as.numeric(difftime(lead(admin.datetime), admin.datetime, units="hours")),
           zero.next = ifelse(lead(dose) == 0 & lead(rate) == 0, TRUE, FALSE)) %>% 
    filter(is.na(time.next) | time.next > 24 | 
               (dose == 0 & rate == 0 & time.next > 12) | 
               (dose == 0 & rate == 0 & zero.next == TRUE & 
                    (lead(time.next > 12) | lead(zero.next == TRUE)))) %>%
    summarize(bival.stop = min(admin.datetime))

## check for inclusion criteria, bivalirudin and warfarin overlap for >= 72 hours
pts.eligible <- left_join(tmp.bival.start, tmp.bival.stop, by="pie.id") %>%
    inner_join(tmp.warf.start, by="pie.id") %>%
    mutate(duration = as.numeric(difftime(bival.stop, bival.start, units="hours")),
           overlap = as.numeric(difftime(bival.stop, warf.start, units="hours"))) %>%
    filter(overlap >= ec.overlap) 

## check for any alternate anticoagulants during transition interval
ref.med.class <- read.csv("Lookup/drug_categories.csv", colClasses = "character") %>%
    transmute(med.class = factor(Drug.Catalog),
              med = str_to_lower(Generic.Drug.Name))

tmp.anticoags <- filter(ref.med.class, med.class == "anticoagulants", 
                        med != "warfarin",
                        med != "bivalirudin")

excl.alt.anticoag <- filter(raw.meds, med %in% tmp.anticoags$med) %>%
    inner_join(pts.eligible, by="pie.id") %>%
    filter(admin.datetime >= bival.start,
           admin.datetime <= bival.stop) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.eligible, !(pie.id %in% excl.alt.anticoag$pie.id))

## check for vitamin K within 5 days prior to bival start
excl.vitk <- filter(raw.meds, med == "phytonadione") %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(admin.datetime >= bival.start - days(5),
           admin.datetime <= bival.start) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.vitk$pie.id))

## check for thrombolytics within 48 hours prior to bival start
tmp.tpa <- filter(ref.med.class, med.class == "thrombolytics")

excl.tpa <- filter(raw.meds, med %in% tmp.tpa$med) %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(admin.datetime >= bival.start,
           admin.datetime <= bival.stop) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.tpa$pie.id))

## check for major inducers of warfarin metabolism
tmp.barb <- c("barbiturate anticonvulsants", "barbiturates")
tmp.inducer <- c("carbamazepine", "dicloxacillin", "rifampin", "phenytoin")

tmp.ixns <- filter(ref.med.class, med.class %in% tmp.barb | med %in% tmp.inducer) %>%
    select(med) %>%
    distinct

excl.inducer <- filter(raw.meds, med %in% tmp.ixns$med) %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(admin.datetime >= bival.start,
           admin.datetime <= bival.stop) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.inducer$pie.id))

## labs which were included in query
levels.labs <- c("scr", "ast", "alt", "t.bili", "hct", "hgb", "inr", "platelet", 
                 "ptt", "ur.preg", "ser.preg")

## read in lab data
raw.labs <- list.files("Data", pattern="^labs", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              lab.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              lab = factor(Clinical.Event),
              result = Clinical.Event.Result) %>%
    mutate(result = str_replace(result, "&gt;", ""),
           result = str_replace(result, "&lt;", ""),
           lab = str_to_lower(lab),
           lab = str_replace(lab, "u preg", "ur.preg"),
           lab = str_replace(lab, "s preg", "ser.preg"),
           lab = str_replace(lab, "creatinine lvl", "scr"),
           lab = str_replace(lab, "bili total", "t.bili"),
           lab = factor(lab, levels=levels.labs))

## check for INR > 1.5 at bival start
excl.inr <- filter(raw.labs, lab == "inr") %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(result > 1.5,
           lab.datetime >= bival.start - days(ec.inr),
           lab.datetime <= bival.start) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.inr$pie.id))

## check for major bleeding (hgb drop) within 48 hours prior to bival start

## function to count how many rows to go back
## lab.dt: a vector of POSIXct values
countrows <- function(datetime) {
    ## loop through all the lab values, count up how many there are between 
    ## 48 hours ago and current date/time 
    sapply(datetime, function(curr.val)
        sum(ifelse(datetime >= curr.val - days(2) & datetime <= curr.val, TRUE, FALSE))
    )
}

## determine if the hgb dropped by >= 2g/dL in 48 hours
## uses rollapplyr from zoo package to check offset
excl.bleed <- filter(raw.labs, lab == "hgb") %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab.datetime <= bival.start) %>%
    group_by(pie.id) %>%
    arrange(lab.datetime) %>%
    mutate(rowsback = countrows(lab.datetime),
           runmax = rollapplyr(as.numeric(result), rowsback, max, fill=NA, partial=TRUE),
           drop = as.numeric(result) - runmax,
           draw.diff = as.numeric(difftime(bival.start, lab.datetime, units="hours"))) %>%
    filter(drop <= -2,
           draw.diff <= 48) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.bleed$pie.id))

## check for pregnancy by lab test
excl.preg <- filter(raw.labs, lab == "ur.preg" | lab == "ser.preg",
                    result == "Positive",
                    pie.id %in% pts.include$pie.id) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.preg$pie.id))

## read in diagnosis codes
raw.diagnosis <- list.files("Data", pattern="^diagnosis", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              diag.code = ICD9.Diagnosis.Code,
              diag.type = factor(Diagnosis.Type),
              diag.primary = factor(Diagnosis.Code.Sequence))

## read in exclusion codes
ref.excl.codes <- read.csv("Lookup/exclusion_codes.csv", colClasses = "character")

## read in ICD9-CCS codes
ref.ccs.diag <- read.csv("Lookup/icd9_ccs_diagnosis.csv", colClasses="character") %>%
    transmute(ccs.code = as.numeric(CCS.CATEGORY),
              icd9.code = ICD9.CODE.FRMT) 

## find the ICD9 codes for the desired exclusions by CCS code
tmp.ccs <- filter(ref.excl.codes, type == "CCS") %>% 
    mutate(ccs.code = as.numeric(code)) %>%
    inner_join(ref.ccs.diag, by="ccs.code")

## ICD9 codes for non-CCS code exclusions
tmp.icd9 <- filter(ref.excl.codes, type=="ICD9") %>% 
    mutate(icd9.code = code) %>%
    inner_join(ref.ccs.diag, by="icd9.code")

## create one table with all ICD9 codes that should be excluded
tmp.excl.icd9 <- bind_rows(tmp.ccs, tmp.icd9) %>%
    select(disease.state, icd9.code) %>%
    group_by(disease.state)

## check for exclusions diagnosis: pregnant, hepatitis, cirrhosis, anti-phospholipid
## antibody, lupus anticoagulant
excl.diag <- filter(raw.diagnosis, diag.code %in% tmp.excl.icd9$icd9.code,
                    pie.id %in% pts.include$pie.id) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.diag$pie.id))

## remove patients without any diagnosis codes
excl.nodiag <- filter(pts.include, !(pie.id %in% raw.diagnosis$pie.id)) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.nodiag$pie.id))

## create list of all patients with an exclusion
# pts.exclude <- bind_rows(excl.alt.anticoag, excl.bleed, excl.diag, excl.inducer, 
#                          excl.inr, excl.preg, excl.tpa, excl.vitk, excl.nodiag) %>%
#     distinct


## remove any patient meeting an exclusion criteria and create data frame of study patients
# pts.include <- tmp.bival.interval %>%
#     filter(!(pie.id %in% pts.exclude$pie.id)) %>%
#     select(-(warf.doses:overlap))
pts.include <- select(pts.include, -(warf.doses:overlap))
