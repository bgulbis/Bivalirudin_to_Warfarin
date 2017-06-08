## tidy.R
##
## Perform data tidying
##

source("02_exclude-patients.R")
library(MESS)

## set the units to use for reporting differences in time
units.diff <- "days"

## read in demographics data
raw.demographics <- list.files("data/raw", pattern="^demographics", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              fin = Formatted.Financial.Nbr,
              age = as.numeric(Age..Years..Visit.),
              sex = factor(Sex),
              race = factor(Race),
              los = as.numeric(LOS..Actual.),
              disposition = factor(Discharge.Disposition))

data.demograph <- inner_join(raw.demographics, pts.include, by="pie.id") %>%
    mutate(bival.duration = as.numeric(difftime(bival.stop, bival.start, units=units.diff)),
           bival.prior.warf = as.numeric(difftime(warf.start, bival.start, units=units.diff)),
           overlap = as.numeric(difftime(bival.stop, warf.start, units=units.diff)))

## read in height and weight data
raw.htwt <- list.files("data/raw", pattern="^ht_wt", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              htwt.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(Clinical.Event),
              result = as.numeric(Clinical.Event.Result),
              units = factor(Clinical.Event.Result.Units)) %>%
    filter(units == "cm" | units == "kg")

tmp.height <- raw.htwt %>%
    filter(pie.id %in% pts.include$pie.id, 
           event == "Height") %>%
    group_by(pie.id) %>%
    arrange(htwt.datetime) %>%
    summarize(height = first(result))

data.demograph <- inner_join(data.demograph, tmp.height, by="pie.id")

## find the closest weight prior to bival start
tmp.weight <- raw.htwt %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(event == "Weight",
           htwt.datetime <= bival.start) %>%
    group_by(pie.id) %>%
    arrange(htwt.datetime) %>%
    summarize(weight = last(result))

data.demograph <- inner_join(data.demograph, tmp.weight, by="pie.id")

## find warfarin indication and goal range
raw.warfarin <- list.files("data/raw", pattern="^warfarin", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(Clinical.Event),
              result = Clinical.Event.Result)

## use the last INR range as the result
## **to do** split into numeric values indicating lower and upper ends of range
## ideas: use stringr to split based on "-" or "to"; also look for "<" or ">"
tmp.inr.range <- raw.warfarin %>%
    filter(pie.id %in% pts.include$pie.id,
           event == "INR Range") %>%
    group_by(pie.id) %>%
    arrange(datetime) %>%
    summarize(inr.range = last(result))

data.demograph <- inner_join(data.demograph, tmp.inr.range, by="pie.id")

## use the last indication as the result    
## **to do** separate indications and categorize
## ideas: use tidyr to split column based on ","
tmp.warf.indict <- raw.warfarin %>%
    filter(pie.id %in% pts.include$pie.id,
           event == "Warfarin Indication") %>%
    group_by(pie.id) %>%
    arrange(datetime) %>%
    summarize(indication = last(result)) 

data.demograph <- inner_join(data.demograph, tmp.warf.indict, by="pie.id")

## look for medications given during hospitalization
ref.concom.meds <- read.csv("data/external/lookup_drug-classes.csv", colClasses="character") %>%
    mutate(type = factor(type),
           label = factor(label))

## get all meds in desired classes
tmp.med.class <- ref.concom.meds %>%
    filter(type == "class") %>%
    inner_join(ref.med.class, by=c("name"="med.class")) %>%
    filter(med != "bivalirudin",
           !str_detect(med, "aspirin"))

## join individual meds from classes with those single meds to create one list
tmp.med.drug <- ref.concom.meds %>%
    filter(type == "drug") %>%
    bind_rows(select(tmp.med.class, med, type, label) %>% rename(name = med))

## get patient who received any of the desired meds, group based on the label
## then spread into wide dataset
data.meds <- select(raw.meds, pie.id, med) %>%
    filter(pie.id %in% pts.include$pie.id) %>%
    inner_join(tmp.med.drug, by=c("med"="name")) %>%
    select(pie.id, label) %>%
    group_by(pie.id, label) %>%
    distinct %>%
    mutate(value = TRUE) %>%
    spread(label, value, fill=FALSE, drop=FALSE)

## look for pmh
ref.pmh.codes <- read.csv("data/external/lookup_pmh-codes.csv", colClasses="character") %>%
    mutate(disease.state = factor(disease.state),
           type = factor(type))

## find the ICD9 codes for the desired pmh by CCS code
tmp.ccs <- filter(ref.pmh.codes, type == "CCS") %>% 
    mutate(ccs.code = as.numeric(code)) %>%
    inner_join(ref.ccs.diag, by="ccs.code")

## ICD9 codes for non-CCS code pmh
tmp.icd9 <- filter(ref.pmh.codes, type == "ICD9") %>% 
    mutate(icd9.code = code) %>%
    inner_join(ref.ccs.diag, by="icd9.code")

## create one table with all ICD9 codes that should be excluded
tmp.pmh.icd9 <- bind_rows(tmp.ccs, tmp.icd9) %>%
    select(disease.state, icd9.code) %>%
    group_by(disease.state)

## create table of patients with the desired disease states
data.pmh <- raw.diagnosis %>%
    filter(pie.id %in% pts.include$pie.id,
           diag.type == "Final" | diag.type == "Billing") %>%
    inner_join(tmp.pmh.icd9, by=c("diag.code"="icd9.code")) %>%
    select(pie.id, disease.state) %>%
    group_by(pie.id, disease.state) %>%
    distinct %>%
    mutate(value = TRUE) %>%
    spread(disease.state, value, fill=FALSE, drop=FALSE) %>%
    full_join(select(pts.include, pie.id), by="pie.id") %>%
    mutate_each(funs(ifelse(is.na(.), FALSE, .))) 

## get home meds data
raw.home.meds <- list.files("data/raw", pattern="^home_meds", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              home.med = str_to_lower(Order.Catalog.Mnemonic))

## find all patients who were on warfarin at home
tmp.hm <- raw.home.meds %>%
    filter(str_detect(home.med, "warfarin")) %>%
    mutate(hmed = "warfarin") %>%
    select(pie.id, hmed) %>%
    group_by(pie.id, hmed) %>%
    distinct 

## add them to the list
data.home.meds <- tmp.hm

## repeat for other desired home meds
tmp.hm <- raw.home.meds %>%
    filter(str_detect(home.med, "rivaroxaban")) %>%
    mutate(hmed = "rivaroxaban") %>%
    select(pie.id, hmed) %>%
    group_by(pie.id, hmed) %>%
    distinct 

data.home.meds <- bind_rows(data.home.meds, tmp.hm)

tmp.hm <- raw.home.meds %>%
    filter(str_detect(home.med, "apixaban")) %>%
    mutate(hmed = "apixaban") %>%
    select(pie.id, hmed) %>%
    group_by(pie.id, hmed) %>%
    distinct 

data.home.meds <- bind_rows(data.home.meds, tmp.hm)

tmp.hm <- raw.home.meds %>%
    filter(str_detect(home.med, "dabigatran")) %>%
    mutate(hmed = "dabigatran") %>%
    select(pie.id, hmed) %>%
    group_by(pie.id, hmed) %>%
    distinct 

data.home.meds <- bind_rows(data.home.meds, tmp.hm)

tmp.hm <- raw.home.meds %>%
    filter(str_detect(home.med, "edoxaban")) %>%
    mutate(hmed = "edoxaban") %>%
    select(pie.id, hmed) %>%
    group_by(pie.id, hmed) %>%
    distinct 

tmp.levels <- c("warfarin", "rivaroxaban", "apixaban", "dabigatran", "edoxaban")

## add the last home med to the list; make into a factor with levels for each
## desired home med (in case there are meds with no patients on them)
## convert to wide data set; make sure all included patients are on the list
## change NA to FALSE
data.home.meds <- bind_rows(data.home.meds, tmp.hm) %>%
    group_by(pie.id) %>%
    mutate(hmed = factor(hmed, levels=tmp.levels),
           value = TRUE) %>%
    spread(hmed, value, fill=FALSE, drop=FALSE) %>%
    full_join(select(pts.include, pie.id), by="pie.id") %>%
    mutate_each(funs(ifelse(is.na(.), FALSE, .))) %>%
    filter(pie.id %in% pts.include$pie.id)

## check if procedure in 48 hours prior to bival initiation
raw.procedures <- list.files("data/raw", pattern="^procedures", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              proc.datetime = mdy_hms(Procedure.Date.and.Time),
              proc.code = ICD9.Procedure.Code)

## join procedures with included patients; make bival start time 12:00 AM for
## comparison to procedure dates
tmp.proc <- raw.procedures %>%
    inner_join(pts.include, by="pie.id") %>%
    mutate(bival.date = floor_date(bival.start, "day"),
           proc.days = as.numeric(difftime(proc.datetime, bival.date, units="days"))) %>%
    filter(proc.days >= -2 & proc.days <= 0) %>%
    select(pie.id) %>%
    distinct %>%
    mutate(proc.48hrs = TRUE)

data.demograph <- left_join(data.demograph, tmp.proc, by="pie.id") %>%
    mutate(proc.48hrs = ifelse(is.na(proc.48hrs), FALSE, proc.48hrs))

## function to use for checking whether a bleed or reversal was related to a procedure
check.procedure <- function(pid, date, time.back = 0, time.forward = 0) {
    
    ## check if there was an event that occured within the desired time frame
    compare.dates <- function(event.date, id) {
        x <- filter(tmp.proc, pie.id == id,
                    proc.datetime >= event.date - days(time.back),
                    proc.datetime <= event.date + days(time.forward))
        if(nrow(x) >= 1) TRUE else FALSE
    }
    
    lapply(seq_along(date), function(i) compare.dates(date[[i]], pid[[i]]))
}

## get the procedure descriptions
ref.ccs.proc <- read.csv("data/external/lookup_icd9-procedure-codes.csv", colClasses="character") %>%
    transmute(proc.code = ICD9.CODE.FRMT,
              proc.desc = ICD.9.CM.CODE.DESCRIPTION,
              proc.ccs.code = as.numeric(CCS.CATEGORY),
              proc.ccs.desc = CCS.CATEGORY.DESCRIPTION)

## find procuedres that occured during transition interval for use with 
## check.procedures function
tmp.proc <- raw.procedures %>%
    inner_join(pts.include, by="pie.id") %>%
    mutate(bival.start = floor_date(bival.start, "day"),
           bival.stop = floor_date(bival.stop, "day")) %>%
    filter(proc.datetime >= bival.start & proc.datetime <= bival.stop + days(5)) %>%
    select(pie.id, proc.datetime, proc.code) %>%
    left_join(ref.ccs.proc, by="proc.code")

## get bivalirudin infusion rates; summarize with time-weighted average
data.bival <- raw.meds %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(str_detect(med, "bivalirudin"),
           admin.datetime >= bival.start & admin.datetime <= bival.stop,
           rate.units != "") %>%
    group_by(pie.id) %>%
    mutate(time.hours = as.numeric(difftime(admin.datetime, first(admin.datetime), units="hours"))) %>%
    summarize(first.rate = first(rate),
              last.rate = last(rate),
              min.rate = min(rate),
              max.rate = max(rate),
              auc.rate = auc(time.hours, rate),
              bival.duration = as.numeric(difftime(first(bival.stop), first(bival.start), units=units.diff))) %>%
    mutate(time.wt.rate = auc.rate / (bival.duration*24))
           
## get baseline labs
tmp.labs <- c("scr", "ast", "alt", "t.bili")

## use the last lab prior to bival initiation; convert to wide data set
## ??? remove labs that were drawn well in advance of bival ??? 
data.labs.baseline <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab.datetime <= bival.start,
           lab %in% tmp.labs) %>%
    mutate(time.diff = as.numeric(difftime(lab.datetime, bival.start, units="hours")),
           lab = factor(lab, levels=tmp.labs)) %>%
    group_by(pie.id, lab) %>%
    arrange(lab.datetime) %>%
    summarize(result = last(result)) %>%
    spread(lab, result, drop=FALSE) %>%
    mutate_each(funs(as.numeric), scr:t.bili)

## calculate CrCl
tmp.crcl <- select(data.demograph, pie.id, age, sex, height, weight) %>%
    inner_join(data.labs.baseline, by="pie.id") %>%
    mutate(ibw = ifelse(sex == "Female", 45.5 + 0.91 * (height - 152.4), 50 + 0.81 * (height - 152.4)),
           wt = ifelse(weight <= ibw, weight, ibw),
           crcl = (140 - age) * wt / (scr * 72),
           crcl = ifelse(sex == "Female", crcl * 0.85, crcl)) %>%
    select(pie.id, crcl) 

## add crcl to baseline labs data
data.labs.baseline <- left_join(data.labs.baseline, tmp.crcl, by="pie.id") 

## serial lab measures: INR, PTT, Platlets, Hgb, Hct
## 48 hours prior to bival to 5 days after bival stop
tmp.labs <- c("hct", "hgb", "inr", "platelet", "ptt")

tmp.labs.serial <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab.datetime >= bival.start - days(2),
           lab.datetime <= bival.stop + days(5),
           lab %in% tmp.labs) %>%
    mutate(lab = factor(lab, levels=tmp.labs),
           result = as.numeric(result)) %>%
    group_by(pie.id, lab) %>%
    arrange(pie.id, lab.datetime) %>%
    mutate(time.diff = as.numeric(difftime(lab.datetime, first(lab.datetime), units="hours")))

data.labs.serial <- tmp.labs.serial %>%
    summarize(first = first(result),
              last = last(result),
              change = last(result) - first(result),
              change.prct = last(result) / first(result),
              min = min(result),
              min.prct = min(result) / first(result),
              max = max(result),
              max.prct = max(result) / first(result),
              auc = auc(time.diff, result),
              time.wt.avg = auc(time.diff, result) / last(time.diff))

## get HIT labs
raw.hit <- list.files("data/raw", pattern="^hit", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              lab.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              lab = factor(Clinical.Event),
              result = Clinical.Event.Result)

## get result of last ELISA
tmp.hit <- raw.hit %>%
    filter(pie.id %in% pts.include$pie.id,
           str_detect(lab, "ELISA")) %>%
    group_by(pie.id) %>%
    summarize(elisa = last(result))

data.labs.baseline <- full_join(data.labs.baseline, tmp.hit, by="pie.id")

## will need to manually review SRA and HIPA
tmp.hit <- raw.hit %>%
    filter(pie.id %in% pts.include$pie.id,
           !str_detect(lab, "ELISA")) 

data.manual.hit <- select(data.demograph, pie.id, fin) %>%
    inner_join(tmp.hit, by="pie.id")

data.manual.hit %>%
    select(-pie.id) %>%
    write_csv("data/external/manual_sra-patients.csv")

## reversal agents
tmp.reversal <- ref.concom.meds %>%
    filter(label == "reversal")

data.reversal <- raw.meds %>%
    inner_join(pts.include, by="pie.id") %>%
    inner_join(tmp.reversal, by=c("med"="name")) %>%
    filter(admin.datetime >= bival.start,
           admin.datetime <= bival.stop + days(5)) %>%
    mutate(reverse.date = floor_date(admin.datetime, "day"),
           procedure = check.procedure(pie.id, reverse.date, 1, 0)) %>%
    filter(procedure == FALSE) %>%
    select(pie.id, med) %>%
    group_by(pie.id, med) %>%
    distinct %>%
    mutate(value = TRUE) %>%
    full_join(pts.include, by="pie.id") %>%
    ungroup %>%
    mutate(med = ifelse(is.na(med), "phytonadione", med),
           value = ifelse(is.na(value), FALSE, value)) %>%
    mutate(med = factor(med, levels=tmp.reversal$name)) %>%
    select(-(bival.start:warf.start)) %>%
    spread(med, value, fill=FALSE, drop=FALSE) 

## blood products
raw.blood <- list.files("data/raw", pattern="^blood[^_]", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              blood.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(str_to_lower(Clinical.Event)),
              result = Clinical.Event.Result) 

# raw.blood.orders <- read.csv("Data/blood_orders.csv", colClasses="character") %>%
#     transmute(pie.id = PowerInsight.Encounter.Id,
#               order.datetime = mdy_hms(Order.Date..amp..Time),
#               order = factor(str_to_lower(Order.Catalog.Mnemonic))) %>%
#     mutate(type = "order")
# 
# ## combine orders and clinical events in one data frame
# tmp.blood <- raw.blood.orders %>%
#     rename(blood.datetime = order.datetime,
#            event = order) %>%
#     bind_rows(raw.blood) %>%
#     arrange(pie.id, blood.datetime) %>%
#     mutate(event = factor(event),
#            type = factor(type))

## evaluate string and determine which blood product was given
## event: vector of strings which contain the type of blood product being charted
assign.products <- function(event) {

    ## sub-function to evaluate each row
    get.prod <- function(x) {
        if(str_detect(x, "cryo")) {
            prod <- "cryo"
        } else if(str_detect(x, "ffp")) {
            prod <- "ffp"
        } else if(str_detect(x, "rbc")) {
            prod <- "prbc"
        } else if(str_detect(x, "platelet")) {
            prod <- "platelet"
        } else {
            prod <- "unknown"
        }
    }
    
    ## loop through each element of the vector; returns a new vector with a 
    ## string identifying the product type for each element in the original vector
    sapply(event, get.prod)
}

levels <- c("prbc", "ffp", "platelet", "cryo")
## eliminate anything not in the desired date range
data.blood <- raw.blood %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(blood.datetime >= bival.start,
           blood.datetime <= bival.stop + days(5)) %>%
    mutate(blood.date = floor_date(blood.datetime, "day"),
           procedure = check.procedure(pie.id, blood.date, 1, 1)) %>%
    filter(procedure == FALSE) %>%
    select(-result, -(bival.start:warf.start)) %>%
    group_by(pie.id) %>%
    mutate(prod = assign.products(event)) %>%
    filter(prod != "unknown") %>%
    ungroup %>%
    mutate(prod = factor(prod, levels=levels),
           value = TRUE) %>%
    distinct(pie.id, prod, value) %>%
    spread(prod, value, fill=FALSE, drop=FALSE) %>%
    full_join(pts.include, by="pie.id") %>%
    select(-(bival.start:warf.start)) %>%
    mutate_each(funs(ifelse(is.na(.), FALSE, .)))

## *** to consider ***
## if product or volume event exists, then transfusion is true
## if date of transfusion or transfused event exists, then need to figure out which
## product was transfused by looking for product order

## radiology orders
## look for any orders which occur after bival start which may require manual review
raw.rad <- list.files("data/raw", pattern="^radiology", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              rad.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(Clinical.Event)) 

tmp.rad <- raw.rad %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(rad.datetime >= bival.start + days(2),
           str_detect(event, "CT|Doppler")) %>%
    select(-(bival.start:warf.start)) 
    
data.manual.rad <- select(data.demograph, pie.id, fin) %>%
    inner_join(tmp.rad, by="pie.id")

data.manual.rad %>%
    select(-pie.id) %>%
    write_csv("data/external/manual_diagnostic-scans.csv")
