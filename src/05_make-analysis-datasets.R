##
## analyze.R
##
## perform data analysis
##

source("src/04_merge-manual-data.R")
library(purrrlyr)

## Analyze PTT and INR data 

## find the baseline PTT and INR before bivalirudin begins
tmp.coags.first <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab == "inr" | lab == "ptt",
           lab.datetime >= bival.start - days(2),
           lab.datetime < bival.start) %>%
    group_by(pie.id, lab) %>%
    arrange(pie.id, lab, lab.datetime) %>%
    dmap_at("result", as.numeric) %>%
    summarize(baseline = last(result)) %>%
    spread(lab, baseline) %>%
    rename(inr.before = inr,
           ptt.before = ptt)

## find the date/time of the first therapeutic PTT
tmp.ptt.tx <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab == "ptt",
           lab.datetime > bival.start,
           lab.datetime < warf.start,
           result >= 50,
           result <= 80) %>%
    group_by(pie.id) %>%
    arrange(pie.id, lab.datetime) %>%
    dmap_at("result", as.numeric) %>%
    summarize(ptt.tx = first(result),
              ptt.datetime = first(lab.datetime),
              bival.start = first(bival.start)) %>%
    mutate(ptt.tx.time = as.numeric(difftime(ptt.datetime, bival.start, units="hours")))

## find the INR at the first therapeutic PTT
tmp.inr <- raw.labs %>%
    inner_join(tmp.ptt.tx, by="pie.id") %>%
    filter(lab == "inr",
           lab.datetime >= ptt.datetime - hours(2),
           lab.datetime <= ptt.datetime + hours(2)) %>%
    group_by(pie.id) %>%
    mutate(inr.diff = as.numeric(abs(difftime(lab.datetime, ptt.datetime, units="hours")))) %>%
    arrange(pie.id, inr.diff) %>%
    dmap_at("result", as.numeric) %>%
    summarize(inr.txptt = first(result),
              ptt.tx = first(ptt.tx),
              inr.txptt.time = as.numeric(difftime(first(lab.datetime), first(bival.start), units="hours")))

## find the change in INR from baseline to first therapeutic PTT
data.labs.coags <- left_join(tmp.coags.first, tmp.inr, by="pie.id") %>%
    mutate(inr.change = inr.txptt - inr.before,
           inr.change.prct = inr.txptt / inr.before,
           ptt.change = ptt.tx - ptt.before,
           ptt.change.prct = ptt.tx / ptt.before)

## find the PTT and INR after at least 4 hours on bivalirudin, prior to warfarin starting
tmp.coags.4hr <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab == "inr" | lab == "ptt",
           lab.datetime >= bival.start + hours(4),
           lab.datetime < warf.start) %>%
    group_by(pie.id, lab) %>%
    mutate(lab.diff = as.numeric(abs(difftime(lab.datetime, bival.start, units="hours")))) %>%
    arrange(pie.id, lab.diff) %>%
    dmap_at("result", as.numeric) %>%
    summarize(lab.4hr = first(result)) %>%
    spread(lab, lab.4hr) %>%
    rename(inr.4hr = inr,
           ptt.4hr = ptt) 

## find the change in INR from baseline to after 4+ hours on bivalirudin
data.labs.coags <- left_join(data.labs.coags, tmp.coags.4hr, by="pie.id") %>%
    mutate(inr.change.4hr = inr.4hr - inr.before,
           inr.change.4hr.prct = inr.4hr / inr.before,
           ptt.change.4hr = ptt.4hr - ptt.before,
           ptt.change.4hr.prct = ptt.4hr / ptt.before)

## find the PTT and INR after 6 hours on bivalirudin, prior to warfarin starting
tmp.coags.6hr <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab == "inr" | lab == "ptt",
           lab.datetime >= bival.start + hours(4),
           lab.datetime <= bival.start + hours(8),
           lab.datetime < warf.start) %>%
    group_by(pie.id, lab) %>%
    mutate(lab.diff = as.numeric(abs(difftime(lab.datetime, bival.start + hours(6), units="hours")))) %>%
    arrange(pie.id, lab.diff) %>%
    dmap_at("result", as.numeric) %>%
    summarize(lab.6hr = first(result)) %>%
    spread(lab, lab.6hr) %>%
    rename(inr.6hr = inr,
           ptt.6hr = ptt) 

## find the change in INR from baseline to 6 hours
data.labs.coags <- left_join(data.labs.coags, tmp.coags.6hr, by="pie.id") %>%
    mutate(inr.change.6hr = inr.6hr - inr.before,
           inr.change.6hr.prct = inr.6hr / inr.before,
           ptt.change.6hr = ptt.6hr - ptt.before,
           ptt.change.6hr.prct = ptt.6hr / ptt.before)

## find the PTT and INR after 12 hours on bivalirudin, prior to warfarin starting
tmp.coags.12hr <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab == "inr" | lab == "ptt",
           lab.datetime >= bival.start + hours(10),
           lab.datetime <= bival.start + hours(14),
           lab.datetime < warf.start) %>%
    group_by(pie.id, lab) %>%
    mutate(lab.diff = as.numeric(abs(difftime(lab.datetime, bival.start + hours(12), units="hours")))) %>%
    arrange(pie.id, lab.diff) %>%
    dmap_at("result", as.numeric) %>%
    summarize(lab.12hr = first(result)) %>%
    spread(lab, lab.12hr) %>%
    rename(inr.12hr = inr,
           ptt.12hr = ptt) 
# %>%
#     filter(ptt.12hr >= 50,
#            ptt.12hr <= 80)

## find the change in INR from baseline to 12 hours
data.labs.coags <- left_join(data.labs.coags, tmp.coags.12hr, by="pie.id") %>%
    mutate(inr.change.12hr = inr.12hr - inr.before,
           inr.change.12hr.prct = inr.12hr / inr.before,
           ptt.change.12hr = ptt.12hr - ptt.before,
           ptt.change.12hr.prct = ptt.12hr / ptt.before)

## find the PTT and INR after 24 hours on bivalirudin
tmp.coags.24hr <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab == "inr" | lab == "ptt",
           lab.datetime < warf.start,
           lab.datetime >= bival.start + hours(22),
           lab.datetime <= bival.start + hours(26)) %>%
    group_by(pie.id, lab) %>%
    mutate(lab.diff = as.numeric(abs(difftime(lab.datetime, bival.start + hours(24), units="hours")))) %>%
    arrange(pie.id, lab.diff) %>%
    dmap_at("result", as.numeric) %>%
    summarize(lab.24hr = first(result)) %>%
    spread(lab, lab.24hr) %>%
    rename(inr.24hr = inr,
           ptt.24hr = ptt) 
# %>%
#     filter(ptt.24hr >= 50,
#            ptt.24hr <= 80)

## find the change in PTT and INR from baseline to 24 hours
data.labs.coags <- left_join(data.labs.coags, tmp.coags.24hr, by="pie.id") %>%
    mutate(inr.change.24hr = inr.24hr - inr.before,
           inr.change.24hr.prct = inr.24hr / inr.before,
           ptt.change.24hr = ptt.24hr - ptt.before,
           ptt.change.24hr.prct = ptt.24hr / ptt.before)

## find the PTT and INR just prior to stopping bivalirudin
tmp.coags.before.stop <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab == "inr" | lab == "ptt",
           lab.datetime >= bival.stop - hours(8),
           lab.datetime <= bival.stop) %>%
    group_by(pie.id, lab) %>%
    arrange(pie.id, lab, lab.datetime) %>%
    dmap_at("result", as.numeric) %>%
    summarize(lab.before.stop = last(result)) %>%
    spread(lab, lab.before.stop) %>%
    rename(inr.before.stop = inr,
           ptt.before.stop = ptt)

data.labs.coags <- full_join(data.labs.coags, tmp.coags.before.stop, by="pie.id") 

## find the PTT and INR 4 hours after bivalirudin stop
tmp.coags.after.stop <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab == "inr" | lab == "ptt",
           lab.datetime >= bival.stop + hours(4),
           lab.datetime <= bival.stop + hours(8)) %>%
    group_by(pie.id, lab) %>%
    arrange(pie.id, lab, lab.datetime) %>%
    dmap_at("result", as.numeric) %>%
    summarize(lab.after.stop = first(result)) %>%
    spread(lab, lab.after.stop) %>%
    rename(inr.after.stop = inr,
           ptt.after.stop = ptt) 
# %>%
#     filter(ptt.after.stop >= 50,
#            ptt.after.stop <= 80)

## find the change in PTT and INR from before bival stop to after
data.labs.coags <- left_join(data.labs.coags, tmp.coags.after.stop, by="pie.id") %>%
    mutate(inr.change.stop = inr.after.stop - inr.before.stop,
           inr.change.stop.prct = inr.after.stop / inr.before.stop,
           ptt.change.stop = ptt.after.stop - ptt.before.stop,
           ptt.change.stop.prct = ptt.after.stop / ptt.before.stop)


## determine if the hgb dropped by >= 2g/dL in 48 hours while on bivalirudin
## uses rollapplyr from zoo package to check offset
tmp.hgb <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab == "hgb",
           lab.datetime >= bival.start,
           lab.datetime <= bival.start + days(5)) %>%
    group_by(pie.id) %>%
    arrange(pie.id, lab.datetime) %>%
    dmap_at("result", as.numeric) %>%
    mutate(rowsback = countrows(lab.datetime),
           runmax = rollapplyr(result, rowsback, max, fill=NA, partial=TRUE),
           drop = as.numeric(result) - runmax,
           bival.drop.diff = as.numeric(difftime(lab.datetime, bival.start, units="hours")),
           warf.drop.diff = as.numeric(difftime(lab.datetime, warf.start, units="hours"))) %>%
    filter(drop <= -2) %>%
    mutate(drop.date = floor_date(lab.datetime, "day"),
           procedure = check.procedure(pie.id, drop.date, 1, 0)) %>%
    filter(procedure == FALSE) %>%
    summarize(hgb.drop = first(drop),
              hgb.drop.date = first(lab.datetime),
              bival.drop.diff = first(bival.drop.diff),
              warf.drop.diff = first(warf.drop.diff))

data.bleed.bival <- data.pmh %>%
    select(pie.id, contains("bleed")) %>%
    full_join(tmp.hgb, by="pie.id") %>%
    full_join(data.blood, by="pie.id") %>%
    mutate(major.bleed = ifelse(bleed.major == TRUE | bleed.minor == TRUE & 
                                    !is.na(hgb.drop), TRUE, FALSE),
           minor.bleed = ifelse(major.bleed == FALSE & bleed.minor == TRUE, TRUE, FALSE))

## warfarin indications
data.warf <- tmp.warf.indict %>%
    mutate(afib = str_detect(indication, "Atrial fibrillation"),
           dvt = str_detect(indication, "Deep vein thrombosis"),
           pe = str_detect(indication, "Pulmonary embolism"),
           valve = str_detect(indication, "Heart valve"),
           other = str_detect(indication, "Other"))

# new thrombosis ----
# look for positive scan while on bivalirudin
data.new.thrmb <- man.rad %>%
    inner_join(pts.include, by = "pie.id") %>%
    filter(rad.datetime >= bival.start,
           rad.datetime <= bival.stop + days(5)) %>%
    group_by(pie.id) %>%
    summarize(manual = ifelse(sum(manual, na.rm = TRUE) >= 1, TRUE, FALSE)) %>%
    full_join(select(pts.include, pie.id), by = "pie.id") %>%
    mutate(manual = ifelse(is.na(manual), FALSE, manual))

# determine percent time anticoagulated
tmp.labs.ptt <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab.datetime >= bival.start,
           lab.datetime <= bival.stop,
           lab == "ptt") %>%
    mutate(lab = factor(lab, levels=tmp.labs),
           result = as.numeric(result)) %>%
    group_by(pie.id, lab) %>%
    arrange(pie.id, lab.datetime) %>%
    mutate(run.time = as.numeric(difftime(lab.datetime, first(lab.datetime), units="hours")),
           duration = as.numeric(difftime(lead(lab.datetime), lab.datetime, units = "hours")),
           duration = ifelse(is.na(duration), 1, duration))

tmp.labs.inr <- raw.labs %>%
    inner_join(pts.include, by="pie.id") %>%
    filter(lab.datetime >= bival.stop,
           lab.datetime <= bival.stop + days(5),
           lab == "inr") %>%
    mutate(lab = factor(lab, levels=tmp.labs),
           result = as.numeric(result)) %>%
    group_by(pie.id, lab) %>%
    arrange(pie.id, lab.datetime) %>%
    mutate(run.time = as.numeric(difftime(lab.datetime, first(lab.datetime), units="hours")),
           duration = as.numeric(difftime(lead(lab.datetime), lab.datetime, units = "hours")),
           duration = ifelse(is.na(duration), 1, duration))

class(tmp.labs.ptt) <- append(class(tmp.labs.ptt), "labs", after = 0L)

tmp.ptt.goal <- calc_perctime(tmp.labs.ptt, list(~result >= 50, ~result <= 80)) %>%
    select(pie.id, perc.time.goal = perc.time)
tmp.ptt.low <- calc_perctime(tmp.labs.ptt, list(~result < 50)) %>%
    select(pie.id, perc.time.low = perc.time)
tmp.ptt.high <- calc_perctime(tmp.labs.ptt, list(~result > 80)) %>%
    select(pie.id, perc.time.high = perc.time)

data.labs.ptt.perc <- inner_join(tmp.ptt.goal, tmp.ptt.low, by = "pie.id") %>%
    inner_join(tmp.ptt.high, by = "pie.id") %>%
    group_by(pie.id) %>%
    mutate(total = sum(perc.time.goal, perc.time.low, perc.time.high))

# tmp.labs.perc.inr <- calc_perc_time(tmp.labs.inr, list(~result >= 2, ~result <= 3), meds = FALSE)

# restart bival within 24 hours after bival.stop?

# make analysis tables

analyze.demographics <- select(data.demograph, pie.id, age:disposition, bival.duration:weight, proc.48hrs)

analyze.diagnosis <- data.pmh

analyze.indications <- select(data.warf, -indication) %>%
    group_by(pie.id) %>%
    mutate(vte = dvt == TRUE | pe == TRUE,
           multiple = sum(afib, dvt, pe, valve, other) > 1)

analyze.bleed <- select(data.bleed.bival, pie.id, major.bleed, minor.bleed, hgb.drop, bival.drop.diff, warf.drop.diff) %>%
    left_join(data.new.thrmb, by = "pie.id") %>%
    rename(new.thrombosis = manual) %>%
    ungroup

analyze.labs <- left_join(data.labs.baseline, data.sra, by = "pie.id") %>% 
    ungroup()
analyze.ptt.perc <- data.labs.ptt.perc %>% ungroup
analyze.bival <- data.bival
analyze.meds <- data.meds %>% ungroup
analyze.prbc <- data.blood %>% ungroup
analyze.reversal <- data.reversal
analyze.coags <- data.labs.coags %>% ungroup
analyze.labs.serial <- tmp.labs.serial %>% ungroup

dirr::save_rds("data/final", "analyze")
