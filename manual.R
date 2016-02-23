# manual.R
# 
# read in manually collected data

source("library.R")

tmp.fins <- select(data.demograph, pie.id, fin)

man.rad <- read_data(manual.dir, "scans_review.csv") %>%
    mutate(rad.datetime = mdy_hms(rad.datetime),
           event = factor(event, levels = levels(tmp.rad$event)),
           manual = as.logical(manual)) %>%
    inner_join(tmp.fins, by = "fin") %>%
    select(pie.id, everything(), -fin)

man.sra <- read_data(manual.dir, "sra_review.csv") %>%
    mutate(lab.datetime = mdy_hms(lab.datetime),
           lab = factor(lab, levels = levels(tmp.hit$lab)),
           manual = as.logical(manual)) %>%
    inner_join(tmp.fins, by = "fin") %>%
    select(pie.id, everything(), -fin)

# scans ----
