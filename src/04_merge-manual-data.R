# manual.R
# 
# read in manually collected data

source("03_tidy-data.R")

tmp.fins <- select(data.demograph, pie.id, fin)

man.rad <- read_csv("data/external/manual_diagnostic-scans_collected.csv", col_types = cols(.default = "c")) %>%
    mutate(rad.datetime = mdy_hms(rad.datetime),
           event = factor(event, levels = levels(tmp.rad$event)),
           manual = as.logical(manual)) %>%
    inner_join(tmp.fins, by = "fin") %>%
    select(pie.id, everything(), -fin)

man.sra <- read_csv("data/external/manual_sra-patients_collected.csv", col_types = cols(.default = "c")) %>%
    mutate(lab.datetime = mdy_hms(lab.datetime),
           lab = factor(lab, levels = levels(tmp.hit$lab)),
           manual = as.logical(manual)) %>%
    inner_join(tmp.fins, by = "fin") %>%
    select(pie.id, everything(), -fin)

data.sra <- man.sra %>%
    group_by(pie.id) %>%
    summarize(sra = sum(manual) > 1)
