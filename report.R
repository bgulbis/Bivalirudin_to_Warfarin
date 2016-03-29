# report.R

source("library.R")

if (!exists("analyze.demographics")) analyze.demographics <- readRDS(analyze.demographics, paste(analysis.dir, "demographics.Rds", sep = "/"))

if (!exists("analyze.diagnosis")) analyze.diagnosis <- readRDS(analyze.diagnosis, paste(analysis.dir, "diagnosis.Rds", sep = "/"))

if (!exists("analyze.indications")) analyze.indications <- readRDS(analyze.indications, paste(analysis.dir, "indications.Rds", sep = "/"))

if (!exists("analyze.bleed")) analyze.bleed <- readRDS(analyze.bleed, paste(analysis.dir, "bleed.Rds", sep = "/"))

if (!exists("analyze.labs")) analyze.labs <- readRDS(analyze.labs, paste(analysis.dir, "labs.Rds", sep = "/"))

if (!exists("analyze.ptt.perc")) analyze.ptt.perc <- readRDS(analyze.ptt.perc, paste(analysis.dir, "ptt_percents.Rds", sep = "/"))

if (!exists("analyze.bival")) analyze.bival <- readRDS(analyze.bival, paste(analysis.dir, "bivalirudin.Rds", sep = "/"))

if (!exists("analyze.meds")) analyze.meds <- readRDS(analyze.meds, paste(analysis.dir, "meds.Rds", sep = "/"))

if (!exists("analyze.blood")) analyze.blood <- readRDS(analyze.blood, paste(analysis.dir, "blood.Rds", sep = "/"))

if (!exists("analyze.reversal")) analyze.reversal <- readRDS(analyze.reversal, paste(analysis.dir, "reversal.Rds", sep = "/"))

if (!exists("analyze.coags")) analyze.coags <- readRDS(analyze.coags, paste(analysis.dir, "coags.Rds", sep = "/"))

if (!exists("analyze.labs.serial")) analyze.labs.serial <- readRDS(analyze.labs.serial, paste(analysis.dir, "labs_serial.Rds", sep = "/"))


# create docx object with project title and authors
project <- "Evaluation of Bivalirudin's Effect on the INR"
authors <- "Andrea Fetea, Brian Gulbis, Andrea C. Hall"

mydoc <- result_docx(project, authors)

# add results tables
mydoc <- result_table(mydoc, analyze.demographics, "Demographics", group = NULL)
mydoc <- result_table(mydoc, analyze.diagnosis, "Past Medical History", group = NULL)
mydoc <- result_table(mydoc, analyze.indications, "Anticoagulation Indications", group = NULL)
mydoc <- result_table(mydoc, analyze.bleed, "Bleeding and Thrombosis", group = NULL)
mydoc <- result_table(mydoc, analyze.labs, "Baseline Labs", group = NULL)
mydoc <- result_table(mydoc, analyze.ptt.perc, "Percent PTT Values", group = NULL)
mydoc <- result_table(mydoc, analyze.bival, "Bivalirudin Infusion", group = NULL)
mydoc <- result_table(mydoc, analyze.meds, "Medications During Hospitalization", group = NULL)
mydoc <- result_table(mydoc, analyze.blood, "Blood Products", group = NULL)
mydoc <- result_table(mydoc, analyze.reversal, "Reversal Agents", group = NULL)
mydoc <- result_table(mydoc, analyze.coags, "Primary and Secondary Endpoints", group = NULL)

# create graphs
results <- select(analyze.coags, pie.id, inr.before, inr.txptt) %>%
    rename(inr1 = inr.before,
           inr2 = inr.txptt) %>%
    group_by(pie.id) %>%
    filter(!is.na(inr1),
           !is.na(inr2)) %>%
    gather(period, inr, inr1, inr2)

cols <- colorRampPalette(brewer.pal(8, "Set1"))
num.cols <- nrow(select(results, pie.id) %>% distinct)

graph <- ggplot(data = results, aes(x = period, y = inr, group = factor(pie.id))) +
    geom_line(size = 1, aes(colour = pie.id)) +
    geom_point(size = 5, pch = 21, fill = "salmon", alpha = 0.5) +
    ggtitle("INR Value at Baseline and\nTime of First Therapeutic aPTT") +
    xlab(NULL) +
    ylab("INR") +
    scale_x_discrete(labels = c("Baseline", "First Therapeutic\naPTT"), expand = c(0.25, 0.25)) +
    scale_color_manual(values = cols(num.cols)) +
    theme_bw() +
    theme(legend.position = "none", plot.title = element_text(size = 20), 
          axis.title = element_text(size = 18), axis.text = element_text(size = 16))

mydoc <- result_plot(mydoc, graph, "Figure 1. Change in INR from Baseline to Time of First Therapeutic aPTT")

results <- select(analyze.coags, pie.id, inr.before.stop, inr.after.stop) %>%
    rename(inr1 = inr.before.stop,
           inr2 = inr.after.stop) %>%
    group_by(pie.id) %>%
    filter(!is.na(inr1),
           !is.na(inr2)) %>%
    gather(period, inr, inr1, inr2)

graph <- ggplot(data = results, aes(x = period, y = inr, group=factor(pie.id))) +
    geom_line(size = 1, aes(colour = pie.id)) +
    geom_point(size = 5, pch = 21, fill = "salmon", alpha = 0.5) +
    ggtitle("INR Value Prior to and\n4 to 8 hours After Bivalirudin Cessation") +
    xlab(NULL) +
    ylab("INR") +
    scale_x_discrete(labels = c("Prior to\nCessation", "After Cessation"), expand = c(0.25, 0.25)) +
    theme_bw() +
    theme(legend.position = "none", plot.title = element_text(size = 20), 
          axis.title = element_text(size = 18), axis.text = element_text(size = 16))

mydoc <- result_plot(mydoc, graph, "Figure 2. Change in INR from Last on Baseline to Between 4 and 8 hours after Bivalirudin Cessation")

results <- select(analyze.coags, pie.id, inr.before, inr.6hr) %>%
    rename(inr1 = inr.before,
           inr2 = inr.6hr) %>%
    group_by(pie.id) %>%
    filter(!is.na(inr1),
           !is.na(inr2)) %>%
    gather(period, inr, inr1, inr2)

cols <- colorRampPalette(brewer.pal(8, "Set1"))
num.cols <- nrow(select(results, pie.id) %>% distinct)

graph <- ggplot(data = results, aes(x = period, y = inr, group = factor(pie.id))) +
    geom_line(size = 1, aes(colour = pie.id)) +
    geom_point(size = 5, pch = 21, fill = "salmon", alpha = 0.5) +
    ggtitle("INR Value at Baseline and\n6-hours after Bivalirudin Initiation") +
    xlab(NULL) +
    ylab("INR") +
    scale_x_discrete(labels = c("Baseline", "6 Hours"), expand = c(0.25, 0.25)) +
    scale_color_manual(values = cols(num.cols)) +
    theme_bw() +
    theme(legend.position = "none", plot.title = element_text(size = 20), 
          axis.title = element_text(size = 18), axis.text = element_text(size = 16))

mydoc <- result_plot(mydoc, graph, "Figure 3. Change in INR from Baseline to 6-hours after Bivalirudin Initiation")

results <- select(analyze.coags, pie.id, inr.before, inr.4hr) %>%
    rename(inr1 = inr.before,
           inr2 = inr.4hr) %>%
    group_by(pie.id) %>%
    filter(!is.na(inr1),
           !is.na(inr2)) %>%
    gather(period, inr, inr1, inr2)

cols <- colorRampPalette(brewer.pal(8, "Set1"))
num.cols <- nrow(select(results, pie.id) %>% distinct)

graph <- ggplot(data = results, aes(x = period, y = inr, group = factor(pie.id))) +
    geom_line(size = 1, aes(colour = pie.id)) +
    geom_point(size = 5, pch = 21, fill = "salmon", alpha = 0.5) +
    ggtitle("INR Value at Baseline and\nafter 4 hours on Bivalirudin") +
    xlab(NULL) +
    ylab("INR") +
    scale_x_discrete(labels = c("Baseline", "4 Hours"), expand = c(0.25, 0.25)) +
    scale_color_manual(values = cols(num.cols)) +
    theme_bw() +
    theme(legend.position = "none", plot.title = element_text(size = 20), 
          axis.title = element_text(size = 18), axis.text = element_text(size = 16))

mydoc <- result_plot(mydoc, graph, "Figure 4. Change in INR from Baseline to after at least 4 hours on Bivalirudin")

graph <- analyze.labs.serial %>%
    filter(lab == "inr",
           first(lab.datetime) < bival.start,
           last(lab.datetime) > bival.start,
           lab.datetime <= bival.start + days(3),
           warf.start >= bival.start + days(1)) %>%
    mutate(bival.diff = as.numeric(difftime(lab.datetime, bival.start, units = "hours"))) %>%
    ggplot(aes(x = bival.diff, y = result, color = pie.id)) +
    geom_point() +
    geom_line() +
    geom_smooth(color = "black") +
    theme(legend.position = "none")

mydoc <- result_plot(mydoc, graph, "Figure 5. Change in INR values before and after bivalirudin initiation")

graph <- analyze.labs.serial %>%
    filter(lab == "inr",
           last(lab.datetime) > bival.stop,
           lab.datetime >= bival.stop - hours(48),
           lab.datetime <= bival.stop + days(3),
           warf.start >= bival.start + days(1)) %>%
    mutate(bival.diff = as.numeric(difftime(lab.datetime, bival.stop, units = "hours"))) %>%
    ggplot(aes(x = bival.diff, y = result, color = pie.id)) +
    geom_point() +
    geom_line() +
    geom_smooth(color = "black") +
    theme(legend.position = "none")

mydoc <- result_plot(mydoc, graph, "Figure 6. Change in INR values before and after bivalirudin cessation")

# add citation and write docx to Word
write_docx(mydoc, file = paste(analysis.dir, "results.docx", sep = "/"))
