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

# add result table for each continuous agent
# mydoc <- result_table2(mydoc, analyze.sedatives, "med", "Continuous Medications")

# add citation and write docx to Word
write_docx(mydoc, file = "Analysis/results.docx")
