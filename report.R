# report.R

source("library.R")

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
