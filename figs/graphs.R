##
## graphs.R
##
## produces images to be used in Midyear poster
##

source("library.R")
library(ReporteRs)

## unused options
# mydoc <- pptx(template="graph_template.pptx")
# mydoc = addTitle(mydoc, "Figure")

## slide spacing
# 36" wide; 0.5" margins
# 8.5" left & right text columns; 0.5" margin
# 17" results column; divide into 3 sub-columns, 5.5" each, 0.25" margin between

## create PowerPoint file
mydoc <- pptx()

## add new PowerPoint slide
mydoc <- addSlide(mydoc, slide.layout = "Blank")

results <- select(data.labs.coags, pie.id, inr.before, inr.txptt) %>%
    group_by(pie.id) %>%
    filter(!is.na(inr.before),
           !is.na(inr.txptt)) %>%
    gather(period, inr, inr.before, inr.txptt)

cols <- colorRampPalette(brewer.pal(8, "Set1"))
num.cols <- nrow(select(results, pie.id) %>% distinct)

graph <- ggplot(data=results, aes(x=period, y=inr, group=factor(pie.id))) +
    geom_line(size=1, aes(colour=pie.id)) +
    geom_point(size=5, pch=21, fill="salmon", alpha=0.5) +
    ggtitle("A. INR Value at Baseline and\nTime of First Therapeutic aPTT") +
    xlab(NULL) +
    ylab("INR") +
    scale_x_discrete(labels=c("Baseline", "First Therapeutic\naPTT"), expand=c(0.25,0.25)) +
    scale_color_manual(values=cols(num.cols)) +
    theme_bw() +
    theme(legend.position="none", plot.title=element_text(size=20), 
          axis.title=element_text(size=18), axis.text=element_text(size=16))
# graph

## save the graph to the PowerPoint slide
mydoc <- addPlot(mydoc, fun=print, x=graph, offx=1, offy=1, width=5.5, height=5*5.5/7, vector.graphic=TRUE, fontname="Calibri")

## add new PowerPoint slide
mydoc <- addSlide(mydoc, slide.layout = "Blank")

results <- select(data.labs.coags, pie.id, inr.before.stop, inr.after.stop) %>%
    group_by(pie.id) %>%
    filter(!is.na(inr.before.stop),
           !is.na(inr.after.stop)) %>%
    gather(period, inr, inr.before.stop, inr.after.stop)

graph <- ggplot(data=results, aes(x=period, y=inr, group=factor(pie.id))) +
    geom_line(size=1, aes(colour=pie.id)) +
    geom_point(size=5, pch=21, fill="salmon", alpha=0.5) +
    ggtitle("B. INR Value Prior to and\n4 to 8 hours After Bivalirudin Cessation") +
    xlab(NULL) +
    ylab("INR") +
    scale_x_discrete(labels=c("Prior to\nCessation", "After Cessation"), expand=c(0.25,0.25)) +
    theme_bw() +
    theme(legend.position="none", plot.title=element_text(size=20), 
          axis.title=element_text(size=18), axis.text=element_text(size=16))
# graph

## save the graph to the PowerPoint slide
mydoc <- addPlot(mydoc, fun=print, x=graph, offx=1, offy=1, width=5.5, height=5*5.5/7, vector.graphic=TRUE, fontname="Calibri")
## add new PowerPoint slide
mydoc <- addSlide(mydoc, slide.layout = "Blank")

results <- select(data.labs.coags, inr.change, inr.change.stop) %>%
    mutate(inr.change.stop = abs(inr.change.stop)) %>%
    gather(period, change) %>%
    filter(!is.na(change))

graph <- ggplot(data=results, aes(x=period, y=change)) +
    geom_boxplot() + 
    # ggtitle("Figure 4. Absolute INR change at\nbivalirudin initiation and cesation") +
    xlab(NULL) +
    ylab("Change in INR") +
    scale_x_discrete(labels=c("Initiation", "Cessation")) +
    theme_bw() +
    theme(legend.position="none", plot.title=element_text(size=20), 
          axis.title=element_text(size=18), axis.text=element_text(size=16))
# theme(legend.position="none")
graph

## save the graph to the PowerPoint slide
mydoc <- addPlot(mydoc, fun=print, x=graph, offx=1, offy=1, width=5.5, height=5*5.5/7, vector.graphic=TRUE, fontname="Calibri")

## table 1: age, male, weight, race, los, anticoag indications
## table 2: bival starting rate, avg rate, duration, duration prior warf, duration overlap

# mysummary <- function(x) {
#     if(is.numeric(x)) {
#         med <- median(x)
#         iqr <- quantile(x, probs=c(0.25, 0.75))
#         str_c(med, " (", round(iqr[1], 1), " - ", round(iqr[2], 1), ")")
#     } else {
# Summarize(data.demograph$race, percdigs=0, addtotal=FALSE)
#         "Factor variable"
#     }
# }
# 
# table1 <- select(data.demograph, age, sex, weight, race, los) %>%
#     summarise_each(funs(mysummary(.)))

## write the PowerPoint slides to file
writeDoc(mydoc, file = "figures.pptx")

