##
## graphs.R
##
## produces images to be used in Midyear poster
##

source("library.R")

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
# mydoc <- addSlide(mydoc, slide.layout = "Blank")

# library(DiagrammeR)
# library(magrittr)
# 
# graph <-
#     create_graph() %>%
#     set_graph_name("Boxes and Circles") %>%
#     set_graph_time() %>%
#     set_global_graph_attr("graph", "overlap", "true") %>%
#     set_global_graph_attr("node", "shape", "box") %>%
#     set_global_graph_attr("node", "fontname", "Helvetica") %>%
#     set_global_graph_attr("node", "color", "blue") %>%
#     set_global_graph_attr("edge", "color", "gray") %>%
#     add_node_df(create_nodes(c("A", "B", "C", "D", "E", "F"))) %>%
#     set_node_attr("F", "color", "black") %>%
#     add_node_df(create_nodes(1:8)) %>%
#     select_nodes(nodes = 1:8) %>%
#     set_node_attr_with_selection("shape", "circle") %>%
#     set_node_attr_with_selection("fixedsize", "true") %>%
#     set_node_attr_with_selection("width", 0.9) %>%
#     clear_selection() %>%
#     add_edge_df(create_edges(c("A", "B", "B", "B",
#                                "C", "1", "E", "2",
#                                "1", "1", "E", "4",
#                                "5", "6", "3"),
#                              c("1", "2", "3", "4",
#                                "A", "D", "A", "4",
#                                "5", "F", "6", "6",
#                                "7", "7", "8"))) %>%
#     set_edge_attr("B", "3", "color", "red") %>%
#     set_edge_attr("C", "A", "color", "green") %>%
#     set_edge_attr("3", "8", "color", "blue")
# 
# render_graph(graph)
# diag <- grViz("
# digraph {
#       
#       # graph attributes
#       graph [overlap = true]
#       
#       # node attributes
#       node [shape = box,
#       fontname = Helvetica,
#       color = blue]
#       
#       # edge attributes
#       edge [color = gray]
#       
#       # node statements
#       A; B; C; D; E
#       F [color = black]
#       
#       # node attributes
#       node [shape = circle,
#       fixedsize = true,
#       width = 0.9]
#       
#       # node statements
#       1; 2; 3; 4; 5; 6; 7; 8
#       
#       # edge statements
#       A->1; B->2                   // gray
#       B->3 [color = red]           // red
#       B->4                         // gray
#       C->A [color = green]         // green
#       1->D; E->A; 2->4; 1->5; 1->F // gray
#       E->6; 4->6; 5->7; 6->7       // gray
#       3->8 [color = blue]          // blue
#       }
#       ")

## save the graph to the PowerPoint slide
# mydoc <- addPlot(mydoc, fun=print, x=diag, vector.graphic=TRUE, fontname="Calibri")

## use diagram to create flow chart
# library(diagram)
# par(mar = c(1, 1, 1, 1))
# openplotmat()
# elpos <- coordinates (c(1, 1, 2, 4))
# fromto <- matrix(ncol = 2, byrow = TRUE, data = c(1, 2, 2, 3, 2, 4, 4, 7, 4, 8))
# nr <- nrow(fromto)
# arrpos <- matrix(ncol = 2, nrow = nr)
# for (i in 1:nr) {
#     arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ], 
#                                   from = elpos[fromto[i, 1], ], 
#                                   lwd = 2, arr.pos = 0.6, arr.length = 0.5)
# }
# 
# flow <- textellipse(elpos[1,], 0.1, lab = "start", box.col = "green", 
#             shadow.col = "darkgreen", shadow.size = 0.005, cex = 1.5)
# 
# textrect (elpos[2,], 0.15, 0.05,lab = "found term?", box.col = "blue", 
#           shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
# 
# textrect (elpos[4,], 0.15, 0.05,lab = "related?", box.col = "blue", 
#           shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
# 
# textellipse(elpos[3,], 0.1, 0.1, lab = c("other","term"), box.col = "orange",
#             shadow.col = "red", shadow.size = 0.005, cex = 1.5)
# 
# textellipse(elpos[3,], 0.1, 0.1, lab = c("other","term"), box.col = "orange",
#             shadow.col = "red", shadow.size = 0.005, cex = 1.5)
# 
# textellipse(elpos[7,], 0.1, 0.1, lab = c("make","a link"),box.col = "orange", 
#             shadow.col = "red", shadow.size = 0.005, cex = 1.5)
# 
# textellipse(elpos[8,], 0.1, 0.1, lab = c("new","article"),box.col = "orange", 
#             shadow.col = "red", shadow.size = 0.005, cex = 1.5)
# #
# dd <- c(0.0, 0.025)
# text(arrpos[2, 1] + 0.05, arrpos[2, 2], "yes")
# text(arrpos[3, 1] - 0.05, arrpos[3, 2], "no")
# text(arrpos[4, 1] + 0.05, arrpos[4, 2] + 0.05, "yes")
# text(arrpos[5, 1] - 0.05, arrpos[5, 2] + 0.05, "no")
              
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

