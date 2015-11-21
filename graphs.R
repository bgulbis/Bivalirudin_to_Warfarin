##
## graphs.R
##
## produces images to be used in Midyear poster
##

source("library.R")

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
    ggtitle("INR value at baseline and time of\nfirst therapeutic aPTT") +
    xlab(NULL) +
    ylab("INR") +
    scale_x_discrete(labels=c("Baseline", "First Therapeutic\naPTT"), expand=c(0.25,0.25)) +
    scale_color_manual(values=cols(num.cols)) +
    theme_bw() +
    theme(legend.position="none", plot.title=element_text(size=20), 
          axis.title=element_text(size=18), axis.text=element_text(size=16))
graph

graph2ppt(graph, "export_test.pptx", font="Calibri", width=5, height=25/7)


results <- select(data.labs.coags, pie.id, inr.before.stop, inr.after.stop) %>%
    group_by(pie.id) %>%
    filter(!is.na(inr.before.stop),
           !is.na(inr.after.stop)) %>%
    gather(period, inr, inr.before.stop, inr.after.stop)

graph2 <- ggplot(data=results, aes(x=period, y=inr, group=factor(pie.id))) +
    geom_line(size=1, aes(colour=pie.id)) +
    geom_point(size=5, pch=21, fill="salmon", alpha=0.5) +
    ggtitle("INR value prior to and 4 to 8 hours\nafter bivalirudin cessation") +
    xlab(NULL) +
    ylab("INR") +
    scale_x_discrete(labels=c("Prior to Cessation", "After Cessation")) +
    theme_bw() +
    theme(legend.position="none", plot.title=element_text(size=20), 
          axis.title=element_text(size=18), axis.text=element_text(size=16))
graph2

graph2ppt(graph2, "export_test.pptx", font="Calibri", width=5, height=25/7, append=TRUE)
