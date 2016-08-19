require(ggplot2)
require(scales)
require(plyr)
library(grid)

## From http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## from http://stackoverflow.com/questions/14255533/pretty-ticks-for-log-normal-scale-using-ggplot2-dynamic-not-manual
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

system("mkdir -p out")

dimsToKeep <- c("hitId", "hitType", "questionOverlapCount", "questionOverlapProportion", "questionOverlapPerQA",
                "answerOverlapCount", "answerOverlapProportion", "answerOverlapPerQA",
                "coveredLabelProportion", "someWordCoveredLabelProportion", "allWordsCoveredLabelProportion"
                )

assignmentData <- read.csv("data/assignments.tsv", sep="\t")
assignmentData <- assignmentData[dimsToKeep]
assignmentData$type <- "Assignments"

hitData <- read.csv("data/hits.tsv", sep="\t")
hitData <- hitData[dimsToKeep]
hitData$type <- "HITs"

data <- rbind(assignmentData, hitData)

levels(data$hitType) <- c(levels(data$hitType), "med/4", "long/6")
data$hitType[data$hitType == "3NWM3X0LA7LBIGM2QP1Y4BFHQN9XPC"] <- "med/4"
data$hitType[data$hitType == "3JH21YRKZ6B7ZFDTS077Y16U7HJ0JL"] <- "long/6"
data <- droplevels(data)

assignmentData <- subset(data, type == "Assignments")
hitData <- subset(data, type == "HITs")
dataMed4 <- subset(data, hitType == "med/4")
dataLong6 <- subset(data, hitType == "long/6")

## dataMed4 <- subset(data, hitType == "med/4")
## dataLong6 <- subset(data, hitType == "long/6")
## dataBothTypes <- subset(data, workerId %in% dataLong6$workerId & workerId %in% dataMed4$workerId)

pdf("out/assignment_stats.pdf")

## Question overlap with sentences
make_hist <- function(dat, groupFactor, setLabel, statLabel, variable, binwidth = 1) {
  means <- ddply(dat, groupFactor, .fun = function(grp) { c(mean = mean(grp[,variable])) })
  ggplot(data = subset(dat, get(variable) < 500), aes(get(variable), colour = get(groupFactor), fill = get(groupFactor))) +
    geom_histogram(binwidth = binwidth, alpha = .3, position = "identity") +
    geom_vline(data = means, aes(xintercept = mean, colour = get(groupFactor)), linetype="dashed", size=1) +
    scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
    theme(legend.title = element_blank(), text = element_text(size = 5)) +
    ggtitle(paste(setLabel, "by", statLabel, sep = " ")) +
    labs(x = statLabel, y = paste("Number of ",  setLabel, sep = " "))
}

make_ecdf <- function(dat, groupFactor, setLabel, statLabel, variable) {
  ggplot(data = subset(dat, get(variable) < 500), aes(get(variable), colour = get(groupFactor))) +
    stat_ecdf() +
    ggtitle(paste(setLabel, "by", statLabel, sep = " ")) +
    scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
    theme(legend.title = element_blank(), text = element_text(size = 5)) +
    labs(x = statLabel, y = paste("Proportion of", setLabel, sep = " "))
}

## make all the graphs
make_graphs_for_stat <- function(variable, statLabel, binwidth = 1) {

  h1 <- (make_hist(assignmentData,
            groupFactor = "hitType",
            setLabel = "Assignments",
            statLabel = statLabel,
            variable = variable,
            binwidth = binwidth))

  e1 <- (make_ecdf(assignmentData,
            groupFactor = "hitType",
            setLabel = "Assignments",
            statLabel = statLabel,
            variable = variable))

#### by HIT aggregate data instead

  h2 <- (make_hist(hitData,
            groupFactor = "hitType",
            setLabel = "HITs",
            statLabel = statLabel,
            variable = variable,
            binwidth = binwidth))

  e2 <- (make_ecdf(hitData,
            groupFactor = "hitType",
            setLabel = "HITs",
            statLabel = statLabel,
            variable = variable))

#### HITs v Assignments

  h3 <- (make_hist(dataMed4,
            groupFactor = "type",
            setLabel = "Assignments / HITs (med/4)",
            statLabel = statLabel,
            variable = variable,
            binwidth = binwidth))

  e3 <- (make_ecdf(dataMed4,
            groupFactor = "type",
            setLabel = "Assignments / HITs (med/4)",
            statLabel = statLabel,
            variable = variable))

  h4 <- (make_hist(dataLong6,
            groupFactor = "type",
            setLabel = "Assignments / HITs (long/6)",
            statLabel = statLabel,
            variable = variable,
            binwidth = binwidth))

  e4 <- (make_ecdf(dataLong6,
            groupFactor = "type",
            setLabel = "Assignments / HITs (long/6)",
            statLabel = statLabel,
            variable = variable))

  multiplot(h1, e1, h2, e2, h3, e3, h4, e4,
            layout = matrix(c(1, 2, 3, 4, 5, 6, 7, 8), ncol=2, byrow=TRUE))
}

## make_graphs_for_stat("questionOverlapCount", "Number of sentence words covered by questions")
make_graphs_for_stat("questionOverlapProportion", "Proportion of sentence words covered by questions",
                     binwidth = .05)
make_graphs_for_stat("questionOverlapPerQA", "Number unique sentence words covered per question",
                     binwidth = .25)

## make_graphs_for_stat("answerOverlapCount", "Number of sentence words covered by answers")
make_graphs_for_stat("answerOverlapProportion", "Proportion of sentence words covered by answers",
                     binwidth = .05)
make_graphs_for_stat("answerOverlapPerQA", "Number unique sentence words covered per answer",
                     binwidth = .25)

## make_graphs_for_stat("coveredLabelProportion", "Aggregate proportion of SRL dep arg spans coverage",
##                      binwidth = .05)
make_graphs_for_stat("someWordCoveredLabelProportion", "Proportion of SRL dep arg spans with some word covered",
                     binwidth = .05)
## make_graphs_for_stat("allWordsCoveredLabelProportion", "Proportion of SRL dep arg spans with all words covered",
##                      binwidth = .05)


dev.off()
