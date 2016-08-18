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

system("mkdir -p out")

data <- read.csv("data/answers.tsv", sep="\t")

pdf("out/answer_stats.pdf")

ea1 <- ggplot(data = data, aes(maxExactAgreement)) +
  geom_histogram(binwidth = 1, alpha = .3, position = "identity") +
  geom_vline(data = data, aes(xintercept = mean(maxExactAgreement)), linetype="dashed", size=1) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Max exact agreement") +
  labs(x = "Number in agreement", y = "Number of QA pairs")

ea2 <- ggplot(data = data, aes(maxExactAgreement)) +
  stat_ecdf() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1)) +
  ggtitle("Max exact agreement") +
  labs(x = "Number in agreement", y = "Proportion of QA pairs")

multiplot(ea1, ea2,
          layout = matrix(c(1, 2), ncol=2, byrow=TRUE))

la1 <- ggplot(data = data, aes(maxLooseAgreement)) +
  geom_histogram(binwidth = 1, alpha = .3, position = "identity") +
  geom_vline(data = data, aes(xintercept = mean(maxLooseAgreement)), linetype="dashed", size=1) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Max loose agreement") +
  labs(x = "Number in agreement", y = "Number of QA pairs")

la2 <- ggplot(data = data, aes(maxLooseAgreement)) +
  stat_ecdf() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1)) +
  ggtitle("Max loose agreement") +
  labs(x = "Number in agreement", y = "Proportion of QA pairs")

multiplot(la1, la2,
          layout = matrix(c(1, 2), ncol=2, byrow=TRUE))

ia1 <- ggplot(data = data, aes(numInvalidAnswers)) +
  geom_histogram(binwidth = 1, alpha = .3, position = "identity") +
  geom_vline(data = data, aes(xintercept = mean(numInvalidAnswers)), linetype="dashed", size=1) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Number of 'N/A' answers") +
  labs(x = "Number of 'N/A' answers", y = "Number of QA pairs")

ia2 <- ggplot(data = data, aes(numInvalidAnswers)) +
  stat_ecdf() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1)) +
  ggtitle("Number of 'N/A' answers") +
  labs(x = "Number of 'N/A' answers", y = "Proportion of QA pairs")

multiplot(ia1, ia2,
          layout = matrix(c(1, 2), ncol=2, byrow=TRUE))

dev.off()
