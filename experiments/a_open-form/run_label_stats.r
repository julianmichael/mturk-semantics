require(ggplot2)
require(scales)
require(plyr)
library(grid)

system("mkdir -p out")

aggCoverageData <- read.csv("data/label-agg-coverage.tsv", sep = "\t")
aggCoverageData <- within(aggCoverageData, Label <- factor(Label, levels = names(sort(table(Label), decreasing = TRUE))))
aggCoverageTable = table(aggCoverageData)

someWordCoverageData <- read.csv("data/label-some-word-coverage.tsv", sep = "\t")
someWordCoverageData <- within(someWordCoverageData, Label <- factor(Label, levels = names(sort(table(Label), decreasing = TRUE))))

allWordsCoverageData <- read.csv("data/label-all-words-coverage.tsv", sep = "\t")
allWordsCoverageData <- within(allWordsCoverageData, Label <- factor(Label, levels = names(sort(table(Label), decreasing = TRUE))))

pdf("out/label-stats.pdf")

## ggplot(data = aggCoverageData, aes(x = Label, colour = IsCovered, fill = IsCovered, label = (aggCoverageTable["Covered", Label]/ (aggCoverageTable["Covered", Label] + aggCoverageTable["Uncovered", Label])))) +
##   geom_bar() +
##   coord_flip() +
##   ggtitle("Aggregate coverage of argument span words, by dep label")

ggplot(data = someWordCoverageData, aes(x = Label, colour = IsCovered, fill = IsCovered)) +
  geom_bar() +
  coord_flip() +
  ggtitle("Coverage of some argument span word, by dep label")

## ggplot(data = allWordsCoverageData, aes(x = Label, colour = IsCovered, fill = IsCovered)) +
##   geom_bar() +
##   coord_flip() +
##   ggtitle("Coverage of all argument span words, by dep label")

arcMatchingData <- read.csv("data/arc-matches.tsv", sep = "\t")
arcMatchingData <- within(arcMatchingData, QuestionLabel <- factor(QuestionLabel, levels = names(sort(table(QuestionLabel), decreasing = TRUE))))
arcMatchingTable <- table(arcMatchingData)
questionLabelSums <- colSums(arcMatchingTable)

confusion = as.data.frame(table(arcMatchingData))

ggplot(confusion, aes(x = QuestionLabel, y = DepLabel, fill = Freq)) +
  geom_tile() +
  scale_x_discrete(name = "First Question Word") +
  scale_y_discrete(name = "PropBank Dependency Label") +
  scale_fill_gradient(trans = "log", breaks = c(1, 2, 5, 10, 25, 50, 100)) +
  labs(fill = "Frequency") +
  theme(axis.text.x = element_text(angle=45, hjust = 1, margin = margin(t = 2))) +
  ggtitle("Propbank dep label and first word of question for QAs covering some arg word") +
  coord_flip()

dev.off()
