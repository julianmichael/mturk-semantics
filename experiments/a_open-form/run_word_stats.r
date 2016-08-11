require(ggplot2)
require(scales)
require(plyr)

## from http://stackoverflow.com/questions/14255533/pretty-ticks-for-log-normal-scale-using-ggplot2-dynamic-not-manual
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

system("mkdir -p out")

firstQWord <- read.csv("data/first-qword.tsv", sep="\t")
newQWords <- read.csv("data/new-qwords.tsv", sep="\t")
newAWords <- read.csv("data/new-awords.tsv", sep="\t")

pdf("out/word_stats.pdf")

ggplot(subset(firstQWord, Count > 1), aes(x = reorder(Word, -Count), y = Count, label = Count)) +
  geom_bar(stat = "identity") +
  scale_y_log10(breaks = base_breaks()) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, margin = margin(t = 2))) +
  ggtitle("First question word") +
  labs(x = "Word at beginning of question", y = "Number of occurrences")

ggplot(subset(newQWords, Count > 12), aes(x = reorder(Word, -Count), y = Count, label = Count)) +
  geom_bar(stat = "identity") +
  scale_y_log10(breaks = base_breaks()) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, margin = margin(t = 2))) +
  ggtitle("Question words not appearing in the sentence") +
  labs(x = "Word", y = "Number of occurrences")

ggplot(subset(newAWords, Count > 2), aes(x = reorder(Word, -Count), y = Count, label = Count)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = pretty_breaks()) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, margin = margin(t = 2))) +
  ggtitle("Answer words not appearing in the sentence") +
  labs(x = "Word", y = "Number of occurrences")

dev.off()
