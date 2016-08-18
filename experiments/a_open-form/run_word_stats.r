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
firstQWord <- firstQWord[order(-firstQWord$Count),]
firstQPhrase <- read.csv("data/first-qphrase.tsv", sep="\t")
firstQPhrase <- firstQPhrase[order(-firstQPhrase$Count),]
firstQWordNew <- read.csv("data/first-qword-new.tsv", sep="\t")
firstQWordNew <- firstQWordNew[order(-firstQWordNew$Count),]
newQWords <- read.csv("data/new-qwords.tsv", sep="\t")
newAWords <- read.csv("data/new-awords.tsv", sep="\t")

pdf("out/word_stats.pdf")

## first question words not appearing in sentence

total = sum(firstQWordNew$Count)
firstQWordNew <- within(firstQWordNew, acc_proportion <- (cumsum(Count) / total))
ggplot(subset(firstQWordNew, Count > 1), aes(x = reorder(Word, -Count), y = Count, label = Count)) +
  geom_bar(stat = "identity") +
  scale_y_log10(breaks = base_breaks()) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, margin = margin(t = 2))) +
  ggtitle("First question word (if not in sentence)") +
  labs(x = "Word at beginning of question", y = "Number of occurrences")
ggplot(subset(firstQWordNew, Count > 1), aes(x = reorder(Word, acc_proportion), y = acc_proportion, group = 1)) +
  geom_step() +
  ## geom_line(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1, margin = margin(t = 2))) +
  ggtitle("First question word (Cumulative; if not in sentence)") +
  labs(x = "Word at beginning of question", y = "Number of occurrences")

total = sum(firstQPhrase$Count)
firstQPhrase <- within(firstQPhrase, acc_proportion <- (cumsum(Count) / total))
ggplot(subset(firstQPhrase, Count > 1), aes(x = reorder(Word, -Count), y = Count, label = Count)) +
  geom_bar(stat = "identity") +
  scale_y_log10(breaks = base_breaks()) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, margin = margin(t = 2))) +
  ggtitle("First question phrase") +
  labs(x = "Phrase at beginning of question", y = "Number of occurrences")
ggplot(subset(firstQPhrase, Count > 1), aes(x = reorder(Word, acc_proportion), y = acc_proportion, group = 1)) +
  geom_step() +
  ## geom_line(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1, margin = margin(t = 2))) +
  ggtitle("First question phrase (Cumulative)") +
  labs(x = "Phrase at beginning of question", y = "Number of occurrences")

ggplot(subset(newQWords, Count > 10), aes(x = reorder(Word, -Count), y = Count, label = Count)) +
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
