require(ggplot2)
require(scales)
require(plyr)

system("mkdir -p out")

data <- read.csv("data/workers.tsv", sep="\t")
# convert times to minutes since beginning
startTime <- min(data$acceptTime)
data$submitTime <- (data$submitTime - startTime) / 60000.0
data$acceptTime <- (data$acceptTime - startTime) / 60000.0

numAssignments <- dim(data)[1]
cat(sprintf("Assignments: %d\n", numAssignments))

hitIds <- unique(data[,c("hitId")])
numHITs <- length(hitIds)
cat(sprintf("HITs: %d\n", numHITs))

assignmentsPerHIT <- numAssignments / numHITs
cat(sprintf("Assignments per HIT: %.2f\n", assignmentsPerHIT))

workerIds <- data$workerId
uniqueWorkerIds <- unique(workerIds)

numUniqueWorkers <- length(uniqueWorkerIds)
cat(sprintf("Number of workers: %d\n", numUniqueWorkers))

printStats <- function(vec, label) {
  cat(sprintf("Mean %s: %.2f\n", label, mean(vec)))
  cat(sprintf("Stdev %s: %.2f\n", label, sd(vec)))
  cat(sprintf("Quantiles of %s:\n", label))
  print(quantile(vec, c(0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0)))
}

workerAssignments <- as.data.frame(table(workerIds))
workerAssignmentCounts <- workerAssignments$Freq
printStats(workerAssignmentCounts, "assignments per worker")

data$timeTaken <- data$submitTime - data$acceptTime
printStats(data$timeTaken, "HIT completion time")

pdf("out/worker_stats.pdf")

## Assignments per worker relation
ggplot(data = workerAssignments, aes(x = Freq)) +
  geom_histogram(binwidth = 1, alpha = .3, position = "identity") +
  geom_vline(data = workerAssignments, aes(xintercept = mean(Freq)), linetype="dashed", size=1) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Workers by number of assigments completed") +
  labs(x = "Number of assignments completed", y = "Number of workers")

ggplot(data = workerAssignments, aes(x = Freq)) +
  stat_ecdf() +
  ggtitle("workers by number of assigments completed") +
  labs(x = "number of assignments completed", y = "proportion of workers")

## Time taken to complete assignment
ggplot(data = data, aes(timeTaken)) +
  geom_histogram(binwidth = 0.1, alpha = .3, position = "identity") +
  geom_vline(data = data, aes(xintercept = mean(timeTaken)), linetype="dashed", size=1) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Assignments by time taken to complete") +
  labs(x = "time taken (minutes)", y = "Number of assignments")

ggplot(data=data, aes(timeTaken)) +
  stat_ecdf() +
  ggtitle("Assignments by time taken to complete") +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  labs(x = "time taken (minutes)", y = "Proportion of assignments")

## Assignment progress over time
ggplot(data=data, aes(submitTime)) +
  stat_ecdf() +
  ggtitle("Assignments by submission time") +
  labs(x = "Submission time (minutes)", y = "Proportion of assignments")

## Time to complete successive assignments
ggplot(data = data,
       aes(x = workerAssignmentNum, y = timeTaken, colour = workerId)) +
  geom_line() + 
  stat_summary_bin(fun.data = mean_se, colour = "black") + 
  guides(colour=FALSE) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Time taken by worker for successive assignments (med/4)") +
  labs(x = "Number of assignment", y = "Time taken (minutes)")

# Worker timelines
uptakeStats <- ddply(data, .(workerId, assignmentId), summarise, min = min(acceptTime), max = max(submitTime))
ggplot(data = uptakeStats, aes(x = reorder(workerId, -min),
                               ymin = min, ymax = max,
                               lower = min, upper = max,
                               middle = max, # arbitrary
                               )) +
  geom_hline(data = data, aes(yintercept = max(acceptTime)), linetype = "dashed") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  geom_boxplot(stat = "identity", position = "identity", aes(alpha = .1)) +
  guides(alpha = FALSE) +
  ggtitle("Worker participation timeframes") + 
  labs(y = "Time (minutes)", x = "Worker") +
  coord_flip()

dev.off()
