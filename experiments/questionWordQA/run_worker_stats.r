require(ggplot2)
require(scales)
require(plyr)

system("mkdir -p out")

## data loading

data <- read.csv("data/assignments.tsv", sep="\t")
# convert times to minutes since beginning
startTime <- min(data$acceptTime)
data$submitTime <- (data$submitTime - startTime) / 60000.0
data$acceptTime <- (data$acceptTime - startTime) / 60000.0
data$timeTaken <- data$submitTime - data$acceptTime
data$timeTakenPerQAPair <- data$timeTaken / data$numQAPairs

## stat summaries

numAssignments <- dim(data)[1]
cat(sprintf("Assignments: %d\n", numAssignments))

hitIds <- unique(data[,c("hitId")])
numHITs <- length(hitIds)
cat(sprintf("HITs: %d\n", numHITs))

assignmentsPerHIT <- numAssignments / numHITs
cat(sprintf("Assignments per HIT: %.2f\n", assignmentsPerHIT))

workersAndHITTypes <- data[,c("workerId", "hitType")]
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

printStats(as.data.frame(table(workerIds))$Freq, "assignments per worker")

printStats(data$timeTaken, "HIT completion time")

printStats(data$timeTakenPerQAPair, "HIT completion time per QA pair")

## graphs

pdf("out/worker_stats.pdf")

## Assignments per worker relation
histData <- as.data.frame(table(workerIds))
ggplot(data=histData, aes(x = Freq)) +
  geom_histogram(binwidth = 1, alpha = .3, position = "identity") +
  geom_vline(xintercept=mean(histData$Freq), linetype="dashed", size=1) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Workers by number of assigments completed") +
  labs(x = "Number of assignments completed", y = "Number of workers")

ggplot(data=as.data.frame(table(workerIds)), aes(x = Freq)) +
  stat_ecdf() +
  ggtitle("Workers by number of assigments completed") +
  labs(x = "Number of assignments completed", y = "Proportion of workers")

## Time taken to complete assignment
ggplot(data = data, aes(timeTaken)) +
  geom_histogram(binwidth = 0.5, alpha = .3, position = "identity") +
  geom_vline(xintercept = mean(data$timeTaken), linetype="dashed", size=1) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Assignments by time taken to complete") +
  labs(x = "Time taken (minutes)", y = "Number of assignments")

ggplot(data=data, aes(data$timeTaken)) +
  stat_ecdf() +
  ggtitle("Assignments by time taken to complete") +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  labs(x = "Time taken (minutes)", y = "Proportion of assignments")

## Assignment progress over time
ggplot(data=data, aes(data$submitTime)) +
  stat_ecdf() +
  ggtitle("assignments by submission time") +
  labs(x = "submission time (minutes)", y = "proportion of assignments")

## Time to complete successive assignments
ggplot(data=data,
       aes(x = workerAssignmentNum, y = timeTaken, colour = workerId)) +
  geom_line() + 
  stat_summary_bin(fun.data = mean_se, colour = "black") + 
  guides(colour=FALSE) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Time taken by worker for successive assignments") +
  labs(x = "Number of assignment", y = "Time taken (minutes)")

ggplot(data=data,
       aes(x = workerAssignmentNum, y = timeTakenPerQAPair, colour = workerId)) +
  geom_line() + 
  stat_summary_bin(fun.data = mean_se, colour = "black") + 
  guides(colour=FALSE) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Time taken by worker for successive assignments (QA pair adjusted)") +
  labs(x = "Number of assignment", y = "Time taken per QA pair (minutes)")

# Worker timelines
filteredData <- subset(data, workerId != "A2LT6KC1X51FVW")
uptakeStats <- ddply(filteredData, .(workerId, assignmentId), summarise, min = min(acceptTime), max = max(submitTime))
ggplot(data = uptakeStats, aes(x = reorder(workerId, -min),
                        ymin = min, ymax = max,
                        lower = min, upper = max,
                        middle = max, # arbitrary
                        )) +
  geom_hline(data = filteredData, aes(yintercept = max(acceptTime)), linetype = "dashed") +
  theme(axis.ticks = element_blank()) +
  geom_boxplot(stat = "identity", position = "identity") +
  ggtitle("Worker participation timeframes") + 
  labs(y = "Time (minutes)", x = "Worker (missing outlier A2LT6KC1X51FVW)") +
  coord_flip()

dev.off()