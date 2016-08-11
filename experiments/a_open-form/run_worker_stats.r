require(ggplot2)
require(scales)
require(plyr)

system("mkdir -p out")

data <- read.csv("data/assignments.tsv", sep="\t")
# convert times to minutes since beginning
startTime <- min(data$acceptTime)
data$submitTime <- (data$submitTime - startTime) / 60000.0
data$acceptTime <- (data$acceptTime - startTime) / 60000.0

levels(data$hitType) <- c(levels(data$hitType), "med/4", "long/6")
data$hitType[data$hitType == "3NWM3X0LA7LBIGM2QP1Y4BFHQN9XPC"] <- "med/4"
data$hitType[data$hitType == "3JH21YRKZ6B7ZFDTS077Y16U7HJ0JL"] <- "long/6"
data <- droplevels(data)
dataMed4 <- subset(data, hitType == "med/4")
dataLong6 <- subset(data, hitType == "long/6")
dataBothTypes <- subset(data, workerId %in% dataLong6$workerId & workerId %in% dataMed4$workerId)

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
hitTypes <- unique(data$hitType)

numUniqueWorkers <- length(uniqueWorkerIds)
cat(sprintf("Number of workers: %d\n", numUniqueWorkers))
numUniqueMed4Workers <- length(unique(subset(data, hitType == "med/4")$workerId))
cat(sprintf("Number of workers for med/4: %d\n", numUniqueMed4Workers))
numUniqueLong6Workers <- length(unique(subset(data, hitType == "long/6")$workerId))
cat(sprintf("Number of workers for long/6: %d\n", numUniqueLong6Workers))

printStats <- function(vec, label) {
  cat(sprintf("Mean %s: %.2f\n", label, mean(vec)))
  cat(sprintf("Stdev %s: %.2f\n", label, sd(vec)))
  cat(sprintf("Quantiles of %s:\n", label))
  print(quantile(vec, c(0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0)))
}

# TODO group by HIT type as well
workerAssignments <- as.data.frame(table(workerIds))
workerAssignmentCounts <- workerAssignments$Freq
printStats(workerAssignmentCounts, "assignments per worker")

## TODO group by HIT type
## completionTimes <- data[,c("hitType", "submitTime", "acceptTime")]
data$timeTaken <- data$submitTime - data$acceptTime
printStats(data$timeTaken, "HIT completion time")

pdf("out/worker_stats.pdf")

## Assignments per worker relation
means <- ddply(as.data.frame(table(workersAndHITTypes)), "hitType", summarise, grp.mean=mean(Freq))
ggplot(data=as.data.frame(table(workersAndHITTypes)), aes(x = Freq, colour = hitType, fill = hitType)) +
  geom_histogram(binwidth = 1, alpha = .3, position = "identity") +
  geom_vline(data = means, aes(xintercept = grp.mean, colour = hitType), linetype="dashed", size=1) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Workers by number of assigments completed") +
  labs(x = "Number of assignments completed", y = "Number of workers")

ggplot(data=as.data.frame(table(workersAndHITTypes)), aes(x = Freq, colour = hitType)) +
  stat_ecdf() +
  ggtitle("workers by number of assigments completed") +
  labs(x = "number of assignments completed", y = "proportion of workers")

## Time taken to complete assignment
means <- ddply(data, "hitType", summarise, grp.mean=mean(timeTaken))
ggplot(data = data, aes(timeTaken, colour = hitType, fill = hitType)) +
  geom_histogram(binwidth = 0.5, alpha = .3, position = "identity") +
  geom_vline(data = means, aes(xintercept = grp.mean, colour = hitType), linetype="dashed", size=1) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Assignments by time taken to complete") +
  labs(x = "time taken (minutes)", y = "Number of assignments")

ggplot(data=data, aes(data$timeTaken, colour = hitType)) +
  stat_ecdf() +
  ggtitle("Assignments by time taken to complete") +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  labs(x = "time taken (minutes)", y = "Proportion of assignments")

## Assignment progress over time
ggplot(data=data, aes(data$submitTime, colour = hitType)) +
  stat_ecdf() +
  ggtitle("assignments by submission time") +
  labs(x = "submission time (minutes)", y = "proportion of assignments")

## Time to complete successive assignments
ggplot(data=subset(data, hitType == "med/4"),
       aes(x = workerAssignmentNum, y = timeTaken, colour = workerId)) +
  geom_line() + 
  stat_summary_bin(fun.data = mean_se, colour = "black") + 
  guides(colour=FALSE) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Time taken by worker for successive assignments (med/4)") +
  labs(x = "Number of assignment", y = "Time taken (minutes)")

ggplot(data=subset(data, hitType == "long/6"),
       aes(x = workerAssignmentNum, y = timeTaken, colour = workerId)) +
  geom_line() + 
  stat_summary_bin(fun.data = mean_se, colour = "black") + 
  guides(colour=FALSE) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  ggtitle("Time taken by worker for successive assignments (long/6)") +
  labs(x = "Number of assignment", y = "Time taken (minutes)")

# Worker timelines
uptakeStats <- ddply(data, .(workerId, hitType, assignmentId), summarise, min = min(acceptTime), max = max(submitTime))
uptakeStatsBothTypes <- ddply(dataBothTypes, .(workerId, hitType, assignmentId), summarise, min = min(acceptTime), max = max(submitTime))
uptakeStatsMed4 <- ddply(dataMed4, .(workerId, assignmentId), summarise, min = min(acceptTime), max = max(submitTime))
uptakeStatsLong6 <- ddply(dataLong6, .(workerId, assignmentId), summarise, min = min(acceptTime), max = max(submitTime))

ggplot(data = uptakeStats, aes(x = reorder(workerId, -min), colour = hitType,
                        ymin = min, ymax = max,
                        lower = min, upper = max,
                        middle = max, # arbitrary
                        )) +
  geom_hline(data = dataLong6, aes(yintercept = max(acceptTime), colour = hitType), linetype = "dashed") +
  geom_hline(data = dataMed4, aes(yintercept = max(acceptTime), colour = hitType), linetype = "dashed") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  geom_boxplot(stat = "identity", position = "identity", aes(alpha = .1)) +
  guides(alpha = FALSE) +
  ggtitle("Worker participation timeframes") + 
  labs(y = "Time (minutes)", x = "Worker") +
  coord_flip()

ggplot(data = uptakeStatsMed4, aes(x = reorder(workerId, -min),
                                   ymin = min, ymax = max,
                                   lower = min, upper = max,
                                   middle = max, # arbitrary
                                   )) +
  geom_hline(data = dataMed4, aes(yintercept = max(acceptTime)), linetype = "dashed") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  geom_boxplot(stat = "identity", position = "identity", aes(alpha = .1)) +
  guides(alpha = FALSE) +
  ggtitle("Worker participation timeframes (med/4)") + 
  labs(y = "Time (minutes)", x = "Worker") +
  coord_flip()

ggplot(data = uptakeStatsLong6, aes(x = reorder(workerId, -min),
                                   ymin = min, ymax = max,
                                   lower = min, upper = max,
                                   middle = max, # arbitrary
                                   )) +
  geom_hline(data = dataLong6, aes(yintercept = max(acceptTime)), linetype = "dashed") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  geom_boxplot(stat = "identity", position = "identity", aes(alpha = .1)) +
  guides(alpha = FALSE) +
  ggtitle("Worker participation timeframes (long/6)") + 
  labs(y = "Time (minutes)", x = "Worker") +
  coord_flip()

ggplot(data = uptakeStatsBothTypes, aes(x = reorder(workerId, -min), colour = hitType,
                        ymin = min, ymax = max,
                        lower = min, upper = max,
                        middle = max, # arbitrary
                        )) +
  geom_hline(data = dataMed4, aes(yintercept = max(acceptTime), colour = hitType), linetype = "dashed") +
  geom_hline(data = dataLong6, aes(yintercept = max(acceptTime), colour = hitType), linetype = "dashed") +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  geom_boxplot(stat = "identity", position = "identity", aes(alpha = .1)) +
  guides(alpha = FALSE) +
  ggtitle("Worker participation timeframes (workers doing both HIT types)") + 
  labs(y = "Time (minutes)", x = "Worker") +
  coord_flip()

dev.off()
