
res_temp <- readRDS(file = "generated-data/sim-iv-data-100.rds")

res <- do.call(rbind, lapply(res_temp, function(l) l[[1]]))
N <- dim(res)[1]

print(dim(res))

## Columns of res
##  1: Sensint bootstrap: usual, percentile
##  3: Sensint bootstrap: usual, basic
##  5: Sensint bootstrap: usual, bca
##  7: Sensint bootstrap: conservative, percentile
##  9: Sensint bootstrap: conservative, basic
## 11: Sensint bootstrap: conservative, bca
## 13: Sensint heuristic
## 15: Confint bootstrap: percentile
## 17: Confint bootstrap: basic
## 19: Confint bootstrap: bca
## 21: Confint
## 23: n_empty
## 24: PIR

names <- c("S - Boot - usual - perc",
           "S - Boot - usual - basic",
           "S - Boot - usual - bca",
           "S - Boot - conserv - perc",
           "S - Boot - conserv - basic",
           "S - Boot - conserv - bca",
           "S - heuristic",
           "C - Boot - perc",
           "C - Boot - basic",
           "C - Boot - bca",
           "C")


####################
## Coverage
###################

N <- dim(res)[1]

cat("Coverage:\n\n")
for (i in 1:11) {
  cat(paste(names[i], ":\n"))
  cov <- sum((res[, 2*i-1] <= 1) &
               (res[, 2*i] >= 1)) / N
  cat(paste("Coverage: ", cov, "\n\n"))
}

cat(paste("Probability of empty sets :",
          mean(res[, 23]) / N, "\n\n"))

cat(paste("Cases of empty sets: ",
          sum(res[,23]), "\n\n"))


############
## Length of Intervals
############

cat("\nLength of Intervals:\n\n")
for (i in 1:11) {
  cat(paste(names[i], ":\n"))
  
  lengths <- 1 - res[res[, 2*i-1] <= 1, 2*i-1]
  cat(paste("Mean: ", round(mean(lengths), digits = 3), "\n"))
  cat(paste("Median: ", round(median(lengths), digits = 3), "\n\n"))
}