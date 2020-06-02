func_7a <- function(x) {
  (x-2)^2
}

func_7d <-function(count = 200, sampleSize = 40, sampleMax = 90) {
  mu <- vector(mode = "numeric", count)
  for (i in 1:count) {
    popSample <- sample(1:sampleMax, sampleSize, TRUE)
    median <- sum(popSample)/sampleSize
    mu[i] <- median
  }
  mu
}