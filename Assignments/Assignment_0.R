func_7a <- function(x) {
  (x-2)^2
}

func_7b <- function(){
  vase <- c(1:100)
  sample_vase <- sample(vase, 50, replace = TRUE)
  red <- sum(sample_vase <= 30)/50
  white <- sum(sample_vase > 30)/50
  data.frame(c(red, white), row.names = c("red","white"))
}

func_7c <- function(n = 10, mr = 30, mw = 70){
  total_balls <- mr + mw
  vase <- c(1:total_balls)
  sample_vase <- sample(vase, n, replace=TRUE)
  red <- sum(sample_vase <= mr)/n
  white <- sum(sample_vase > mr)/n
  data.frame(c(red, white), row.names = c("red","white"))
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