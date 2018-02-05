getMoments <- function(samples) {
  mean = sum(samples) / length(samples)
  sq_samples = sapply(samples, function(x) { return (x * x);})
  variance = sum(sq_samples) / length(sq_samples) - mean * mean
  return (list("mean" = mean, "variance" = variance))
}

printMoments <- function(moments, thMean, thVar, title) {
  print (title)
  print (paste("media calculata", moments$mean))
  print (paste("media teoretica:", thMean))
  print (paste("variatia calculate:", moments$variance))
  print (paste("variatia teoretica:", thVar))
}

binMoments <- function(count, n, p) {
  samples = rbinom(count, n, p)
  moments = getMoments(samples)
  printMoments(moments, thMean = n * p, thVar = n * p * (1 - p), "Binomiala: ")
  return (moments)
}

poisMoments <- function(count, l) {
  samples = rpois(count, l)
  moments = getMoments(samples)
  printMoments(moments, thMean = l, thVar = l, title = "Poisson:")
  return (moments)
}

expMoments <- function(count, rate = 1) {
  samples = rexp(count, rate)
  moments = getMoments(samples)
  printMoments(moments, thMean = 1 / rate, thVar = 1 / (rate * rate), "Exponentiala:")
  return (moments)
}

nomMoments <- function(count, mean = 0, variance = 1) {
  samples = rnorm(count, mean, sqrt(variance))
  moments = getMoments(samples)
  printMoments(moments, thMean = mean, thVar = variance, "Normala:")
  return (moments)
}

point1 <- function() {
  sampleNum = 1000
  binMoments(sampleNum, 5, 0.7)
  poisMoments(sampleNum, 5)
  expMoments(sampleNum, 5)
  nomMoments(sampleNum)
}

main <- function() {
  point1()
}

main()