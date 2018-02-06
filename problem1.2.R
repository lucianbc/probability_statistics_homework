plotBin <- function(params, legend, colors) {
  # plots probability mass function
  x_axis = c(0, 50)
  y_axis = c(0, .25)
  points = seq(from = x_axis[1], to = x_axis[2], by = 1)
  
  plot.new()
  plot.window(xlim = x_axis, ylim = y_axis)
  axis(1)
  axis(2)
  title(xlab = "points", ylab = "mass")
  
  apply(params, 1, function(r) {
    d = dbinom(points, as.numeric(r["n"]), as.numeric(r["p"]))
    points(points, d, col = r["c"], pch = 19)
  })
  
  legend("topright", bty = "n", legend = paste("(n, p) = ", legend), fill = colors, cex = 0.7)
  grid()
}

plotBinDist <- function(params, legend, colors) {
  x_axis = c(0, 50)
  y_axis = c(0, 1)
  points = seq(from = x_axis[1], to = x_axis[2], by = 1)
  
  plot.new()
  plot.window(xlim = x_axis, ylim = y_axis)
  axis(1)
  axis(2)
  title(xlab = "points", ylab = "repartitie")
  apply(params, 1, function(r) {
    p = pbinom(points, as.numeric(r["n"]), as.numeric(r["p"]))
    lines(points, p, col = r["c"])
  })
  legend("topright", bty = "n", legend = paste("(n, p) = ", legend), fill = colors, cex = 0.7)
}

bin <- function() {
  # n, p, c
  params = matrix(c(40, .7, "red", 40, .2, "green", 30, .6, "yellow", 20, .7, "purple", 50, .79, "black"), ncol = 3, byrow = TRUE)
  legend = c("(40, .7)", "(40, .2)", "(30, .6)", "(20, .7)", "(50, .79)")
  colors = c("red", "green", "yellow", "purple", "black")
  dimnames(params) = list(NULL, c("n", "p", "c"))
  
  # plotBin(params, legend, colors)
  plotBinDist(params, legend, colors)
}

plotPoisMass <- function(params) {
  x_axis = c(-1, 30)
  y_axis = c(0, .5)
  points = seq(from = x_axis[1], to = x_axis[2], by = 1)
  
  plot.new()
  plot.window(xlim = x_axis, ylim = y_axis)
  axis(1)
  axis(2)
  title(xlab = "points", ylab = "masa")
  apply(params, 2, function(r) {
    m = dpois(points, as.numeric(r[1]))
    points(points, m, col = r[2], pch = 19, cex = .8)
  })
  legend("topright", bty = "n", legend = paste("l = ", params[1,]), fill = params[2,], cex = 0.7)
  grid()
}

plotPoisDist <- function(params) {
  x_axis = c(-1, 30)
  y_axis = c(0, 1)
  points = seq(from = x_axis[1], to = x_axis[2], by = 1)
  
  plot.new()
  plot.window(xlim = x_axis, ylim = y_axis)
  axis(1)
  axis(2)
  title(xlab = "points", ylab = "repartitie")
  apply(params, 2, function(r) {
    p = ppois(points, as.numeric(r[1]))
    lines(points, p, col = r[2])
  })
  legend("topright", bty = "n", legend = paste("l = ", params[1,]), fill = params[2,], cex = 0.7)
  grid()
}

pois <- function() {
  # l c
  colors = c("red", "green", "yellow", "purple", "black")
  l = c(1.3, 2.1, 4, 8.7, 4)
  params = matrix(c(l, colors), ncol = 5, byrow = TRUE)
  # plotPoisMass(params)
  plotPoisDist(params)
}

plotExpDensity <- function(params) {
  x_axis = c(-1.5, 7)
  y_axis = c(0, 4.3)
  points = seq(from = x_axis[1], to = x_axis[2], by = .1)
  
  plot.new()
  plot.window(xlim = x_axis, ylim = y_axis)
  axis(1)
  axis(2)
  title(xlab = "points", ylab = "densitate")
  apply(params, 2, function(r) {
    p = dexp(points, as.numeric(r[1]))
    lines(points, p, col = r[2])
  })
  legend("topright", bty = "n", legend = paste("l = ", params[1,]), fill = params[2,], cex = 0.7)
  grid()
}

plotExpDist <- function(params) {
  x_axis = c(-1.5, 7)
  y_axis = c(0, 1)
  points = seq(from = x_axis[1], to = x_axis[2], by = .1)
  
  plot.new()
  plot.window(xlim = x_axis, ylim = y_axis)
  axis(1)
  axis(2)
  title(xlab = "points", ylab = "repartitie")
  apply(params, 2, function(r) {
    p = pexp(points, as.numeric(r[1]))
    lines(points, p, col = r[2])
  })
  legend("topright", bty = "n", legend = paste("l = ", params[1,]), fill = params[2,], cex = 0.7)
  grid()
}

exp <- function() {
  # l c
  colors = c("red", "green", "yellow", "purple", "black")
  l = c(0.4, 0.7, 1.3, 2.1, 4)
  params = matrix(c(l, colors), ncol = 5, byrow = TRUE)
  # plotExpDensity(params)
  plotExpDist(params)
}

plotNormDensity <- function(params) {
  x_axis = c(-5, 10)
  y_axis = c(0, 1.5)
  points = seq(from = x_axis[1], to = x_axis[2], by = .1)
  
  plot.new()
  plot.window(xlim = x_axis, ylim = y_axis)
  axis(1)
  axis(2)
  title(xlab = "points", ylab = "densitate")
  
  apply(params, 2, function(r) {
    p = dnorm(points, as.numeric(r[1]), as.numeric(r[2]))
    lines(points, p, col = r[3])
  })
  legend("topright", bty = "n", legend = paste("(m, sd) = ", c(params[1,]), params[2,]), fill = params[3,], cex = 0.7)
  grid()
}

plotNormDist <- function(params) {
  x_axis = c(-5, 10)
  y_axis = c(0, 1)
  points = seq(from = x_axis[1], to = x_axis[2], by = .1)
  
  plot.new()
  plot.window(xlim = x_axis, ylim = y_axis)
  axis(1)
  axis(2)
  title(xlab = "points", ylab = "repartitie")
  
  apply(params, 2, function(r) {
    p = pnorm(points, as.numeric(r[1]), as.numeric(r[2]))
    lines(points, p, col = r[3])
  })
  legend("topright", bty = "n", legend = paste("(m, sd) = ", c(params[1,]), params[2,]), fill = params[3,], cex = 0.7)
  grid()
}


norm <- function() {
  # m d c
  colors = c("red", "green", "yellow", "purple", "black")
  m = c(-1, 0, 1.5, 2.5, 4)
  d = c(.3, 2, .5, 1.5, 4)
  
  params = matrix(c(m, d, colors), ncol = 5, byrow = TRUE)
  # plotNormDensity(params)
  plotNormDist(params)
}

main <- function() {
  # bin()
  # pois()
  # exp()
  norm()
}

main()