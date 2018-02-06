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

bin <- function() {
  
  # n, p, c
  params = matrix(c(40, .7, "red", 40, .2, "green", 30, .6, "yellow", 20, .7, "purple", 50, .79, "black"), ncol = 3, byrow = TRUE)
  legend = c("(40, .7)", "(40, .2)", "(30, .6)", "(20, .7)", "(50, .79)")
  colors = c("red", "green", "yellow", "purple", "black")
  dimnames(params) = list(NULL, c("n", "p", "c"))
  
  plotBin(params, legend, colors)
}

main <- function() {
  bin()
}

main()