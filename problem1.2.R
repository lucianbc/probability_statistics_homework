plotBin <- function() {
  # plots probability mass function
  x_axis = c(0, 40)
  y_axis = c(0, .25)
  points = seq(from = x_axis[1], to = x_axis[2], by = 1)
  
  # n, p, c
  params = matrix(c(40, .5, "red", 20, .3, "green", 30, .4, "yellow"), ncol = 3, byrow = TRUE)
  dimnames(params) = list(NULL, c("n", "p", "c"))
  
  plot.new()
  plot.window(xlim = x_axis, ylim = y_axis)
  axis(1)
  axis(2)
  title(xlab = "points", ylab = "mass")
  
  apply(params, 1, function(r) {
    d = dbinom(points, as.numeric(r["n"]), as.numeric(r["p"]))
    points(points, d, col = r["c"], pch = 19)
  })
}

main <- function() {
  plotBin()
}

main()