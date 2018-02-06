acceptRejectDisk <- function(ct) {
  onDiskX = c()
  onDiskY = c()
  offDiskX = c()
  offDiskY = c()
  
  while(ct > 0) {
    x_y = runif(2, -1, 1)
    ct--
    if (x_y[1] * x_y[1] + x_y[2] * x_y[2] <= 1) {
      ct = ct - 1
      onDiskX = c(onDiskX, x_y[1])
      onDiskY = c(onDiskY, x_y[2])
    } else {
      offDiskX = c(offDiskX, x_y[1])
      offDiskY = c(offDiskY, x_y[2])
    }
  }
  
  resp = list()
  resp[[1]] = onDiskX
  resp[[2]] = onDiskY
  resp[[3]] = offDiskX
  resp[[4]] = offDiskY

  return(resp)
}

polarCoordinates <- function(ct) {
  radix = sqrt(runif(ct))
  thetas = runif(ct, 0, 2 * pi)
  px = radix * cos(thetas)
  py = radix * sin(thetas)
  resp = list()
  resp[[1]] = px
  resp[[2]] = py
  return (resp)
}

plotOn <- function(px, py) {
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
  axis(1)
  axis(2)
  box(which = "plot", lty = "solid")
  title(xlab = "x", ylab = "y")
  
  points(px, py, col="blue", pch = 19, cex = .8)
}

plotOnOff <- function(onX, onY, offX, offY) {
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
  axis(1)
  axis(2)
  box(which = "plot", lty = "solid")
  title(xlab = "x", ylab = "y")
  
  points(onX, onY, col = "blue", pch = 19)
  points(offX, offY, col = "red", pch = 19)
}

computeAverage <- function(px, py) {
  px = px * px
  py = py * py
  dists = sqrt(px + py)
  return(mean(dists))
}

plotAcRej <- function(ct) {
  r = acceptRejectDisk(ct)
  plotOnOff(r[[1]], r[[2]], r[[3]], r[[4]])
  print(computeAverage(r[[1]], r[[2]]))  
}

plotPolars <- function(ct) {
  r = polarCoordinates(ct)
  plotOn(r[[1]], r[[2]])
  
  outlineX = seq(-1, 1, by = .01)
  lines(outlineX, sqrt(1 - outlineX * outlineX), col = "red")
  lines(outlineX, -sqrt(1 - outlineX * outlineX), col = "red")
  
  print(computeAverage(r[[1]], r[[2]]))
}

main <- function() {
  n = 1000
  # plotAcRej(n)
  plotPolars(n)
}

main()