# R Snowflakes - Inspired by the nord palette
# 
# Authori: Daan Seynaeve 2018
###############################################################################

plotFlake <- function(
    depth = 2,
    x = c(0, 1, -1),
    y = c(-sqrt(3), 0, 0),
    k = 1000,
    pal = c("black", "black", "black", "white"),
    ...) {
  
  stopifnot(depth > 1)
  
  # recurse in enough triangles
  for (j in seq(depth)) {
    x <- c(x,
        x + max(x) - min(x),
        x + (max(x) - min(x))/2,
        x + (max(x) - min(x))/2)
    y <- c(y,
        y,
        y - max(y) + min(y),
        -y - (max(y) - min(y)))
  }
  
  # move to origin
  y <- y - min(y)
  x <- x - max(x) - min(x - max(x))/2
  
  # mirror around x = 0 and rotate pi/3
  rot <- function(x, y, theta) {
    cbind(c(cos(theta), sin(theta)), c(-sin(theta), cos(theta))) %*% rbind(x, y)
  }
  M <- rot(-x, y, pi/3)
  x <- c(x, M[1,])
  y <- c(y, M[2,])
  
  # rotate pi/3 in both directions
  Ml <- rot(x, y, 2*pi/3)
  Mr <- rot(x, y, 4*pi/3)
  x <- c(x, Ml[1,], Mr[1,])
  y <- c(y, Ml[2,], Mr[2,])
  
  # mirror along y = 2x
  
  xlim <- c(min(x, y), max(x, y))
  ylim <- xlim
  
  plot.new()
  plot.window(xlim, ylim)
  
  for (j in seq((4 ^ depth) * 6))  {
    p <- ((j*3)-2):(j*3)
    polygon(x[p], y[p], col = pal[1 + (j %% k %% length(pal))], ...)
  }
  
}

salt <- as.numeric(Sys.time()) %% 17
numPlots <- 10
depth <- 2

par(mfrow = c(2, numPlots / 2), bg = nord::nord("polarnight")[1])
for (s in seq(numPlots)) {
  
  # generate a set of responses
  set.seed(s * salt)
  sequence <- runif(4 ^ depth) > .5
  
  # repeated palettes
  pal[sequence] <- rep(nord::nord("polarnight"), length(sequence))[which(sequence)]
  pal[!sequence] <- rep(nord::nord("snowstorm"), length(sequence))[which(!sequence)]
  
  # draw a snowflake
  plotFlake(depth, pal = rep(pal, 6), lty = 0)
  
}

