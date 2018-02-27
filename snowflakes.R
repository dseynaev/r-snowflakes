# R Snowflakes - Inspired by the nord palette
# 
# Author: Daan Seynaeve
###############################################################################

# middle right left
# down up up
plotFlake <- function(
    depth = 2,
    x = c(0, 1, -1),
    y = c(-sqrt(3), 0, 0),
    k = 100000,
    pal = c("black", "black", "black", "white"),
    sym = TRUE,
    ...) {
  
  stopifnot(depth > 0)
  
  # recurse in enough triangles
  # left right bottom middle
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
  
  if (sym) {
    
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
    
  }
  
  # mirror along y = 2x
  
  xlim <- c(min(x, y), max(x, y))
  ylim <- xlim
  
  plot.new()
  plot.window(xlim, ylim)
  
  for (j in seq((4 ^ depth) * 6))  {
    p <- ((j*3)-2):(j*3)
    polygon(x[p], y[p], col = pal[1 + ((j - 1) %% k %% length(pal))], ...)
  }
  
  invisible(rbind(x, y))
  
}


numPlots <- 10

par(mfrow = c(2, numPlots / 2), bg = nord::nord("polarnight")[1])
for (s in seq(numPlots)) {
  

  # actual snowflake: 6-fold symmetry
  
  mir <- function(tri) {
    if (l <- length(tri) == 1) {
      tri
    } else {
      l <- length(tri)
      m <- l/2
      a <- m/2
      b <- a+m
      c(mir(tri[(a+1):m]), mir(tri[1:a]),
          mir(tri[(m+1):b]), mir(tri[(b+1):l]))
    }
  }
  
  make_mir <- function(tri) {
    if (l <- length(tri) == 1) {
      tri
    } else {
      l <- length(tri)
      m <- l/2
      a <- m/2
      b <- a+m
      c(tri[1:a], mir(tri[1:a]),
          make_mir(tri[(m+1):b]), make_mir(tri[(b+1):l]))
    }
  }
  
  make_side <- function(tri, left = TRUE) {
    if (l <- length(tri) == 1) {
      TRUE
    } else {
      l <- length(tri)
      m <- l/2
      a <- m/2
      b <- a+m
      c(if (left) make_side(tri[1:a], left) else tri[1:a],
          if (!left) make_side(tri[(a+1):m], left) else tri[(a+1):m],
          make_side(tri[(m+1):b], left),
          tri[(b+1):l])
    }
  }
  
  depth <- 4
  
  # generate a set of responses
  salt <- as.numeric(Sys.time()) %% 17
  set.seed(salt + s)
  sequence <- runif(4 ^ depth) > .6
  
  
  sequence <- make_mir(make_side(sequence))
  
  # repeated palettes
  pal <- sequence
  pal[!sequence] <- rep(nord::nord("polarnight"), length(sequence))[which(sequence)]
  pal[sequence] <- rep(nord::nord("snowstorm"), length(sequence))[which(!sequence)]
  
  # draw a snowflake
  plotFlake(depth, pal = rep(pal, 6), lty = 0, sym = TRUE)
 
}

## sierpinski pal
#depth <- 4
#sierpal <- pal <- c(TRUE, TRUE, TRUE, FALSE)
#for (j in seq(depth)) {
#  sierpal <- unlist(lapply(sierpal, function(x) x * pal))
#}
#sierpal <- ifelse(sierpal, "black", "white")
#plotFlake(depth, pal = sierpal, sym = TRUE, lty = 0)




