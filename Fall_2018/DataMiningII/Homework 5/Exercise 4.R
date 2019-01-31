library(geometry)
library(tidyr)
library(dplyr)

################################################################################
#### Read in Data ##############################################################
################################################################################

# training data is in train$X
df <- R.matlab::readMat("anomaly.mat") %>% lapply(t) %>% lapply(as_tibble)
train <- as.data.frame(df$X)
names(train)[1] <- 'X'
train$index <- rownames(train)

# test points 1 and 2
tp1 <- df$xtest1$V1
tp2 <- df$xtest2$V1

################################################################################
#### Function Definitions ######################################################
################################################################################

gaussKernel <- function(vec, h){
  # calculates gaussian kernel for a given vector of values.
  # Args:
  #   vec: vector of numeric values for which the kernel is to be estimated.
  #   h: bandwidth parameter. 
  d <- length(vec)
  vec.h <- vec/h
  if(d > 1){
    kern <- ((2*pi)^(-d/2))*exp(-0.5*(dot(vec.h, vec.h)))
  } else {
    kern <- ((2*pi)^(-d/2))*exp(-0.5*(vec.h * vec.h))
  }
  return((h^(-d)) * kern)
}

estBandWidth <- function(df, h){
  # calculates objective function for LS-LOOCV
  # Args:
  #   df: data frame of training data (train) containing index and X .
  #   h: bandwidth parameter. 
  n <- nrow(df)
  t1 <- 0
  t2 <- 0
  ind <- df$index
  X <- df$X
  
  for(i in 1:n){
    for(j in 1:n){
      newTerm1 <- gaussKernel(vec = (X[i] - X[j]), h = (sqrt(2)*h))
      t1 <- t1 + newTerm1
      if(j != i){
        newTerm2 <- gaussKernel(vec = (X[i] - X[j]), h = h)
        t2 <- t2 + newTerm2
      }
    }
  }
  t1 <- (1/(n^2)) * t1
  t2 <- (2/(n*(n-1))) * t2
  result <- t1 - t2
  return(result)
}


kdeRange <- function(range, X, h){
  # calculates kernel density estimate for points in range. Calls kdePoint for 
  # all values in range.
  # Args:
  #   range: vector of numeric values for which the KDE is to be calculated
  #   X: training data
  #   h: bandwidth parameter. 
  #   
  densityEst <- data.frame(x = range, density = double(length(range)))
  for(i in 1:length(range)){
    densityEst$density[i] <- kdePoint(point = range[i], X = X, h = h)
  }
  return(densityEst) 
}

kdePoint <- function(point, X, h){
  # calculates kernel desnsity estimate for point. Called by kdeRange
  # Args:
  #   point: point for which density is to be estimated.
  #   X: training data
  #   h: bandwidth parameter. 
  term <- 0
  n <- length(X)
  for(i in 1:n){
    val <- gaussKernel(vec = (X[i] - point), h = h)
    term <- term + val
  }
  fhat <- (1/n) * term
  return(fhat)
}

fhat.i <- function(point, X, exclude.i, h){
  # calculates leave one out kernel density estiamate. 
  # Used by outlierScore1().
  # Args:
  #   point: point for which density is to be estimated.
  #   X: training data
  #   exclude.i: index for point which is to be excluded
  #   h: bandwidth parameter. 
  term <- 0
  n <- length(X)
  for(i in 1:n){
    if(i != exclude.i){
      val <- gaussKernel(vec = (point - X[i]), h = h)
      term <- term + val
    }
  }
  fhat <- (1/(n-1)) * term
  return(fhat)
}

outlierScore1 <- function(point, X, h){
  # calculates OutlierScore_1 for point. 
  # Args:
  #   point: point for which outlier score is to be calculated.
  #   X: training data
  #   h: bandwidth parameter. 
  n <- length(X)
  numerator <- kdePoint(point = point, X = X, h = h)
  denominator <- 0
  for(i in 1:n){
    term<- fhat.i(point = X[i], X = X, exclude.i = i, h = h)
    denominator <- term + denominator
  }
  denominator <- (1/n) * denominator
  return(numerator/denominator)
}

getKnn <- function(point, X, k){
  # Finds k nearest neighbors and distances. Used by outlierScore2()
  # Args:
  #   point: point for which k nearest neighbors are to be obtained.
  #   X: training data
  #   k: number of nearest neighbors to get
  pointvec <- rep(point, length(X))
  dist <- abs(X - pointvec)
  df <- data.frame(X = X, dist = dist)
  df <- arrange(df, dist)
  knn <- slice(df, 1:k)
  return(knn)
}

outlierScore2 <- function(point, X, k){
  # calculates OultierScore_2 for point
  # Args:
  #   point: point for which OutlierScore_2 is to be estimated.
  #   X: training data
  #   k: number of nearest neighbors to calculate 
  knn <- getKnn(point, X, k)
  numerator <- knn$dist[k]
  
  denominator <- (1/k) * sum(knn$dist)
  
  return(numerator/denominator)
}

################################################################################
#### To Run ####################################################################
################################################################################

# grid of possible bandwidths
grid <- seq(.01,.5, by = .01) 
gridn <- length(grid)

# calculate objective function from LS-LOOCV for each value in grid
hs <- data.frame(index = 1:gridn, h = grid, objF = rep(0, gridn))
for(i in 1:nrow(hs)){
  objF <- estBandWidth(df = train, h = hs$h[i])
  hs$index[i] <- i
  hs$objF[i] <- objF
}

# plot objective function by h
plot(hs$h, hs$objF)

# get h for which objective function is minimized.
min.h <- hs[hs$objF == min(hs$objF),]
hhat <- min.h$h

# hhat is optimal value of h. 
hhat

densRange <- seq(-2, 4, by = 0.01)
# estimate density
densEst <- kdeRange(range = densRange, X = train$X, h = hhat)
# plot estimated density
plot(densEst$x, densEst$density, type = 'l', main = "Estimated Density", xlab = "X", ylab = "density")

# calculate OutlierScore_1 for xtest1 and xtest2
os1.tp1 <- outlierScore1(point = tp1, X = train$X, h = 0.08)
os1.tp2 <- outlierScore1(point = tp2, X = train$X, h = 0.08)

os1.tp1
os1.tp2

# calculate OutlierScore_2 for xtest1 and xtest2, with various values of k
os2.tp1.100 <- outlierScore2(point = tp1, X = train$X, k = 100)
os2.tp1.150 <- outlierScore2(point = tp1, X = train$X, k = 150)
os2.tp1.200 <- outlierScore2(point = tp1, X = train$X, k = 200)

os2.tp1.100
os2.tp1.150
os2.tp1.200

os2.tp2.100 <- outlierScore2(point = tp2, X = train$X, k = 100)
os2.tp2.150 <- outlierScore2(point = tp2, X = train$X, k = 150)
os2.tp2.200 <- outlierScore2(point = tp2, X = train$X, k = 200)

os2.tp2.100
os2.tp2.150
os2.tp2.200

