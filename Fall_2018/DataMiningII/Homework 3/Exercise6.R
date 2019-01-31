library(R.matlab)
library(dplyr)
library(geometry)
library(pracma)

# Read in and format data into dataframe with column "Y" and columns "X_1", "X_2", ...
df <- R.matlab::readMat("mnist_49_3000.mat") %>% lapply(t) %>% lapply(as_tibble)
colnames(df[[1]]) <- sprintf("X_%s",seq(1:ncol(df[[1]])))
colnames(df[[2]]) <- c("Y")
df <- bind_cols(df) %>% select(Y, everything())

objFunc <- function(xs, y, theta, lambda){
  # objFunc() calculates the negative log likelihood with the added regularization term.
  # Args:
  #   xs: dataframe of training predictor variables (i.e. feature vectors)
  #   y:  vector of training response variables. Must be of the same length as xs.
  #
  # Returns:
  #   The J(theta) = negative log likelihood for logistic regression with regulizer term added on.
  terms <- double(nrow(xs))
  for(i in 1:nrow(xs)){
    xi <- as.numeric(xs[i,])
    yi <- as.numeric(y$Y[i])
    t1 <- yi*log(1/(1 + exp(dot(-theta, xi))))
    t2 <- (1-yi)*log((exp(dot(-theta, xi)))/(1 + exp(dot(-theta, xi))))
    terms[i] <- t1 + t2
  }
  Jmin <- (-1 * sum(terms)) + (lambda * dot(theta, theta))
  return(Jmin)
}

calcmu <- function(theta, xi){
  # calculates mu as defined in 
  # Args:
  #   theta: vector containing intercept (b) and weights (w)
  #   xi: vector of predictor variables (one observation ("row"))
  #   
  #   theta and xi must be vectors of same length
  #
  # Returns:
  #   The optimized value of theta
  denom <- (1 + exp(dot(-theta, xi)))
  return(1/denom)
}

mellonGH <- function(xmat, yvec, theta){
  # Kicks off newtonsMethod() recursive function
  # Args:
  #   xmat: matrix of predictor variables (including column of 1's)
  #   yvec: dataframe (tibble?) of response values
  #   theta: vector of b and w's. 
  #
  # Returns:
  #   list containing gradient (grad) and hessian (hess)
  lambda <- 1
  mus <- double(nrow(xmat))
  for(i in 1:nrow(xmat)){
    xi <- as.numeric(xmat[i,])
    mus[i] <- calcmu(theta, xi)
  }
  grad <- t(xmat) %*% (mus - as.numeric(yvec$Y)) + 2*lambda*theta
  ds <- mus * (1 - mus)
  D <- diag(ds)
  XT <- t(sapply(xmat, as.numeric))
  X <- sapply(xmat, as.numeric)
  tlambI <- diag(rep(2 * lambda, 785))
  hess <- XT %*% D %*% X + tlambI
  
  results <- list("grad" = grad, "hess" = hess)
  return(results)
}

newtonsMethodinit <- function(maxitr, predictors, response){
  # Kicks off newtonsMethod() recursive function
  # Args:
  #   maxitr: maximum iterations for Newton's Method
  #   predictors: dataframe of predictor variables
  #   response: dataframe (tibble?) of response values
  #
  # Returns:
  #   The optimized value of theta
  
  b = 1 # initial guess of b
  ws = rep(0, 784) # initial guess of w's
  inittheta <- c(b, ws)

  theta <- newtonsMethod(theta = inittheta, nitr = 0, maxitr = maxitr, xs = predictors, y = response)
  return(theta)
}

newtonsMethod <- function(theta, nitr, maxitr, xs, y){
  # Recursive function that implements Newton's method
  # Args:
  #   theta: vector of w's and b's
  #   nitr: current iteration number
  #   maxitr: maximum number of iterations for Newton's method
  #   xs: dataframe of predictor variables
  #   y: dataframe (tibble?) of response
  #
  # Returns:
  #   The optimized value of theta, or calls itself again.
  if(nitr >= maxitr){
    return(theta)
  } else {
    print(paste("Iteration Number: ", nitr))
    
    gh <- mellonGH(xmat = xs, yvec = y, theta = theta)
    grad <- gh$grad
    hess <- gh$hess
    
    # grad <- gradJ(xmat = xs, yvec = y, theta = theta)
    # hess <- hessJ(xs = xs, theta = theta)

    theta.1 <- theta - (inv(hess) %*% grad)
    nitr <- nitr + 1
    return(newtonsMethod(theta = theta.1, nitr = nitr, maxitr = maxitr, xs = xs, y = y))
  }
}

predictNum <- function(xs, yi, theta){
  # Conducts logistic regression to predict classes
  # Args:
  #   xs: dataframe of predictor variables (without column of 1's)
  #   yi: dataframe (tibble?) of response
  #   theta: vector of b and w's
  #
  # Returns:
  #   list containing a dataframe of probabilities, predicted value, actual value, 
  #   and correct indicator, as well as pcc (percent correctly classified)
  w <- theta[2:length(theta)]
  b <- theta[1]
  probs <- double(nrow(xs))
  predict <- double(nrow(xs))
  correct <- double(nrow(xs))
  for(i in 1:nrow(xs)){
    xi <- as.numeric(xs[i,])
    p <- 1 / (1 + exp(-dot(w, xi) + b))
    probs[i] <- p
    if(p > 0.5){
      predict[i] <- 1
    } else{
      predict[i] <- -1
    }
    
    if(as.numeric(predict[i]) == as.numeric(yi$Y[i])){
      correct[i] = 1
    } else {
      correct[i] = 0
    }
  }
  result <- data.frame(probs, predict, Y = yi$Y, correct)
  pcc <- sum(correct)/nrow(xs)
  lst <- list("results" = result, "pcc" = pcc)
  
  return(lst)
}

# create and format training data 
train <- dplyr::slice(df, 1:2000)
trainPred <- select(train, -Y)
X0 <- rep(1, 2000)
trainPred <- cbind(X0, trainPred)
trainResp <- resp <- select(train, Y)

# create and format test data
test <- dplyr::slice(df, 2001:3000)
testPred <- select(test, -Y)
X0 <- rep(1000)
testPred <- cbind(X0, testPred)
testResp <- select(test, Y)

# get theta from training data
thetatrain <- newtonsMethodinit(maxitr = 2, predictors = trainPred, response = trainResp)

# Predict classes of test data
pred <- predictNum(xs = select(testPred, -X0), yi = testResp, theta = thetatrain)
results <- pred$results
pcc <- pred$pcc
pcc # view PCC

# Calculate minimum value of Objective function
minOF <- objFunc(xs = trainPred, y = trainResp, theta = thetatrain, lambda = 1)

# Get top 20 "most sure but misclassified" observations

incorrect <- results[results$correct == 0,]
incorrect$confidence <- abs(incorrect$probs - 0.5)
incorrect <- incorrect[order(-incorrect$confidence, incorrect$probs), ]
incorrect.20 <- incorrect[1:20,]
incorrect.20$index <- row.names(incorrect.20)

# results <- mutate(results, conf = probs - 0.5000000)
# results$index <- as.numeric(row.names(results))
# allWrong <- results[results$correct == 0,]
# allWrong <- arrange(allWrong, desc(conf))
# # allWrong <- allWrong[order(-allWrong$conf),]
# worst20 <- allWrong[1:20,]

mnist <- readMat("mnist_49_3000.mat")
imgList = list()
for(i in seq(1, length(mnist$x), by = 784)) {
  img <- matrix(mnist$x[i:(i + 783)], nrow = 28, byrow = TRUE)
  img <- t(apply(img, 2, rev))
  imgList[[length(imgList)+1]] = img
}

test.imgList <- imgList[2001:3000]

par(mfrow = c(4,5))
# to visualize:
# counter <- 1
for(i in row.names(incorrect.20)){
  print(i)
  correctLabel = "intial"
  if(incorrect.20$Y[which(incorrect.20$index == i)] == -1){
    correctLabel = "4"
  } else {
    correctLabel = "9"
  }
  image(1:28, 1:28, test.imgList[[as.numeric(i)]], col=gray((255:0)/255), main = paste("Img#: ", i, "\nCorrect Label: ", correctLabel))
}



# Run classification for entire data set
{
# preds <- select(df, -Y) # separate predictor variables: everything except Y
# X0 <- rep(1, 3000) 
# preds <- cbind(X0, preds) # add column of 1's (to line up with intercept)
# resp <- select(df, Y) # get response variable, Y
# output <- predictNum(xs = select(preds, -X0), yi = resp, theta = theta)
# theta <- newtonsMethodinit(maxitr = 5, predictors = preds, response = resp)
}

# Old Gradient & Hessian Functions
{
  # gradJ <- function(xmat, yvec, theta){
  #   lambda <- 10
  #   vecs <- data.frame(matrix(ncol = 3000, nrow = 785))
  #   for(i in 1:nrow(xmat)){
  #     xi <- as.numeric(xmat[i,])
  #     yi <- as.numeric(yvec$Y[i])
  #     val <- (exp(dot(-theta,xi)) * xi)/(1 + exp(dot(-theta,xi))) + ((yi*xi) - (xi))
  #     vecs[,i] <- val
  #   }
  #   gradient <- (-1 * rowSums(vecs)) + 2*lambda*theta
  #   return(gradient)
  # }
  
  # hessJ <- function(xs,theta){
  #   lambda <- 10
  #   tlambI <- diag(rep(2 * lambda, 785))
  #   
  #   ds <- double(nrow(xs))
  #   
  #   for(i in 1:nrow(xs)){
  #     xi <- as.numeric(xs[i,])
  #     entry <- exp(dot(-theta,xi))/((1 + exp(dot(-theta,xi)))^2)
  #     ds[i] <- entry
  #   }
  #   terms <- double(nrow(xs))
  #   for(i in 1:nrow(xs)){
  #     xi <- as.numeric(xs[i,])
  #   }
  #   D <- diag(ds)
  #   XT <- t(sapply(xs, as.numeric))
  #   X <- sapply(xs, as.numeric)
  #   hess <- XT %*% D %*% X
  #   hess <- hess + tlambI
  #   return(hess)
  # }
}