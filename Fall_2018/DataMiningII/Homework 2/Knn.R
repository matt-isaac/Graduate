library(caret)

gen.data <- function(n){
  y <- rbinom(p = 0.7, size = 1, n = n)
  dat <- data.frame(x = double(n), y = y)
  for(i in 1:n){
    if(dat$y[i] == 1){
      dat$x[i] <- rnorm(n = 1, mean = 1.5, sd = 1)
    } else {
      dat$x[i] <- rnorm(n = 1, mean = 0, sd = 1)
    }
  }
  return(dat)
}

ds100 <- gen.data(100)
ds200 <- gen.data(200)
ds500 <- gen.data(500)
ds1000 <- gen.data(1000)

ds <- list("ds100" = ds100, 
           "ds200" = ds200, 
           "ds500" = ds500, 
           "ds1000" = ds1000)

knn.er <- data.frame(n = c(100, 200, 500, 1000), er = double(4))

# k nearest neighbors
ncount <- 1
for(d in ds){
  data <- d
  data$y <- as.factor(data$y)
  data.sh<-data[sample(nrow(data)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data.sh)),breaks=10,labels=FALSE)
   
  # create empty vector to hold 10 misclassification rates.
  mcrates <- double(10)
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data.sh[testIndexes, ]
    trainData <- data.sh[-testIndexes, ]
    #Use the test and train data partitions however you desire...
    model <- train(y~x, data = trainData, method = "knn",
                   preProcess = c("center", "scale"))
    pred <- predict(model, testData, type = "raw")
    misclass <- double(nrow(trainData))
    for(j in 1:nrow(testData)){
      if(pred[j] == testData$y[j]){
        misclass[j] <- 0
      } else {
        misclass[j] <- 1
      }
    }
    mcr <- sum(misclass)/nrow(testData)
    mcrates[i] <- mcr
  }
  cvmcrate <- mean(mcrates)
  knn.er$er[ncount] <- cvmcrate
  ncount <- ncount + 1
}


