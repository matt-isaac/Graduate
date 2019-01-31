library(caret)
library(Rlab)
library(ggplot2)
library(dplyr)

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


n <-  100
ks <- double(100)
lr.mrs <- double(100)
knn.mrs <- double(100)
for(i in 1:100){
  dat <- gen.data(n)
  dat$y<- as.factor(dat$y)
  logreg <- train(y ~ x, data = dat, method = 'glm', family = binomial, trControl = trainControl(method = 'cv', number = 10))
  lr.mrs[i] <- 1 - logreg$results$Accuracy
  
  knn <- train(y ~ x, data = dat, method = 'knn', trControl = trainControl(method = 'cv', number = 10))
  knn.mrs[i] <- 1 - max(knn$results$Accuracy)
  ks[i] <- knn$results$k[which(knn$results$Accuracy == max(knn$results$Accuracy))]
}

n <-  200
ks.200 <- double(100)
lr.mrs.200 <- double(100)
knn.mrs.200 <- double(100)
for(i in 1:100){
  dat <- gen.data(n)
  dat$y<- as.factor(dat$y)
  logreg <- train(y ~ x, data = dat, method = 'glm', family = binomial, trControl = trainControl(method = 'cv', number = 10))
  lr.mrs.200[i] <- 1 - logreg$results$Accuracy
  
  knn <- train(y ~ x, data = dat, method = 'knn', trControl = trainControl(method = 'cv', number = 10))
  knn.mrs.200[i] <- 1 - max(knn$results$Accuracy)
  ks.200[i] <- knn$results$k[which(knn$results$Accuracy == max(knn$results$Accuracy))]
}

n <-  500
ks.500 <- double(100)
lr.mrs.500 <- double(100)
knn.mrs.500 <- double(100)
for(i in 1:100){
  dat <- gen.data(n)
  dat$y<- as.factor(dat$y)
  logreg <- train(y ~ x, data = dat, method = 'glm', family = binomial, trControl = trainControl(method = 'cv', number = 10))
  lr.mrs.500[i] <- 1 - logreg$results$Accuracy
  
  knn <- train(y ~ x, data = dat, method = 'knn', trControl = trainControl(method = 'cv', number = 10))
  knn.mrs.500[i] <- 1 - max(knn$results$Accuracy)
  ks.500[i] <- knn$results$k[which(knn$results$Accuracy == max(knn$results$Accuracy))]
}

n <-  1000
ks.1000 <- double(100)
lr.mrs.1000 <- double(100)
knn.mrs.1000 <- double(100)
for(i in 1:100){
  dat <- gen.data(n)
  dat$y<- as.factor(dat$y)
  logreg <- train(y ~ x, data = dat, method = 'glm', family = binomial, trControl = trainControl(method = 'cv', number = 10))
  lr.mrs.1000[i] <- 1 - logreg$results$Accuracy
  
  knn <- train(y ~ x, data = dat, method = 'knn', trControl = trainControl(method = 'cv', number = 10))
  knn.mrs.1000[i] <- 1 - max(knn$results$Accuracy)
  ks.1000[i] <- knn$results$k[which(knn$results$Accuracy == max(knn$results$Accuracy))]
}

# Bayes Classifier
n = 100
bc.mrs.100 <- double(100)
for(i in 1:100){
  dat <- gen.data(n)
  dat$y<- as.factor(dat$y)
  preds <- double(nrow(dat))
  for(k in 1:nrow(dat)){
    if(dat$x[k] >= b.cutoff){
      pred.class <- 1
    } else {
      pred.class <- 0
    }
    preds[k] <- pred.class
  }
  for(j in 1:nrow(dat)){
    if(preds[j] == dat$y[j]){
      dat$misclass[j] <- 0
    } else {
      dat$misclass[j] <- 1
    }
  }
  bc.mrs.100[i] <- sum(dat$misclass)/nrow(dat)
}

n = 200
bc.mrs.200 <- double(100)
for(i in 1:100){
  dat <- gen.data(n)
  dat$y<- as.factor(dat$y)
  preds <- double(nrow(dat))
  for(k in 1:nrow(dat)){
    if(dat$x[k] >= b.cutoff){
      pred.class <- 1
    } else {
      pred.class <- 0
    }
    preds[k] <- pred.class
  }
  for(j in 1:nrow(dat)){
    if(preds[j] == dat$y[j]){
      dat$misclass[j] <- 0
    } else {
      dat$misclass[j] <- 1
    }
  }
  bc.mrs.200[i] <- sum(dat$misclass)/nrow(dat)
}

n = 500
bc.mrs.500 <- double(100)
for(i in 1:100){
  dat <- gen.data(n)
  dat$y<- as.factor(dat$y)
  preds <- double(nrow(dat))
  for(k in 1:nrow(dat)){
    if(dat$x[k] >= b.cutoff){
      pred.class <- 1
    } else {
      pred.class <- 0
    }
    preds[k] <- pred.class
  }
  for(j in 1:nrow(dat)){
    if(preds[j] == dat$y[j]){
      dat$misclass[j] <- 0
    } else {
      dat$misclass[j] <- 1
    }
  }
  bc.mrs.500[i] <- sum(dat$misclass)/nrow(dat)
}

n = 1000
bc.mrs.1000 <- double(100)
for(i in 1:100){
  dat <- gen.data(n)
  dat$y<- as.factor(dat$y)
  preds <- double(nrow(dat))
  for(k in 1:nrow(dat)){
    if(dat$x[k] >= b.cutoff){
      pred.class <- 1
    } else {
      pred.class <- 0
    }
    preds[k] <- pred.class
  }
  for(j in 1:nrow(dat)){
    if(preds[j] == dat$y[j]){
      dat$misclass[j] <- 0
    } else {
      dat$misclass[j] <- 1
    }
  }
  bc.mrs.1000[i] <- sum(dat$misclass)/nrow(dat)
}

lr.100 <- data.frame(n = rep(100, 100), classifier = rep("LR", 100), mcr = lr.mrs.100)
lr.200 <- data.frame(n = rep(200, 100), classifier = rep("LR", 100), mcr = lr.mrs.200)
lr.500 <- data.frame(n = rep(500, 100), classifier = rep("LR", 100), mcr = lr.mrs.500)
lr.1000 <- data.frame(n = rep(1000, 100), classifier = rep("LR", 100), mcr = lr.mrs.1000)

knn.100 <- data.frame(n = rep(100, 100), classifier = rep("KNN", 100), mcr = knn.mrs.100)
knn.200 <- data.frame(n = rep(200, 100), classifier = rep("KNN", 100), mcr = knn.mrs.200)
knn.500 <- data.frame(n = rep(500, 100), classifier = rep("KNN", 100), mcr = knn.mrs.500)
knn.1000 <- data.frame(n = rep(1000, 100), classifier = rep("KNN", 100), mcr = knn.mrs.1000)

bc.100 <- data.frame(n = rep(100, 100), classifier = rep("BC", 100), mcr = bc.mrs.100)
bc.200 <- data.frame(n = rep(200, 100), classifier = rep("BC", 100), mcr = bc.mrs.200)
bc.500 <- data.frame(n = rep(500, 100), classifier = rep("BC", 100), mcr = bc.mrs.500)
bc.1000 <- data.frame(n = rep(1000, 100), classifier = rep("BC", 100), mcr = bc.mrs.1000)


alldata <- rbind(lr.100, lr.200, lr.500, lr.1000,
                 knn.100, knn.200, knn.500, knn.1000,
                 bc.100, bc.200, bc.500, bc.1000)

alldata$n <- as.factor(alldata$n)

ggplot(data = alldata, aes(x = n, y = mcr, fill = classifier)) +
  geom_boxplot() +
  ylab("Misclassification Rate")

h1 <- pnorm(0.185, mean = 0, sd = 1, lower.tail = FALSE)
h2 <- pnorm(0.185, mean = 1.5, sd = 1, lower.tail = TRUE)
0.3*h1 + 0.7*h2

meanks <- c(mean(ks.100), mean(ks.200), mean(ks.500), mean(ks.1000))
knn.mmc <- c(mean(knn.mrs.100), mean(knn.mrs.200), mean(knn.mrs.500), mean(knn.mrs.1000))
knn.sdmc <- c(sd(knn.mrs.100), sd(knn.mrs.200), sd(knn.mrs.500), sd(knn.mrs.1000))

bc.mmc <- c(mean(bc.mrs.100), mean(bc.mrs.200), mean(bc.mrs.500), mean(bc.mrs.1000))
bc.sdmc <- c(sd(bc.mrs.100), sd(bc.mrs.200), sd(bc.mrs.500), sd(bc.mrs.1000))

lr.mmc <- c(mean(lr.mrs.100), mean(lr.mrs.200), mean(lr.mrs.500), mean(lr.mrs.1000))
lr.sdmc <- c(sd(lr.mrs.100), sd(lr.mrs.200), sd(lr.mrs.500), sd(lr.mrs.1000))

