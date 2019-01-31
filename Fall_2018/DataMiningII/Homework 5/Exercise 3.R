library(R.matlab)
library(dplyr)
library(randomForest)
library(caret)
library(magrittr)
library(e1071)

# Read in and format data into dataframe with column "Y" and columns "X_1", "X_2", ...
df <- R.matlab::readMat("mnist_49_3000.mat") %>% lapply(t) %>% lapply(as_tibble)
colnames(df[[1]]) <- sprintf("X_%s",seq(1:ncol(df[[1]])))
colnames(df[[2]]) <- c("Y")
df <- bind_cols(df) %>% select(Y, everything())
df$Y <- as.factor(df$Y)


# Read in and format data into dataframe with column "Y" and columns "X_1", "X_2", ...
s_df <- R.matlab::readMat("mnist_49_1000_shifted.mat") %>% lapply(t) %>% lapply(as_tibble)
colnames(s_df[[1]]) <- sprintf("X_%s",seq(1:ncol(s_df[[1]])))
colnames(s_df[[2]]) <- c("Y")
s_df <- bind_cols(s_df) %>% select(Y, everything())
s_df$Y <- as.factor(s_df$Y)
test_sh <- s_df

################################################################################
### 0. Test/Train split ##########################################################
################################################################################

# create and format training data 
train <- dplyr::slice(df, 1:2000)
trainPred <- select(train, -Y)
trainResp <- resp <- select(train, Y)

# create and format test data
test <- dplyr::slice(df, 2001:3000)
testPred <- select(test, -Y)
testResp <- select(test, Y)

# ################################################################################
# ### 1. Random Forests ##########################################################
# ################################################################################

# Train random forest model, using default parameters
# ntree = 500
# mtry = sqrt(785) = 28.01785
# nodesize = 1
rf.model <- randomForest(Y~., data = train)

# predict onto test data
rf.predict <- predict(rf.model, testPred)
# rf.predict <- predict(rf.model, test_sh) # for predicting on shifted

rf.correct <- rf.predict == testResp$Y
rf.pcc <- sum(rf.correct)/length(rf.correct)
rf.testError <- 1 - rf.pcc
rf.testError
# Random Forest Test Error: 0.015

# ################################################################################
# ### 2. Suppor Vector Machines ##################################################
# ################################################################################

# Tune linear-kernel SVM
svm.tune.linear <- tune.svm(Y~., data = train, kernel = 'linear', cost = 1.0*10^(-3:2))
# best: cost = 0.1

svm.tune.linear <- tune.svm(Y~., data = train, kernel = 'linear', cost = seq(0.01, 0.17, by = 0.03))
# best: cost = 0.07

svm.tune.linear <- tune.svm(Y~., data = train, kernel = 'linear', cost = seq(0.05, 0.07, by = 0.01))
# best: cost = 0.05

# train model
svm.model <- svm(Y~., data = train, kernel = 'linear', cost = 0.05)

svm.predict <- predict(svm.model, testPred)
# svm.predict <- predict(svm.model, test_sh) # for predicting on shifted

svm.correct <- svm.predict == testResp$Y
svm.pcc <- sum(svm.correct)/length(svm.correct)
svm.testError <- 1 - svm.pcc
svm.testError
# SVM (linear kernel) Test Error: 0.042

# ################################################################################
# #### Logistic Regression #######################################################
# ################################################################################

# train logistic regression model
logReg.model <- glm(Y ~ ., data = train, family = binomial)

# predict on test data
logReg.predictProb <- predict(logReg.model, test, type = "response")
# logReg.predictProb <- predict(logReg.model, test_sh, type = "response") # for predicting on shifted

logReg.predict <- ifelse(logReg.predictProb > 0.5, "1", "-1")
logReg.correct <- logReg.predict == testResp$Y
logReg.pcc <- sum(logReg.correct)/length(logReg.correct)
logReg.testError <- 1 - logReg.pcc
logReg.testError # test error = 0.152

################################################################################
#### Generate Bootstrap Samples ################################################
################################################################################
samp.size <- 2000

genBoot <- function(data, sample.size){
  smpl <- dplyr::sample_n(tbl = data, size = sample.size, replace = TRUE)
  return(smpl)
}

bs_list_50 <- list()
for (i in 1:50) {
  bs_list_50[[i]] <- genBoot(train, sample.size = samp.size)
}

bs_list_100 <- list()
for (i in 1:100) {
  bs_list_100[[i]] <- genBoot(train, sample.size = samp.size)
}

################################################################################
#### Logistic Regression - Bagging w/ 50 Bootstrap Samples #####################
################################################################################

lr.bag50 <- data.frame(n = double(nrow(test)))
lr.bag50.sh <- data.frame(n = double(nrow(test_sh)))

for(bs in bs_list_50){
  # train logistic regression model
  lr.bs.model <- glm(Y ~ ., data = bs, family = binomial)

  # predict on test data
  logReg.predictProb <- predict(lr.bs.model, test, type = "response")
  
  logReg.predictProb.shift <- predict(lr.bs.model, test_sh, type = "response")
  
  lr.bag50 <- cbind(lr.bag50, logReg.predictProb)
  lr.bag50.sh <- cbind(lr.bag50.sh, logReg.predictProb.shift)
}

lr.bag50.vote <- rowMeans(lr.bag50[,-1])
lr.bag50.predict <- ifelse(lr.bag50.vote > 0.5, "1", "-1")
lr.bag50.correct <- lr.bag50.predict == testResp$Y
lr.bag50.pcc <- sum(lr.bag50.correct)/length(lr.bag50.correct)
lr.bag50.testError <- 1 - lr.bag50.pcc
lr.bag50.testError # test error = 0.116

lr.bag50.vote <- rowMeans(lr.bag50.sh[,-1])
lr.bag50.predict <- ifelse(lr.bag50.vote > 0.5, "1", "-1")
lr.bag50.correct <- lr.bag50.predict == testResp$Y
lr.bag50.pcc <- sum(lr.bag50.correct)/length(lr.bag50.correct)
lr.bag50.testError.sh <- 1 - lr.bag50.pcc
lr.bag50.testError.sh # test error =

################################################################################
#### Logistic Regression - Bagging w/ 100 Bootstrap Samples #####################
################################################################################

lr.bag100 <- data.frame(n = double(nrow(test)))
lr.bag100.sh <- data.frame(n = double(nrow(test_sh)))
lr.models <- list()

counter = 1
for(bs in bs_list_100){
  # train logistic regression model
  lr.bs.model <- glm(Y ~ ., data = bs, family = binomial)
  
  # predict on test data
  logReg.predictProb <- predict(lr.bs.model, test, type = "response")
  
  logReg.predictProb.shift <- predict(lr.bs.model, test_sh, type = "response")
  
  lr.bag100 <- cbind(lr.bag100, logReg.predictProb)
  lr.bag100.sh <- cbind(lr.bag100.sh, logReg.predictProb.shift)
  
  lr.models[counter] <- lr.bs.model
  
  counter = counter + 1
}

lr.bag100.vote <- rowMeans(lr.bag100[,-1])
lr.bag100.predict <- ifelse(lr.bag100.vote > 0.5, "1", "-1")
lr.bag100.correct <- lr.bag100.predict == testResp$Y
lr.bag100.pcc <- sum(lr.bag100.correct)/length(lr.bag100.correct)
lr.bag100.testError <- 1 - lr.bag100.pcc
lr.bag100.testError # test error = 0.121

lr.bag100.vote <- rowMeans(lr.bag100.sh[,-1])
lr.bag100.predict <- ifelse(lr.bag100.vote > 0.5, "1", "-1")
lr.bag100.correct <- lr.bag100.predict == testResp$Y
lr.bag100.pcc <- sum(lr.bag100.correct)/length(lr.bag100.correct)
lr.bag100.testError.sh <- 1 - lr.bag100.pcc
lr.bag100.testError.sh # test error = 0.551
