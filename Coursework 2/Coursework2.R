library("PRROC") # import library
# load in data
load("C:/Users/Alex/OneDrive/Documents/Imperial Projects/Quant Finance/testD2.RData")
load("C:/Users/Alex/OneDrive/Documents/Imperial Projects/Quant Finance/trainD2.RData")

KDE_score <- function(x,h,train){ # create function to calculate score using given algorithm
  x <- as.matrix(x) #'set to matrix type
  train <- as.matrix(train) # set to matrix type
  n <- nrow(train) # calculate size of training dataset
  s <- nrow(x) # calculate size of test dataset
  score <- matrix(NA, nrow = s, ncol = 1) # create matrix to store scores
  for (t in 1:s){ # for each point in the test set
    phis <- matrix(NA, nrow = n, ncol = 1) # create matrix to store phis
    for (i in 1:n){ 
      phis[i] <- -(x[t,] - train[i,]) %*% (x[t,] - train[i,]) / (2 * h^2) # calculate phis
    }
    phi <- max(phis) # calculate max 
    sum <- 0 # initialise sum
    for (i in 1:n){
      sum <- sum + exp(phis[i] - phi) # calculate sum
    }
    score[t] <- phi + log(sum) # store score
  }
  
  
  
  return(score) # return scores
}

preds <- KDE_score(testD2[,-31],0.1,trainD2[,-31]) # calculate scores


pr <- pr.curve(scores.class0 = preds[testD2$Class==1],
               scores.class1 = preds[testD2$Class==0], curve = T) # create pr curve
plot(pr) # plot it



p0 <- sum(testD2$Class==1)/nrow(testD2) # calculate p0
alarm_rate <- p0 * pr$curve[,2]/pr$curve[,1]# calculate alarm rate
# print max recall for given alarm rate of 0.05%
sprintf("Highest recall for the given alarm rate %s",pr$curve[sum(alarm_rate<0.005),2])