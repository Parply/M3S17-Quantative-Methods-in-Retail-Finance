#libraries
library("dplyr")
library("nnet")
library("PRROC")


#summarise time amount class
summary(D2[,c(1,30,31)])

#plot histograms of time and amount
par(mfrow=c(1,2))
hist(D2[,1],main = "Histogram of times of transactions",xlab = "Time")
hist(D2[,30],main = "Histogram of transaction amount",xlab = "Transaction amount")

# calculate numbers of each class
D2 %>% group_by(Class) %>% tally()
# print proportions of each class
sprintf("Proportion of fraudulent transactions:%s",sum(D2$Class==1)/nrow(D2))

#plot histograms of amount and time given class
par(mfrow=c(2,2))
hist(D2[which(D2$Class == 1),1],main = "Histogram of times of fraudulent transactions",
     xlab = "Time",breaks = 50)
hist(D2[which(D2$Class == 1),30],main = "Histogram of fraudulent transaction amount",
     xlab = "Transaction amount",breaks = 50)
hist(D2[which(D2$Class == 0),1],main = "Histogram of times of non-fraudulent transactions",
     xlab = "Time",breaks = 50)
hist(D2[which(D2$Class == 0),30],main = "Histogram of non-fraudulent transaction amount",
     xlab = "Transaction amount",breaks = 50)

#set seed to keep same results for each run
set.seed(1729)

#randomly select a training and test sample indices
trainSample=sample(1:nrow(D2),nrow(D2)*2/3,replace = F)
testSample=setdiff(1:nrow(D2),trainSample)

#create training and test data sets
trainD2=D2[trainSample,]
testD2=D2[testSample,]

#check proportion of fradulant transactions in each
trainD2 %>% group_by(Class) %>% tally()
testD2 %>% group_by(Class) %>% tally()

#save training and test datasets
save(trainD2,file = "trainD2.RData")
save(testD2,file = "testD2.RData")

#create a normalising function
normalise <- function(x) (x-min(x))/(max(x)-min(x))

#scale the datasets
scaled_train <- as.data.frame(apply(trainD2, 2, normalise))
scaled_test <- as.data.frame(apply(testD2, 2, normalise))

#create place to store PRAUCs and name columns
pr <- matrix(NA, nrow=5,ncol=5)
colnames(pr) <- c("5","10","15","20")

for (n in 1:5){# test 5 hidden layer sizes
  for (i in 1:5){# train each ANN 5 times to ensure we find a global not local maximum AUC 
    model <- nnet(Class~.,data = scaled_train,size=n*5,maxit=1000,linout=T, trace=F)# create modek
    
    preds <- predict(model, newdata = scaled_test)# create predictions
    
    pr[i,n] <- pr.curve(scores.class0 = preds[scaled_test$Class==1],
                   scores.class1 = preds[scaled_test$Class==0], curve = F)$auc.integral# store PRAUC's
    
    if (pr[i,n]==max(pr,na.rm=T)){# save model with best AUC
      best_model <- model
    }
    
  }
}

summary(pr)# summarise results

preds <- predict(best_model, newdata = scaled_test)# create predictions using best model

best_pr <- pr.curve(scores.class0 = preds[scaled_test$Class==1],
                    scores.class1 = preds[scaled_test$Class==0], curve = T)# create pr curve
plot(best_pr)# plot it


p0 <- sum(scaled_test$Class==1)/nrow(scaled_test)# calculate p0
alarm_rate <- p0 * best_pr$curve[,2]/best_pr$curve[,1]# calculate alarm rate
# print max recall for given alarm rate of 0.05%
sprintf("Highest recall for the given alarm rate %s",best_pr$curve[sum(alarm_rate<0.005),2])



