library(e1071)
library(caret)

load('PostForest.RData')

#TODO: cbind predictions to training and testing data
prediction.val <- data.frame(prediction.val)
prediction.train <- data.frame(prediction.train)
prediction.test <- data.frame(prediction.test)
prediction <- rbind(prediction.train, prediction.val)
train<-merge(train, prediction, by='row.names')
test<-merge(test, prediction.test, by='row.names')

#New samples for training and validation
trainCrimes <- sample(1:dim(train)[1], 500000)
valCrimes <- setdiff(1:dim(train)[1], trainCrimes)


#Helper function to extract features for Random Forest
extractFeatures <- function(data) {
          features <- c(
			"DayOfWeek",
                        "weekday",
                        "hour",
                        "daypart",
			"businessHour",
                        "PdDistrict",
			"dist2PdMean",
			"dist2Mean",
                        "Unemployment",
#                        "Address",
                        "X",
                        "Y", 
			weather.cols[2:length(weather.cols)],
			names(prediction))
                  return(data[,features])
}


trainFrame <- droplevels(cbind(train[trainCrimes,]$Category, extractFeatures(train[trainCrimes,])))
colnames(trainFrame)[1] <- "Category"


nb <- naiveBayes(Category ~ ., 
   data = trainFrame, apriori=class.weights
   )


#Predict validation and training data, compute logloss
prediction.val <- predict(nb, extractFeatures(train[valCrimes,]), type="raw")
loss.val <- logLoss(train[valCrimes,]$Category, prediction.val)
print(loss.val)
# prediction.train <- predict(rf, extractFeatures(train[trainCrimes,]), type="prob")
prediction.train <- predict(nb, extractFeatures(train[trainCrimes,]), type="raw")
loss.train <- logLoss(train[trainCrimes,]$Category, prediction.train)
print(loss.train)
prediction.test <- predict(nb, extractFeatures(test), type="raw")


save.image(file="PostBayes.RData")

#Look at misclassified crimes
# class.val has the category with the largest prediction probability
class.val <- unlist(
	    apply(prediction.val, 1, 
	      function(x) names(which(max(x) == x))[[1]]) )
misclass <- subset(train[valCrimes,], class.val != train[valCrimes,"Category"])
isclass <- subset(train[valCrimes,], class.val == train[valCrimes,"Category"])

cm <- confusionMatrix(train[valCrimes,"Category"], class.val)



save.image(file="PostBayes.RData")

#prepare submission csv
submission <- data.frame(Id = test$Id, prediction.test)
write.csv(submission, file="submission.csv", row.names=FALSE, quote=FALSE)