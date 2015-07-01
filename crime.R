library(randomForest)

#read in train/test data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Create a date column
train$date <- substring(train$Dates,1,10)
test$date <- substring(test$Dates,1,10)

#Create a time column by stripping out timestamp
train$time <- substring(train$Dates,12,20)
test$time <- substring(test$Dates,12,20)

#factorize new timestamp column
train$time <- factor(train$time)
test$time <- factor(test$time)

#Create a weekday column and factorize
train$weekday <- ! (train$DayOfWeek == 'Saturday' | train$DayOfWeek == 'Sunday')
train$weekday <- as.factor(train$weekday)
test$weekday <- ! (test$DayOfWeek == 'Saturday' | test$DayOfWeek == 'Sunday')
test$weekday <- as.factor(test$weekday)

#Create a portion of day column
train$hour <- as.numeric(substr(train$time, 1,2))
test$hour <- as.numeric(substr(test$time, 1, 2))

#1AM-5AM = 1 Late night
train$daypart[(train$hour < 6) & (train$hour > 0)] <- 1
test$daypart[(test$hour < 6) & (test$hour > 0)] <- 1

#5AM-11AM = 2 Morning
train$daypart[(train$hour < 12) & (train$hour > 4)] <- 2
test$daypart[(test$hour < 12) & (test$hour > 4)] <- 2

#11AM-5PM = 3 Afternoon
train$daypart[(train$hour < 18) & (train$hour > 10)] <- 3
test$daypart[(test$hour < 18) & (test$hour > 10)] <- 3

#5PM-1AM = 4 Evening
train$daypart[(train$hour > 17) | (train$hour == 0)] <- 4
test$daypart[(test$hour > 17) | (test$hour == 0)] <- 4

#factorize
train$daypart <- as.factor(train$daypart)
test$daypart <- as.factor(test$daypart)

#Convert hour back to factor
train$hour <- as.factor(train$hour)
test$hour <- as.factor(test$hour)

#Fix the mislabelled lattitude and longitudes by using addresses
train[train$Address == "AUSTIN ST / LARKIN ST",]$Y <-  37.789379
train[train$Address == "AUSTIN ST / LARKIN ST",]$X <- -122.418752
test[test$Address == "AUSTIN ST / LARKIN ST",]$Y <-  37.789379
test[test$Address == "AUSTIN ST / LARKIN ST",]$X <- -122.418752
train[train$Address == "LARKIN ST / AUSTIN ST",]$Y <-  37.789379
train[train$Address == "LARKIN ST / AUSTIN ST",]$X <- -122.418752
test[test$Address == "LARKIN ST / AUSTIN ST",]$Y <-  37.789379
test[test$Address == "LARKIN ST / AUSTIN ST",]$X <- -122.418752
train[train$Address == "5THSTNORTH ST / EDDY ST",]$Y <-  37.784463 
train[train$Address == "5THSTNORTH ST / EDDY ST",]$X <- -122.40856037
test[test$Address == "5THSTNORTH ST / EDDY ST",]$Y <-  37.784463
test[test$Address == "5THSTNORTH ST / EDDY ST",]$X <- -122.40856037
train[train$Address == "EDDY ST / 5THSTNORTH ST",]$Y <-  37.784463 
train[train$Address == "EDDY ST / 5THSTNORTH ST",]$X <- -122.40856037
test[test$Address == "EDDY ST / 5THSTNORTH ST",]$Y <-  37.784463
test[test$Address == "EDDY ST / 5THSTNORTH ST",]$X <- -122.40856037
train[train$Address == "BRANNAN ST / 1ST ST",]$Y <-  37.783618 
train[train$Address == "BRANNAN ST / 1ST ST",]$X <- -122.389857
test[test$Address == "BRANNAN ST / 1ST ST",]$Y <-  37.783618
test[test$Address == "BRANNAN ST / 1ST ST",]$X <- -122.389857
#Not sure about this address...
train[train$Address == "AVENUE OF THE PALMS / EUCLID AV",]$Y <-  37.818805 
train[train$Address == "AVENUE OF THE PALMS / EUCLID AV",]$X <- -122.373570
test[test$Address == "AVENUE OF THE PALMS / EUCLID AV",]$Y <-  37.818805
test[test$Address == "AVENUE OF THE PALMS / EUCLID AV",]$X <- -122.373570
train[train$Address == "ELLIS ST / 5THSTNORTH ST",]$Y <-  37.785408 
train[train$Address == "ELLIS ST / 5THSTNORTH ST",]$X <- -122.408751
test[test$Address == "ELLIS ST / 5THSTNORTH ST",]$Y <-  37.785408
test[test$Address == "ELLIS ST / 5THSTNORTH ST",]$X <- -122.408751
train[train$Address == "GILMAN AV / FITCH ST",]$Y <-  37.717144 
train[train$Address == "GILMAN AV / FITCH ST",]$X <- -122.386218
test[test$Address == "GILMAN AV / FITCH ST",]$Y <-  37.717144
test[test$Address == "GILMAN AV / FITCH ST",]$X <- -122.386218
train[train$Address == "MCALLISTER ST / 7THSTNORTH ST",]$Y <-  37.780991 
train[train$Address == "MCALLISTER ST / 7THSTNORTH ST",]$X <-  -122.413101
test[test$Address == "MCALLISTER ST / 7THSTNORTH ST",]$Y <-  37.780991
test[test$Address == "MCALLISTER ST / 7THSTNORTH ST",]$X <-  -122.413101
train[train$Address == "7THSTNORTH ST / MCALLISTER ST",]$Y <-  37.780991 
train[train$Address == "7THSTNORTH ST / MCALLISTER ST",]$X <-  -122.413101
test[test$Address == "7THSTNORTH ST / MCALLISTER ST",]$Y <-  37.780991
test[test$Address == "7THSTNORTH ST / MCALLISTER ST",]$X <-  -122.413101
#And...I give up.



# Read in weather data
weather <- read.csv('http://www.wunderground.com/history/airport/KSFO/2003/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2003&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1', stringsAsFactors=FALSE)
for (year in as.character(2004:2015))
{
    url <- paste('http://www.wunderground.com/history/airport/KSFO/',
                 year,
                 '/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=',
                 year,
                 '&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1',
                 sep = "")
    newdata <- read.csv(url, stringsAsFactors=FALSE)
    names(newdata) <- c("PST", names(newdata)[2:23])
    weather <- rbind(weather, newdata)
}

#Modify date string to match the crime data date string
weather$PST <- strftime(as.POSIXct(weather$PST, format="%Y-%m-%d"), format="%Y-%m-%d")


#Specify the weather columns that will be used for our model
weather.cols <- c("PST",
#                  "Events",
                  "Max.TemperatureC",
#                  "Mean.TemperatureC", "Min.TemperatureC",
                  "Max.Humidity",
#                  "Mean.Humidity", "Min.Humidity",
#                  "Max.VisibilityKm",
#                  "Mean.VisibilityKm",
                  "Min.VisibilitykM",
                  "Max.Wind.SpeedKm.h",
#                  "Precipitationmm",
                  "CloudCover")

# Remove the other columns				  
weather <- weather[,weather.cols]
# Convert weather columns to numeric data
for (col in weather.cols[3:length(weather.cols)])
{
    weather[,col] <- as.numeric(weather[,col])
}
#Replace missing data with large negative value
weather[is.na(weather)] <- -1000.0
#Merge weather data with crime data
train <- merge(x=train, y=weather, by.x = "date", by.y="PST", all.x=TRUE)
test <- merge(x=test, y=weather, by.x = "date", by.y="PST", all.x=TRUE)


# Read in unemployment data
unemployment <- read.csv('SFUnemployment-NotSeasonallyAdjusted.csv')
#Temporary stopgap until the real number is released.
unemployment[unemployment$Year=="2015", "May"] <- 3.3

#Extract Years and months
train$Year <- substring(train$Dates, 1, 4)
test$Year <- substring(test$Dates, 1, 4)
train$Month <- substring(train$Dates, 6, 7)
test$Month <- substring(test$Dates, 6, 7)

#Merge unemployment data with crime data
train$Unemployment <- 0.0
for (year in unemployment[,1])
{
    for (month in c("01", "02", "03", "04", "05", "06",
                    "07", "08", "09", "10", "11", "12"))
    {
        train[train$Month==month & train$Year==year, "Unemployment"] <-
            unemployment[unemployment$Year == year, as.numeric(month) + 1]
        test[test$Month==month & test$Year==year, "Unemployment"] <-
            unemployment[unemployment$Year == year, as.numeric(month) + 1]
    }
            
}



#Helper function to extract features for Random Forest
extractFeatures <- function(data) {
          features <- c("DayOfWeek",
#                        "weekday",
                        "hour",
#                        "daypart",
                        "PdDistrict",
                        "Unemployment",
#                        "Address",
                        "X",
                        "Y", weather.cols[2:length(weather.cols)])
                  return(data[,features])
}

#Define the log loss function
# This is the Kaggle competition's leaderboard measure
logLoss <- function(true, pred)
{
    eps <- 1e-15;
    nr <- nrow(pred)
    pred <- matrix(sapply( pred, function(x) max(eps, x)), nrow = nr)
    pred <- matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
    ll <- 0
    for (i in 1:nr)
    {
        ll <- ll + log(pred[i, true[i]])
    }
    ll <- ll * -1/(nrow(pred))
    return(ll);
}

#Divide data set into training and validation sets
trainCrimes <- sample(1:dim(train)[1], 350000)
valCrimes <- setdiff(1:dim(train)[1], trainCrimes)

#Train a random forest model
rf <- randomForest(droplevels(extractFeatures(train[trainCrimes,])),
                   train[trainCrimes,]$Category, ntree=1000,
                   mtry=5, sampsize=1000, importance=TRUE)

#Extract the feature importances and print
imp <- importance(rf, type=2)
print(imp)

#Predict validation and training data, compute logloss
prediction.val <- predict(rf, extractFeatures(train[valCrimes,]), type="prob")
loss.val <- logLoss(train[valCrimes,]$Category, prediction.val)
print(loss.val)
prediction.train <- predict(rf, type="prob")
loss.train <- logLoss(train[trainCrimes,]$Category, prediction.train)
print(loss.train)
prediction.test <- predict(rf, extractFeatures(test), type="prob")

#prepare submission csv
submission <- data.frame(Id = test$Id, prediction.test)
write.csv(submission, file="submission.csv", row.names=FALSE, quote=FALSE)
