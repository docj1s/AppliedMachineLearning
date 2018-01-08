library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

set.seed(2245) #for reproduceability

#training <- read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!", ""))
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings=c("NA", "#DIV/0!", ""))

training <- training[, colSums(is.na(training)) ==0]

indexes <- createDataPartition(y=training$classe, p=.7, list=FALSE)

subTraining <- training[indexes, ]
subTesting <- training[-indexes, ]

plot(subTraining$classe, col="blue", main="Freq vs Classe Level (subtraining only)", xlab="Classe Levels", ylab="Frequency")

fit.dt <- rpart(classe ~ ., data = subTraining, method="class")

fit.predictionDT <- predict(fit.dt, subTesting, type="class")

rpart.plot(fit.dt, main="Decision Tree", extra=102, under=TRUE, faclen=0)

confusionMatrix(fit.predictionDT, subTesting$classe)

fit.rf <- randomForest(classe ~. data=subTraining, method="class")

fit.predictionRF <- predict(fit.rf, subTesting, type="class")

confusionMatrix(fit.predictionRF, subTesting$classe)