ml <- function()
{
    pml_training <- as.data.frame(pml_training)
    pml_training$classe <- as.factor(pml_training$classe)
    pml_training$user_name <- as.factor(pml_training$user_name)
    
    #mytrainingdf <- pml_training[, colnames(pml_training)[colSums(!is.na(pml_training)) > 0]]
    
    mytrainingdf <- pml_training[, c(2, 6:11, 37:49, 60:68, 85:ncol(pml_training))]
    mytrainingdf <- mytrainingdf[, c(1:31, 47, 58:ncol(mytrainingdf))]
    mytrainingdf <- mytrainingdf[, c(1:45, 60, 71:ncol(mytrainingdf))]
    mytrainingdf <- mytrainingdf[, c(1:44, 46:ncol(mytrainingdf))]
    mytestdf <- pml_testing[, colnames(pml_testing)[colSums(!is.na(pml_testing)) > 0]]

    mytrainingdf$user_name <- as.factor(mytrainingdf$user_name)
    mytrainingdf$new_window <- as.factor(mytrainingdf$new_window)
    
    traindf <- as.data.frame(mytrainingdf)

    pml_testing <- as.data.frame(pml_testing)
    mytestdf$user_name <- as.factor(mytestdf$user_name)
    mytestdf$new_window <- as.factor(mytestdf$new_window)

    testdf <- as.data.frame(mytestdf[, c(2, 6:ncol(mytestdf))])
    
    #gtraindf <<- traindf
    #gtestdf <<- testdf
    
    library(caret)
    fit <- train(classe ~ user_name + roll_belt + pitch_belt + yaw_belt + total_accel_belt, data=pml_training, method="rf")
    predictions <- predict(fit, newdata=pml_testing)
    
    print(summary(fit))
}




