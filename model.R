set.seed(800)
# 5 fold cv
fitControl <- trainControl(method = "cv",number = 5, allowParallel=T, summaryFunction = twoClassSummary,classProbs = TRUE)

# Used transformation of the dataset
usedDataset <- dataset.final#[sample(c(1:nrow(dataset.final)),round(nrow(dataset.final))*0.8),]

index <- c(1:nrow(usedDataset))
trainingData <- sample(index,round(nrow(usedDataset))*0.8)
test <- usedDataset[-trainingData,]
training <- usedDataset[trainingData,]

# delete the temp datasets
remove(trainingData)
remove(usedDataset)

# One xgboost model has shown us that the variables we include are ok and can be used from now on
# However, AUC is not satisfactory. The next step is to try and build up an ensemble
# We will retain the Xgboost model as a strong base learner
# We are first testing on a smaller subset of the whole data to speed up training times and make trying out
# different things possible
xgb.grid <- expand.grid(nrounds = c(250,500), eta = c(0.03,0.05), max_depth = c(6), colsample_bytree = c(0.8), min_child_weight = 1, gamma=0)
xgb.optimal <- expand.grid(nrounds = 500,eta = 0.03,max_depth = 6, colsample_bytree = 0.8, min_child_weight = 1,gamma=0)
xgbFit <- train(churn ~ ., data = training,method = "xgbTree",trControl = fitControl,tuneGrid= xgb.optimal,verbose=TRUE, metric="ROC")
pred.xgb.prob <- predict(xgbFit,test, type="prob")["leave"]

# Since we converted everything to numerical features we can include a neural network
# Sadly nnet performance absolutely sucks, on its own and in an ensemble
# nn.grid <- expand.grid(size=c(8),decay=c(0.01,0.1))
# nn.optimal <- expand.grid(size=8,decay=0.01)
# nnFit <- train(churn ~ ., data = training,
#                 method = "nnet",
#                 trControl = fitControl,
#                 tuneGrid= nn.optimal,
#                 verbose=TRUE,
#                 metric="ROC"
# )

#pred.nn.prob <- predict(nnFit,test, type="prob")["leave"]
#performance(pred.nn.prob,test$churn)

# Random Forest might be too similar to gradient boosting, but worth a try
# Result: Random forest performs a lot better than nnet but still worse than xgboost while being highly correlated to it 0.8
# the ensemble performs worse than a single xgboost
rf.grid <- expand.grid(mtry=c(2,8,16,30))
rf.optimal <- expand.grid(mtry=30)
rfFit <- train(churn ~ ., data = training,
                method = "rf",
                trControl = fitControl,
                tuneGrid= rf.optimal,
                verbose=TRUE,
                metric="ROC"
)
pred.rf.prob <- predict(rfFit,test, type="prob")["leave"]

# Assemble predictions
performance(pred.xgb.prob,test$churn)
performance(pred.rf.prob,test$churn)
# We are going for a simple mean prediction
# Stacking a meta learner posses a huge problem because of possible overfitting
# We will see if it is feasible to try or if the improvement through the basic ensemble is enough
ensemble <- data.frame(xgb=pred.xgb.prob$leave,second=pred.rf.prob$leave)
ensemble$combined <- ave(ensemble$xgb,ensemble$secon)
performance(ensemble$combined,test$churn)

# Another option is to train multiple xgboost models on different parts of the data and use the predictions in an ensemble
# Either averaging or with a meta learner. The problem here is that we don't have a lot of data and it might not  be enough
# to train 3 models reliably
index <- c(1:nrow(training))

trainingData <- sample(index,round(nrow(training))*0.6)
training.sample.1 <- training[trainingData,]
test.sample.1 <-training[-trainingData,]
xgbFit.1 <- train(churn ~ ., data = training.sample.1,method = "xgbTree",trControl = fitControl,tuneGrid= xgb.optimal,verbose=TRUE, metric="ROC")

trainingData <- sample(index,round(nrow(training))*0.6)
training.sample.2 <- training[trainingData,]
test.sample.2 <- training[-trainingData,]
xgbFit.2 <- train(churn ~ ., data = training.sample.2,method = "xgbTree",trControl = fitControl,tuneGrid= xgb.optimal,verbose=TRUE, metric="ROC")

trainingData <- sample(index,round(nrow(training))*0.6)
training.sample.3 <- training[trainingData,]
test.sample.3 <- training[-trainingData,]
xgbFit.3 <- train(churn ~ ., data = training.sample.3,method = "xgbTree",trControl = fitControl,tuneGrid= xgb.optimal,verbose=TRUE, metric="ROC")

# Split the test set in 2 sets. I set for training the stacked classifier
# the second set for model assessment
index <- c(1:nrow(test))
testData <- sample(index,round(nrow(test))*0.8)
test.sample <- test[testData,]
validation.sample <-test[-testData,]
pred.xgb.1 <- predict(xgbFit.1,test.sample, type="prob")["leave"]
pred.xgb.2 <- predict(xgbFit.2,test.sample, type="prob")["leave"]
pred.xgb.3 <- predict(xgbFit.3,test.sample, type="prob")["leave"]

# Put them together in an ensemble and train a xgboost
xgb.ensemble <- data.frame(first=pred.xgb.1$leave,second=pred.xgb.2$leave,third=pred.xgb.3$leave)
xgbFit.stacked <- train(test.sample$churn ~ ., data = xgb.ensemble,method = "xgbTree",trControl = fitControl,tuneGrid= xgb.optimal,verbose=TRUE, metric="ROC")
# Train a logistic regression
lrFit.stacked <- train(test.sample$churn ~ ., data = xgb.ensemble,method = "plr",trControl = fitControl, metric="ROC")

# Create predictions for the validation set
pred.xgb.1 <- predict(xgbFit.1,validation.sample, type="prob")["leave"]
pred.xgb.2 <- predict(xgbFit.2,validation.sample, type="prob")["leave"]
pred.xgb.3 <- predict(xgbFit.3,validation.sample, type="prob")["leave"]
xgb.ensemble.validation <- data.frame(first=pred.xgb.1$leave,second=pred.xgb.2$leave,third=pred.xgb.3$leave)

# The xgboost stacked ensemble gives the following results
pred.stack <- predict(xgbFit.stacked,xgb.ensemble.validation, type="prob")["leave"]
performance(pred.stack$leave,validation.sample$churn)
#Accuracy     Kappa       AUC  Lift 10% 
#0.6170000 0.2350029 0.6681202 1.5188583 

# Logistic regression stack gives the following results
pred.stack.lr <- predict(lrFit.stacked,xgb.ensemble.validation, type="prob")["leave"]
performance(pred.stack.lr$leave,validation.sample$churn)
#Accuracy     Kappa       AUC  Lift 10% 
#0.6385000 0.2772197 0.6875232 1.5188583

# A single xgboost model on the validation sample gives the following results
pred.xgb.prob <- predict(xgbFit,validation.sample, type="prob")["leave"]
performance(pred.xgb.prob,validation.sample$churn)
#Accuracy     Kappa       AUC  Lift 10% 
#0.6415000 0.2831090 0.6912806 1.5392457 

# Simply averaging the 3 xgboost models gives the following results
xgb.ensemble.validation$ave <- ave(xgb.ensemble.validation$first,xgb.ensemble.validation$second,xgb.ensemble.validation$third)
performance(xgb.ensemble.validation$ave,validation.sample$churn)
#Accuracy     Kappa       AUC  Lift 10% 
#0.6190000 0.2376958 0.6720756 1.4882773

# It seems as if a single xgboost model gives the best performance out of the box and stacking does not improve the AUC
# This is probably due to the amount of data we have. However, training each xgboost base model on more than 60% of the data will probably make them 
# too similar to each other

# Ensemble with xgboost,rf, nnet and a logistic regresission stacking model
pred.xgb <- predict(xgbFit,test.sample, type="prob")["leave"]
pred.nn <- predict(nnFit,test.sample, type="prob")["leave"]
pred.rf <- predict(rfFit,test.sample, type="prob")["leave"]
model.ensemble <- data.frame(first=pred.xgb$leave,second=pred.nn$leave,third=pred.rf$leave) # combine to dataframe

# Learn the regression
lrFit.stacked.model <- train(test.sample$churn ~ ., data = model.ensemble,method = "plr",trControl = fitControl, metric="ROC")

# predict on validation set
validation.sample <- test
pred.xgb <- predict(xgbFit,validation.sample, type="prob")["leave"]
pred.nn <- predict(nnFit,validation.sample, type="prob")["leave"]
pred.rf <- predict(rfFit,validation.sample, type="prob")["leave"]
model.ensemble <- data.frame(first=pred.xgb$leave,second=pred.nn$leave,third=pred.rf$leave) # combine to dataframe
pred.stack.lr.model <- predict(lrFit.stacked.model,model.ensemble, type="prob")["leave"]
performance(pred.stack.lr.model$leave,validation.sample$churn)

# Results on the validation set
# Accuracy     Kappa       AUC  Lift 10% 
#0.6390000 0.2780412 0.6914136 1.5698267 
# This stacking model seems to beat a single xgboost model!!
