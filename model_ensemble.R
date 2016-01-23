set.seed(800)
# 5 fold cv
fitControl <- trainControl(method = "cv",number = 5, allowParallel=T, summaryFunction = twoClassSummary,classProbs = TRUE)

# The idea is to set up a multi model ensemble and then stack a glm regression model on top of the predictions of these models
# This requires us to split the dataset into 3 parts: A training set for the base models, a stacking set for the glm regression and
# a validation set for assessing the final model
# Spliting the training set this way should combat overfitting well enough
# The optimal parameters for the base models were trained with 5-fold cv
# For the final predictions we used 90% of the data for the base learners and 10% for the metalearner
# if training time allows we can try to cram in even more models
# adding more than 3 models did not improve predictions, so in the spirit of keeping model complexity low we retained 3 models

# Used transformation of the dataset
usedDataset <- dataset.final#[sample(c(1:nrow(dataset.final)),round(nrow(dataset.final))*0.1),]

index <- c(1:nrow(usedDataset))
trainingData <- sample(index,round(nrow(usedDataset))*0.9)
training <- usedDataset[trainingData,]
test <- usedDataset[-trainingData,]
# Split the test set into stacked and validation
index <- c(1:nrow(test))
testData <- sample(index,round(nrow(test))*1)
stacked <- test[testData,]
validation <- test[-testData,]
# delete the temp datasets
remove(trainingData)
remove(usedDataset)

# ------------------------------------ TRAINING THE BASE MODELS ---------------------------------------------------------------
# model xgboost
xgb.grid <- expand.grid(nrounds = c(250,500), eta = c(0.03,0.05), max_depth = c(4,6,8), colsample_bytree = c(0.6,0.8,1), min_child_weight = c(1,3,5), gamma=0)
xgb.optimal <- expand.grid(nrounds = 500,eta = 0.03,max_depth = 6, colsample_bytree = 0.8, min_child_weight = 1,gamma=0)
xgbFit <- train(churn ~ ., data = training,method = "xgbTree",trControl = fitControl,tuneGrid= xgb.grid,verbose=TRUE, metric="ROC")

# model random forest.
rf.grid <- expand.grid(mtry=c(8,16,30,50))
rf.optimal <- expand.grid(mtry=30)
rfFit <- train(churn ~ ., data = training, method = "rf", trControl = fitControl, tuneGrid= rf.grid,verbose=TRUE, metric="ROC")

# model glm
glmFit <- train(churn ~ ., data = training,method = "glm",trControl = fitControl, metric="ROC")

#-------------------------------------- TRAINING THE STACKING MODEL---------------------------------------------------------
# Creating predictions
pred.xgb.stacked <- predict(xgbFit,stacked, type="prob")["leave"]
model.ensemble <- data.frame(pred.xgb=pred.xgb.stacked$leave)
model.ensemble$pred.rf <- predict(rfFit,stacked, type="prob")["leave"]$leave
model.ensemble$pred.glm <- predict(glmFit,stacked, type="prob")["leave"]$leave

# Stacking model: Simple glm regression seems to works best, better than xgboost, and vastly better than just averaging
lrFit <- train(stacked$churn ~ ., data = model.ensemble,method = "plr",trControl = fitControl,metric="ROC")

#--------------------------------- SAVING THE MODELS ----------------------------------
save(xgbFit,list=c("xgbFit"),file="results/xgbFit")
save(rfFit,list=c("rfFit"),file="results/rfFit")
save(glmFit,list=c("glmFit"),file="results/glmFit")
save(lrFit,list=c("lrFit"),file="results/lrFit")

#-----------------------------------------MODEL ASSESSMENT-----------------------------------------------------------------------------
# Predicting on the validation set with the base models
pred.xgb <- predict(xgbFit,validation, type="prob")["leave"]$leave
model.ensemble.val <- data.frame(pred.xgb) # combine to dataframe
model.ensemble.val$pred.rf <- predict(rfFit,validation, type="prob")["leave"]$leave
model.ensemble.val$pred.glm <- predict(glmFit,validation, type="prob")["leave"]$leave

# glm regression stack gives the following results
pred.stack.lr <- predict(lrFit,model.ensemble.val, type="prob")["leave"]
performance(pred.stack.lr$leave,validation$churn)
#Accuracy     Kappa       AUC  Lift 10% 
#0.6257778 0.2517399 0.6847480 1.5848717 

# Comparing to a single xgboost model
xgb.single <- predict(xgbFit,validation, type="prob")["leave"]
performance(xgb.single$leave,validation$churn)
# Accuracy     Kappa       AUC  Lift 10% 
# 0.6231111 0.2464251 0.6838233 1.5848717 
# Sadly the ensemble does not do a whole lot better than a single xgboost model. 
