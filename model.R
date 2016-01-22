set.seed(800)

index <- c(1:nrow(dataset.imputed))
trainingData <- sample(index,round(nrow(dataset.imputed))*0.8)
test <- dataset.imputed[-trainingData,]
training <- dataset.imputed[trainingData,]
remove(trainingData)

fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 5,
  allowParallel=T,
  summaryFunction = twoClassSummary,
  classProbs = TRUE)

xgb.grid <- expand.grid(nrounds = c(250,500),
                        eta = c(0.01,0.03,0.05),
                        max_depth = c(2,4,6),
                        colsample_bytree = c(0.8,1),
                        min_child_weight = 1,
                        gamma=0
)

xgb.optimal <- expand.grid(nrounds = 500,
                           eta = 0.03,
                           max_depth = 6,
                           colsample_bytree = 0.8,
                           min_child_weight = 1,
                           gamma=0
)


xgbFit <- train(churn ~ ., data = training,
                method = "xgbTree",
                trControl = fitControl,
                tuneGrid= xgb.grid,
                verbose=TRUE,
                metric="ROC"
)

pred.xgb.prob <- predict(xgbFit,test, type="prob")["leave"]
pred.xgb.binary <- predict(xgbFit,test)
cm.xgb <- confusionMatrix(pred.xgb.binary,test$churn,positive = "leave")

# Performance Metrics
performance(pred.xgb.prob,test$churn)
