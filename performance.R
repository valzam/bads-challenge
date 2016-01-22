# x is the predictin vector, y is the vector of actual outcomes
performance <- function(x,y){
  if(typeof(x) %in% c("list")){
    x <- unlist(x)
  }
  if(typeof(x) %in% c("data.frame")){
    x <- x[1]
  }
  x.binary <- round(x)
  x.binary <- factor(x.binary,c(0,1),c("stay","leave"))
  cm.gbm <- confusionMatrix(x.binary,y,positive = "leave")
  auc.score <- roc(y, x)$auc
  result <- c(cm.gbm$overall[c(1:2)],auc.score,lift.custom(x,y))
  names(result) <- c("Accuracy","Kappa","AUC", "Lift 10%")
  return (result)
  
}

lift.custom <- function(preds,actual){
  combine <- data.frame(actual,preds)
  names(combine) <- c("Actual","Pred")
  combine <- combine[with(combine, order(-Pred)),]
  
  combine.ten <- combine[1:(nrow(combine)*0.1),]
  hits <- nrow(combine.ten[combine.ten$Actual=="leave",])
  right.per <- hits/(nrow(combine.ten))
  right.per <- right.per*100
  
  actuals <- nrow(combine[combine$Actual=="leave",])
  actuals.per <- (actuals/nrow(combine) )* 100
  
  return(right.per/actuals.per)
}