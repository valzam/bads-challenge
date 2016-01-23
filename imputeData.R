# save ids for later
ids <- dataset$id



# The remainder of the data can be imputed with the median or 0
# Performance needs to be tested
dataset.imputed <- dataset[,!names(dataset)%in% c("id")]

# Some usage based features need to be imputed by hand
dataset.imputed[is.na(dataset$RETDAYS),]$RETDAYS <- 0 # Retdays are 0 if no call has been made
dataset.imputed[is.na(dataset$RMCALLS),]$RMCALLS <- 0 # No roaming calls
dataset.imputed[is.na(dataset$RMMOU),]$RMMOU <- 0 # No roaming calls
dataset.imputed[is.na(dataset$RMREV),]$RMREV <- 0 # No roaming calls

imputeMedian <- function(x){
  miss <- is.na(x)
  x[miss] <- 0
  return(x)
}

imputeZero <- function(x){
  miss <- is.na(x)
  x[miss] <- 0
  return(x)
}

# Usage based features with NA seem to have a higher possibility of churn
# The tables for the most important features (according to xgboost)
# CHANGE_MOU, CHANGE_REV.. 
# stay leave 
# 121   353 
# TOTMRC_MEAN, MOU_RANGE and most other range and mean features
# stay leave 
# 67   139 
# To make use of this information we should encode the missings in a special way and not just impute the mean
# Making dummy variables would probably result in near zero variance features
# Imputing the missing with an extreme value like -999999 does not seem to affect the xgboost model very much
# Probably because the number of missings is very small (around 400 for CHANGE_MOU and CHANGE_REV and around 200 for the others)
# We decided to simply impute the missing features with 0
any(is.na(dataset.imputed))
for (i in 1:length(dataset.imputed)) {
  if(is.numeric(dataset.imputed[,i])){
    dataset.imputed[,i] <- imputeMedian(dataset.imputed[,i])
  }
}
any(is.na(dataset.imputed))
