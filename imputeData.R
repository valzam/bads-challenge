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
  x[miss] <- median(x,na.rm = T)
  return(x)
}

imputeZero <- function(x){
  miss <- is.na(x)
  x[miss] <- median(x,na.rm = T)
  return(x)
}

any(is.na(dataset.imputed))
for (i in 1:length(dataset.imputed)) {
  if(is.numeric(dataset.imputed[,i])){
    dataset.imputed[,i] <- imputeMedian(dataset.imputed[,i])
  }
}
any(is.na(dataset.imputed))