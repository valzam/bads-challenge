# First some assumptions on what makes people churn
# Their contract ends
# They want a new phone
# They are in financial trouble
# They are unhappy with the service they get
# They don't use their phone anymore / a lot less
# They are moving away
# Parents / Spouse / friends switch to a new carrier
# They want to switch to prepaid
# We try to have features for these assumptions in the dataset

# First we can check for features with near zero variance
nzv <- nearZeroVar(dataset.imputed)
# There are quite a lot of nzv predictors. This means that values of the predictor are highly skewed towards one value
# for example a lot of the dummy variables have almost 99% 0s
# We will try and delete these variables and see how the performance increases
dataset.i.nzv <- dataset.imputed[,-nzv]
# This transformation did not change the overall accuracy but increased training time
# Accuracy     Kappa       AUC  Lift 10% 
#0.6350000 0.2700546 0.6864603 1.5475463 
# Sadly most variables that got canned were the dummy variables we encoded by hand. Time well spent!


# A lot of the usage based features are highly correlated
# Some are just linear combinations of other features, like total number of calls
# We can try to find linear combinations with an inbuilt caret feature
#linCombs <- findLinearCombos(dataset.i.nzv[,names(dataset.i.nzv) %in% numerical_features])
# This does not find any linear combinations, we might need to look by hand

# We can also try to find high correlations
descrCor <-  cor(dataset.i.nzv[,names(dataset.i.nzv) %in% numerical_features])
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
# We found 1 unneccessary variable: MOU_MEAN, which is basically just adding up other usage variables
dataset.i.nzv.nc <- dataset.i.nzv[,-highCorr]

# This improves the AUC and lift score on the test set, yay!
# Accuracy     Kappa       AUC  Lift 10% 
#0.6396000 0.2793179 0.6925864 1.5626893 
# MOU_MEAN was a very important feature when including all variables
# excluding it has made other features more important, while improving AUC on the test set
# I guess this means that excluding it has reduced overfitting and made the model more general, which is good!
# Looking at the variable importance plot (in the results folder) we see that all variables have at least minor influence
# it seems we cannot improve the model a lot more with feature selection, especially since gradient boosting basically selects features
# We basically have 2 options to improve AUC: Try to engineer new features or create an ensemble of models

dataset.i.nzv.nc <- dataset.i.nzv.nc[,!names(dataset.i.nzv.nc) %in% c("OWNS_RV","FOREIGN_TRAVEL","NEW_CAR","NEG_SENT","LAST_PHONE_SWAP")]
dataset.final <- dataset.i.nzv.nc
