# When we look at the assumptions that we formulated in the beginning we see that most features that predict
# customer happiness have been deleted due to non zero variance. We can try to engineer new features with more variance
# that combine the happiness and customer support features.
# The features we deleted were
# RETDAYS DO_NOT_MAIL DO_NOT_SOLICIT DECLINED_OFFER RECEIVED_OFFER NUMBER_OF_RETENTION_CALLS RECEIVED_OFFER
# Also we deleted features that gave indication about the customers financial stability
# CREDIT_ADJUSTED BAD_CREDIT_SCORE
# When looking at the frequency tables we can clearly see that happyness has a strong influence on churning
# We also exlucded another feater: REF_QTY_NUM, how many referals customers have made. Customers who have referred are less likely to churn
# but the absolute number is very small
# We can try to engineers new features out of these features
happy <- dataset.imputed[,names(dataset.imputed) %in% c("RETDAYS","RECEIVED_OFFER","NUMBER_OF_RETENTION_CALLS","NUMBER_OF_OFFERS","DO_NOT_MAIL","DO_NOT_SOLICIT","DECLINED_OFFER")]
credit <- dataset.imputed[,names(dataset.imputed) %in% c("CREDIT_ADJUSTED","BAD_CREDIT_SCORE")]
ref <- dataset.imputed[,"REF_QTY_NUM"]

# Make a variable that checks if something has made a negative sentiment
#happy$NEG_SENT <- 0
#happy[happy$DO_NOT_MAIL==1,]$NEG_SENT <- 1
#happy[happy$DO_NOT_SOLICIT==1,]$NEG_SENT <- 1
#happy[happy$DECLINED_OFFER==1,]$NEG_SENT <- 1
# This variable still has near zero variance, however people that do show negative sentiment are much more likely to churn (785 stay, 1146 churn)
# Inlcuding the variable slightly improves AUC
# But after including the RET_CONTCAT NEG_SENT has no importance anymore, as the 2 are highly correlated

# Make a variable if someone has had contact with retention 
happy$RET_CONTACT <- 0
happy[happy$NUMBER_OF_RETENTION_CALLS > 0,]$RET_CONTACT <- 1
happy[happy$NUMBER_OF_OFFERS > 0,]$RET_CONTACT <- 1
# it seems if you have been contacted you are at risk of churning
#     stay leave
#  0 24594 23479
#  1   625  1302
# Ret contact has a high importance and improves the model slightly, so we decide to keep it

#credit$BAD_CREDIT <- 0
#credit[credit$BAD_CREDIT_SCORE==1,]$BAD_CREDIT <- 1
#credit[credit$CREDIT_ADJUSTED==1,]$BAD_CREDIT <- 1
# This variable does not provide useful information
#     stay leave
#  0 23506 23167
#  1  1713  1614


# After including RET_CONTACT the NEG_SENT has almost no importance, alongside other features with near 0 importance
# We exclude those to improve training times
dataset.final$RET_CONTACT <- happy$RET_CONTACT