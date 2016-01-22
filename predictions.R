pred.xgb.1 <- predict(xgbFit.1,dataset.final, type="prob")["leave"]
pred.xgb.2 <- predict(xgbFit.2,dataset.final, type="prob")["leave"]
pred.xgb.3 <- predict(xgbFit.3,dataset.final, type="prob")["leave"]
xgb.ensemble.validation <- data.frame(first=pred.xgb.1$leave,second=pred.xgb.2$leave,third=pred.xgb.3$leave)
pred.stack.lr <- predict(lrFit.stacked,xgb.ensemble.validation, type="prob")["leave"]
performance(pred.stack.lr$leave,dataset.final$churn)

x <- data.frame(Customer_ID=ids,EstimatedChurnProbability=pred.stack.lr$leave)
write.table(x,"submis_lr_ensemble.csv",sep=",",row.names=FALSE)

pred.xgb.prob <- predict(xgbFit,dataset.final, type="prob")["leave"]
performance(pred.xgb.prob,dataset.final$churn)

x <- data.frame(Customer_ID=ids,EstimatedChurnProbability=pred.xgb.prob$leave)
write.table(x,"submis_single_xgboost.csv",sep=",",row.names=FALSE)


# Final submissions
dataset <- read.csv2("data/test.csv",sep=",", stringsAsFactors = FALSE)
# Functions for data prep
source("encodeFeatures.R")
source("performance.R")

# Encode data as dummies and numeric
names(dataset) <- toupper(names(dataset))
dataset <- encodeFeatures(dataset)

# All the numerical features from the spreadsheet. All usage based
numerical_features <-c("ADJMOU","ADJQTY","ADJREV","ATTEMPT_MEAN","ATTEMPT_RANGE","AVG3MOU","AVG3QTY","AVG3REV","AVG6MOU","AVG6QTY","AVG6REV","AVGMOU","AVGQTY","AVGREV","BLCK_DAT_MEAN","BLCK_DAT_RANGE","BLCK_VCE_MEAN","BLCK_VCE_RANGE","CALLFWDV_MEAN","CALLFWDV_RANGE","CALLWAIT_MEAN","CALLWAIT_RANGE","CC_MOU_MEAN","CC_MOU_RANGE","CCRNDMOU_MEAN","CCRNDMOU_RANGE","CHANGE_MOU","CHANGE_REV","COMP_DAT_MEAN","COMP_DAT_RANGE","COMP_VCE_MEAN","COMP_VCE_RANGE","COMPLETE_MEAN","COMPLETE_RANGE","CUSTCARE_MEAN","CUSTCARE_RANGE","DA_MEAN","DA_RANGE","DATOVR_MEAN","DATOVR_RANGE","DROP_BLK_MEAN","DROP_BLK_RANGE","DROP_DAT_MEAN","DROP_DAT_RANGE","DROP_VCE_MEAN","DROP_VCE_RANGE","EQPDAYS","INONEMIN_MEAN","INONEMIN_RANGE","IWYLIS_VCE_MEAN","IWYLIS_VCE_RANGE","MONTHS","MOU_CDAT_MEAN","MOU_CDAT_RANGE","MOU_CVCE_MEAN","MOU_CVCE_RANGE","MOU_MEAN","MOU_OPKD_MEAN","MOU_OPKD_RANGE","MOU_OPKV_MEAN","MOU_OPKV_RANGE","MOU_PEAD_MEAN","MOU_PEAD_RANGE","MOU_PEAV_MEAN","MOU_PEAV_RANGE","MOU_RANGE","MOU_RVCE_MEAN","MOU_RVCE_RANGE","MOUIWYLISV_MEAN","MOUIWYLISV_RANGE","MOUOWYLISV_MEAN","MOUOWYLISV_RANGE","OWYLIS_VCE_MEAN","OWYLIS_VCE_RANGE","OPK_DAT_MEAN","OPK_DAT_RANGE","OPK_VCE_MEAN","OPK_VCE_RANGE","OVRMOU_MEAN","OVRMOU_RANGE","OVRREV_MEAN","OVRREV_RANGE","PEAK_DAT_MEAN","PEAK_DAT_RANGE","PEAK_VCE_MEAN","PEAK_VCE_RANGE","PLCD_DAT_MEAN","PLCD_DAT_RANGE","PLCD_VCE_MEAN","PLCD_VCE_RANGE","RECV_SMS_MEAN","RECV_SMS_RANGE","RECV_VCE_MEAN","RECV_VCE_RANGE","RETDAYS","REV_MEAN","REV_RANGE","RMCALLS","RMMOU","RMREV","ROAM_MEAN","ROAM_RANGE","THREEWAY_MEAN","THREEWAY_RANGE","TOTCALLS","TOTMOU","TOTMRC_MEAN","TOTMRC_RANGE","TOTREV","UNAN_DAT_MEAN","UNAN_DAT_RANGE","UNAN_VCE_MEAN","UNAN_VCE_RANGE","VCEOVR_MEAN","VCEOVR_RANGE")

# impute the data
source('imputeData.R')
#select features
source("selectFeatures.R")

pred.xgb.prob <- predict(xgbFit,dataset.final, type="prob")["leave"]

x <- data.frame(Customer_ID=ids,EstimatedChurnProbability=pred.xgb.prob$leave)
write.table(x,"results/final_submissions.csv",sep=",",row.names=FALSE)
