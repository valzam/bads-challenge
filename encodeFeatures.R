encodeFeatures <- function(x){
  # This function properly encodes all features
  # First it turns all the usage based features into numerical features
  # Then it encodes the categorical variables into either dummy or numerical rank variables
  # Some categorical variables are left out at this stage allready
  # Mostly because the number of missing cases made them unworkable or the number of levels is very high with no obvious benefit
  # Like the state the respondent lives in.
  
  
  # All the numerical features from the spreadsheet. All usage based
  numerical_features <-c("ADJMOU","ADJQTY","ADJREV","ATTEMPT_MEAN","ATTEMPT_RANGE","AVG3MOU","AVG3QTY","AVG3REV","AVG6MOU","AVG6QTY","AVG6REV","AVGMOU","AVGQTY","AVGREV","BLCK_DAT_MEAN","BLCK_DAT_RANGE","BLCK_VCE_MEAN","BLCK_VCE_RANGE","CALLFWDV_MEAN","CALLFWDV_RANGE","CALLWAIT_MEAN","CALLWAIT_RANGE","CC_MOU_MEAN","CC_MOU_RANGE","CCRNDMOU_MEAN","CCRNDMOU_RANGE","CHANGE_MOU","CHANGE_REV","COMP_DAT_MEAN","COMP_DAT_RANGE","COMP_VCE_MEAN","COMP_VCE_RANGE","COMPLETE_MEAN","COMPLETE_RANGE","CUSTCARE_MEAN","CUSTCARE_RANGE","DA_MEAN","DA_RANGE","DATOVR_MEAN","DATOVR_RANGE","DROP_BLK_MEAN","DROP_BLK_RANGE","DROP_DAT_MEAN","DROP_DAT_RANGE","DROP_VCE_MEAN","DROP_VCE_RANGE","EQPDAYS","INONEMIN_MEAN","INONEMIN_RANGE","IWYLIS_VCE_MEAN","IWYLIS_VCE_RANGE","MONTHS","MOU_CDAT_MEAN","MOU_CDAT_RANGE","MOU_CVCE_MEAN","MOU_CVCE_RANGE","MOU_MEAN","MOU_OPKD_MEAN","MOU_OPKD_RANGE","MOU_OPKV_MEAN","MOU_OPKV_RANGE","MOU_PEAD_MEAN","MOU_PEAD_RANGE","MOU_PEAV_MEAN","MOU_PEAV_RANGE","MOU_RANGE","MOU_RVCE_MEAN","MOU_RVCE_RANGE","MOUIWYLISV_MEAN","MOUIWYLISV_RANGE","MOUOWYLISV_MEAN","MOUOWYLISV_RANGE","OWYLIS_VCE_MEAN","OWYLIS_VCE_RANGE","OPK_DAT_MEAN","OPK_DAT_RANGE","OPK_VCE_MEAN","OPK_VCE_RANGE","OVRMOU_MEAN","OVRMOU_RANGE","OVRREV_MEAN","OVRREV_RANGE","PEAK_DAT_MEAN","PEAK_DAT_RANGE","PEAK_VCE_MEAN","PEAK_VCE_RANGE","PLCD_DAT_MEAN","PLCD_DAT_RANGE","PLCD_VCE_MEAN","PLCD_VCE_RANGE","RECV_SMS_MEAN","RECV_SMS_RANGE","RECV_VCE_MEAN","RECV_VCE_RANGE","RETDAYS","REV_MEAN","REV_RANGE","RMCALLS","RMMOU","RMREV","ROAM_MEAN","ROAM_RANGE","THREEWAY_MEAN","THREEWAY_RANGE","TOTCALLS","TOTMOU","TOTMRC_MEAN","TOTMRC_RANGE","TOTREV","UNAN_DAT_MEAN","UNAN_DAT_RANGE","UNAN_VCE_MEAN","UNAN_VCE_RANGE","VCEOVR_MEAN","VCEOVR_RANGE")
  
  # All the categorical features from the spreadsheet. Most of them will be transformed into dummy variables, numerical variables or dropped
  categorical_features <- c("ACTVSUBS","ADULTS","AGE1","AGE2","AREA","ASL_FLAG","CAR_BUY","CARTYPE","CHILDREN","CHURN","CRCLSCOD","CREDITCD","CRTCOUNT","CSA","CUSTOMER_ID","DIV_TYPE","DUALBAND","DWLLSIZE","DWLLTYPE","EDUC1","ETHNIC","FORGNTVL","HND_PRICE","HHSTATIN","HND_WEBCAP","INCOME","INFOBASE","KID0_2","KID3_5","KID6_10","KID11_15","KID16_17","LAST_SWAP","LOR","MAILFLAG","MAILORDR","MAILRESP","MARITAL","MODELS","MTRCYCLE","NEW_CELL","NUMBCARS","OCCU1","OWNRENT","PCOWNER","PHONES","PRE_HND_PRICE","PRIZM_SOCIAL_ONE","PROPTYPE","REF_QTY","REFURB_NEW","RV","SOLFLAG","TOT_ACPT","TOT_RET","TRUCK","UNIQSUBS","WRKWOMAN")
  
  # Encode all numerical features as such
  x[,names(x) %in% numerical_features] <- apply(x[,names(x) %in% numerical_features],2,as.numeric)
  
  # Save the categorical features seperately and delete from dataframe                                               
  cats <- x[,names(x) %in% categorical_features]
  x <- x[,!names(x) %in% categorical_features]
  
  # Encode active subs as numerical
  cats$ACTIVESUBS <- as.numeric(cats$ACTVSUBS)
  
  # Dummyfy spending limit
  cats$SPENDING_LIMIT <- 0
  cats[cats$ASL_FLAG=="Y",]$SPENDING_LIMIT <- 1
  
  # Dummyfy new car
  cats$NEW_CAR<- 0
  cats[cats$CAR_BUY=="New",]$NEW_CAR <- 1
  
  # Dummyfy credit class
  cats$GOOD_CREDIT_SCORE <- 0
  cats[cats$CRCLSCOD  %in% c("A","A2","AA","B","B2","BA"),]$GOOD_CREDIT_SCORE <- 1
  
  cats$BAD_CREDIT_SCORE <- 0
  cats[cats$CRCLSCOD  %in% c("Z1","Z2","Z3","Z4","Z5","ZA","ZB","ZF","ZY","ZZ"),]$BAD_CREDIT_SCORE <- 1
  
  # Dummyfy credit card indicator
  cats$HAS_CREDIT_CARD <- 0
  cats[cats$CREDITCD=="Y",]$HAS_CREDIT_CARD <- 1
  
  # make numerical out of adjustment made to credit rating
  cats[is.na(cats$CRTCOUNT),]$CRTCOUNT <- 0
  cats$CREDIT_ADJUSTED <- as.numeric(cats$CRTCOUNT)
  
  # Dummyfy additional services
  cats$LONG_DISTANCE_SERVICE <- 0
  cats$LOCAL_PHONE_SERVICE <- 0
  cats[cats$DIV_TYPE=="LDD",]$LONG_DISTANCE_SERVICE <- 1
  cats[cats$DIV_TYPE=="BTH",]$LONG_DISTANCE_SERVICE <- 1
  cats[cats$DIV_TYPE=="LTD",]$LOCAL_PHONE_SERVICE <- 1
  cats[cats$DIV_TYPE=="BTH",]$LOCAL_PHONE_SERVICE <- 1
  
  # Dummyfy dualband
  cats$DB <- 0
  cats[cats$DUALBAND=="Y",]$DB <- 1
  cats[cats$DUALBAND=="T",]$DB <- 1
  
  # dummyfy foreign travel
  cats$FOREIGN_TRAVEL <- ifelse(!is.na(cats$FORGNTVL) & cats$FORGNTVL==1 , 1 , 0)
  
  # Convert current handset price to numerical
  cats$CURRENT_PHONE_PRICE <- as.numeric(cats$HND_PRICE)
  cats[is.na(cats$CURRENT_PHONE_PRICE),]$CURRENT_PHONE_PRICE <- median(cats$CURRENT_PHONE_PRICE, na.rm = TRUE)
  
  # Dummy db information about customer
  cats$FULL_INFORMATION <- 0
  cats[cats$HHSTATIN=="C",]$FULL_INFORMATION <- 1
  cats[cats$HHSTATIN=="I",]$FULL_INFORMATION <- 1
  
  # Convert estimated income to numerical
  cats$ESTIMATED_INCOME <- as.numeric(cats$INCOME)
  cats[is.na(cats$ESTIMATED_INCOME),]$ESTIMATED_INCOME<- median(cats$ESTIMATED_INCOME, na.rm = TRUE)
  
  # Get the years of the last swapped variable
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  years <- substrRight(cats$LAST_SWAP,4)
  cats$LAST_PHONE_SWAP = 0
  cats[years=="1999",]$LAST_PHONE_SWAP<- 2
  cats[years=="2000",]$LAST_PHONE_SWAP<- 1
  
  # dummyfy do not mail flag
  cats$DO_NOT_MAIL <- 0
  cats[cats$MAILFLAG=="Y",]$DO_NOT_MAIL <- 1
  
  # dummyfy mail responder
  cats$RESPOND_TO_MAIL <- 0
  cats[cats$MAILRESP=="R",]$RESPOND_TO_MAIL <- 1
  
  # Make number of models numeric
  cats$MODELS_ISSUED <- as.numeric(cats$MODELS)
  
  # make binary for mobile home owners
  cats$OWNS_MOBILE_HOME <- 0
  cats[cats$PROPTYPE=="M",]$OWNS_MOBILE_HOME <- 1
  
  # make number of referrals numeric
  cats[is.na(cats$REF_QTY),]$REF_QTY <- 0
  cats$REF_QTY_NUM <- as.numeric(cats$REF_QTY)
  
  # dummyfy refurbished phone
  cats$REFURBISHED <- 0
  cats[cats$REFURB_NEW=="R",]$REFURBISHED <- 1
  
  # dummyfy RV
  cats$OWNS_RV <- ifelse(!is.na(cats$RV) & cats$RV==1 , 1 , 0)
  
  # dummyfy do not solicit
  cats$DO_NOT_SOLICIT <- 0
  cats[cats$SOLFLAG=="N",]$DO_NOT_SOLICIT <- 1
  
  # encode if they have received and declined offers from retention team
  cats[is.na(cats$TOT_ACPT),]$TOT_ACPT <- 99
  cats$TOT_ACPT <- as.numeric(cats$TOT_ACPT)
  cats$RECEIVED_OFFER <- 1
  cats[cats$TOT_ACPT==99,]$RECEIVED_OFFER <- 0
  
  cats$DECLINED_OFFER <- 0
  cats[cats$TOT_ACPT==0,]$DECLINED_OFFER <- 1
  
  cats$NUMBER_OF_OFFERS <- 0
  cats$NUMBER_OF_OFFERS <- ifelse(cats$RECEIVED_OFFER==1,cats$TOT_ACPT,0)
  
  # make numerical the number of retention calls they have received
  cats$NUMBER_OF_RETENTION_CALLS <- as.numeric(cats$TOT_RET)
  cats[is.na(cats$TOT_RET),]$NUMBER_OF_RETENTION_CALLS <- 0
  
  # make numerical the number of subs they have
  cats$NUMBER_OF_SUBS <- as.numeric(cats$UNIQSUBS)
  
  id <- cats$CUSTOMER_ID
  
  if ("CHURN" %in% colnames(cats)) {
    churn <- cats$CHURN
    churn <- factor(churn,levels=c(0,1),labels=c("stay","leave"))
    cats <- cats[,!names(cats) %in% categorical_features]
    result <- cbind(x,cats,churn,id)
  } else {
    cats <- cats[,!names(cats) %in% categorical_features]
    result <- cbind(x,cats,id)
  }

  return( result)
  
}