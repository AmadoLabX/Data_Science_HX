
#Setup-download data

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(pander)) install.packages("pander", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")

#https://www.foreignlaborcert.doleta.gov/pdf/PerformanceData/2018/H-1B_Disclosure_Data_FY2018_EOY.xlsx

temp <- tempfile()
download.file("https://www.foreignlaborcert.doleta.gov/pdf/PerformanceData/2018/H-1B_Disclosure_Data_FY2018_EOY.xlsx",temp)

data <- read.xlsx(temp)

# Preparing the data
data_aus <- data %>% filter(VISA_CLASS=="E-3 Australian") %>% select(-c("VISA_CLASS","CASE_NUMBER","EMPLOYER_POSTAL_CODE","EMPLOYER_PHONE","EMPLOYER_ADDRESS","EMPLOYER_PHONE_EXT","WORKSITE_POSTAL_CODE","WORKSITE_COUNTY","WORKSITE_CITY","EMPLOYER_NAME","EMPLOYER_BUSINESS_DBA","EMPLOYER_BUSINESS_DBA","EMPLOYER_CITY","AGENT_ATTORNEY_NAME"))

# Create train set, test set.

# New Field
data_aus$contract_days <- abs(data_aus$EMPLOYMENT_END_DATE - data_aus$EMPLOYMENT_START_DATE)

library(caret)
set.seed(754)
test_index <- createDataPartition(y = data_aus$CASE_STATUS, times = 1, p = 0.2, list = FALSE)
train_set <- data_aus[-test_index,]
test_set <- data_aus[test_index,]

rm(test_index)

# preparing the data for training the models

test_set$CASE_STATUS <- as.factor(test_set$CASE_STATUS)
test_set$AGENT_REPRESENTING_EMPLOYER <- as.factor(test_set$AGENT_REPRESENTING_EMPLOYER)
test_set$AMENDED_PETITION <- as.numeric(test_set$AMENDED_PETITION)
test_set$PW_WAGE_LEVEL <- as.factor(test_set$PW_WAGE_LEVEL)
test_set$EMPLOYER_COUNTRY <- as.factor(test_set$EMPLOYER_COUNTRY)
test_set$WAGE_UNIT_OF_PAY <- as.factor(test_set$WAGE_UNIT_OF_PAY)
test_set$FULL_TIME_POSITION <- as.factor(test_set$FULL_TIME_POSITION)
test_set$PW_SOURCE <- as.factor(test_set$PW_SOURCE)
test_set$PW_SOURCE_YEAR <- as.factor(test_set$PW_SOURCE_YEAR)
test_set$NEW_CONCURRENT_EMP <- as.numeric(test_set$NEW_CONCURRENT_EMP)
test_set$CHANGE_EMPLOYER <- as.numeric(test_set$CHANGE_EMPLOYER)
test_set$CHANGE_PREVIOUS_EMPLOYMENT <- as.numeric(test_set$CHANGE_PREVIOUS_EMPLOYMENT)
test_set$CONTINUED_EMPLOYMENT <- as.numeric(test_set$CONTINUED_EMPLOYMENT)
test_set$NEW_EMPLOYMENT <- as.numeric(test_set$NEW_EMPLOYMENT)
test_set$contract_days <- as.numeric(test_set$contract_days)

ind <- is.na.data.frame(test_set$WAGE_UNIT_OF_PAY)
index1 <- which(ind)
ind <- is.na.data.frame(test_set$EMPLOYER_COUNTRY)
index2 <- which(ind)
ind <- is.na.data.frame(test_set$AMENDED_PETITION) 
index3 <- which(ind)
ind <- is.na.data.frame(test_set$FULL_TIME_POSITION)
index4 <- which(ind)
ind <- is.na.data.frame(test_set$AGENT_REPRESENTING_EMPLOYER)
index5 <- which(ind)
ind <- is.na.data.frame(test_set$PW_WAGE_LEVEL)
index6 <- which(ind)
ind <- is.na.data.frame(test_set$PW_SOURCE)
index7 <- which(ind)
ind <- is.na.data.frame(test_set$PW_SOURCE_YEAR)
index8 <- which(ind)
ind <- is.na.data.frame(test_set$NEW_CONCURRENT_EMP)
index9 <- which(ind)
ind <- is.na.data.frame(test_set$CHANGE_EMPLOYER)
index10 <- which(ind)
ind <- is.na.data.frame(test_set$CHANGE_PREVIOUS_EMPLOYMENT)
index11 <- which(ind)
ind <- is.na.data.frame(test_set$CONTINUED_EMPLOYMENT)
index12 <- which(ind)
ind <- is.na.data.frame(test_set$NEW_EMPLOYMENT)
index13 <- which(ind)
ind <- is.na.data.frame(test_set$PREVAILING_WAGE)
index14 <- which(ind)
ind <- is.na.data.frame(test_set$contract_days)
index15 <- which(ind)
ind <- is.na.data.frame(test_set$TOTAL_WORKERS)
index16 <- which(ind)
ind <- is.na.data.frame(test_set$WAGE_RATE_OF_PAY_FROM)
index17 <- which(ind)
ind <- is.na.data.frame(test_set$WAGE_RATE_OF_PAY_TO)
index18 <- which(ind)

# a few rows with NA are removed

test_set_na <- test_set[-c(index1,index2,index3,index4,index5,index6,index7,index8,index9,index10,index11,index12,index13,index14,index15,index16,index17,index18),]

train_set$CASE_STATUS <- as.factor(train_set$CASE_STATUS)
train_set$AGENT_REPRESENTING_EMPLOYER <- as.factor(train_set$AGENT_REPRESENTING_EMPLOYER)
train_set$AMENDED_PETITION <- as.numeric(train_set$AMENDED_PETITION)
train_set$PW_WAGE_LEVEL <- as.factor(train_set$PW_WAGE_LEVEL)
train_set$EMPLOYER_COUNTRY <- as.factor(train_set$EMPLOYER_COUNTRY)
train_set$WAGE_UNIT_OF_PAY <- as.factor(train_set$WAGE_UNIT_OF_PAY)
train_set$FULL_TIME_POSITION <- as.factor(train_set$FULL_TIME_POSITION)
train_set$PW_SOURCE <- as.factor(train_set$PW_SOURCE)
train_set$PW_SOURCE_YEAR <- as.factor(train_set$PW_SOURCE_YEAR)
train_set$NEW_CONCURRENT_EMP <- as.numeric(train_set$NEW_CONCURRENT_EMP)
train_set$CHANGE_EMPLOYER <- as.numeric(train_set$CHANGE_EMPLOYER)
train_set$CHANGE_PREVIOUS_EMPLOYMENT <- as.numeric(train_set$CHANGE_PREVIOUS_EMPLOYMENT)
train_set$CONTINUED_EMPLOYMENT <- as.numeric(train_set$CONTINUED_EMPLOYMENT)
train_set$NEW_EMPLOYMENT <- as.numeric(train_set$NEW_EMPLOYMENT)
train_set$contract_days <- as.numeric(train_set$contract_days)

ind <- is.na.data.frame(train_set$WAGE_UNIT_OF_PAY)
index1 <- which(ind)
ind <- is.na.data.frame(train_set$EMPLOYER_COUNTRY)
index2 <- which(ind)
ind <- is.na.data.frame(train_set$AMENDED_PETITION) 
index3 <- which(ind)
ind <- is.na.data.frame(train_set$FULL_TIME_POSITION)
index4 <- which(ind)
ind <- is.na.data.frame(train_set$AGENT_REPRESENTING_EMPLOYER)
index5 <- which(ind)
ind <- is.na.data.frame(train_set$PW_WAGE_LEVEL)
index6 <- which(ind)
ind <- is.na.data.frame(train_set$PW_SOURCE)
index7 <- which(ind)
ind <- is.na.data.frame(train_set$PW_SOURCE_YEAR)
index8 <- which(ind)
ind <- is.na.data.frame(train_set$NEW_CONCURRENT_EMP)
index9 <- which(ind)
ind <- is.na.data.frame(train_set$CHANGE_EMPLOYER)
index10 <- which(ind)
ind <- is.na.data.frame(train_set$CHANGE_PREVIOUS_EMPLOYMENT)
index11 <- which(ind)
ind <- is.na.data.frame(train_set$CONTINUED_EMPLOYMENT)
index12 <- which(ind)
ind <- is.na.data.frame(train_set$NEW_EMPLOYMENT)
index13 <- which(ind)
ind <- is.na.data.frame(train_set$PREVAILING_WAGE)
index14 <- which(ind)
ind <- is.na.data.frame(train_set$contract_days)
index15 <- which(ind)
ind <- is.na.data.frame(train_set$TOTAL_WORKERS)
index16 <- which(ind)
ind <- is.na.data.frame(train_set$WAGE_RATE_OF_PAY_FROM)
index17 <- which(ind)
ind <- is.na.data.frame(train_set$WAGE_RATE_OF_PAY_TO)
index18 <- which(ind)

# a few rows with NA are removed

train_set_na <- train_set[-c(index1,index2,index3,index4,index5,index6,index7,index8,index9,index10,index11,index12,index13,index14,index15,index16,index17,index18),]

rm(ind,index1,index2,index3,index4,index5,index6,index7,index8,index9,index10,index11,index12,index13,index14,index15,index16,index17,index18)

rm(train_set,test_set)

# This line fixs an incompatibility issue between _RandodomForest_ and the _predict_ funtion
test_set_na <- rbind(train_set_na[1, ] , test_set_na)
test_set_na <- test_set_na[-1,]

# Caret Package

models <- c("svmRadial","ranger","wsrf","kknn")
models_long_name <- c("Support_Vector_Machine_Radial","ranger", "Weighted_Subspace_Random_Forest","k-Nearest Neighbors")
print(models_long_name) 

length(models)

fits <- lapply(models, function(model){ 
  train(CASE_STATUS ~WAGE_UNIT_OF_PAY+AGENT_REPRESENTING_EMPLOYER+PW_WAGE_LEVEL+PW_SOURCE+PW_SOURCE_YEAR+CHANGE_EMPLOYER+CHANGE_PREVIOUS_EMPLOYMENT+CONTINUED_EMPLOYMENT+PREVAILING_WAGE+WAGE_RATE_OF_PAY_FROM+WAGE_RATE_OF_PAY_TO+contract_days, method = model, data=train_set_na)
})

times <- length(models)

res_test2 <- confusionMatrix(predict(fits[[1]], data=train_set_na),as.factor(train_set_na$CASE_STATUS))
res_train2 <- confusionMatrix(predict(fits[[1]], data=train_set_na),as.factor(train_set_na$CASE_STATUS))

# Caret Results on train set
res <- as.numeric()
Accuracy_test_set <- as.numeric()
Accuracy_train_set <- as.numeric()

resultss <- for(i in 1:times){
  
  res_train2[[i]] <- confusionMatrix(predict(fits[[i]], data=train_set_na),as.factor(train_set_na$CASE_STATUS))
  res_test2[[i]] <- confusionMatrix(predict(fits[[i]], newdata=test_set_na),as.factor(test_set_na$CASE_STATUS))
  Accuracy_train_set[i] <- res_train2[[i]]$overall["Accuracy"]
  Accuracy_test_set[i] <- res_test2[[i]]$overall["Accuracy"]  
}

Accuracy <- (Accuracy_train_set) %>% t()
colnames(Accuracy) <- models
Accuracy %>% pander()

# _Croos Validation_ on _Caret approach_

library(caret)
set.seed(756)
n_times <- 50  # Consider to update the number of samples to accelerate the upcoming chunk of code

test_index <- createDataPartition(y = train_set_na$CASE_STATUS, times = n_times, p = 2.5/10, list = FALSE)

times <- length(models)

res_test2 <- confusionMatrix(predict(fits[[1]], data=train_set_na),as.factor(train_set_na$CASE_STATUS))
res_train2 <- confusionMatrix(predict(fits[[1]], data=train_set_na),as.factor(train_set_na$CASE_STATUS))

res <- as.numeric()
Accuracy_test_set <- as.numeric()
Accuracy_train_set <- as.numeric()
Accuracy_test_set_all <- as.numeric()
Acc <- as.numeric()
Acc_test <- as.numeric()

resultss <- for(i in 1:times){
  
  for(j in 1:n_times){
    #track_time[i] <- Sys.time()  # "track_time"
    
    train_set_rf <- data_aus[-test_index[,j],]
    dim(train_set_rf)
    test_set_rf <- data_aus[test_index[,j],]
    dim(test_set_rf)
    
    test_set_rf$CASE_STATUS <- as.factor(test_set_rf$CASE_STATUS)
    test_set_rf$AGENT_REPRESENTING_EMPLOYER <- as.factor(test_set_rf$AGENT_REPRESENTING_EMPLOYER)
    test_set_rf$AMENDED_PETITION <- as.numeric(test_set_rf$AMENDED_PETITION)
    test_set_rf$PW_WAGE_LEVEL <- as.factor(test_set_rf$PW_WAGE_LEVEL)
    test_set_rf$EMPLOYER_COUNTRY <- as.factor(test_set_rf$EMPLOYER_COUNTRY)
    test_set_rf$WAGE_UNIT_OF_PAY <- as.factor(test_set_rf$WAGE_UNIT_OF_PAY)
    test_set_rf$FULL_TIME_POSITION <- as.factor(test_set_rf$FULL_TIME_POSITION)
    test_set_rf$PW_SOURCE <- as.factor(test_set_rf$PW_SOURCE)
    test_set_rf$PW_SOURCE_YEAR <- as.factor(test_set_rf$PW_SOURCE_YEAR)
    test_set_rf$NEW_CONCURRENT_EMP <- as.numeric(test_set_rf$NEW_CONCURRENT_EMP)
    test_set_rf$CHANGE_EMPLOYER <- as.numeric(test_set_rf$CHANGE_EMPLOYER)
    test_set_rf$CHANGE_PREVIOUS_EMPLOYMENT <- as.numeric(test_set_rf$CHANGE_PREVIOUS_EMPLOYMENT)
    test_set_rf$CONTINUED_EMPLOYMENT <- as.numeric(test_set_rf$CONTINUED_EMPLOYMENT)
    test_set_rf$NEW_EMPLOYMENT <- as.numeric(test_set_rf$NEW_EMPLOYMENT)
    test_set_rf$contract_days <- as.numeric(test_set_rf$contract_days)
    
    ind <- is.na.data.frame(test_set_rf$WAGE_UNIT_OF_PAY)
    index1 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$EMPLOYER_COUNTRY)
    index2 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$AMENDED_PETITION) 
    index3 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$FULL_TIME_POSITION)
    index4 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$AGENT_REPRESENTING_EMPLOYER)
    index5 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$PW_WAGE_LEVEL)
    index6 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$PW_SOURCE)
    index7 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$PW_SOURCE_YEAR)
    index8 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$NEW_CONCURRENT_EMP)
    index9 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$CHANGE_EMPLOYER)
    index10 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$CHANGE_PREVIOUS_EMPLOYMENT)
    index11 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$CONTINUED_EMPLOYMENT)
    index12 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$NEW_EMPLOYMENT)
    index13 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$PREVAILING_WAGE)
    index14 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$contract_days)
    index15 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$TOTAL_WORKERS)
    index16 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$WAGE_RATE_OF_PAY_FROM)
    index17 <- which(ind)
    ind <- is.na.data.frame(test_set_rf$WAGE_RATE_OF_PAY_TO)
    index18 <- which(ind)
    
    # a few rows with NA are removed
    
    test_set_rf_na <- test_set_rf[-c(index1,index2,index3,index4,index5,index6,index7,index8,index9,index10,index11,index12,index13,index14,index15,index16,index17,index18),]
    
    # This line fixs an incompatibility issue between _RandodomForest_ and the _predict_ funtion
    test_set_rf_na <- rbind(train_set_na[1, ] , test_set_rf_na)
    test_set_rf_na <- test_set_rf_na[-1,] 
    
    
    res_test2[[j]] <- confusionMatrix(predict(fits[[i]], newdata=test_set_rf_na),as.factor(test_set_rf_na$CASE_STATUS))
    
    Accuracy_test_set[j] <- res_test2[[j]]$overall["Accuracy"]
    
  } 
  
  Accuracy_test_set_all[[i]] <- mean(Accuracy_test_set)
  
  #pb <- txtProgressBar(0, times, style=3) # "pb"
  #setTxtProgressBar(pb, i) # setTxtProgressBar
}

Accuracy <- Accuracy_test_set_all %>% t()
colnames(Accuracy) <- models
rownames(Accuracy) <- "Mean-Accuracy"
Accuracy %>% pander()

# predict(fits[[1]], data=train_set_na)
# test_set$CASE_STATUS
# max(fits[[1]][["results"]][["Accuracy"]])

# FINAL RESULT
Accuracy_test_set[3] %>% pander() 

