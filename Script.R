#Load Librarries
library(data.table)
library(caret)

#Function for mode
getmode <- function (x) {
  names(table(x))[which.max(table(na.omit(x)))]
}


#Set working directory
setwd("C:/Users/Aravind Atreya/Desktop/Analytics Vidhya/Loan Prediction")

#Read train and test files
df_train <- fread("train.txt",stringsAsFactors = TRUE)
df_test <- fread("test.txt",stringsAsFactors = TRUE)

Loan_Status <- df_train$Loan_Status
df_train$Loan_Status <- NULL

df_total <- rbind(df_train,df_test)
testrow_num <- nrow(df_train)

str(df_total)
head(df_total)

df_total$Loan_ID <- as.character(df_total$Loan_ID)
df_total$Credit_History <- as.factor(df_total$Credit_History)
table(df_total$Gender)
table(df_total$Married)
table(df_total$Dependents)
table(df_total$Education)
table(df_total$Self_Employed)
table(df_total$Property_Area)
table(df_total$Credit_History)

df_total$Gender[which(df_total$Gender=="")] <- NA
df_total$Married[which(df_total$Married=="")] <- NA
df_total$Dependents[which(df_total$Dependents=="")] <- NA
df_total$Self_Employed[which(df_total$Self_Employed=="")] <- NA

which(df_total$ApplicantIncome=="")
which(df_total$CoapplicantIncome=="")
which(df_total$LoanAmount=="")
which(df_total$Loan_Amount_Term=="")


colSums(sapply(df_total,is.na))
#Impute missing values with means and modes
df_total$Gender[is.na(df_total$Gender)] <- getmode(df_total$Gender)
df_total$Married[is.na(df_total$Married)] <- getmode(df_total$Married)
df_total$Dependents[is.na(df_total$Dependents)] <- getmode(df_total$Dependents)
df_total$Self_Employed[is.na(df_total$Self_Employed)] <- getmode(df_total$Self_Employed)
df_total$LoanAmount[is.na(df_total$LoanAmount)] <- mean(df_total$LoanAmount,na.rm = T)
df_total$Loan_Amount_Term[is.na(df_total$Loan_Amount_Term)] <- mean(df_total$Loan_Amount_Term,na.rm = T)
df_total$Credit_History[is.na(df_total$Credit_History)] <- getmode(df_total$Credit_History)

colSums(sapply(df_total,is.na))

#Split train and test
train_new <- df_total[1:testrow_num,]
test_new <- df_total[((testrow_num+1):nrow(df_total)),]
testLoanID <- test_new$Loan_ID
test_new$Loan_ID <- NULL

train_new <- cbind(train_new,Loan_Status)
train_new$Loan_ID <- NULL

fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 3,classProbs = TRUE)
gbm_fit <- train(Loan_Status~.,method="gbm",trControl= fitControl,metric="ROC",data=train_new)

results <- predict(gbm_fit,test_new)
sampleSubmission <- data.frame(Loan_ID=testLoanID,Loan_Status=results)
write.csv(sampleSubmission,"Sample_Submission.csv",row.names = F)

