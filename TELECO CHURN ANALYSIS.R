# TELCO CUSTOMER CHURN
# EDA BY: RAVNEET SINGH
setwd("C:/Users/DELL/Documents")
getwd()
#EDA LIBS
suppressMessages(library(gridExtra)) # arranging grids 
suppressMessages(library(tidyverse)) # for dplyr and visualizations
suppressMessages(library(ggplot2))  # for visualization
#ML LIBS
suppressMessages(library(randomForest)) #rf
suppressMessages(library(rpart)) #class tree
suppressMessages(library(rpart.plot))
suppressMessages(library(e1071)) #svm

df = read.csv("Customer-Churn-R-Project.csv", sep = ",", na.strings = "?")
View(df)

colnames(df) <- c("customerID", "gender", "seniorCitizen", "partner", "dependents", "tenure", "phoneService", "multipleLines", "internetService", "onlineSecurity", "onlineBackup", "deviceProtection", "techSupport", "streamingTV", "streamingMovies", "contract", "paperlessBilling", "paymentMethod", "monthlyCharges", "totalCharges", "churn")
summary(df)

df <- df %>% 
  mutate(seniorCitizen = as.factor(seniorCitizen)) %>% 
  na.omit() 
summary(df)


df <- df %>% dplyr::select(-customerID) %>%
  mutate_at(7,~as.factor(case_when(. =="No phone service"~"No",.=="No"~"No",.=="Yes"~"Yes"))) %>%
  mutate_at(c(9:14),~as.factor(case_when(.=="No internet service"~"No", .=="No"~"No", .=="Yes"~"Yes")))
summary(df)


df %>% group_by(gender) %>% 
  summarize("Number of observations" = n(), "Average Tenure in Months" = round(mean(tenure), 0), "Monthly Charges" = round(mean(monthlyCharges), 2), "Average Total Charges" = round(mean(totalCharges), 2))

df <-df %>% mutate(churn=as.factor(churn))

g1 <- df %>% ggplot(aes(x = churn, y = tenure, fill = fct_rev(churn))) + 
  geom_bar(stat = "summary", fun = "mean", alpha = 0.6, show.legend = F) + 
  stat_summary(aes(label = paste(round(..y.., 0), "months")), fun = mean, geom = "text", size = 3.5, vjust = -0.5) + 
  labs(title = "Average Tenure")

g2 <- df %>% ggplot(aes(x = churn, y = monthlyCharges, fill = fct_rev(churn))) + 
  geom_bar(stat = "summary", fun = "mean", alpha = 0.6, show.legend = F) + 
  stat_summary(aes(label = paste(round(..y.., 0), "$")), fun = mean, geom = "text", size = 3.5, vjust = -0.5) + 
  labs(title = "Average Monthly Charges")

grid.arrange(g1, g2, ncol = 2, nrow = 1)

g3 <- df %>% ggplot(aes(x = contract, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn by Contract Type", y = "Count of Contract Type")

g4 <- df %>% ggplot(aes(x = paymentMethod, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn by Payment Method", y = "Count of Payment Method")

g5 <- df %>% ggplot(aes(x = internetService, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn by Internet Service", y = "Count of Internet Service")

grid.arrange(g3, g4, g5)

g6 <- df %>% ggplot(aes(x = ifelse(seniorCitizen == 1, "Senior", "Not Senior"), fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Senior Citizens", y = "Count of Senior Citizen")

g7 <- df %>% ggplot(aes(x = gender, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Gender", y = "Count of Gender")

g8 <- df %>% ggplot(aes(x = partner, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Partner", y = "Count of Partner")

g9 <- df %>% ggplot(aes(x = dependents, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Dependents", y = "Count of Dependents")

grid.arrange(g6, g7, g8, g9)

g10 <- df %>% ggplot(aes(x = phoneService, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Phone Service", y = "Count of Phone Service")

g11 <- df %>% ggplot(aes(x = multipleLines, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Multiple Lines", y = "Count of Multiple Lines")

g12 <- df %>% ggplot(aes(x = onlineSecurity, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Online Security", y = "Count of Online Security")

g13 <- df %>% ggplot(aes(x = onlineBackup, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Online Backup", y = "Count of Online Backup")

grid.arrange(g10, g11, g12, g13, ncol = 2)

g14 <- df %>% ggplot(aes(x = deviceProtection, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Device Protection", y = "Count of Device Protection")

g15 <- df %>% ggplot(aes(x = techSupport, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Tech Support", y = "Count of Tech Support")

g16 <- df %>% ggplot(aes(x = streamingTV, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Streaming TV", y = "Count of Streaming TV")

g17 <- df %>% ggplot(aes(x = streamingMovies, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Streaming Movies", y = "Count of Streaming Movies")

g18 <- df %>% ggplot(aes(x = paperlessBilling, fill = fct_rev(churn))) + 
  geom_bar(alpha = 0.6) + labs(title = "Customer Churn on Paperless Billing", y = "Count of Paperless Billing")

grid.arrange(g14, g15, g16, g17, g18, ncol = 2)

grid.arrange(g10, g11, g12, g13, g14, g15, g16, g17, g18, ncol = 3)

df1 <- df %>% 
  dplyr::select(-totalCharges) 
glimpse(df1) 

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

norm <- as.data.frame(lapply(df1[, c(5, 18)], min_max_norm))
summary(norm) 


df_normed <- df1 %>% 
  dplyr::select(-c(5, 18)) %>% 
  cbind(norm)
glimpse(df_normed)

set.seed(123) 
train <- sample(nrow(df_normed), nrow(df_normed) * 0.8, replace = FALSE)
df_train <- df_normed[train,]
df_test <- df_normed[-train,]
dim(df_train)
dim(df_test)

#TRYING LOGISTIC REGRESSION
#why :-> better for binary classification and usually the thematic starting point


model_logit_train <- glm(churn ~ ., df_train, family = "binomial") 
summary(model_logit_train) 


df_test$logit_pred_prob <- predict(model_logit_train, df_test, type = "response")
df_test$logit_pred_class <- ifelse(df_test$logit_pred_prob > 0.5, "Yes", "No")
glimpse(df_test) 

#accuracy : overall correctness 82.44%
mean(df_test$logit_pred_class == df_test$churn) 

logit_ct <- table(df_test$logit_pred_class, df_test$churn)
logit_ct
logit_recall <- logit_ct[2, 2] / (logit_ct[2, 2] + logit_ct[1, 2]) 

#recall : the true positivity 56.04%
logit_recall 

#Decision tree
#why:  identifying patterns and relationships between different variables
#handle both numerical and categorical data

model_tree <- rpart(churn ~ ., df_train, method = "class", 
                    control = rpart.control(cp = 0.03)) 

#tree structure
rpart.plot(model_tree) 

df_test$tree_pred_prob <- predict(model_tree, df_test)[, 2]
df_test$tree_pred_class <- ifelse(df_test$tree_pred_prob > 0.5, "Yes", "No")

#accuracy : 80.95%
mean(df_test$tree_pred_class == df_test$churn) 

tree_ct <- table(df_test$tree_pred_class, df_test$churn)
tree_ct 
tree_recall <- tree_ct[2, 2] / (tree_ct[2, 2] + tree_ct[1, 2]) 

#tpr : 40.93%
tree_recall 

#Random forest
#why: effective in handling complex datasets and reducing overfitting.

model_rf <- randomForest(churn ~ ., df_train, ntree = 500, mtry = 2) 
df_test$rf_vote <- predict(model_rf, df_test, type = "class")

#accuracy : 82.01%
mean(df_test$rf_vote == df_test$churn) 



rf_ct <- table(df_test$rf_vote, df_test$churn)
rf_ct 
rf_recall <- rf_ct[2, 2] / (rf_ct[2, 2] + rf_ct[1, 2]) 

#recall : 49.17%
rf_recall 

#SVM
#why: dealing with complex decision boundaries and high-dimensional data

model_svm <- svm(churn ~ ., df_train, kernel = "linear", cost = 0.1) 
model_svm 
predicted_svm <- predict(model_svm, df_test)

#accuracy: 82.30%
mean(predicted_svm == df_test$churn) 


svm_ct <- table(predicted_svm, df_test$churn)
svm_ct 
svm_recall <- svm_ct[2, 2] / (svm_ct[2, 2] + svm_ct[1, 2]) 

#recall : 56.31%
svm_recall 



'''
#My recommendation : Use SVM for customer churn!!

why???

#best accuracy : logistic > svm > randomforest > classification tree
#best recall : svm > logistic > classification tree > randomforest


If the primary objective is to minimize the number of missed churn cases,
the model with the highest recall might be preferred, even if it comes 
witha slight compromise in overall accuracy
'''

