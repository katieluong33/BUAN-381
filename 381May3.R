
# BUAN 381 Final Project
# Brianna Floyd, Katie Luong, Anastasia Lomtadze, Sophia Saenger

### Load libraries
library(psych)
library(polycor)
library(tidyverse)
library(mgcv)
library(e1071) #SVM LIBRARY
library(caret) #FOR confusionMatrix()
library(parsnip)
library(rpart)
library(rpart.plot)
library(randomForest)
library(Metrics)
library(pROC)

set.seed(314)  # for reproducibility

### Import data
df_p <- read.table('https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/student-por.csv', header = TRUE, sep = ";")
df_m <- read.table("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/student-mat.csv", header = TRUE, sep = ";")

### Data cleaning
df_p$class <- "p"
df_p$class <- as.factor(df_p$class)
df_m$class <- "m"
df_m$class <- as.factor(df_m$class)
df <- rbind(df_p,df_m)

df <- na.omit(df)

df[df == "yes"] <- 1
df[df == "no"] <- 0
df$activities <- as.numeric(df$activities)

View(df)

print(dim(df))
print(colnames(df))

numeric_data <- df[sapply(df, is.numeric)]

### Correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
print(cor_matrix)
print(nrow(df))

### Split the data - training and remaining
train_index <- createDataPartition(df$G3, p = 0.70, list = FALSE)
train_data <- df[train_index, ]
remaining <- df[-train_index, ]

### Split the data - validation and test
valid_index <- createDataPartition(remaining$G3, p = 0.5, list = FALSE)
valid_data <- remaining[valid_index, ]
test_data <- remaining[-valid_index, ]

########## REGRESSION ##########
### Bivariate model
bivariate_model <- lm(G3 ~ studytime, data = train_data)
summary(bivariate_model)

bivariate_pred_train <- predict(bivariate_model, train_data)
bivariate_pred_valid <- predict(bivariate_model, valid_data)

bivariate_rmse_train <- rmse(train_data$G3, bivariate_pred_train)
bivariate_rmse_valid <- rmse(valid_data$G3, bivariate_pred_valid)

### Multivariate model
multivariate_model <- lm(G3 ~ ., data = train_data)
summary(multivariate_model)

multivariate_pred_train <- predict(multivariate_model, train_data)
multivariate_pred_valid <- predict(multivariate_model, valid_data)

multivariate_rmse_train <- rmse(train_data$G3, multivariate_pred_train)
multivariate_rmse_valid <- rmse(valid_data$G3, multivariate_pred_valid)

### Spline model
spline_model <- gam(G3 ~ s(absences), data = train_data, family = gaussian)
summary(spline_model)

spline_pred_train <- predict(spline_model, train_data)
spline_pred_valid <- predict(spline_model, valid_data)

spline_rmse_train <- rmse(train_data$G3, spline_pred_train)
spline_rmse_valid <- rmse(valid_data$G3, spline_pred_valid)

### SVM model
svm_model<- svm(G3 ~ ., 
                data = train_data, 
                type = "eps-regression",
                kernel = "radial",
                cost=1,                   #REGULARIZATION PARAMETER
                gamma = 1/(ncol(df)-1), #DEFAULT KERNEL PARAMETER
                coef0 = 0,                    #DEFAULT KERNEL PARAMETER
                degree=2,                     #POLYNOMIAL KERNEL PARAMETER
                scale = FALSE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)
summary(svm_model)

svm_pred_train <- predict(svm_model, train_data)
svm_pred_valid <- predict(svm_model, valid_data)

svm_rmse_train <- rmse(train_data$G3, svm_pred_train)
svm_rmse_valid <- rmse(valid_data$G3, svm_pred_valid)

### Decision Tree model
tree_model <- train(G3 ~ ., data = train_data, method = "rpart")
print(tree_model)

tree_pred_train <- predict(tree_model, train_data)
tree_pred_valid <- predict(tree_model, valid_data)

tree_rmse_train <- rmse(train_data$G3, tree_pred_train)
tree_rmse_valid <- rmse(valid_data$G3, tree_pred_valid)

### Random Forest model
forest_model <- train(
  G3 ~ .,                    # Target variable (G3) and predictors
  data = train_data,                 # Data used for training
  method = "rf",             # Random Forest method
  importance = TRUE          # Keep track of variable importance
)
print(forest_model)

forest_pred_train <- predict(forest_model, train_data)
forest_pred_valid <- predict(forest_model, valid_data)

forest_rmse_train <- rmse(train_data$G3, forest_pred_train)
forest_rmse_valid <- rmse(valid_data$G3, forest_pred_valid)

### 2nd Random Forest model
forest_model_2 <- randomForest(
  G3 ~ ., 
  data = train_data, 
  ntree = 500, 
  mtry = 4, 
  maxnodes = 20,   # restrict tree size
  importance = TRUE
)
print(forest_model_2)

forest_2_pred_train <- predict(forest_model_2, train_data)
forest_2_pred_valid <- predict(forest_model_2, valid_data)

forest_2_rmse_train <- rmse(train_data$G3, forest_2_pred_train)
forest_2_rmse_valid <- rmse(valid_data$G3, forest_2_pred_valid)

### Table to compare the models
Validation_Table <- as.table(matrix(c(bivariate_rmse_train, multivariate_rmse_train, spline_rmse_train, 
                                      svm_rmse_train, tree_rmse_train, forest_rmse_train, forest_2_rmse_train, 
                                      bivariate_rmse_valid, multivariate_rmse_valid, spline_rmse_valid, 
                                      svm_rmse_valid, tree_rmse_valid, forest_rmse_valid, forest_2_rmse_valid), ncol=7, byrow=TRUE))

colnames(Validation_Table) <- c('Bivariate', 'Multivariate', 'Spline', 'SVM','Decision Tree', 'Random Forest', 'Random Forest 2')
rownames(Validation_Table) <- c('RMSE_IN', 'RMSE_OUT')
Validation_Table

# Test on best model
forest_pred_test <- predict(forest_model, test_data)
forest_rmse_test <- rmse(train_data$G3, forest_pred_test)
print(forest_rmse_test)

########## CLASSIFICATION ##########
### Logistic regression model
logit_model <- glm(activities ~ ., data = train_data, family = binomial(link="logit"), )
summary(logit_model)

# Accuracy
logit_prob_train <- predict(logit_model, newdata = train_data, type = "response")
logit_pred_train <- ifelse(logit_prob_train > 0.5, 1, 0)
logit_acc_train <- mean(logit_pred_train == train_data$activities)

logit_prob_valid <- predict(logit_model, newdata = valid_data, type = "response")
logit_pred_valid <- ifelse(logit_prob_valid > 0.5, 1, 0)
logit_acc_valid <- mean(logit_pred_valid == valid_data$activities)

# ROC and AUC
logit_roc_train <- roc(train_data$activities, logit_prob_train)
plot(logit_roc_train)
logit_auc_train <- auc(logit_roc_train)

logit_roc_valid <- roc(valid_data$activities, logit_prob_valid)
plot(logit_roc_valid)
logit_auc_valid <- auc(logit_roc_valid)

### Probit model
probit_model <- glm(activities ~ ., data = train_data, family = binomial(link="probit"), )
summary(probit_model)

# Accuracy
probit_prob_train <- predict(probit_model, newdata = train_data, type = "response")
probit_pred_train <- ifelse(probit_prob_train > 0.5, 1, 0)
probit_acc_train <- mean(probit_pred_train == train_data$activities)

probit_prob_valid <- predict(probit_model, newdata = valid_data, type = "response")
probit_pred_valid <- ifelse(probit_prob_valid > 0.5, 1, 0)
probit_acc_valid <- mean(probit_pred_valid == valid_data$activities)

# ROC and AUC
probit_roc_train <- roc(train_data$activities, probit_prob_train)
plot(probit_roc_train)
probit_auc_train <- auc(probit_roc_train)

probit_roc_valid <- roc(valid_data$activities, probit_prob_valid)
plot(probit_roc_valid)
probit_auc_valid <- auc(probit_roc_valid)

### SVM model
svm_model_2<- svm(activities ~ ., 
                data = train_data, 
                type = "C-classification",
                kernel = "radial",
                cost=1,                   #REGULARIZATION PARAMETER
                gamma = 1/(ncol(df)-1), #DEFAULT KERNEL PARAMETER
                coef0 = 0,                    #DEFAULT KERNEL PARAMETER
                degree=2,                     #POLYNOMIAL KERNEL PARAMETER
                scale = FALSE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)
summary(svm_model_2)

# Accuracy
svm_2_pred_train <- predict(svm_model_2, newdata = train_data)
svm_2_acc_train <- mean(svm_2_pred_train == train_data$activities)

svm_2_pred_valid <- predict(svm_model_2, newdata = valid_data)
svm_2_acc_valid <- mean(svm_2_pred_valid == valid_data$activities)

### Decision Tree model
train_data$activities <- as.factor(train_data$activities)
valid_data$activities <- as.factor(valid_data$activities)

tree_model_2 <- train(activities ~ ., data = train_data, method = "rpart")
print(tree_model_2)

tree_2_pred_train <- predict(tree_model_2, train_data)
tree_2_acc_train <- mean(tree_2_pred_train == train_data$activities)

tree_2_pred_valid <- predict(tree_model_2, valid_data)
tree_2_acc_valid <- mean(tree_2_pred_valid == valid_data$activities)

### Random Forest model
train_data$activities <- as.factor(train_data$activities)
valid_data$activities <- as.factor(valid_data$activities)

forest_model_3 <- train(
  activities ~ .,                    # Target variable (activities) and predictors
  data = train_data,                 # Data used for training
  method = "rf",             # Random Forest method
  importance = TRUE          # Keep track of variable importance
)
print(forest_model_3)

forest_3_pred_train <- predict(forest_model_3, train_data)
forest_3_acc_train <- mean(forest_3_pred_train == train_data$activities)

forest_3_pred_valid <- predict(forest_model_3, valid_data)
forest_3_acc_valid <- mean(forest_3_pred_valid == valid_data$activities)

### 2nd Random Forest model
forest_model_4 <- randomForest(
  activities ~ ., 
  data = train_data, 
  ntree = 500, 
  mtry = 4, 
  maxnodes = 20,   # restrict tree size
  importance = TRUE
)
print(forest_model_4)

forest_4_pred_train <- predict(forest_model_4, train_data)
forest_4_acc_train <- mean(forest_4_pred_train == train_data$activities)

forest_4_pred_valid <- predict(forest_model_4, valid_data)
forest_4_acc_valid <- mean(forest_4_pred_valid == valid_data$activities)

### Table to compare the models
Validation_Table_2 <- as.table(matrix(c(logit_acc_train, probit_acc_train, svm_2_acc_train, 
                                      tree_2_acc_train, forest_3_acc_train, forest_4_acc_train,
                                      logit_acc_valid, probit_acc_valid, svm_2_acc_valid, 
                                      tree_2_acc_valid, forest_3_acc_valid, forest_4_acc_valid), ncol=6, byrow=TRUE))

colnames(Validation_Table_2) <- c('Logit', 'Probit', 'SVM', 'Decision Tree', 'Random Forest', 'Random Forest 2')
rownames(Validation_Table_2) <- c('ACC_IN', 'ACC_OUT')
Validation_Table_2

# Test on best model
forest_3_pred_test <- predict(forest_model_3, test_data)
forest_3_acc_test <- mean(forest_3_pred_test == test_data$activities)
print(forest_3_acc_test)
