# getwd()

# you can use this line to set you path according to your pc:
# setwd("C:/Users/reemj/University of Bahrain/BDSA602- project - Documents")


install.packages("farff")
# using package farff to read file type arff
library(farff)

# setwd("d:/master/sem1/stat/stat project")



data <- readARFF("chronic_kidney_disease_full.arff")
fix(data)
summary(data)

# checking structure of data
str(data)
# cleaning data
# Number of null values
sum(is.na(data))

# Number of null values for each column
sapply(data, function(x) sum(length(which(is.na(x)))))

# since there is a lot of null  values we cannot drop the rows, instead median and mode will be set to replace nulls

# first for numerical columns, replacing with median
library(tidyr)
install.packages("devtools")
library(devtools)
library(dplyr)
sessionInfo()

data <- data %>% mutate(across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE))))
# rlang::last_error()
# now for categorical columns, replacing with mode
# creating a function to calculate mood
calc_mode <- function(x) {

  # List the distinct / unique values
  distinct_values <- unique(x)

  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))

  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}
attach(data)
calc_mode(pe)


# replacing with mode
data <- data %>% mutate(across(where(is.factor), ~ replace_na(., calc_mode(.))))

# checking again if any null values exists after replacing
sapply(data, function(x) sum(length(which(is.na(x)))))

# Replacing column names abbreviation with their full name
colnames(data)
colnames(data) <- c(
  "age", "blood_pressure", "specific_gravity", "albumin", "sugar", "red_blood_cells", "pus_cell",
  "pus_cell_clumps", "bacteria", "blood_glucose_random", "blood_urea", "serum_creatinine", "sodium",
  "potassium", "haemoglobin", "packed_cell_volume", "white_blood_cell_count", "red_blood_cell_count",
  "hypertension", "diabetes_mellitus", "coronary_artery_disease", "appetite", "peda_edema",
  "aanemia", "class"
)

# Exploratory analysis between num features

# This is an optional way of showing summary statistics
install.packages("summarytools")
library(summarytools)
view(dfSummary(data))

# get statistical summary for numerical columns
summary(data[, c(1, 2, 10, 11, 12, 13, 14, 15, 16, 17, 18)])

install.packages("vtable")
library(vtable)
sumtable(data[, c(1, 2, 10, 11, 12, 13, 14, 15, 16, 17, 18)],
  vars = NA,
  out = NA,
  file = NA,
  summ = c(
    "mean(x)",
    "median(x)",
    "min(x)",
    "max(x)"
  ),
  summ.names = NA,
  group = NA,
  group.long = FALSE,
  group.test = FALSE,
  group.weights = NA,
  col.breaks = NA,
  digits = NA,
  fixed.digits = FALSE,
  factor.percent = TRUE,
  factor.counts = TRUE,
  factor.numeric = FALSE,
  logical.numeric = FALSE,
  logical.labels = c("No", "Yes"),
  labels = NA,
  title = "Summary Statistics",
  note = NA,
  anchor = NA,
  col.width = NA,
  col.align = NA,
  align = NA,
  note.align = "l",
  fit.page = NA,
  simple.kable = FALSE,
  opts = list()
)
sumtable?

# plot num features
attach(data)
par(mfrow = c(2, 3))
hist(age)
hist(blood_pressure)
hist(blood_glucose_random)
hist(blood_urea)
hist(serum_creatinine)
hist(sodium)
par(mfrow = c(2, 3))
hist(potassium)
hist(haemoglobin)
hist(packed_cell_volume)
hist(white_blood_cell_count)
hist(red_blood_cell_count)

# check the collinearity of the numerical features
install.packages("corrplot")
library(corrplot)
par(mfrow = c(1, 1))

correlation <- cor(data[, c(1, 2, 10, 11, 12, 13, 14, 15, 16, 17, 18)])

corrplot(correlation, method = "number", type = "upper", diag = TRUE)

# plot haemoglobin versus packed_cell_volume

par(mfrow = c(1, 1))
plot(data$haemoglobin[data$class == "notckd"], data$packed_cell_volume[data$class == "notckd"], xlab = "haemoglobin", ylab = "packed_cell_volume", col = 2)
points(data$haemoglobin[data$class == "ckd"], data$packed_cell_volume[data$class == "ckd"], col = 1)
legend("bottomright", c("notckd", "ckd"), cex = 0.5, text.col = 1:2)


# provide summary statistics for the categorical features
table(specific_gravity)
table(albumin)
table(sugar)
table(red_blood_cells)
table(pus_cell)
table(pus_cell_clumps)
table(bacteria)
table(hypertension)
table(diabetes_mellitus)
table(coronary_artery_disease)
table(appetite)
table(peda_edema)
table(aanemia)
table(class)

par(mfrow = c(2, 3))
plot(specific_gravity, main = "specific_gravity", col = 1:05) # we can customize
plot(albumin, main = "albumin", col = 1:06)
plot(sugar, main = "sugar", col = 1:06)
plot(red_blood_cells, main = "red_blood_cells", col = 1:09)
plot(pus_cell, main = "pus_cell", col = 1:20)
plot(pus_cell_clumps, main = "pus_cell_clumps", col = 1:02)
par(mfrow = c(2, 3))
plot(bacteria, main = "bacteria", col = 1:02)
plot(hypertension, main = "hypertension", col = 1:02)
plot(diabetes_mellitus, main = "diabetes_mellitus", col = 1:02)
plot(coronary_artery_disease, main = "coronary_artery_disease", col = 1:02)
plot(appetite, main = "appetite", col = 1:02)
plot(peda_edema, main = "peda_edema", col = 1:02)

par(mfrow = c(1, 2))
plot(aanemia, main = "aanemia", col = 1:02)
plot(class, main = "class", col = 1:02)


# investigate the predictor-response relationships: specific_gravity - class
par(mfrow = c(1, 1))

table <- table(specific_gravity, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "specific_gravity")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: albumin - class

table <- table(albumin, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "albumin")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: sugar - class

table <- table(sugar, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "sugar")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: red_blood_cells - class

table <- table(red_blood_cells, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "red_blood_cells")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: pus_cell - class

table <- table(pus_cell, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "pus_cell")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: pus_cell_clumps - class

table <- table(pus_cell_clumps, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "pus_cell_clumps")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: bacteria - class

table <- table(bacteria, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "bacteria")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: hypertension - class

table <- table(hypertension, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "hypertension")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: diabetes_mellitus - class

table <- table(diabetes_mellitus, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "diabetes_mellitus")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: coronary_artery_disease - class

table <- table(coronary_artery_disease, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "coronary_artery_disease")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: appetite - class

table <- table(appetite, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "appetite")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: peda_edema - class

table <- table(peda_edema, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "peda_edema")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# investigate the predictor-response relationships: aanemia - class

table <- table(aanemia, class)
colnames(table) <- c("ckd", "notckd")
barplot(t(table), col = 1:2, xlab = "aanemia")
legend("topright", colnames(table), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2, cex = 0.5)

# Feature Encoding


# data split/sampling(oversampling/downsampling) - possible to do but not necassary (Turns out it's very necessary)
# Will use Oversampling because we have a big overfitting issue. this will fix the low data size and imbalance
install.packages("ROSE")

predictor_variables <- data[-Class]
data_reduced <- select(data, specific_gravity, albumin, serum_creatinine, haemoglobin, packed_cell_volume, white_blood_cell_count, red_blood_cell_count, hypertension)
oversampled_data <- ubBalance(predictor_variables,
  response_variable,
  type = "ubOver", # Option for oversampling
  k = 0, # Value of 0 creates 50:50 split
  verbose = TRUE
)
data_balanced <- ovun.sample(class ~ ., data = data, method = "over", N = 600)$data
data_reduced_balanced <- ovun.sample(class ~ ., data = data_balanced, method = "over", N = 600)$data

dim(data_balanced)


install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")

# install.packages("ggplot2")
library("caret")
library("rpart")
library("rpart.plot")






# Data split into train and test, Using an 80-20 split
set.seed(1)
n <- dim(data_balanced)[1]
n
split <- createDataPartition(1:n, p = 0.8, list = FALSE)
train <- data_balanced[split, ]
test <- data_balanced[-split, ]

n <- dim(train)[1]
N <- dim(test)[1]
n
N
# Modeling

# Metrics Function goes here :
metrics <- function(CM) {
  Accuracy <- (CM[1, 1] + CM[2, 2]) / sum(CM)
  ErrorRate <- (CM[1, 2] + CM[2, 1]) / sum(CM)
  Specificity <- CM[1, 1] / sum(CM[, 1])
  Sensitivity <- CM[2, 2] / sum(CM[, 2])
  Recall <- Sensitivity
  Precision <- CM[2, 2] / (CM[2, 1] + CM[2, 2])
  F1score <- 2 * (Precision * Recall) / (Precision + Recall)

  cat(
    "Accuracy    = ", round(Accuracy, 3), "\n",
    "Error Rate  = ", round(ErrorRate, 3), "\n",
    "Specificity = ", round(Specificity, 3), "\n",
    "Sensitivity = ", round(Sensitivity, 3), "\n",
    "Recall      = ", round(Recall, 3), "\n",
    "Precision   = ", round(Precision, 3), "\n",
    "F1Score     = ", round(F1score, 3)
  )
}


# Model 1
# Random Forest :
# Construct a random forest.
install.packages("randomForest")
library(randomForest)
set.seed(1) # Please always use this value for set.seed
TREE.forest <- randomForest(formula = class ~ ., data = train, importance = TRUE)

fix(data)


# Present the variable importance plot for the random forest constructed.


varImpPlot(TREE.forest)


# Evaluate the performance of the constructed tree by estimating the test misclassification rate.
prediction <- predict(object = TREE.forest, newdata = test, type = "class")
CM <- table(prediction, test$class)

CM
metrics(CM)

# Model 2

# LOGISTIC REGRESSION

### to check which value 0 and 1 --- chk as 0 and notchk as 1
contrasts(data_balanced$class)


fix(data_balanced)


# logistic regression model
log_model <- glm(formula = class ~ ., data = train, family = "binomial")


# Model Details
summary(log_model)

### got an error which is>>>>>>  Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) :
###  factor sugar has new levels 5 ... so i found the rows with level 5 and they were 3  only ... since the are three only i removed them.




## checking the index of sugar variable.
grep("sugar", colnames(test))

# checking the differences values between train and test in terms of sugar variable
unique(test$sugar)
unique(train$sugar)

# getting the rows with value of 5
# test[test$sugar == 5 ]


# then finally, excluding them from the test data
test <- test[test$sugar != 5, ]

# confirming the modification happened
unique(test$sugar)
unique(train$sugar)


# evaluate the model with test Data ...
log_prediction <- predict(object = log_model, newdata = test, type = "response")


### probability > 50% then 1 else 0
log_prediction <- ifelse(log_prediction > 0.5, "notckd", "ckd")

head(log_prediction)

log_prediction

tail(log_prediction)

log_CM <- table(log_prediction, test$class)

log_CM

metrics(log_CM)

# Model 3
################ KNN Classifier Model


## confirming there is no null values

sapply(data_balanced, function(x) sum(length(which(is.na(x)))))



####
library(dplyr)


### extracting predictors and converting them to numeric.
### here we are excluding index 25 from our data because it is our target (class)
newDataKNN <- data_balanced[1:24] %>% mutate_if(is.factor, as.numeric)

# confirming that the values has been changed
class(newDataKNN$bacteria)




## here, confirming there is no null values after converting.

sapply(newDataKNN, function(x) sum(length(which(is.na(x)))))

# checking the structure of the new data
str(newDataKNN)


## normalizing numeric values

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


newDataKNN_norm <- as.data.frame(lapply(newDataKNN, normalize))

dim(newDataKNN_norm)

## Data splitting

set.seed(1)

n <- dim(newDataKNN_norm)[1]
n
library("caret")
split <- createDataPartition(1:n, p = 0.8, list = FALSE)
trainkNN <- newDataKNN_norm[split, ]
testKNN <- newDataKNN_norm[-split, ]


## splitting our target variable from balanced data for train and test splits

trainkNN_Label <- data_balanced[25][split, ]

testKNN_Label <- data_balanced[25][-split, ]


## using KNN model

install.packages("class")
library(class)

#### k=23
KNN_model_23 <- knn(train = trainkNN, test = testKNN, cl = trainkNN_Label, k = 23) ## creating the model
cmKNN_23 <- table(KNN_model_23, testKNN_Label)
cmKNN_23
metrics(cmKNN_23)


#### k=24
KNN_model_24 <- knn(train = trainkNN, test = testKNN, cl = trainkNN_Label, k = 24)
cmKNN_24 <- table(KNN_model_24, testKNN_Label)
cmKNN_24
metrics(cmKNN_24)


#### k=25
KNN_model_25 <- knn(train = trainkNN, test = testKNN, cl = trainkNN_Label, k = 25)
cmKNN_25 <- table(KNN_model_25, testKNN_Label)
cmKNN_25
metrics(cmKNN_25)