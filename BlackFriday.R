library(dplyr)
library(ggplot2)
library(cowplot)
library(dummies)

## read datasets
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

## structure of data
str(train)
str(test)

test$Purchase <- "NA"
Cdata <- rbind(train,test)

## EDA
## Univariate

ggplot(train) + geom_histogram(aes(train$Purchase), binwidth = 400, fill = 'darkblue')  #target variable

ggplot(Cdata) + geom_histogram(aes(Cdata$Occupation), binwidth = 0.5, fill = 'orange')

ggplot(Cdata) + geom_histogram(aes(Cdata$Marital_Status), binwidth = 0.3, fill = 'orange')

p1 = ggplot(Cdata) + geom_histogram(aes(Cdata$Product_Category_1), binwidth = 0.5, fill = 'orange')

p2 = ggplot(Cdata) + geom_histogram(aes(Cdata$Product_Category_2), binwidth = 0.5, fill = 'orange')

p3 = ggplot(Cdata) + geom_histogram(aes(Cdata$Product_Category_3), binwidth = 0.5, fill = 'orange')

plot_grid(p1,p2,p3, ncol = 1)

## Bivariate

ggplot(train) + geom_point(aes(Occupation, Marital_Status ), colour = "violet", alpha = 0.1)


#category 18 and 19 is present in Product_Category1 only and small in number
X_train <- subset(train, !Product_Category_1 %in% c(19,20))
X_test <- test

# onehot-encoding city variable
X_train <- dummy.data.frame(X_train, names=c("City_Category"), sep="_")
X_test <- dummy.data.frame(X_test, names=c("City_Category"), sep="_")

# converting age variable to numeric
X_train$Age[X_train$Age == "0-17"] <- "15"
X_train$Age[X_train$Age == "18-25"] <- "21"
X_train$Age[X_train$Age == "26-35"] <- "30"
X_train$Age[X_train$Age == "36-45"] <- "40"
X_train$Age[X_train$Age == "46-50"] <- "48"
X_train$Age[X_train$Age == "51-55"] <- "53"
X_train$Age[X_train$Age == "55+"] <- "60"

X_test$Age[X_test$Age == "0-17"] <- "15"
X_test$Age[X_test$Age == "18-25"] <- "21"
X_test$Age[X_test$Age == "26-35"] <- "30"
X_test$Age[X_test$Age == "36-45"] <- "40"
X_test$Age[X_test$Age == "46-50"] <- "48"
X_test$Age[X_test$Age == "51-55"] <- "53"
X_test$Age[X_test$Age == "55+"] <- "60"

X_train$Age <- as.integer(X_train$Age)
X_test$Age <- as.integer(X_test$Age)

# converting stay in current city to numeric
X_train$Stay_In_Current_City_Years[X_train$Stay_In_Current_City_Years == "4+"] <- "4"
X_test$Stay_In_Current_City_Years[X_test$Stay_In_Current_City_Years == "4+"] <- "4"

X_train$Stay_In_Current_City_Years <- as.integer(X_train$Stay_In_Current_City_Years)
X_test$Stay_In_Current_City_Years <- as.integer(X_test$Stay_In_Current_City_Years)

# converting gender to binary
X_train$Gender <- ifelse(X_train$Gender == "F", 1, 0)
X_test$Gender <- ifelse(X_test$Gender == "F", 1, 0)

# feature representing the count of each user
user_count <- X_train %>% 
  group_by(User_ID) %>%
  summarise(User_Count = n())

X_train <- merge(X_train, user_count, by="User_ID")
X_test <- merge(X_test, user_count, all.x=T, by="User_ID")

# feature representing the count of each product
product_count <- X_train %>%
  group_by(Product_ID) %>%
  summarise(Product_Count = n())

X_train <- merge(X_train, product_count, by="Product_ID")
X_test <- merge(X_test, product_count, all.x=T, by="Product_ID")
X_test$Product_Count[is.na(X_test$Product_Count)] <- 0

# feature representing the average Purchase of each product
product_mean <- X_train %>%
  group_by(Product_ID) %>%
  summarise(Product_Mean = mean(Purchase))

X_train <- merge(X_train, product_mean, by="Product_ID")
X_test <- merge(X_test, product_mean, all.x=T, by="Product_ID")
X_test$Product_Mean[is.na(X_test$Product_Mean)] <- mean(X_train$Purchase)

# feature representing the proportion of times the user purchases the product more than the product's average
X_train$flag_high <- ifelse(X_train$Purchase > X_train$Product_Mean,1,0)
user_high <- X_train %>%
  group_by(User_ID) %>%
  summarise(user_High = mean(flag_high))

X_train <- merge(X_train, user_high, by="User_ID")
X_test <- merge(X_test, user_high, by="User_ID")

y <- X_train$Purchase
x <- X_train[,-c(1,2,18)]

#Linear regression model
lrm <- lm(Purchase ~ ., data = x[,-c(2,3,6,10,11)])
summary(lrm)





