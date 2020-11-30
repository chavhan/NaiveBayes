## Naive Byes on salary data
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

View(s_train)
names(s_train)
names(s_test)

str(s_train)


## converting categorical values into numeric 
## For train dataset 
s_train_numeric <- s_train
head(s_train_numeric)
View(s_train_numeric)
s_train_numeric$workclassno <- as.numeric(as.factor(s_train_numeric$workclass))
s_train_numeric$maritalstatusno <- as.numeric(as.factor(s_train_numeric$maritalstatus))
s_train_numeric$occupationno <- as.numeric(as.factor(s_train_numeric$occupation))
s_train_numeric$relationshipno <- as.numeric(as.factor(s_train_numeric$relationship))
s_train_numeric$raceno  <- as.numeric(as.factor(s_train_numeric$race))
s_train_numeric$nativeno <- as.numeric(as.factor(s_train_numeric$native))
s_train_numeric$sexno <- as.numeric(as.factor(s_train_numeric$sex))
s_train_numeric$salaryno <- as.numeric(as.factor(s_train_numeric$Salary))

s_train_filter <- s_train_numeric[,c(-2,-3,-5,-6,-7,-8,-9,-13,-14)]
head(s_train_filter)

## For test dataset 
s_test_numeric <- s_test
head(s_test_numeric)
View(s_test_numeric)
s_test_numeric$workclassno <- as.numeric(as.factor(s_test_numeric$workclass))
s_test_numeric$maritalstatusno <- as.numeric(as.factor(s_test_numeric$maritalstatus))
s_test_numeric$occupationno <- as.numeric(as.factor(s_test_numeric$occupation))
s_test_numeric$relationshipno <- as.numeric(as.factor(s_test_numeric$relationship))
s_test_numeric$raceno  <- as.numeric(as.factor(s_test_numeric$race))
s_test_numeric$nativeno <- as.numeric(as.factor(s_test_numeric$native))
s_test_numeric$sexno <- as.numeric(as.factor(s_test_numeric$sex))
s_test_numeric$salaryno <- as.numeric(as.factor(s_test_numeric$Salary))

s_test_filter <- s_test_numeric[,c(-2,-3,-5,-6,-7,-8,-9,-13,-14)]
head(s_test_filter)

pairs.panels(s_test_filter[1:500,-13])
s_test_filter %>%
  ggplot(aes(x=salaryno, y=maritalstatusno, fill = salaryno)) +
  geom_boxplot() +
  ggtitle("Box Plot")

s_test_filter %>% ggplot(aes(x=maritalstatusno, fill = salaryno)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")

convert_value <- function(x)
                {
                   if(x == 1)
                     return(0)
                   if(x == 2)
                     return(1)
}
View(s_train_filter[1:10, ])
s_train_filter$salaryno <- as.factor(sapply(s_train_filter$salaryno,convert_value))
class(s_train_filter$salaryno)
str(s_train_filter)

# Naive Bayes Model for train data 
model <- naive_bayes(s_train_filter$salaryno ~ ., data = s_train_filter)
#model <- naive_bayes(s_train_filter$salaryno ~ ., data = s_train_filter, usekernel= T)

summary(model$data)
plot(model)

p <- predict(model, s_train_filter, type = 'prob')
head(cbind(p, s_train$Salary))
head(p)

# Confusion Matrix - train data
p_train <- predict(model, s_train_filter)
head(p_train)
(tab1 <- table(p_train, s_train_filter$salaryno))
accuracy_train <- sum(diag(tab1))/sum(tab1)
accuracy_train

# Naive Bayes model1 for test data 
head(s_test_filter)
str(s_test_filter)
s_test_filter$salaryno <- as.factor(sapply(s_test_filter$salaryno,convert_value))
model1 <- naive_bayes(s_test_filter$salaryno ~ ., data = s_test_filter)
model1 <- naive_bayes(s_test_filter$salaryno ~ ., data = s_test_filter, usekernel= T)

summary(model1$data)
plot(model1)

p1 <- predict(model1, s_test_filter, type = 'prob')
head(cbind(p1, s_test$Salary))
head(p1)

# Confusion Matrix - test data
p_test <- predict(model1, s_test_filter)
head(p_test)
(tab2 <- table(p_test, s_test_filter$salaryno))
accuracy_test <- sum(diag(tab2))/sum(tab2)
accuracy_test

total_result <- data.frame(
  'dataset' =NULL,
  'beforeKernel' = NULL,
  'afterkernel' = NULL
)

temp.data <- data.frame('test','0.789575','0.8027224')
names(temp.data) <- c('dataset','beforeKernel','afterkernel')
total_result <- rbind(total_result,temp.data)
total_result

View(total_result)
