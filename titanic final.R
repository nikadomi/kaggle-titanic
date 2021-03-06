
### Kaggle Titanic tutorial
### Author: Dominika Kata
###########################

## Getting and Cleaning Data

# Loading packages

library(readr) # Data import
library(dplyr) # Data manipulation
library(ggplot2) # Data visualization
library(missForest) # Missing data imputation
library(rpart) # Decision Tree classification algorythm
library(rpart.plot) # Decision Tree visualization
library(randomForest) # Random Forest classification algorythm

# Getting data

train <- read_csv("train.csv")
test <- read_csv("test.csv")

# Joining datasets

all <- bind_rows(train, test)

# Quick look

str(all)
summary(all)

# Looking at the top and the bottom of the data

head(all)
tail(all)

# Missing data

sapply(all, function(x) sum(is.na(x)))

## Variables Exploration and Feature Engineering

# Survived variable

table(train$Survived)

# Pclass variable

ggplot(all[1:891,], aes(x = Pclass, fill = factor(Survived)))+
    geom_bar(stat = 'count', position = 'stack') +
    ggtitle("Survival Based on Ticket Class") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived")+
    scale_x_continuous(breaks = c(1:11))

# Percentage of people survived in each class

all[1:891,] %>% group_by(Pclass) %>% summarise(survived_percentage = (sum(Survived)/length(Pclass)*100))

# Table: Pclass and Sex

table(train$Pclass, train$Sex)

# Table: Pclass and Sex (Survived only)

table(train[train$Survived == 1,]$Pclass,train[train$Survived == 1,]$Sex)

# Changing Pclass to factor variable

all$Pclass <- as.factor(all$Pclass)

# Name Variable

# Getting the title from the Name variable

name_split <- strsplit(all$Name, split = '[,.]')
name_title <-sapply(name_split, "[", 2)
name_title <- substring(name_title, 2)
table(name_title)

# Grouping similar titles

uncommon_titles <- c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady",
                     "Major", "Rev", "Sir", "the Countess")

name_title[name_title %in% uncommon_titles] <- "uncommon"

name_title[name_title == "Mlle"] <- "Miss"
name_title[name_title %in% c("Mme", "Ms")] <- "Mrs"

# Adding Title variable and changing class to factor

all <- mutate(all, Title = name_title)
all$Title <- as.factor(all$Title)

# Unset helper variables

rm(name_split, name_title, uncommon_titles)

# Sex variable

# Sex variable visualisation

ggplot(all[1:891,], aes(x = Sex, fill = factor(Survived)))+
    geom_bar(stat = 'count', position = 'stack') +
    ggtitle("Survival Based on Gender") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived")

# Changing class to factor

all$Sex <- as.factor(all$Sex)

# Age Variable

sum(is.na(all$Age))

# Age variable imputation using missForest package

# Dataset used for imputation

age.mis <- as.data.frame(all[,c(2,3,5,6,7,8,10,12)])

# Changing class to factor

age.mis$Embarked <- as.factor(age.mis$Embarked)
age.mis$Survived <- as.factor(age.mis$Survived)

# Imputation

age_imp <- missForest(age.mis)
age_new <- age_imp[[1]][4]
age_new <- as.numeric(age_new$Age)

# Age variable histogram

hist(age_new, freq = F)
median(age_new)

# Adding the new Age variable

all$Age <- age_new

# Age variable visualization

ggplot(all[1:891,], aes(factor(Survived), Age))+
    geom_boxplot() +
    ggtitle("Survival Based on Age") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived")

# Unset helper variables

rm(age_imp, age_new, age.mis)

## SibSp, Parch and new family_size variables

all <- mutate(all, family_size = SibSp+Parch+1)

ggplot(all[1:891,], aes(x = family_size, fill = factor(Survived)))+
    geom_bar(stat = 'count', position = 'dodge') +
    ggtitle("Survival Based on family size") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived") +
    scale_x_continuous(breaks = c(1:11))

# Dividing family_size variable into 3 groups: singe, medium, big

all$family_size <- ifelse(all$family_size == 1, "single",
                          ifelse(all$family_size > 4, "big", "medium"))

# Changing class to factor

all$family_size <- as.factor(all$family_size)

## Ticket variable

# Creating new ticket_count variable

ticket_count <- rep("NA", times = nrow(all))

for (i in 1:nrow(all)){

    ticket_count[i] <- nrow(all[all$Ticket == all$Ticket[i],])
}

all <- mutate(all, ticket_count)

# Visualization ticket_count variable on a plot

ggplot(all[1:891,], aes(x = as.numeric(ticket_count), fill = factor(Survived)))+
    geom_bar(stat = 'count', position = 'dodge') +
    ggtitle("Survival Based on a Ticket Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived") +
    scale_x_continuous(breaks = c(1:11))

# Dividing ticket_count variable into 3 groups: singe, medium, big

all$ticket_count <- ifelse(all$ticket_count == 1, "single",
                           ifelse(all$ticket_count > 4, "big", "medium"))

# Changing class to factor

all$ticket_count <- as.factor(all$ticket_count)

# Correlation between family_size and ticket_count

family_size <-  all$SibSp + all$Parch + 1 # variable before dividing into 3 groups
cor(family_size, as.numeric(ticket_count), method = "pearson")

# Unset helper variables

rm(family_size, i, ticket_count)

## Fare Variable

all[is.na(all$Fare),]

# Median for passangers with the same sex, place of embarkement and class

all[is.na(all$Fare),]$Fare <- median(all[(all$Embarked == "S" & all$Sex == "male" & all$Pclass == "3"),"Fare"]$Fare, na.rm = T)

# Checking imputation

all[1044,"Fare"]

# Plotting Density Plot for Fare variable

ggplot(all, aes(x = Fare))+
    geom_density(kernel = "gaussian") +
    ggtitle("Density Plot for Fare Variable") +
    theme(plot.title = element_text(hjust = 0.5))

# Creating new Fare_ln variable

all <- mutate(all, Fare_ln = as.numeric(ifelse(all$Fare == 0,"NA",log(all$Fare))))

# Plotting Density Plot for Fare_ln Variable

ggplot(all, aes(x = Fare_ln))+
    geom_density(kernel = "gaussian") +
    ggtitle("Density Plot for Fare_ln Variable") +
    theme(plot.title = element_text(hjust = 0.5))


## Enbarked Variable

all[is.na(all$Embarked),]

# Finding median for similar observations

all %>%
    filter(Pclass == 1 & Sex == "female" & ticket_count == "medium") %>%
    group_by(Embarked)%>%
    summarise(avg = median(Fare))

# Missing data imputation

all[is.na(all$Embarked),]$Embarked <- "S"

# Checking imputation

all[c(62,830),"Embarked"]

# Changing class to factor

all$Embarked <- as.factor(all$Embarked)

## Creating Mother Variable

mother <- rep("NA", times = nrow(all))

for (i in 1:nrow(all)){

    mother[i] <- ifelse(all$Sex[i] == "female" & all$Age[i]> 16 & all$Parch[i] > 0 &
                    nrow(all[all$Ticket == all$Ticket[i],]) > 1,
                    ifelse(any((all[all$Ticket == all$Ticket[i] & all$Age <10 &
                    all$Parch > 0 ,]$Age + 16) < all$Age[i]), 1, 0) ,0)
}

# Adding the mother variable

all <- all %>%
    mutate(mother)

# Changing class to factor

all$mother <- as.factor(all$mother)

# Unset helper variables

rm(mother, i)

# Adding the child variable

all <- mutate(all, child = ifelse(all$Age < 10,1,0))

# Changing class to factor

all$child <- as.factor(all$child)

## Models

# Choosing variables for modelling

train_model <- all[1:891, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Fare_ln", "Embarked", "Title", "family_size", "ticket_count", "mother", "child")]
test_model <- all[892:1309, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Fare_ln", "Embarked", "Title", "family_size", "ticket_count", "mother", "child")]

# Logistic Regression with Forward Selection

reg0 <- glm(Survived~1,data = train_model, family = binomial)
reg1 <- glm(Survived~.,data = train_model, family = binomial)
step(reg0, scope = formula(reg1), direction = "forward",k = 2)

# Model with selected variables

logistic_forward <- glm(Survived ~ Title + Pclass + family_size + Age + Sex, data = train_model, family = binomial)

# Decision Tree

decision_tree <- rpart(Survived ~., data = train_model, method = "class")

# Decision Tree visualisation

rpart.plot(decision_tree)

# Random Forest

random_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                        Fare + Embarked + Title + family_size + ticket_count + mother +
                        child, data = train_model, importance = TRUE, ntree = 2000)

# Checking variable importance

varImpPlot(random_forest)


# Predictions

# Logistic Regression with Forward Variable Selection

pred_log <- predict(logistic_forward, newdata = test_model, type= "response")
classification <- ifelse(pred_log>0.5,1,0)

# Saving results

Prediction_logistic <- data.frame(PassengerId = test$PassengerId, Survived = classification)
write.csv(Prediction_logistic, file = "logistic.csv", row.names = FALSE)

# Logistic accuracy = 0.77990

# Decision Tree

pred_tree <- predict(decision_tree, newdata = test_model, type = "class")

# Saving results

Prediction_tree <- data.frame(PassengerId = test$PassengerId, Survived = pred_tree)
write.csv(Prediction_tree, file = "tree.csv", row.names = FALSE)

# Tree accuracy = 0.79425

# Random Forest

pred_random <- predict(random_forest, newdata = test_model)

# Saving results

Prediction_random <- data.frame(PassengerId = test$PassengerId, Survived = pred_random)
write.csv(Prediction_random, file = "random.csv", row.names = FALSE)

# accuracy = 0.79904






