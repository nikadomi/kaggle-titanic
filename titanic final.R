
setwd("/Users/dominikakata/Documents/data science/kaggle/titanic")
getwd()

## Getting and Cleaning Data

# Loading packages
library(readr) # data import
library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(missForest) # missing data imputation 
library(corrplot) # used for variables correlation
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
summary (all)

# Looking at the top and the botom of the data

head(all)
tail(all)

# Missing data

sapply(all, function(x) sum(is.na(x)))

## Variables Exploration and Feature Engineering

# Survived Variable 

table(train$Survived)

# Pclass Variable

##################### Stack Overflow
ggplot(all[1:891,], aes(x = Pclass, fill = factor(Survived)))+
    geom_bar(stat='count', position='stack') +
    ggtitle("Survival Based on Ticket Class") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived")+
    scale_x_continuous(breaks=c(1:11)) +
    geom_text(stat = "count", aes(label = ..count.., y = ..count..), size = 4)
   
# Percentage of people survived in each class
all[1:891,] %>% group_by(Pclass) %>% summarise(survived_percentage = (sum(Survived)/length(Pclass)*100))

# Table: Pclass and Sex
table(train$Pclass, train$Sex)

#Table: Pclass and Sex (Survived only)
table(train[train$Survived==1,]$Pclass,train[train$Survived==1,]$Sex)
   
############ Adding variable

all <- mutate(all, women_1_2_class = ifelse(Sex=="female", ifelse(Pclass == 3, 0,1),0))
all$women_1_2_class <- as.factor(all$women_1_2_class)

# Changing Pclass to factor variable
all$Pclass <- as.factor(all$Pclass) 

# Name Variable

#Getting the title from the Name variable
Name_split <- strsplit(all$Name, split='[,.]')
Name_title <-sapply(Name_split, "[", 2)
Name_title <- substring(Name_title, 2)
table(Name_title)
# z misiem pomyslec jak podzielic i potem mutate

# Lenght of the Name variable
Name_len  <- nchar(all$Name)
all <- mutate(all, Name_length = Name_len)


# Sex variable

ggplot(all[1:891,], aes(x = Sex, fill = factor(Survived)))+
    geom_bar(stat='count', position='stack') +
    ggtitle("Survival Based on Gender") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived")

all$Sex <- as.factor(all$Sex)

# Age Variable

sum(is.na(all$Age))

# Age variable imputation using missForest package

# dataset used for imputation
age.mis <- as.data.frame(all[,c(2,3,5,6,7,8,10,12)])

# Changing class to factor
age.mis$Embarked <- as.factor(age.mis$Embarked)
age.mis$Survived <- as.factor(age.mis$Survived)

#Imputation
age_imp <- missForest(age.mis)
a <- age_imp[[1]][4]
a <- as.numeric(a$Age)

table(a == all$Age)


hist(a, freq=F)
par(mfrow = c(1,2))
hist(all$Age, freq = F)
age_imp$OOBerror


age_full <- all[!is.na(all$Age)==T,]


ggplot(all[1:891,], aes(factor(Survived), Age))+
    geom_boxplot() +
    ggtitle("Survival Based on Age") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived")

## SibSp, Parch and new family_size variables

all <- mutate(all, family_size = SibSp+Parch+1) 

ggplot(all[1:891,], aes(x = family_size, fill = factor(Survived)))+
    geom_bar(stat='count', position='dodge') +
    ggtitle("Survival Based on family size") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived") +
    scale_x_continuous(breaks=c(1:11))

# Dividing family_size variable into 3 groups: singe, medium, big

all$family_size <- ifelse(all$family_size == 1, "single", 
                          ifelse(all$family_size > 4, "big", "medium"))


all$family_size <- as.factor(all$family_size)

## Ticket variable

# Creating new ticket_count variable
ticket_count <- rep("NA", times = nrow(all))

for (i in 1:nrow(all)){
    
    ticket_count[i] <- nrow(all[all$Ticket == all$Ticket[i],])
}

all <- mutate(all, ticket_count)

# visualization ticket_count variale on a plot

ggplot(all[1:891,], aes(x = as.numeric(ticket_count), fill = factor(Survived)))+
    geom_bar(stat='count', position='dodge') +
    ggtitle("Survival Based on a Ticket Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived") +
    scale_x_continuous(breaks=c(1:11))

all$ticket_count <- ifelse(all$ticket_count == 1, "single", 
                           ifelse(all$ticket_count > 4, "big", "medium")) 

# Changing class to factor

all$ticket_count <- as.factor(all$ticket_count)

# Correlation between family_size and ticket_count

family_size <-  all$SibSp+all$Parch+1 # variable before dividing into 3 groups
cor(family_size, as.numeric(ticket_count), method = "pearson")

## Fare Variable

all[is.na(all$Fare)==TRUE,]

all[is.na(all$Fare)==TRUE,]$Fare <- median(all[(all$Embarked == "S" & all$Sex == "male" & all$Pclass == "3"),"Fare"]$Fare, na.rm = T)

# Checking imputation

all[1044,"Fare"]

# Plotting Density Plot for Fare Variable

ggplot(all, aes(x = Fare))+
    geom_density(kernel = "gaussian") +
    ggtitle("FDensity Plot for Fare Variable") +
    theme(plot.title = element_text(hjust = 0.5)) 
    
# Creating new Fare_ln variable

all <- mutate(all, Fare_ln = as.numeric(ifelse(all$Fare == 0,"NA",log(all$Fare))))

# Plotting Density Plot for Fare_ln Variable

ggplot(all, aes(x = Fare_ln))+
    geom_density(kernel = "gaussian") +
    ggtitle("Density Plot for Fare_ln Variable") +
    theme(plot.title = element_text(hjust = 0.5)) 
?unique

##Enbarked Variable 

all[is.na(all$Embarked)==T,]

# Finding median for similar observations
all %>%
    filter(Pclass ==1 & Sex == "female" & ticket_count == "medium") %>%
    group_by(Embarked)%>%
    summarise(avg = median(Fare))

# Missing data imputation
all[is.na(all$Embarked)==T,]$Embarked <- "S"

# Checking imputation
all[c(62,830),"Embarked"]

# Changing class to factor

all$Embarked <- as.factor(all$Embarked)





rm(family_size, i, ticket_count)




## Creating Mother Variable

mother <- rep("NA", times = nrow(all))

for (i in 1:nrow(all)){
    
    mother[i]<- ifelse(all$Sex[i]==1 & all$Age[i]>16 & all$Parch[i] >0 & nrow(all[all$Ticket == all$Ticket[i],]) > 1,
                       ifelse(any((all[all$Ticket == all$Ticket[i]& all$Age <10 & all$Parch >0 ,]$Age +16) < all$Age[i]),1,0)
                       ,0)
    
}


all <- all %>%
    mutate(mother)

all$family_size <- as.factor(all$family_size)
all$ticket_count <- as.factor(all$ticket_count)
all <- mutate(all, child = ifelse(all$Age < 9,1,0))

ggplot(all[1:891,], aes(x = as.numeric(child), fill = factor(Survived)))+
    geom_bar(stat='count', position='dodge') +
    ggtitle("Survival Based on being a mother") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = "Survived") +
    scale_x_continuous(breaks=c(1:11))

## korelacja zmiennych

corr_data <- select(all, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Fare_ln, Embarked, family_size, ticket_count, women_child, mother, child)

corr_data[,c("Pclass", "Sex", "Embarked", "family_size", "ticket_count", "women_child", "mother")] <-  lapply(corr_data[,c("Pclass", "Sex", "Embarked", "family_size", "ticket_count", "women_child", "mother")], function(x) as.numeric(x))


cor_matrix <- cor(corr_data[1:891,], method = "pearson")
cor_matrix

corrplot(cor_matrix, method="number")
?corrplot
rm(corr_data)
#model

train_model <- all[1:891, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Fare_ln", "Embarked", "family_size", "ticket_count", "women_child", "mother", "child")]
test_model <- all[892:1309, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Fare_ln", "Embarked", "family_size", "ticket_count", "women_child", "mother", "child")]

logistic_all <- glm(Survived~., data = train_model, family = binomial)
summary(lol)

## formward selection
reg0=glm(Survived~1,data=train_model,family=binomial)
reg1=glm(Survived~.,data=train_model,family=binomial)
step(reg0,scope=formula(reg1), direction="forward",k=2) 

logistic_forward <- glm(Survived ~ Sex + Pclass + family_size + child + Age + Fare_ln, data = train_model, family = binomial)
summary(lpgostic_forward)

#drzewo

tree <- rpart(Survived ~., data = train_model, method = "class")
plot(tree)
text(tree)

#las
random <- randomForest(as.factor(Survived)~.,data = train_model, importance = TRUE, ntree = 2000)
varImpPlot(random)


#predykcje

pred_log <- predict(logistic_forward, newdata = test_model, type= "response")

classification <- ifelse(pred_log>0.5,1,0)

sum(classification)

Prediction_logistic <- data.frame(PassengerId = test$PassengerId, Survived = classification)
write.csv(Prediction_logistic, file = "logistic.csv", row.names = FALSE)

# accuracy = 0.77033

pred_tree <- predict(tree, newdata = test_model, type = "class")
Prediction_tree <- data.frame(PassengerId = test$PassengerId, Survived = pred_tree)
write.csv(Prediction_tree, file = "tree.csv", row.names = FALSE)

# accuracy = 0.76555

pred_random <- predict(random, newdata = test_model)
Prediction_random <- data.frame(PassengerId = test$PassengerId, Survived = pred_random)
write.csv(Prediction_random, file = "random.csv", row.names = FALSE)

# accuracy = 0.77990

pred_cos <- cforest(as.factor(Survived) ~ ., data = train_model, controls=cforest_unbiased(ntree=2000, mtry=3))


hist(all$Age, freq = F)



write.csv(all, file = "all.csv", row.names = FALSE)
