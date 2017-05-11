setwd("C:\\Users\\tom.trinh\\Desktop\\R\\Kaggle_Titanic\\Dataset")
library(ggthemes) # visualization
library(scales) # visualization
library(tidyverse) # data manipulation
library(dplyr) # data manipulation
library(mice) # imputation
library(randomForest) # classification algorithm
library(corrplot) # correlation
library(caret)
library(e1071)

train <- read.csv("train.csv",stringsAsFactors = F)
test <- read.csv("test.csv",stringsAsFactors = F)

full <- bind_rows(train,test)
str(full)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
unique(full$Title)
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
full$Family <- paste(full$Surname, full$Fsize, sep='_')

full$Fsize <- full$SibSp + full$Parch + 1
full$FsizeD[full$Fsize == 1] <- 'single'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
which(is.na(full$Fare))
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


sum(is.na(full$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
# Set a random seed
set.seed(129)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
# Save the complete output 
mice_output <- complete(mice_mod)
# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
# Replace Age variable from the mice model.
full$Age <- mice_output$Age
# Show new number of missing Age values
sum(is.na(full$Age))



full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

#Check whether we have any missing data anymore
md.pattern(full)

#Exploring data
# Age vs Survived
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
        geom_histogram(bins=30) + 
        theme_few() +
        xlab("Age") +
        scale_fill_discrete(name = "Survived") + 
        ggtitle("Age vs Survived")
# Sex vs Survived
ggplot(full[1:891,], aes(Sex, fill = factor(Survived))) + 
        geom_bar(stat = "count", position = 'dodge')+
        theme_few() +
        xlab("Sex") +
        ylab("Count") +
        scale_fill_discrete(name = "Survived") + 
        ggtitle("Sex vs Survived")
tapply(full[1:891,]$Survived,full[1:891,]$Sex,mean)
#Sex vs Survived vs Age 
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
        geom_histogram(bins=30) + 
        theme_few() +
        xlab("Age") +
        ylab("Count") +
        facet_grid(.~Sex)+
        scale_fill_discrete(name = "Survived") + 
        theme_few()+
        ggtitle("Age vs Sex vs Survived")
# Pclass vs Survived
tapply(full[1:891,]$Survived,full[1:891,]$Pclass,mean)
ggplot(full[1:891,], aes(Pclass, fill = factor(Survived))) + 
        geom_bar(stat = "count")+
        theme_few() +
        xlab("Pclass") +
        facet_grid(.~Sex)+
        ylab("Count") +
        scale_fill_discrete(name = "Survived") + 
        ggtitle("Pclass vs Sexe vs Survived")
#Pclass vs Sex vs Age
ggplot(full[1:891,], aes(x = Age, y = Sex)) + 
        geom_jitter(aes(colour = factor(Survived))) + 
        theme_few()+
        theme(legend.title = element_blank())+
        facet_wrap(~Pclass) + 
        labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")+
        scale_fill_discrete(name = "Survived") + 
        scale_x_continuous(name="Age",limits=c(0, 81))
#Fare vs PClass
ggplot(full[1:891,], aes(x = Fare, y = Pclass)) + 
        geom_jitter(aes(colour = factor(Survived))) + 
        theme_few()+
        theme(legend.title = element_blank())+
        labs(x = "Age", y = "Pclass", title = "Fare vs Pclass")+
        scale_fill_discrete(name = "Survived") + 
        scale_x_continuous(name="Fare", limits=c(0, 270), breaks=c(0, 40, 80, 120, 160, 200, 240, 280))

#Age vs Title
ggplot(full[1:891,], aes(x = Age, y = Title)) + 
        geom_jitter(aes(colour = factor(Survived))) + 
        theme_few()+
        theme(legend.title = element_blank())+
        labs(x = "Age", y = "Title", title = "Age vs Title")+
        scale_fill_discrete(name = "Survived") + 
        scale_x_continuous(name="Age", limits=c(0, 81))

#Family size vs vs Sex vs Survived 
ggplot(full[1:891,], aes(Fsize, fill = factor(Survived))) + 
        geom_histogram(bins=30) + 
        theme_few() +
        xlab("Fsize") +
        ylab("Count") +
        facet_grid(.~Sex)+
        scale_fill_discrete(name = "Survived") + 
        theme_few()+
        ggtitle("Age vs Sex vs Survived")

#----------------------------------------------------------------------------------------------------------------------------------------
# Build model
# Split the data back into a train set and a test set
full$Deck <- NULL
train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(111)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                 Fare + Embarked + Title + 
                                 FsizeD + Child + Mother,
                         data = train)


rf_model <- randomForest(factor(Survived) ~ Pclass + Sex*Title + Age + Fare + Embarked + SibSp + Parch +  
                                 FsizeD + Child + Mother,
                         data = train)

rf_model
# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
        mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
        geom_bar(stat='identity') + 
        geom_text(aes(x = Variables, y = 0.5, label = Rank),
                  hjust=0, vjust=0.55, size = 4, colour = 'red') +
        labs(x = 'Variables') +
        coord_flip() + 
        theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_solution.csv', row.names = F)



#------------------------------------------------------------------------------
#----------------------------
#   Ch???y 6 mô hình
#----------------------------
full <- full[1:890,c("Pclass","Survived","Sex","Age","SibSp","Parch","Fare","Embarked","Title","FsizeD","Child","Mother")]

set.seed(123) 
Train <- createDataPartition(full$Survived, p = 50/100, list = FALSE)
train <- full[ Train, ]
test <- full[ -Train, ]

set.seed(123) 

ctrl <- trainControl(method="repeatedcv",
                     number = 5,
                     repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary) 


train$Survived <- recode_factor(train$Survived, 
                         "0" = "Dead", 
                         "1" = "Alive")

# Ch???y LDA: 

set.seed(123)
lda <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                     Fare + Embarked + Title + 
                     FsizeD + Child + Mother, 
             data = train,
             method = "lda", 
             trControl = ctrl) 

# Logistic: 

set.seed(123)
logit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                       Fare + Embarked + Title + 
                       FsizeD + Child + Mother,
               data = train,
               method = "glm", 
               family = "binomial", 
               trControl = ctrl)

# Probit: 

set.seed(123)
probit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                        Fare + Embarked + Title + 
                        FsizeD + Child + Mother,
                data = train,
                method = "glm",
                family = "binomial"(link="probit"),
                trControl = ctrl) 

# SVM: 
set.seed(123)
svm <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                     Fare + Embarked + Title + 
                     FsizeD + Child + Mother ,
             data = train,
             method = "svmRadial",
             preProcess = c("center","scale"),
             trControl = ctrl) 

# RF: 
set.seed(123)
rf <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                    Fare + Embarked + Title + 
                    FsizeD + Child + Mother ,
            data = train,
            method = "rf",
            preProcess = c("center","scale"),
            trControl = ctrl) 

# ANN: 
set.seed(123)
ann <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                     Fare + Embarked + Title + 
                     FsizeD + Child + Mother ,
             data = train,
             method = "nnet",
             preProcess = c("center","scale"),
             trControl = ctrl, 
             trace = FALSE) 


#--------------------------------------------------------
#   Dánh giá các mô hình trên 1000 m???u khác nhau
#   và tính l???i nhu???n tuong ???ng. Gi??? thi???t r???ng
#   m???t h??? so x???u x???p nh???m thành h??? so t???t thì 
#   ngân  hàng  m???t tr???ng v???n. Còn m???t h??? so t???t
#   x???p dúng thành h??? so t???t ngân  hàng  ch???  lãi 5%
#--------------------------------------------------------


# Dánh giá LDA: 

TP <- c()
FP <- c()
TN <- c()
FN <- c()

set.seed(123)
for (i in 1:1000) {
        id <- createDataPartition(y = full$Survived, p = 0.5, list = FALSE)
        test <- full[id, ]
        pred1 <- predict(lda, newdata = test)
        u1 <- confusionMatrix(pred1, test$Survived, positive = NULL)
        k1 <- as.vector(u1$table)
        TN <- c(TN, k1[1])
        FP <- c(FP, k1[3])
        TP <- c(TP, k1[4])
        FN <- c(FN, k1[2])
        mydf1 <- data.frame(TP, FP, TN, FN)
}

# Dánh giá Logistic: 

TP <- c()
FP <- c()
TN <- c()
FN <- c()

set.seed(123)
for (i in 1:1000) {
        pred1 <- predict(logit, newdata = train)
        u1 <- confusionMatrix(pred1, test$Survived, positive = "Bad")
        k1 <- as.vector(u1$table)
        TN <- c(TN, k1[1])
        FP <- c(FP, k1[3])
        TP <- c(TP, k1[4])
        FN <- c(FN, k1[2])
        mydf2 <- data.frame(TP, FP, TN, FN)
}


# Dánh giá Probit:  

TP <- c()
FP <- c()
TN <- c()
FN <- c()

set.seed(123)
for (i in 1:1000) {
        id <- createDataPartition(y = nga$GB, p = 0.5, list = FALSE)
        test <- nga[id, ]
        pred1 <- predict(probit, newdata = test)
        u1 <- confusionMatrix(pred1, test$GB, positive = "Bad")
        k1 <- as.vector(u1$table)
        TN <- c(TN, k1[1])
        FP <- c(FP, k1[3])
        TP <- c(TP, k1[4])
        FN <- c(FN, k1[2])
        mydf3 <- data.frame(TP, FP, TN, FN)
}


# Dánh giá SVM: 

TP <- c()
FP <- c()
TN <- c()
FN <- c()

set.seed(123)
for (i in 1:1000) {
        id <- createDataPartition(y = nga$GB, p = 0.5, list = FALSE)
        test <- nga[id, ]
        pred1 <- predict(svm, newdata = test)
        u1 <- confusionMatrix(pred1, test$GB, positive = "Bad")
        k1 <- as.vector(u1$table)
        TN <- c(TN, k1[1])
        FP <- c(FP, k1[3])
        TP <- c(TP, k1[4])
        FN <- c(FN, k1[2])
        mydf4 <- data.frame(TP, FP, TN, FN)
}


# Dánh giá RF: 

TP <- c()
FP <- c()
TN <- c()
FN <- c()

set.seed(123)
for (i in 1:1000) {
        id <- createDataPartition(y = nga$GB, p = 0.5, list = FALSE)
        test <- nga[id, ]
        pred1 <- predict(rf, newdata = test)
        u1 <- confusionMatrix(pred1, test$GB, positive = "Bad")
        k1 <- as.vector(u1$table)
        TN <- c(TN, k1[1])
        FP <- c(FP, k1[3])
        TP <- c(TP, k1[4])
        FN <- c(FN, k1[2])
        mydf5 <- data.frame(TP, FP, TN, FN)
}

# Dánh giá ANN: 

TP <- c()
FP <- c()
TN <- c()
FN <- c()

set.seed(123)
for (i in 1:1000) {
        id <- createDataPartition(y = nga$GB, p = 0.5, list = FALSE)
        test <- nga[id, ]
        pred1 <- predict(ann, newdata = test)
        u1 <- confusionMatrix(pred1, test$GB, positive = "Bad")
        k1 <- as.vector(u1$table)
        TN <- c(TN, k1[1])
        FP <- c(FP, k1[3])
        TP <- c(TP, k1[4])
        FN <- c(FN, k1[2])
        mydf6 <- data.frame(TP, FP, TN, FN)
}

# Tính toán Profit và Accuracy: 

total <- rbind(mydf1, mydf2, mydf3, mydf4, mydf5, mydf6) %>% 
        mutate(Profit = 0.05*TN - FN, 
               Accuracy = (TP + TN) / (TP + FP + TN + FN),
               Model = c(rep("LDA", 1000), 
                         rep("Logit", 1000), 
                         rep("Probit", 1000), 
                         rep("SVM", 1000), 
                         rep("RF", 1000), 
                         rep("ANN", 1000)))


# X???p h???ng các mô  hình theo tiêu chí l???i nhu???n: 

prof <- total %>% group_by(Model) %>% 
        summarise_each(funs(NN = min, TB =  mean, LN =  max, LC =  sd), Profit) %>% 
        arrange(desc(TB))

prof

#  X???p  h???ng các mô hình theo tiêu  chí m???c chính xác trong phân lo???i: 


acc <- total %>% group_by(Model) %>% 
        summarise_each(funs(NN = min, TB =  mean, LN =  max, LC =  sd), Accuracy) %>% 
        arrange(desc(TB))

acc