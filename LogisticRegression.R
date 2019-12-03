setwd("/Users/lindseybrooks/Desktop/Practicum")

library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(rmarkdown)
library(cowplot)
library(caret)
library(data.table)
library(rattle)
library(rpart.plot)

##########################################
# Preprocessing of data

set.seed(123)
# Loading raw NACC data set 
NACC <- read.csv(file = "Shi05082017-NACC.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)

# New data set from Variable selection
Data_Subset <- select(NACC, NACCID, VISITYR,BIRTHYR,SEX,EDUC,NACCBMI,SMOKYRS,PACKSPER,ALCOHOL,NACCUDSD,CVHATT,CBSTROKE,HYPERTEN,DEP2YRS,NACCAPOE)


# Adding 'AGE' variable calculated from VISITYR and BIRTHYR variables
Data_Subset <- mutate(Data_Subset, AGE = VISITYR - BIRTHYR)

# Change 'SEX' numeric variable to be a factor
Data_Subset$SEX <- factor(Data_Subset$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# Cleaning 'EDUC' variable. 
# Higher educational level: doctorate = 20 to 25. Excludes records with higher values
Data_Subset <- filter(Data_Subset, Data_Subset$EDUC <= 25)

# Change 'EDUC' numeric variable to be a factor
Data_Subset$EDUC <- cut(Data_Subset$EDUC, breaks=c(0, 11, 15, 17, 19, 25), labels=c("Below high school or GRE", "high school or GRE", "bachelor’s degree", "master's degree", "doctorate"))

# Cleaning 'NACCBMI' variable.
Data_Subset <- filter(Data_Subset, Data_Subset$NACCBMI != 888.8, Data_Subset$NACCBMI != -4 )

# In 'SMOKYRS' numeric variable, replace values "88" to "NA", and "99" and "-4" to "Unknown"
Data_Subset$SMOKYRS[Data_Subset$SMOKYRS == 88] <- 0  # 88 is not applicable, assume this is 0
Data_Subset$SMOKYRS[Data_Subset$SMOKYRS == 99] <- NA
Data_Subset$SMOKYRS[Data_Subset$SMOKYRS == -4] <- NA

# Clean PACKSPER
Data_Subset$PACKSPER[Data_Subset$PACKSPER == 8] <- 0 # 8 is not applicable, assumet this is 0
Data_Subset$PACKSPER[Data_Subset$PACKSPER == 9] <- NA
Data_Subset$PACKSPER[Data_Subset$PACKSPER == -4] <- NA

# Create a smoking density variable
Data_Subset <- mutate(Data_Subset, HEAVY_SMOKER = SMOKYRS * PACKSPER)

# Change 'ALCOHOL' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset$ALCOHOL <- factor(Data_Subset$ALCOHOL,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset$ALCOHOL[Data_Subset$ALCOHOL == 9] <- NA
Data_Subset$ALCOHOL[Data_Subset$ALCOHOL == -4] <- NA

# Change 'NACCUDSD' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset$NACCUDSD <- factor(Data_Subset$NACCUDSD,levels=c(1:4),labels=c("Normal","Impaired","MCI","Dementia"))

# Change 'CVHATT' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset$CVHATT <- factor(Data_Subset$CVHATT,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset$CVHATT[Data_Subset$CVHATT == 9] <- NA
Data_Subset$CVHATT[Data_Subset$CVHATT == -4] <- NA

# Change 'CBSTROKE' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset$CBSTROKE <- factor(Data_Subset$CBSTROKE,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset$CBSTROKE[Data_Subset$CBSTROKE == 9] <- NA
Data_Subset$CBSTROKE[Data_Subset$CBSTROKE == -4] <- NA

Data_Subset$HYPERTEN <- factor(Data_Subset$HYPERTEN,levels=c(0,1,2),labels=c("Absent","Recent/Active","Remote/Inactive"))
Data_Subset$HYPERTEN[Data_Subset$HYPERTEN == 9] <- NA
Data_Subset$HYPERTEN[Data_Subset$HYPERTEN == -4] <- NA                                                                  

Data_Subset$DEP2YRS <- factor(Data_Subset$DEP2YRS,levels=c(0,1),labels=c("NO","YES"))
Data_Subset$DEP2YRS[Data_Subset$DEP2YRS == 9] <- NA
Data_Subset$DEP2YRS[Data_Subset$DEP2YRS == -4] <- NA

Data_Subset$NACCAPOE <- factor(Data_Subset$NACCAPOE, levels=c(1,2,3,4,5,6),labels=c("e3e3","e3e4","e3e2","e4e4","e4e2","e2e2"))
Data_Subset$NACCAPOE[Data_Subset$NACCAPOE == 9] <- NA
Data_Subset$NACCAPOE[Data_Subset$NACCAPOE == -4] <- NA

summary(Data_Subset)

# Remove instances with NA values
nrow(Data_Subset) 
Data_Subset <- Data_Subset[!(is.na(Data_Subset$EDUC) |
                               is.na(Data_Subset$HEAVY_SMOKER) |
                               is.na(Data_Subset$ALCOHOL) |
                               is.na(Data_Subset$CVHATT) |
                               is.na(Data_Subset$CBSTROKE) |
                               is.na(Data_Subset$HYPERTEN) |
                               is.na(Data_Subset$DEP2YRS) |
                               is.na(Data_Subset$NACCBMI) |
                               is.na(Data_Subset$NACCAPOE)
),]
nrow(Data_Subset) 


# check for duplicated survey results by same participant
uniq_ids <- aggregate(data.frame(count = Data_Subset$NACCID), list(value = Data_Subset$NACCID), length)

# we only want the latest observation from the participant
# we should end up with 8784 observations
Data_Subset <- setDT(Data_Subset)[,.SD[which.max(VISITYR)],keyby=NACCID]

summary(Data_Subset)
str(Data_Subset)

# transform the data set and check for collinearity
D <- subset(Data_Subset)
D$SEX <- as.numeric(Data_Subset$SEX)
D$EDUC <- as.numeric(Data_Subset$EDUC)
D$ALCOHOL <- as.numeric(Data_Subset$ALCOHOL)
D$CVHATT <- as.numeric(Data_Subset$CVHATT)
D$CBSTROKE <- as.numeric(Data_Subset$CBSTROKE)
D$HYPERTEN <- as.numeric(Data_Subset$HYPERTEN)
D$DEP2YRS <- as.numeric(Data_Subset$DEP2YRS)
D$NACCAPOE <- as.numeric(Data_Subset$NACCAPOE)
str(D)
df <- subset(D, select = -c(1,2,3,7,8,10) )
findCorrelation( cor(df), cutoff = .75, names = TRUE )

##########################################
# Descriptive Analysis

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

hist(Data_Subset$AGE, breaks = 100, col = cbPalette[4], main = "Age Distribution", border = F, xlab = "Age")

hist(Data_Subset$NACCBMI, col = cbPalette[4], main = "Distribution of BMI", 
     xlab = "BMI", ylab = "# Participants" )

agg_as <- aggregate(HEAVY_SMOKER ~ NACCUDSD, data = Data_Subset, mean)

ggplot(agg_as, aes(x = NACCUDSD, y = HEAVY_SMOKER)) + geom_bar(stat = "identity", fill = cbPalette[4]) + 
  ggtitle("Years times packs of smokes by Cognitive Status") + 
  labs (y = "Years times packs of smokes", x = "Cognitive Status")

boxplot(NACCBMI~NACCUDSD,data=Data_Subset, xlab="NACCUDSD", ylab="NACCBMI")
boxplot(AGE~NACCUDSD,data=Data_Subset, xlab="NACCUDSD", ylab="AGE")




##########################################
# Predictive Analysis

# attempt at the multinomial logit model
library(mlogit)
longdata <- mlogit.data(Data_Subset, choice = "NACCUDSD", shape = "wide")
mlogitmodel <- mlogit(NACCUDSD ~ 1 | SEX + EDUC + NACCBMI + ALCOHOL + CVHATT + CBSTROKE + HYPERTEN + DEP2YRS + NACCAPOE + AGE + HEAVY_SMOKER, data = longdata, reflevel = "Dementia")
summary(mlogitmodel)

correct <- mlogitmodel$probabilities
binarycorrect <- colnames(correct)[apply(correct,1,which.max)]
table(Data_Subset$NACCUDSD, binarycorrect)

# remap levels for binomial
Data_Subset$NACCUDSD <- mapvalues(Data_Subset$NACCUDSD, from = c("Impaired", "MCI", "Dementia"), to = c("Abnormal", "Abnormal", "Abnormal"))

# split data into training and test, based on values of dependent variable 
trainIndex <- createDataPartition(Data_Subset$NACCUDSD, p = .75,list=FALSE)
training <- Data_Subset[trainIndex,]
testing <- Data_Subset[-trainIndex,]
trCntl <- trainControl(method = "CV",number = 5)
glmModel <- train(NACCUDSD ~ SEX + EDUC + NACCBMI + ALCOHOL + CVHATT + CBSTROKE + HYPERTEN + DEP2YRS + NACCAPOE + AGE + HEAVY_SMOKER,data = training,trControl = trCntl,method="glm",family = "binomial")
summary(glmModel)

trainPredicted <- predict(glmModel,testing)

confusionMatrix(trainPredicted,reference=testing$NACCUDSD)

m2 <- glm(NACCUDSD ~ SEX + EDUC + NACCBMI + ALCOHOL + CVHATT + CBSTROKE + HYPERTEN + DEP2YRS + NACCAPOE + AGE + HEAVY_SMOKER, data = training, family = 'binomial')
summary(m2)

hist(m2$fitted.values, main = "Distribution of Predicted Probabilities", 
     xlab = "Probability of Cognitive Impairment", col = cbPalette[4], border = F, breaks = 200)
abline(v = .5, col = "red", lwd = 3)

prop.table(table(m2$fitted.values >= .5)) # 0.006829564
prop.table(table(m2$fitted.values >= .3)) # 0.03748672

#To figure out how acccurate our model is at predicting cognitive impairment, 
# we need to first take our model and use it to predict the outcomes for our test dataset. 

m2_test <- predict(m2, newdata = testing, type = "response")

hist(m2_test, main = "Distribution of Test Set \nPredicted Probabilities", 
     xlab = "Probability of Cognitive Impairment", col = cbPalette[4], border = F, breaks = 200)
abline(v = .5, col = "red", lwd = 3)

prop.table(table(m2_test >= .5)) # 0.003189066
prop.table(table(m2_test >= .3)) # 0.0332574


# Decision Trees
dtrees_fit <- rpart(NACCUDSD ~ SEX + EDUC + NACCBMI + ALCOHOL + CVHATT + CBSTROKE + HYPERTEN + DEP2YRS + NACCAPOE + AGE + HEAVY_SMOKER, data=training,
             method="class")
dtrees_fit # basic model results

summary(dtrees_fit)

par(mar = c(5,4,1,2)) #setting the margins
fancyRpartPlot(dtrees_fit, sub = NULL, main = "Final Decision Tree")

fit_test <- predict(dtrees_fit, newdata= testing, type = "prob")

prop.table(table(testing$NACCUDSD))
accuracy <- table(fit_test[,2] > .5, testing$NACCUDSD)

addmargins(table(fit_test[,2] > .5, testing$NACCUDSD))

sum(diag(accuracy))/ sum(accuracy)

##########################################
# ROC Curve
library(ROCR)

# transform the DV so that it is numeric
# 0 = normal
# 1 = abnormal
testing$NACCUDSD <- as.integer(testing$NACCUDSD)
testing$NACCUDSD <- testing$NACCUDSD - 1
model <- glm(NACCUDSD ~ SEX + EDUC + NACCBMI + ALCOHOL + CVHATT + CBSTROKE + HYPERTEN + DEP2YRS + NACCAPOE + AGE + HEAVY_SMOKER,family=binomial(link='logit'),data=training)
p <- predict(model, newdata=subset(testing,select=c(4,5,6,9,11,12,13,14,15,16,17)), type="response")
pr <- prediction(p, testing$NACCUDSD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc






