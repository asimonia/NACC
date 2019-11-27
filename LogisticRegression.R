library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(rmarkdown)
library(ggplot2)
library(cowplot)
library(caret)

# Loading NACC data set 
NACC <- read.csv(file = "/Users/lindseybrooks/Desktop/Practicum/Shi05082017-NACC.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)

# New data set from Variable selection
Data_Subset <- select(NACC, VISITYR,BIRTHYR,SEX,EDUC,NACCBMI,SMOKYRS,PACKSPER,ALCOHOL,NACCUDSD,CVHATT,CBSTROKE,HYPERTEN,DEP2YRS,NACCAPOE)


# Adding 'AGE' variable calculated from VISITYR and BIRTHYR variables
Data_Subset <- mutate(Data_Subset, AGE = VISITYR - BIRTHYR)

# Remove VISITYR and BIRTHYR now we have age
Data_Subset <- Data_Subset[-c(1, 2)]

# Change 'SEX' numeric variable to be a factor
Data_Subset$SEX <- factor(Data_Subset$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# Cleaning 'EDUC' variable. 
# Higher educational level: doctorate = 20 to 25. Excludes records with higher values
Data_Subset <- filter(Data_Subset, Data_Subset$EDUC <= 25)

# Change 'EDUC' numeric variable to be a factor
Data_Subset$EDUC <- cut(Data_Subset$EDUC, breaks=c(0, 11, 15, 17, 19, 25), labels=c("Below high school or GRE", "high school or GRE", "bachelor’s degree", "master's degree", "doctorate"))

# Cleaning 'NACCBMI' variable.
Data_Subset <- filter(Data_Subset, Data_Subset$NACCBMI != "888.8", Data_Subset$NACCBMI != "-4" )

# In 'SMOKYRS' numeric variable, replace values "88" to "NA", and "99" and "-4" to "Unknown"
Data_Subset$SMOKYRS[Data_Subset$SMOKYRS == 88] <- 0
Data_Subset$SMOKYRS[Data_Subset$SMOKYRS == 99] <- NA
Data_Subset$SMOKYRS[Data_Subset$SMOKYRS == -4] <- NA

# Change 'PACKSPER' numeric variable to be a factor
Data_Subset$PACKSPER[Data_Subset$PACKSPER == 9] <- NA
Data_Subset$PACKSPER[Data_Subset$PACKSPER == -4] <- NA
Data_Subset$PACKSPER <- factor(Data_Subset$PACKSPER, levels = c(0, 1, 2, 3, 4, 5), labels = c("No reported cigarette use", "1 cigarette to less than 1/2 pack", "½ pack to less than 1 pack", "1 pack to 1½ packs", "1½ packs to 2 packs", "More than two packs"))

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


View(Data_Subset)
write.csv(Data_Subset, file = "Data_Subset.csv")
summary(Data_Subset)

# Remove instances with NA values
nrow(Data_Subset) # 43086 rows
Data_Subset <- Data_Subset[!(is.na(Data_Subset$SMOKYRS) | 
                               is.na(Data_Subset$EDUC) |
                               is.na(Data_Subset$PACKSPER) |
                               is.na(Data_Subset$ALCOHOL) |
                               is.na(Data_Subset$CVHATT) |
                               is.na(Data_Subset$CBSTROKE) |
                               is.na(Data_Subset$HYPERTEN) |
                               is.na(Data_Subset$DEP2YRS) |
                               is.na(Data_Subset$NACCAPOE)
),]
nrow(Data_Subset)  # 32117 rows
summary(Data_Subset)

hist(Data_Subset$AGE)
hist(Data_Subset$NACCBMI)
boxplot(NACCBMI~NACCUDSD,data=Data_Subset, xlab="NACCUDSD", ylab="NACCBMI")
boxplot(AGE~NACCUDSD,data=Data_Subset, xlab="NACCUDSD", ylab="AGE")


## Quality control by making sure all of the factor
## levels are represented by people with and without Alzheimers
xtabs(~ NACCUDSD + SEX, data=Data_Subset)
xtabs(~ NACCUDSD + EDUC, data=Data_Subset)
xtabs(~ NACCUDSD + NACCBMI, data=Data_Subset)
xtabs(~ NACCUDSD + SMOKYRS, data=Data_Subset)
xtabs(~ NACCUDSD + PACKSPER, data=Data_Subset)
xtabs(~ NACCUDSD + ALCOHOL, data=Data_Subset)
xtabs(~ NACCUDSD + CVHATT, data=Data_Subset)
xtabs(~ NACCUDSD + CBSTROKE, data=Data_Subset)
xtabs(~ NACCUDSD + HYPERTEN, data=Data_Subset)
xtabs(~ NACCUDSD + DEP2YRS, data=Data_Subset)
xtabs(~ NACCUDSD + NACCAPOE, data=Data_Subset)
xtabs(~ NACCUDSD + CBSTROKE, data=Data_Subset)
xtabs(~ NACCUDSD + AGE, data=Data_Subset)


# remap levels for binomial
Data_Subset$NACCUDSD <- mapvalues(Data_Subset$NACCUDSD, from = c("Impaired", "MCI", "Dementia"), to = c("Abnormal", "Abnormal", "Abnormal"))


# split data into training and test, based on values of dependent variable 
trainIndex <- createDataPartition(Data_Subset$NACCUDSD, p = .75,list=FALSE)
training <- Data_Subset[trainIndex,]
testing <- Data_Subset[-trainIndex,]
trCntl <- trainControl(method = "CV",number = 5)
glmModel <- train(NACCUDSD ~ .,data = training,trControl = trCntl,method="glm",family = "binomial")


summary(glmModel)
glmModel


trainPredicted <- predict(glmModel,testing)

confusionMatrix(trainPredicted,reference=testing$NACCUDSD)

# plot the ROC curve and calculate the AUC (area under the curve) which are 
# typical performance measurements for a binary classifier.
# The ROC is a curve generated by plotting the true positive rate (TPR) 
# against the false positive rate (FPR) at various threshold settings while 
# the AUC is the area under the ROC curve. As a rule of thumb, a model with 
# good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
library(ROCR)
model <- glm(NACCUDSD ~.,family=binomial(link='logit'),data=training)
p <- predict(model, newdata=subset(testing,select=c(1,2,3,4,5,6,8,9,10,11,12,13)), type="response")
pr <- prediction(p, testing$NACCUDSD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

