library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(rmarkdown)
library(ggplot2)
library(cowplot)

# Loading NACC data set 
NACC <- read.csv(file = "/Users/lindseybrooks/Desktop/Practicum/Shi05082017-NACC.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)

# New data set from Variable selection
Data_Subset <- select(NACC, VISITYR,BIRTHYR,SEX,EDUC,NACCBMI,SMOKYRS,PACKSPER,ALCOHOL,NACCUDSD,CVHATT,CBSTROKE,HYPERTEN,DEP2YRS,NACCAPOE,NACCNE4S)


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

Data_Subset$NACCNE4S <- factor(Data_Subset$NACCNE4S,levels=c(0,1,2),labels=c("No e4 allele","1 copy of e4 allele","2 copies of e4 allele"))
Data_Subset$NACCNE4S[Data_Subset$NACCNE4S == 9] <- NA


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
                               is.na(Data_Subset$NACCAPOE) |
                               is.na(Data_Subset$NACCNE4S) 
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
xtabs(~ NACCUDSD + NACCNE4S, data=Data_Subset)
xtabs(~ NACCUDSD + NPTHAL, data=Data_Subset)
xtabs(~ NACCUDSD + AGE, data=Data_Subset)


# remap levels for binomial
Data_Subset$NACCUDSD <- mapvalues(Data_Subset$NACCUDSD, from = c("Impaired", "MCI", "Dementia"), to = c("Abnormal", "Abnormal", "Abnormal"))

logistic <- glm(NACCUDSD ~ ., data=Data_Subset, family="binomial")
summary(logistic)

predicted.data <- data.frame(
  probability.of.NACCUDSD=logistic$fitted.values,
  NACCUDSD=Data_Subset$NACCUDSD)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.NACCUDSD, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Plot the predicted probabilities for each sample having
## Alzheimers and color by whether or not they actually had Alzheimers
ggplot(data=predicted.data, aes(x=rank, y=probability.of.NACCUDSD)) +
  geom_point(aes(color=NACCUDSD), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting Alzheimers")


