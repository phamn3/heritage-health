##########Heritage Health Network Analysis
rm(list=ls())
cat("\014")
dev.off()
getwd()

#load in required packages
if(!(require(readr))) install.packages("readr")
library(readr)
if(!(require(dplyr))) install.packages("dplyr")
if(!(require(ggplot2))) install.packages("ggplot2")
if(!require(ggcorrplot)) install.packages("ggcorrplot")
if(!require(ggcorrplot)) install.packages("ggcorrplot")
if(!require(purrr)) install.packages("purrr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(glmnet)) install.packages("glmnet")
if(!require(Metrics)) install.packages("Metrics")
library(purrr)
library(tidyr)
library(Metrics)
library(glmnet)

############### YEAR 1 ################################################################
#######################################################################################
#load in dataset
DIH_Y1_Target <- read_csv("DIH_Y1_Target")
df1<- DIH_Y1_Target

################################ Prelim look at the data ###############################
attach(df1)
str(df1) #overview of data types
colnames(df1)
summary(df1) #summary statistics
dim(df1)
sapply(df1, function(x) sum(is.na(x))) # of missing variables
View(df1)

length(unique(MemberID)) #76038 unique patients

## plot of missing values
df1 %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather %>% 
  ggplot(aes(x = reorder(key, value), y = value)) + geom_bar(stat = "identity") +
  coord_flip() +
  xlab("variable") +
  ylab("Absolute number of missings")



################# Data Manipulation #####################
# fixing DSFS - days since first claim
df1$DSFS<- case_when(
  df1$DSFS == "0- 1 month" ~ 1,
  df1$DSFS == "1- 2 months" ~ 2,
  df1$DSFS == "2- 3 months" ~ 3,
  df1$DSFS == "3- 4 months" ~ 4,
  df1$DSFS == "4- 5 months" ~ 5,
  df1$DSFS == "5- 6 months" ~ 6,
  df1$DSFS == "6- 7 months" ~ 7,
  df1$DSFS == "7- 8 months" ~ 8,
  df1$DSFS == "8- 9 months" ~ 9,
  df1$DSFS == "9-10 months" ~ 10,
  df1$DSFS == "10-11 months" ~ 11,
  df1$DSFS == "11-12 months" ~ 12,
  TRUE~0
)
#sanity check
summary(df1$DSFS)
df1$DSFS <- as.integer(df1$DSFS)

#dropping Year column
df1$Year <- NULL

#converting pseudonyms for EDA later on
df1$MemberID <- as.character(df1$MemberID)
df1$ProviderID <- as.character(df1$ProviderID)
df1$Vendor <- as.character(df1$Vendor)
df1$PCP <- as.character(df1$PCP)

#fixing payDelay
df1$PayDelay[df1$PayDelay == "162+"] <- 162
df1$PayDelay<- as.integer(df1$PayDelay)
str(df1$PayDelay)

# fixing length of stay
df1$LengthOfStay<- case_when(
  df1$LengthOfStay == "1 day" ~ 1,
  df1$LengthOfStay == "2 days" ~ 2,
  df1$LengthOfStay == "3 days" ~ 3,
  df1$LengthOfStay == "4 days" ~ 4,
  df1$LengthOfStay == "5 days" ~ 5,
  df1$LengthOfStay == "6 days" ~ 6,
  df1$LengthOfStay == "1- 2 weeks" ~ 11,
  df1$LengthOfStay == "2- 4 weeks" ~ 21,
  df1$LengthOfStay == "4- 8 weeks" ~ 42,
  df1$LengthOfStay == "26+ weeks" ~ 182,
  TRUE~0
)
summary(df1$LengthOfStay)
df1$LengthOfStay <- as.integer(df1$LengthOfStay)
str(df1$LengthOfStay)

# fixing drug count
df1$DrugCount[df1$DrugCount == "7+"] <- 7
df1$DrugCount[is.na(df1$DrugCount)] <- 0
df1$DrugCount<- as.integer(df1$DrugCount)
str(df1$DrugCount)
summary(df1$DrugCount)

# fixing the lab count 
df1$LabCount[df1$LabCount == "10+"] <- 10
df1$LabCount[is.na(df1$LabCount)]<- 0
df1$LabCount<- as.integer(df1$LabCount)
str(df1$LabCount)
summary(df1$LabCount)

# fixing the age - taking the median of the age range
# 99 represents missing value
df1$AgeAtFirstClaim<- case_when(
  df1$AgeAtFirstClaim == "0-9" ~ 5,
  df1$AgeAtFirstClaim == "10-19" ~ 15,
  df1$AgeAtFirstClaim == "20-29" ~ 25,
  df1$AgeAtFirstClaim == "30-39" ~ 35,
  df1$AgeAtFirstClaim == "40-49" ~ 45,
  df1$AgeAtFirstClaim == "50-59" ~ 55,
  df1$AgeAtFirstClaim == "60-69" ~ 65,
  df1$AgeAtFirstClaim == "70-79" ~ 75,
  df1$AgeAtFirstClaim == "80+" ~ 80,
  TRUE~ 99
)
summary(df1$AgeAtFirstClaim)
df1$AgeAtFirstClaim<- as.integer(df1$AgeAtFirstClaim)
str(df1$AgeAtFirstClaim)

# fixing the sex
df1$Sex[is.na(df1$Sex)]<- "Missing_Sex"

#checking for any more null values
sapply(df1, function(x) sum(is.na(x)))

# fixing the missing values in categorical variables
df1$ProviderID[is.na(df1$ProviderID)]<- "Missing_ProviderID"
df1$Vendor[is.na(df1$Vendor)]<- "Missing_Vendor"
df1$PCP[is.na(df1$PCP)]<- "Missing_PCP"
df1$Specialty[is.na(df1$Specialty)]<- "Missing_Specialty"
df1$PlaceSvc[is.na(df1$PlaceSvc)]<- "Missing_PlaceSvc"
df1$PrimaryConditionGroup[is.na(df1$PrimaryConditionGroup)]<- "Missing_PCGroup"
df1$ProcedureGroup[is.na(df1$ProcedureGroup)]<- "Missing_ProcedureGroup"

str(df1)

################################ Exploratory Data Analysis #################################
ggplot(df1, aes(DSFS)) + geom_histogram(binwidth = 1) + ggtitle("DSFS") + xlab("DSFS")
ggplot(df1, aes(Specialty)) + geom_bar(stat="count") + xlab("Specialty") + coord_flip()
ggplot(df1, aes(PlaceSvc)) + geom_bar(stat="count") + xlab("PlaceSvc") + coord_flip()
ggplot(df1, aes(PayDelay)) + geom_histogram(binwidth = 10) + ggtitle("PayDelay") + xlab("PayDelay")

#Length of Stay
ggplot(df1, aes(LengthOfStay)) + geom_histogram() + ggtitle("LengthOfStay") + xlab("LengthOfStay")
stem(df1$LengthOfStay)

#CharlsonIndex
ggplot(df1, aes(CharlsonIndex)) + geom_bar(stat="count") + xlab("CharlsonIndex")

#Drug Count
ggplot(df1, aes(DrugCount)) + geom_bar(stat="count") + ggtitle("DrugCount") + xlab("DrugCount")

#Lab Count
ggplot(df1, aes(LabCount)) + geom_bar(stat="count") + ggtitle("LabCount") + xlab("LabCount")

#Age
ggplot(df1, aes(AgeAtFirstClaim)) + geom_histogram(binwidth = 10) + ggtitle("AgeAtFirstClaim") + xlab("AgeAtFirstClaim")

#Sex - majority are female
ggplot(df1, aes(Sex)) + geom_bar(stat="count") + ggtitle("Sex") + xlab("Sex")

#DaysInHospital
ggplot(df1, aes(DaysInHospital)) + geom_bar(stat="count") + ggtitle("DaysInHospital") + xlab("DaysInHospital")
ggplot(df1,aes(AgeAtFirstClaim,DaysInHospital)) + geom_jitter()
ggplot(df1,aes(AgeAtFirstClaim,DrugCount)) + geom_jitter()

###################### Correlations ###############################################
#pulling out numerical variables
num_df1 <- df1[,c("DSFS", "PayDelay","LengthOfStay","SupLOS","DrugCount","LabCount","AgeAtFirstClaim",
                "ClaimsTruncated","DaysInHospital")]
corr <- round(cor(num_df1, use="all.obs", method = "pearson"),5)
corr
ggcorrplot(corr,tl.cex = 6, tl.srt = 90) + ggtitle("Correlation of Numerical Variables")
#highest positive correlation is between Age and DrugCount


########################### Categorical variables ##################################
ggplot(data = df1) + geom_count(mapping = aes(x = Sex, y = Specialty))

ggplot(data = df1) + geom_count(mapping = aes(x = Sex, y = AgeAtFirstClaim))

ggplot(data = df1) + geom_count(mapping = aes(x = CharlsonIndex, y = AgeAtFirstClaim))

ggplot(data = df1) + geom_count(mapping = aes(x = LengthOfStay, y = PrimaryConditionGroup))

ggplot(data = df1) + geom_count(mapping = aes(x = LengthOfStay, y = Specialty))

ggplot(data = df1) + geom_count(mapping = aes(x = LengthOfStay, y = AgeAtFirstClaim))

ggplot(data = df1) + geom_count(mapping = aes(y = PlaceSvc, x = AgeAtFirstClaim))

ggplot(data = df1) + geom_count(mapping = aes(x = PlaceSvc, y = Specialty))

ggplot(data = df1) + geom_count(mapping = aes(x = Sex, y = PrimaryConditionGroup))

ggplot(data = df1) + geom_count(mapping = aes(x = Sex, y = DrugCount))

ggplot(data = df1) + geom_count(mapping = aes(y = AgeAtFirstClaim, x = DrugCount))


##########################################################################################
# before output to python lets fix char variable to int
df_python <- df1
str(df_python)
df_python$ProviderID<- as.integer(df_python$ProviderID)
df_python$Vendor<- as.integer(df_python$Vendor)
df_python$PCP<- as.integer(df_python$PCP)

sapply(df_python, function(x) sum(is.na(x))) # of missing variables


#output a csv to try in creating dummy /aggreagation in python
# Write CSV in R
write.csv(df_python, file = "Dummy1.csv")

#########################################################################################
# bring the file back from python with dummy variables
df1_Agg <- read_csv("out_Agg_dummy_Y1.csv")


colnames(df1_Agg)
#drop duplicate column that got introduced in python code
df1_Agg$MemberID_Count_1<- NULL
#fix the values of sex variable
df1_Agg$Sex_F <- ifelse(df1_Agg$Sex_F > 0, 1, 0)
df1_Agg$Sex_M <- ifelse(df1_Agg$Sex_M > 0, 1, 0)
df1_Agg$Sex_Missing <- ifelse(df1_Agg$Sex_Missing > 0, 1, 0)

df1_Agg<- df1_Agg[df1_Agg$DaysInHospital< 365,]


################## Last Minute EDA for some variables #########################################
hist((df1_Agg$DaysInHospital[df1_Agg$DaysInHospital>0]), col="darkgreen", main="Agg_DIH", xlab="DIH", ylab="SUM")

# correlation between dayinhospital and rest of the variables.
cor_output<- as.data.frame(cor(df1_Agg$DaysInHospital,df1_Agg))

corr <- round(cor(df1_Agg, use="all.obs", method = "pearson"),5)
corr
melt_corr <- melt(corr)
attach(melt_corr)
head(melt_corr[order(value, decreasing=TRUE), ], 150)
tail(melt_corr[order(value, decreasing=TRUE), ], 30)
ggcorrplot(corr,tl.cex = 6, tl.srt = 90) + ggtitle("Correlation of Numerical Variables")



###################### adding log of DaysInHospital ###################################
df1_Agg$LogDaysInHospital <- log(df1_Agg$DaysInHospital + 1)

### check to see the distribution of dependent variables after taking log.
hist((df1_Agg$LogDaysInHospital), col="darkgreen", main="Agg_DIH", xlab="Log_DIH", ylab="SUM")


colnames(df1_Agg)

####################################### Modeling #######################################
################ 1. Base Model 1
#results in rmse = 1.3745
rmse(log(0 + 1), df2_Agg$LogDaysInHospital)

################ 2. Base Model 2
# Testing a base model2 with DIH_Y2 = mean(DIH_Y1), results in rmse = 1.2825
rmse(mean(df1_Agg$LogDaysInHospital, na.rm = TRUE), df2_Agg$LogDaysInHospital)


############### 3. Linear Model
#### First iteration - testing linear model with few features, not improving the rmse , can be ignored
#n_data<- df1_Agg[c(1,21:66,108)]
#m_data<- df2_Agg[c(1, 21:66,108)]
#linear.model <- lm(LogDaysInHospital ~. , data=n_data)
#summary(linear.model)

#pred1 <- predict(linear.model, newdata=df2_Agg[,-c(7,108)])

#m<-pred1
#o<-df2_Agg[108]
#rm<- (sqrt(mean((m - o)^2)))

#rmse(o,m)

#### Actual Iteration - linear model using LogDaysInHospital
#rmse = 1.1872, R squared very low = 0.1381

linear.model1 <- lm(LogDaysInHospital ~. , data=df1_Agg[-7])
summary(linear.model1)

pred_lm <- predict(linear.model1, newdata=df2_Agg[,-c(7,108)])
pred_lm

m<-pred_lm
o<-df2_Agg[108]
rm<- (sqrt(mean((m - o)^2)))
rmse(o,m)

# tranforming back to normal number by taking "exp" of the predictors.
dihY2_lm<- floor(exp(pred_lm) - 1)
summary(dihY2_lm)


############### 4. lasso regression ############## rmse = 1.2598
x<- as.matrix(df1_Agg[,-c(7,108)])
y<- df1_Agg$LogDaysInHospital
x1<- as.matrix(df2_Agg[,-c(7,108)])
y1<-df2_Agg$LogDaysInHospital

lambdas <- 10^seq(3, -2, by = -.1)

lasso.mod <- glmnet(x, y, alpha = 1)
lasso.pred <- predict(lasso.mod, s = lambdas, newx = x1)
rm<- (sqrt(mean((lasso.pred-y1)^2)))


# tranforming back to normal number by taking "exp" of the predictors.
dihY2_lasso<-as.vector(floor(exp(lasso.pred) - 1))
summary(dihY2_lasso)

#################### 5. glm #################### rmse = 1.3209
# GBM
mod_glm <- glm(LogDaysInHospital ~ ., family = "poisson", data = df1_Agg[, -c(1, 7)])
pred_glm_orig <- predict(mod_glm, df2_Agg[,-c(1, 7,108)])
pred_glm <- log(ifelse(pred_glm_orig < 0, 0, pred_glm_orig) + 1)
rmse(pred_glm, df2_Agg$LogDaysInHospital)

# tranforming back to normal number by taking "exp" of the predictors.
dihY2_glm<- floor(exp(pred_glm) - 1)
summary(dihY2_glm)


############ Tried this iteration but did not work
# Stepwise Regression
library(MASS)
#linear.model <- lm(LogDaysInHospital ~. , data=df1_Agg[-7])
#summary(linear.model1)

#step <- stepAIC(linear.model1, direction="both")
#step$anova # display results

###############ridge regression using glmnet ############### R squared = 0.1433
x<- as.matrix(df1_Agg[,-c(7,108)])
y<- df1_Agg$LogDaysInHospital

lambdas <- 10^seq(3, -2, by = -.1)

fit <- glmnet(x , y, alpha = 0, lambda = lambdas)
summary(fit)

cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda

fit <- cv_fit$glmnet.fit
summary(fit)

x1<- as.matrix(df2_Agg[,-c(7,108)])

y_predicted <- predict(fit, s = opt_lambda, newx = x1)

y1<-df2_Agg$LogDaysInHospital
# Sum of Squares Total and Error
sst <- sum((y1 - mean(y1))^2)
sse <- sum((y_predicted - y1)^2)

# R squared
rsq <- 1 - sse / sst
rsq

# tranforming back to normal number by taking "exp" of the predictors.
dihY2_glm<- floor(exp(y_predicted) - 1)
summary(dihY2_glm)


######## ENSEMBLING
#ensemble model with weighted average of Lasso, glm and Linear model improves rmse slighlty from 
## lowest rmse we get from linear model of  1.1872  to 1.186385
pred_ensemble<- 0.1*lasso.pred  + 0.04*pred_glm + 0.86*pred_lm
pred_ensemble
rmse(pred_ensemble, df2_Agg$LogDaysInHospital)

normal_pred<- as.vector(floor(exp(pred_ensemble) - 1))
normal_pred
summary(normal_pred)


#random forest ####################### doesn't stop so comenting out 
set.seed(123)
require(randomForest)

library(randomForest)
#set.seed(1234)
#mod_rf <- randomForest(LogDaysInHospital ~ ., 
#                       data = df1_Agg[-7],
#                       importance = TRUE, ntree = 2000, mtry = 3, 
#                       nodesize = 10, maxnodes = 500, replace = FALSE, 
#                       do.trace = 10)
#pred_rf <- predict(mod_rf, df2_Agg[,-c(7,108)])
#rmse(pred_rf, df2_Agg$LogDaysInHospital)



             
