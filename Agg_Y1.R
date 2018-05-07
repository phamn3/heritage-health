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
#majority of claims were within 1 month of the first claim of that year (aka a lot of revisits)
ggplot(df1, aes(DSFS)) + geom_histogram(binwidth = 1) + ggtitle("DSFS") + xlab("DSFS")

#Specialty - most go into for Lab tests, Internal Med, or General Practice
ggplot(df1, aes(Specialty)) + geom_bar(stat="count") + xlab("Specialty") + coord_flip()

#PlaceSvc - majority to an Office
ggplot(df1, aes(PlaceSvc)) + geom_bar(stat="count") + xlab("PlaceSvc") + coord_flip()

#PayDelay - majority paydelays between 30 to 40 days
ggplot(df1, aes(PayDelay)) + geom_histogram(binwidth = 10) + ggtitle("PayDelay") + xlab("PayDelay")

#Length of Stay
# alot of missing variables (0 length of stay), after that ppl typically stay 1 day at the hospital
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






###categorical variables
ggplot(data = df1) +
  geom_count(mapping = aes(x = Sex, y = Specialty))

ggplot(data = df1) +
  geom_count(mapping = aes(x = Sex, y = AgeAtFirstClaim))

ggplot(data = df1) +
  geom_count(mapping = aes(x = CharlsonIndex, y = AgeAtFirstClaim))

ggplot(data = df1) +
  geom_count(mapping = aes(x = LengthOfStay, y = PrimaryConditionGroup))

ggplot(data = df1) +
  geom_count(mapping = aes(x = LengthOfStay, y = Specialty))

ggplot(data = df1) +
  geom_count(mapping = aes(x = LengthOfStay, y = AgeAtFirstClaim))

ggplot(data = df1) +
  geom_count(mapping = aes(y = PlaceSvc, x = AgeAtFirstClaim))


ggplot(data = df1) +
  geom_count(mapping = aes(x = PlaceSvc, y = Specialty))


ggplot(data = df1) +
  geom_count(mapping = aes(x = Sex, y = PrimaryConditionGroup))
### end categorical variables


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


#EDA for some variables
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




####### adding log of DaysInHospital"
df1_Agg$LogDaysInHospital <- log(df1_Agg$DaysInHospital + 1)



#### linear model using LogDaysInHospital
linear.model <- lm(LogDaysInHospital ~. , data=df1_Agg[-7])
summary(linear.model)

pred1 <- predict(linear.model, newdata=df2_Agg[,-c(7,108)])
pred1


m<-pred1
o<-df2_Agg[108]

rm<- (sqrt(mean((m - o)^2)))

rmse(o,m)
###############ridge regression using glmnet ##################
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




# lasso
library(glmnet)

lasso.mod <- glmnet(x, y, alpha = 1)
lasso.pred <- predict(lasso.mod, s = lambdas, newx = x1)
rm<- (sqrt(mean((lasso.pred-y1)^2)))



#random forest #######################
set.seed(123)
require(randomForest)
model.rf=randomForest(df1_Agg$LogDaysInHospital ~ . , data = df1_Agg[-7])

set.seed(1)
rf_mod <- randomForest(LogDaysInHospital~., data=df1_Agg)
print(rf_mod) #print results
importance(rf_mod) #look at importance of predictors
varImpPlot(rf_mod)

#predictions with probabilities
rf_pred <- predict(rf_mod, newdata=test[,-23], type="prob")
rf_auc <- auc(test$readmitted, rf_pred[,2])
plot(roc(test$readmitted,rf_pred[,2]), main="ROC of Random Forest")
rf_auc



             
