
library(readr)
DIH_Y1_Target <- read_csv("DIH_Y1_Target")
df1<- DIH_Y1_Target
View(df1)
require(dplyr)

str(df1)
colnames(df1)

sapply(df1, function(x) sum(is.na(x)))

sum(is.na(df1$DSFS))
sum(is.na(df1$Sex))


# fixing DSFS
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
str(df1$LengthOfStay)

# fixing drug count
df1$DrugCount[df1$DrugCount == "7+"] <- 7
df1$DrugCount[is.na(df1$DrugCount)]<- 0
df1$DrugCount<- as.integer(df1$DrugCount)
str(df1$DrugCount)
summary(df1$DrugCount)

# fixing the lab count 
df1$LabCount[df1$LabCount == "10+"] <- 10
df1$LabCount[is.na(df1$LabCount)]<- 0
df1$LabCount<- as.integer(df1$LabCount)
str(df1$LabCount)
summary(df1$LabCount)

# fixing the age
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
str(df1$AgeAtFirstClaim)

# fixing the sex
df1$Sex[is.na(df1$Sex)]<- "Missing_Sex"

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



# before output to python lets fix char variable to int
df_python<-df1
str(df_python)
df_python$ProviderID<- as.numeric(df_python$ProviderID)
df_python$Vendor<- as.numeric(df_python$Vendor)
df_python$PCP<- as.numeric(df_python$PCP)

sapply(df_python, function(x) sum(is.na(x)))



#output a csv to try in creating dummy /aggreagation in python
# Write CSV in R
write.csv(df_python, file = "MyData_dummy1.csv")



# bring the file back from python with dummy variables
df_Agg1 <- read_csv("out_Agg_dummy_r01.csv")












             
