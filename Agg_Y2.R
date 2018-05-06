############ YEAR 2 ####################################################################
########################################################################################
#run with previous Agg_Y1 R notebook
#load in dataset
DIH_Y2_Target <- read_csv("DIH_Y2_Target")
df2<- DIH_Y2_Target

################################ Prelim look at the data ###############################
str(df2) #overview of data types
colnames(df2)
summary(df2) #summary statistics
dim(df2)
sapply(df2, function(x) sum(is.na(x))) # of missing variables

length(unique(df2$MemberID)) #71435 unique patients

################# Data Manipulation #####################
# fixing DSFS - days since first claim
df2$DSFS <- case_when(
  df2$DSFS == "0- 1 month" ~ 1,
  df2$DSFS == "1- 2 months" ~ 2,
  df2$DSFS == "2- 3 months" ~ 3,
  df2$DSFS == "3- 4 months" ~ 4,
  df2$DSFS == "4- 5 months" ~ 5,
  df2$DSFS == "5- 6 months" ~ 6,
  df2$DSFS == "6- 7 months" ~ 7,
  df2$DSFS == "7- 8 months" ~ 8,
  df2$DSFS == "8- 9 months" ~ 9,
  df2$DSFS == "9-10 months" ~ 10,
  df2$DSFS == "10-11 months" ~ 11,
  df2$DSFS == "11-12 months" ~ 12,
  TRUE~0
)
#sanity check
summary(df2$DSFS)

df2$Year <- NULL


#converting pseudonyms for EDA later on
df2$MemberID <- as.character(df2$MemberID)

#fixing payDelay
unique(df2$PayDelay)
df2$PayDelay[df2$PayDelay == "162+"] <- 162
df2$PayDelay<- as.integer(df2$PayDelay)
str(df2$PayDelay)

# fixing length of stay
df2$LengthOfStay<- case_when(
  df2$LengthOfStay == "1 day" ~ 1,
  df2$LengthOfStay == "2 days" ~ 2,
  df2$LengthOfStay == "3 days" ~ 3,
  df2$LengthOfStay == "4 days" ~ 4,
  df2$LengthOfStay == "5 days" ~ 5,
  df2$LengthOfStay == "6 days" ~ 6,
  df2$LengthOfStay == "1- 2 weeks" ~ 11,
  df2$LengthOfStay == "2- 4 weeks" ~ 21,
  df2$LengthOfStay == "4- 8 weeks" ~ 42,
  df2$LengthOfStay == "26+ weeks" ~ 182,
  TRUE~0
)
summary(df2$LengthOfStay)
df2$LengthOfStay <- as.integer(df2$LengthOfStay)
str(df1$LengthOfStay)

# fixing drug count
df2$DrugCount[df2$DrugCount == "7+"] <- 7
df2$DrugCount[is.na(df2$DrugCount)]<- 0
df2$DrugCount<- as.integer(df2$DrugCount)
str(df2$DrugCount)
summary(df2$DrugCount)

# fixing the lab count 
df2$LabCount[df2$LabCount == "10+"] <- 10
df2$LabCount[is.na(df2$LabCount)]<- 0
df2$LabCount<- as.integer(df2$LabCount)
str(df2$LabCount)
summary(df2$LabCount)

# fixing the age - changing NAs to 99
df2$AgeAtFirstClaim<- case_when(
  df2$AgeAtFirstClaim == "0-9" ~ 5,
  df2$AgeAtFirstClaim == "10-19" ~ 15,
  df2$AgeAtFirstClaim == "20-29" ~ 25,
  df2$AgeAtFirstClaim == "30-39" ~ 35,
  df2$AgeAtFirstClaim == "40-49" ~ 45,
  df2$AgeAtFirstClaim == "50-59" ~ 55,
  df2$AgeAtFirstClaim == "60-69" ~ 65,
  df2$AgeAtFirstClaim == "70-79" ~ 75,
  df2$AgeAtFirstClaim == "80+" ~ 80,
  TRUE~ 99
)
summary(df2$AgeAtFirstClaim)
df2$AgeAtFirstClaim<- as.integer(df2$AgeAtFirstClaim)
str(df2$AgeAtFirstClaim)

# fixing the sex variable
df2$Sex[is.na(df2$Sex)]<- "Missing_Sex"

#checking for any more null values
sapply(df2, function(x) sum(is.na(x)))

# fixing the missing values in categorical variables ( leaving ProviderID, Vendor and PCP as integer only)
df2$ProviderID[is.na(df2$ProviderID)]<- "Missing_ProviderID"
df2$Vendor[is.na(df2$Vendor)]<- "Missing_Vendor"
df2$PCP[is.na(df2$PCP)]<- "Missing_PCP"
df2$Specialty[is.na(df2$Specialty)]<- "Missing_Specialty"
df2$PlaceSvc[is.na(df2$PlaceSvc)]<- "Missing_PlaceSvc"
df2$PrimaryConditionGroup[is.na(df2$PrimaryConditionGroup)]<- "Missing_PCGroup"
df2$ProcedureGroup[is.na(df2$ProcedureGroup)]<- "Missing_ProcedureGroup"

str(df2)



##########################################################################################

df_python2 <- df2
str(df_python2)
df_python2$ProviderID<- as.numeric(df_python2$ProviderID)
df_python2$Vendor<- as.numeric(df_python2$Vendor)
df_python2$PCP<- as.numeric(df_python2$PCP)

sapply(df_python2, function(x) sum(is.na(x)))

# Write CSV in R
write.csv(df_python2, file = "Dummy2.csv")

#########################################################################################
# bring the file back from python with dummy variables
df2_Agg <- read_csv("out_Agg_dummy_Y2.csv")

