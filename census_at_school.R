suppressWarnings(suppressPackageStartupMessages({
  library(readxl)
  library(ggplot2)
  library(dplyr)
  library(MASS)
}))
# read in the data 
data = read_excel("Desktop/140_proj_4/project_4.xlsx")

# create a empty list for storing data
Data_Cleaning_Flag = list()

# Flagging Rule
#   0: normal data
#   1: obvious problem
#   2: not an obvious problem

# Our group will either using Flag to indicate if there is a problem with the observation
# or provided a cleaned data column


## Cleaning of the Response ID
# from summary we know that there is no NA's of Response ID
summary(data$ResponseID)
# Our group think that the response ID should be unique to all response
# So any duplications will be flagged 1 (obvious problem)
duplication <- duplicated(data$ResponseID)
if(any(duplication)){
  dup = which(duplication)
}
ResponseID_flag = as.integer(data$ResponseID %in% data$ResponseID[dup])
Data_Cleaning_Flag[["ResponseID"]] = data$ResponseID
Data_Cleaning_Flag[["ResponseID_flag"]] = ResponseID_flag


## Cleaning of ClassID
# from summary we know that there in no NA's of the Class ID
summary(data$ClassID)
# Since there is no specific rule to examine ClassID column, all the data
# are considered to be valid
ClassID_flag = as.integer(rep_len(0,length.out = length(data$ClassID)))
Data_Cleaning_Flag[["ClassID"]] = data$ClassID
Data_Cleaning_Flag[["ClassID_flat"]] = ClassID_flag


## Cleaning of ClassSize
# from summary we know that there is no NA's of ClassSize
# However, there are bad values such as 0 and 2016
summary(data$ClassSize)
# We use the common knowledge, mark anything that is less than 1 or 
# larger than 200 to be an obvious problem (flag 1)
ClassSize_flag = rep_len(0L,length.out=length(data$ClassSize))
ClassSize_flag[data$ClassSize<1 | data$ClassSize>200] = 1L
# then we also find the 97.5 & 2.5 percentile of the data, we believe that 
# although it is possible, but very unlikely to have class size this 
# large/small, and thus give those observations flag 2 (not obvious problem)
quantile(data$ClassSize, c(0.025,0.975))
ClassSize_flag[(data$ClassSize>=1 & data$ClassID<6) | (data$ClassSize>140 & data$ClassSize>200)] = 2L
Data_Cleaning_Flag[["ClassSize"]] = data$ClassSize
Data_Cleaning_Flag[["ClassSize_flat"]] = ClassSize_flag


# Cleaning of ClassGrade
summary(as.integer(data$ClassGrade))
# from summary, there is no NA, and there is no values out of the 
# range of 4-12, so we consider everything to be valid 
# We also allowed the who column to be character strings
ClassGrade_flag = rep_len(0L, length.out=length(data$ClassGrade))
Data_Cleaning_Flag[["ClassGrade"]] = data$ClassGrade
Data_Cleaning_Flag[["ClassGrade_flag"]] = ClassGrade_flag


## Cleaning of DateYear
summary(data$DataYear)
# from summary, sincce there is no NA's and there is no values 
# indicating a future year (>2019) we consider this column to be valid
DataYear_flag = rep_len(0L, length.out=length(data$DataYear))
Data_Cleaning_Flag[["DataYear"]] = data$DataYear
Data_Cleaning_Flag[["DataYear_flag"]] = DataYear_flag


## Cleaning of Country
table(data$Country,useNA="always")
# We count the unique countries appeared in the data, 
# and we expect to see only USA, since this is the case
# we consider this column to be valid
Country_flag = rep_len(0L, length.out=length(data$Country))
Data_Cleaning_Flag[["Country"]] = data$Country
Data_Cleaning_Flag[["Country_flag"]] = Country_flag


## Cleaning of Region
US_states = c(state.abb,"AS","DC","FM","GU","MH","MP","PW","PR","VI")
obs_states = names(table(data$Region))
all(obs_states %in% US_states)
# we checked all the abbreviations of the 50 states and other territories (9 of them)
# since all of them match, we consider this column to be valid
Region_flag = rep_len(0L, length.out=length(data$Region))
Data_Cleaning_Flag[["Region"]] = data$Region
Data_Cleaning_Flag[["Region_flag"]] = Region_flag


## Cleaning of Gender
table(data$Gender, useNA="always")
# since there is only two choices (male/female), we would be taking
# anything outside this as obvious problem (flag = 1)
# For NA's, we will flag them 0
Gender_flag = rep_len(0L, length.out=length(data$Gender))
Gender_flag[(!is.na(data$Gender)) & data$Gender!="Male" & data$Gender!="Female"] = 1L
Data_Cleaning_Flag[["Gender"]] = data$Gender
Data_Cleaning_Flag[["Gender_flag"]] = Gender_flag


## Cleaning of Age_years
# first we will make a copy of the vector and turn it into number
# anything that cannot be turn into number correctly will be an obvious problem
# in this case, it turns out that there is no such case
suppressWarnings({
  Age = as.numeric(data$Age_years)
})
# find those become NA due to coercion
coercion_flag = ifelse((is.na(Age) & (!is.na(data$Age_years))),1,0)
sum(coercion_flag)
# get the summary of the Age
summary(Age)
# We can see that there are negative ages and also ages greater than 1000,
# which are obviously wrong. Thus, based on common sense, we will rule out
# all age below 5 (as one can only go to school after 6) and above 28 to be
# obviously wrong (10 years older than usual age of the 12th grade)
# We also take any ages that is above 5 years larger than the median age of a
# perticular grade to be a not-so-obvious problem, as it is unlikely for people to be able to 
# get to a grad 5 years younger or older than others
Age_grade_median = (data %>% dplyr::select(Age_years, ClassGrade) %>%
                      group_by(ClassGrade) %>%
                      mutate(Median_Age = median(as.numeric(Age_years),na.rm=TRUE)))$Median_Age
Age_flag = ifelse((Age >= 28 | Age <= 5) & !is.na(Age), 1L, 
                  ifelse(abs(Age-Age_grade_median) > 5 & !is.na(Age-Age_grade_median),2L,0L))
Data_Cleaning_Flag[["Age_years"]] = data$Age_years
Data_Cleaning_Flag[["Age_flag"]] = Age_flag


## Handed
table(data$Handed, useNA="always")
# From the result summary, it is possible to see that the except for one "Question",
# other entries are valid, so flag is set that all (including NA) are 0 while "Question"
# is 1
Handed_flag = ifelse(data$Handed == "Question" & !is.na(data$Handed), 1 ,0)
Data_Cleaning_Flag[["Handed"]] = data$Handed
Data_Cleaning_Flag[["Handed_flag"]] = Handed_flag


## Height & Armspan
# Our group think that what was done by the group whose work is on CCLE is pretty much complete
# Thus, we use the same logic as they did. We removed some unnecessary steps.
# for Height
height = data$Height_cm
suppressWarnings({
  height = as.numeric(height)
})
height[height<0.95] = NA
height[which(height<=2.35)] = 100*(height[which(height<2.35)])   #m-cm
height[which(height>2.35&height<3.1)] = NA
height[which(height>=3.1&height<=7.7)] = 30.48*(height[which(height>=3.1&height<=7.7)])  #foot-cm
height[which(height>7.7&height<37.4)] = NA
height[which(height>=37.4&height<=92.5)] = 2.54*(height[which(height>=37.4&height<=92.5)])  #inch-cm
height[which(height>92.5&height<95)] = NA
height[which(height>235&height<950)] = NA
height[which(height>=950&height<=2350)] = 0.1*(height[which(height<=2350&height>=950)])   #mm-cm
height[which(height>2350)] = NA

# for Armspan
suppressWarnings({
  armspan = as.numeric(data$Armspan_cm)
})
armspan[armspan<0.95] = NA
armspan[which(armspan<=2.35)] = 100*(armspan[which(armspan<2.35)])   #m-cm
armspan[which(armspan>2.35&armspan<3.1)] = NA
armspan[which(armspan>=3.1&armspan<=7.7)] = 30.48*(armspan[which(armspan>=3.1&armspan<=7.7)])  #foot-cm
armspan[which(armspan>7.7&armspan<37.4)] = NA
armspan[which(armspan>=37.4&armspan<=92.5)] = 2.54*(armspan[which(armspan>=37.4&armspan<=92.5)])  #inch-cm
armspan[which(armspan>92.5&armspan<95)] = NA
armspan[which(armspan>235&armspan<950)] = NA
armspan[which(armspan>=950&armspan<=2350)] = 0.1*(armspan[which(armspan<=2350&armspan>=950)])   #mm-cm
armspan[which(armspan>2350)] = NA

# We decided that we will not use armspan to fill height and use height to fill armspan
# since based on the following plot, they do not have to be equal
ggplot(data=NULL,aes(x=height,y=armspan)) + geom_point(alpha=0.1) + labs(title = "Height vs. Armspan Plot")
Data_Cleaning_Flag[["Height_cm_clean"]] = height
Data_Cleaning_Flag[["Armspan_cm_clean"]] = armspan


## for Footlength_cm
# Footlength_cm is actually character type in the excel so we first transform into numeric
# NA would introduced by coercion and we would deal with it later.
num_Footlength_cm = as.numeric(data$Footlength_cm)
summary(num_Footlength_cm)
num_Footlength_cm_flag = rep_len(0L, length.out=length(num_Footlength_cm))
Right_Footlength_cm = rep(NA, length(num_Footlength_cm))
# apply the tranformation of units indicated in the pdf
Right_Footlength_cm[which(num_Footlength_cm>=0.09 & num_Footlength_cm<=0.4)] = 100 * num_Footlength_cm[which(num_Footlength_cm>=0.09 & num_Footlength_cm<=0.4)]
Right_Footlength_cm[which(num_Footlength_cm>=0.4 & num_Footlength_cm<=1.31)] = 30.48 * num_Footlength_cm[which(num_Footlength_cm>=0.4 & num_Footlength_cm<=1.31)]
Right_Footlength_cm[which(num_Footlength_cm>=3.5 & num_Footlength_cm<=9)] = 2.54 * num_Footlength_cm[which(num_Footlength_cm>=3.5 & num_Footlength_cm<=9)]
Right_Footlength_cm[which(num_Footlength_cm>=90 & num_Footlength_cm<=400)] = 0.1 * num_Footlength_cm[which(num_Footlength_cm>=90 & num_Footlength_cm<=400)]
Right_Footlength_cm[which(num_Footlength_cm>9 & num_Footlength_cm<=40)] = num_Footlength_cm[which(num_Footlength_cm>=9 & num_Footlength_cm<=40)]
Data_Cleaning_Flag[["Right_Footlength_cm"]] = Right_Footlength_cm


## for Languages_spoken
typeof(data$Languages_spoken)
num_Languages_spoken = as.numeric(data$Languages_spoken)
summary(num_Languages_spoken)
# flag...
Languages_spoken_flag = rep_len(0L, length.out=length(num_Languages_spoken))
# Data larger than 30 are least possible and we flag them as 2.
Languages_spoken_flag[num_Languages_spoken>30] = 2L
# Flag the data in the range (12, 30] as 1.
Languages_spoken_flag[(num_Languages_spoken>12) & (num_Languages_spoken<=30)] = 1L

# clean our data
Languages_spoken = rep(NA, length(num_Languages_spoken))
# only choose from 1 to 30 maximum, other should be bad variables and set them into NA
Languages_spoken[which((num_Languages_spoken>=1) & (num_Languages_spoken<=30))] = num_Languages_spoken[which((num_Languages_spoken>=1) & (num_Languages_spoken<=30))]

Data_Cleaning_Flag[["Languages_spoken"]] = Languages_spoken
Data_Cleaning_Flag[["Languages_spoken_flag"]] = Languages_spoken_flag
