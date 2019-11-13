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



## for Travel to school
summary.factor(data$Travel_to_school)
# set everything out of range to have flag 1
Travel_to_school_flag = rep_len(0L, length.out=length(data$Travel_to_school))
Travel_to_school_flag[which(data$Travel_to_school!='Car' & data$Travel_to_school!='Bicycle' & data$Travel_to_school!='Boat' & data$Travel_to_school!='Bus' & data$Travel_to_school!='Rail (Train/Tram/Subway)' & data$Travel_to_school!='Skateboard/Scooter/Rollerblade' & data$Travel_to_school!='Walk' & data$Travel_to_school!='Other')] = 1L
Travel_to_school_flag[is.na(data$Travel_to_school)] = 1L
#set everything out of range to NA
Travel_to_school = data$Travel_to_school
Travel_to_school[which(data$Travel_to_school!='Car' & data$Travel_to_school!='Bicycle' & data$Travel_to_school!='Boat' & data$Travel_to_school!='Bus' & data$Travel_to_school!='Rail (Train/Tram/Subway)' & data$Travel_to_school!='Skateboard/Scooter/Rollerblade' & data$Travel_to_school!='Walk' & data$Travel_to_school!='Other')] = NA

Data_Cleaning_Flag[["Travel_to_school"]] = Travel_to_school
Data_Cleaning_Flag[["Travel_to_school_flag"]] = Travel_to_school_flag



## for travel time to school
Travel_time_to_school = as.numeric(data$Travel_time_to_school)
summary(Travel_time_to_school)
# clean data
# To make the data in the range (0,1] more reasonable, we replace them with 60 times of themselves
Travel_time_to_school[which(Travel_time_to_school>0 & Travel_time_to_school<=1)] = 60*Travel_time_to_school[which(Travel_time_to_school>0 & Travel_time_to_school<=1)]
# Flag
Travel_time_to_school_flag = rep_len(0L, length.out=length(Travel_time_to_school))
# range [0,60] flag the data in this range and NAs as 0
Travel_time_to_school_flag[which(Travel_time_to_school>=0 & Travel_time_to_school<=60)] = 0L
# Data in the range (60, 120] are extreme outliers and we flag them as 1
Travel_time_to_school_flag[which(Travel_time_to_school>60 & Travel_time_to_school<=120)] = 1L
# number larger than 120, flag them as 2.
Travel_time_to_school_flag[which(Travel_time_to_school>120)] = 2L
# maybe we should change numbers more than 120 to NA
Travel_time_to_school[which(Travel_time_to_school>120)] = NA

Data_Cleaning_Flag[["Travel_time_to_school"]] = Travel_time_to_school
Data_Cleaning_Flag[["Travel_time_to_school_flag"]] = Travel_time_to_school_flag



## for Reaction_time
# 0.101 seconds as the smallest possible value
Reaction_time = as.numeric(data$Reaction_time)
summary(Reaction_time)
# flag
Reaction_time_flag = rep_len(0L, length.out=length(Reaction_time))
# reasonable range is [0.101, 1.21], e flag the data in this range and NAs as 0
Reaction_time_flag[which(Reaction_time>=0.101 & Reaction_time<=1.21)] = 0L
# a smaller than 0.101 or larger than 1.21 are very extreme outliers and we flag them as 2
Reaction_time_flag[which(Reaction_time<0.101)] = 2L
Reaction_time_flag[which(Reaction_time> 1.21)] = 2L
# clean, set less than 0.101 or larger than 1.21 to NA
Reaction_time[which(Reaction_time<0.101)] = NA
Reaction_time[which(Reaction_time> 1.21)] = NA

Data_Cleaning_Flag[["Reaction_time"]] = Reaction_time
Data_Cleaning_Flag[["Reaction_time_flag"]] = Reaction_time_flag



## for Score in memory game
Score_in_memory_game = as.numeric(data$Score_in_memory_game)
summary(Score_in_memory_game)
# Flag
Score_in_memory_game_flag = rep_len(0L, length.out=length(Score_in_memory_game))
# Range is [2.02, 96], flag the data in this range and NAs as 0
# Smaller than 2.02 or larger than 96 are very extreme outliers and we flag them as 2
Score_in_memory_game_flag[which(Score_in_memory_game<2.02 | Score_in_memory_game>96)] = 2L
# clean
# 2.02 seconds as the smallest possible value
Score_in_memory_game[which(Score_in_memory_game<2.02 | Score_in_memory_game>96)] = NA

Data_Cleaning_Flag[["Score_in_memory_game"]] = Score_in_memory_game
Data_Cleaning_Flag[["Score_in_memory_game_flag"]] = Score_in_memory_game_flag



## Favourite_physical_activity
summary.factor(data$Favourite_physical_activity)
# from this we can see that the good values should be among, "Athletics, Baseball/Softball, Basketball, Bowling, 
# Cycling, Dancing, Football (American), Golf, Gymnastics, Hockey (Field), Hockey (Ice), Lacrosse, Martial Arts, Other
# Rowing, Running/Jogging, Skateboarding/Rollerblading, Soccer, Swimming, Table Tennis, Tennis, Walking/Hiking"
# So we would flag all the nones and NAs into flag 1 and clean them as NAs.
Favourite_physical_activity = data$Favourite_physical_activity
#flag
Favourite_physical_activity_flag = rep_len(0L, length.out=length(Favourite_physical_activity))
# if None or NA, flag as 1
Favourite_physical_activity_flag[which(Favourite_physical_activity=='None' | is.na(Favourite_physical_activity))] = 1L
#clean data
Favourite_physical_activity[which(Favourite_physical_activity=='None' | is.na(Favourite_physical_activity))] = NA

Data_Cleaning_Flag[["Favourite_physical_activity"]] = Favourite_physical_activity
Data_Cleaning_Flag[["Favourite_physical_activity_flag"]] = Favourite_physical_activity_flag



## Importance_reducing_pollution
Importance_reducing_pollution = as.numeric(data$Importance_reducing_pollution)
summary(Importance_reducing_pollution)
#flag
Importance_reducing_pollution_flag = rep_len(0L, length.out=length(Importance_reducing_pollution))
#set all value out of range (0-1000) to have flag 1
Importance_reducing_pollution_flag[which(Importance_reducing_pollution<0 | Importance_reducing_pollution>1000 | is.na(Importance_reducing_pollution))] = 1L
# clean data
# we would consider range (0-1000) to be good values
Importance_reducing_pollution[which(Importance_reducing_pollution<0 | Importance_reducing_pollution>1000 | is.na(Importance_reducing_pollution))] = NA

Data_Cleaning_Flag[["Importance_reducing_pollution"]] = Importance_reducing_pollution
Data_Cleaning_Flag[["Importance_reducing_pollution_flag"]] = Importance_reducing_pollution_flag


## Importance_recycling_rubbish
Importance_recycling_rubbish = as.numeric(data$Importance_recycling_rubbish)
summary(Importance_recycling_rubbish)
#flag
Importance_recycling_rubbish_flag = rep_len(0L, length.out=length(Importance_recycling_rubbish))
#set all value out of range (0-1000) to have flag 1
Importance_recycling_rubbish_flag[which(Importance_recycling_rubbish<0 | Importance_recycling_rubbish>1000 | is.na(Importance_recycling_rubbish))] = 1L
# clean data
# we would consider range (0-1000) to be good values
Importance_recycling_rubbish[which(Importance_recycling_rubbish<0 | Importance_recycling_rubbish>1000 | is.na(Importance_recycling_rubbish))] = NA

Data_Cleaning_Flag[["Importance_recycling_rubbish"]] = Importance_recycling_rubbish
Data_Cleaning_Flag[["Importance_recycling_rubbish_flag"]] = Importance_recycling_rubbish_flag


## Importance_conserving_water
Importance_conserving_water = as.numeric(data$Importance_conserving_water)
summary(Importance_conserving_water)
#flag
Importance_conserving_water_flag = rep_len(0L, length.out=length(Importance_conserving_water))
#set all value out of range (0-1000) to have flag 1
Importance_conserving_water_flag[which(Importance_conserving_water<0 | Importance_conserving_water>1000 | is.na(Importance_conserving_water))] = 1L
# clean data
# we would consider range (0-1000) to be good values
Importance_conserving_water[which(Importance_conserving_water<0 | Importance_conserving_water>1000 | is.na(Importance_conserving_water))] = NA

Data_Cleaning_Flag[["Importance_conserving_water"]] = Importance_conserving_water
Data_Cleaning_Flag[["Importance_conserving_water_flag"]] = Importance_conserving_water_flag



## Importance_saving_energy
Importance_saving_energy = as.numeric(data$Importance_saving_energy)
summary(Importance_saving_energy)
#flag
Importance_saving_energy_flag = rep_len(0L, length.out=length(Importance_saving_energy))
#set all value out of range (0-1000) to have flag 1
Importance_saving_energy_flag[which(Importance_saving_energy<0 | Importance_saving_energy>1000 | is.na(Importance_saving_energy))] = 1L
# clean data
# we would consider range (0-1000) to be good values
Importance_saving_energy[which(Importance_saving_energy<0 | Importance_saving_energy>1000 | is.na(Importance_saving_energy))] = NA

Data_Cleaning_Flag[["Importance_saving_energy"]] = Importance_saving_energy
Data_Cleaning_Flag[["Importance_saving_energy_flag"]] = Importance_saving_energy_flag



## Importance_owning_computer
Importance_owning_computer = as.numeric(data$Importance_owning_computer)
summary(Importance_owning_computer)
#flag
Importance_owning_computer_flag = rep_len(0L, length.out=length(Importance_owning_computer))
#set all value out of range (0-1000) to have flag 1
Importance_owning_computer_flag[which(Importance_owning_computer<0 | Importance_owning_computer>1000 | is.na(Importance_owning_computer))] = 1L
# clean data
# we would consider range (0-1000) to be good values
Importance_owning_computer[which(Importance_owning_computer<0 | Importance_owning_computer>1000 | is.na(Importance_owning_computer))] = NA

Data_Cleaning_Flag[["Importance_owning_computer"]] = Importance_owning_computer
Data_Cleaning_Flag[["Importance_owning_computer_flag"]] = Importance_owning_computer_flag


## Importance_internet_access
Importance_internet_access = as.numeric(data$Importance_internet_access)
summary(Importance_internet_access)
#flag
Importance_internet_access_flag = rep_len(0L, length.out=length(Importance_internet_access))
#set all value out of range (0-1000) to have flag 1
Importance_internet_access_flag[which(Importance_internet_access<0 | Importance_internet_access>1000 | is.na(Importance_internet_access))] = 1L
# clean data
# we would consider range (0-1000) to be good values
Importance_internet_access[which(Importance_internet_access<0 | Importance_internet_access>1000 | is.na(Importance_internet_access))] = NA

Data_Cleaning_Flag[["Importance_internet_access"]] = Importance_internet_access
Data_Cleaning_Flag[["Importance_internet_access_flag"]] = Importance_internet_access_flag
