suppressWarnings(suppressPackageStartupMessages({
  library(readxl)
  library(ggplot2)
}))
# read in the data 
data = read_excel("Desktop/140_proj_4/project_4.xlsx")

# create a empty list for storing data
Data_Cleaning_Flag = list()

# Flagging Rule
#   0: normal data
#   1: obvious problem
#   2: not an obvious problem

# Our group will not change any value of the original data, only using Flag 
# to indicate if there is a problem with the observation


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
# Since there is no specific rule to exmain ClassID column, all the data
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
# since there is 
