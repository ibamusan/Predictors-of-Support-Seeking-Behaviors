#Calling of packages and Set Working Directory####
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(skimr)
library(broom)
library(margins)
library(stargazer)  
library(rpart)    #for decision trees
library(rattle)   #to visualize decision trees
library(caret)    #for random forests with cross validation

#Setting working directory.
setwd("C:/Users/USER/OneDrive - Bournemouth University/BUSINESS INTELLIGENCE/Individual Assignment")
getwd()

#Data Cleaning ####
bIDataset <- read_dta("my_dataset.dta")

summary(bIDataset)

selected_Data <- bIDataset %>% 
  select(Y11_HH2a, Y11_HHsize, Y11_HHstructure, Y11_Q17, Y11_Q21a, Y11_Q21c,Y11_Q21d, Y11_Q22d, Y11_Q29i, Y11_Q29e, Y11_Q29c, Y11_Q31, Y11_Q32, Y11_Q33a, Y11_Q35a, Y11_Q39a, Y11_Q40e, Y11_Q40f, Y11_Q54b_1, Y11_Q58, Y11_Q59f, Y11_Q60a, Y11_Q60b)

selected_Data %>% 
  map(~sum(is.na(.)))

table(selected_Data$Y11_Q35a)

skim(selected_Data$Y11_Q35a)

Main_data <- selected_Data %>% 
  drop_na()

skim(Main_data)

#Data Preparation and Transformation ####
Main_data <- Main_data %>% 
  mutate(cid = row_number(), Y11_Q35a =  case_when(
    Y11_Q35a %in% 1:2 ~ "Internal", 
      Y11_Q35a == 3 ~ "External",
    TRUE ~ "Nobody"
    ),
    Y11_Q33a = case_when(
      Y11_Q33a %in% 1:2 ~ "Frequent",
      TRUE ~ "Infrequent"
    ),
    Y11_Q29i = case_when(
      Y11_Q29i %in% 1:2 ~ "Close",
      Y11_Q29i == 3 ~ "Neutral",
      Y11_Q29i %in% 4:5 ~ "Not Close"
    ),
    Y11_Q29c = case_when(
      Y11_Q29c %in% 1:2 ~ "High Freedom",
      Y11_Q29c == 3 ~ "Neutral",
      Y11_Q29c %in% 4:5 ~ "Low Freedom"
    ),
    Y11_Q39a = case_when(
      Y11_Q39a == 1 ~ "Spend less time",
      Y11_Q39a == 2 ~ "As much time as currently",
      Y11_Q39a == 3 ~ "Spend more time"
    ),
    Y11_Q21d = case_when(
      Y11_Q21d %in% 1:2 ~ "Frequent",
      TRUE ~ "Infrequent"
    ),
    Y11_Q21a = case_when(
      Y11_Q21a %in% 1:2 ~ "Frequent",
      TRUE ~ "Infrequent"
    ),
    Y11_Q22d = case_when(
      Y11_Q22d %in% 1:2 ~ "Regular",
      Y11_Q22d == 3 ~ "Irregular",
      Y11_Q22d == 4 ~ "Not at all"
    ),
    Y11_Q29e = case_when(
      Y11_Q29e %in% 1:2 ~ "Left out",
      Y11_Q29e == 3 ~ "Neutral",
      Y11_Q29e %in% 4:5 ~ "Included"
    ),
    Y11_Q31 = case_when(
      Y11_Q31 == 1 ~ "Married",
      Y11_Q31 %in% 2:3 ~ "Not Living with partner",
      Y11_Q31 == 4 ~ "Never Married"
    ),
    Y11_Q32 = case_when(
      Y11_Q32 == 0 ~ "No Children",
      Y11_Q32 == 1 ~ "1 Child",
      Y11_Q32 == 2 ~ "2 Children",
      Y11_Q32 == 3 ~ "3 Children",
      Y11_Q32 == 4 ~ "4 Children",
      Y11_Q32 == 5 ~ "5 or More Children"
    ),
    Y11_Q40e = case_when(
      Y11_Q40e %in% 1:4 ~ "Low Satisfaction",
      Y11_Q40e %in% 5:7 ~ "Medium Satisfaction",
      Y11_Q40e %in% 8:10 ~ "High Satisfaction",
    ),
    Y11_Q21c = case_when(
      Y11_Q21c %in% 1:2 ~ "High Frequency",
      Y11_Q21c == 3 ~ "Moderate Frequency",
      Y11_Q21c %in% 4:5 ~ "Low Frequency"
    ),
    Y11_Q40f = case_when(
      Y11_Q40f %in% 1:4 ~ "Low Satisfaction",
      Y11_Q40f %in% 5:7 ~ "Medium Satisfaction",
      Y11_Q40f %in% 8:10 ~ "High Satisfaction",
    ),
    Y11_Q54b_1 = case_when(
      Y11_Q54b_1 == 1 ~ "NO",
      Y11_Q54b_1 == 2 ~ "YES"
    ),
    Y11_Q58 = case_when(
      Y11_Q58 %in% 1:3 ~ "Able to Make Ends Meet",
      Y11_Q58 == 4 ~ "With Some Difficulty",
      Y11_Q58 %in% 5:6 ~ "With Difficulty"
    ),
    Y11_Q59f = case_when(
      Y11_Q59f == 1 ~ "Yes, Can Afford if Want",
      Y11_Q59f == 2 ~ "No, Cannot Afford it"
    ),
    Y11_Q60a = case_when(
      Y11_Q60a == 1 ~ "Yes",
      Y11_Q60a == 2 ~ "No"
    ),
    Y11_Q60b = case_when(
      Y11_Q60b == 1 ~ "Yes",
      Y11_Q60b == 2 ~ "No"
    ),
    Y11_HH2a = case_when(
      Y11_HH2a == 1 ~ "Male",
      Y11_HH2a == 2 ~ "Female"
    ),
    Y11_HHstructure = case_when(
      Y11_HHstructure == 1 ~ "Single",
      Y11_HHstructure == 2 ~ "Couple",
      Y11_HHstructure == 3 ~ "Single Parent",
      Y11_HHstructure == 4 ~ "Couple with Children",
      Y11_HHstructure == 5 ~ "Other"
    ),
    Y11_HHsize = case_when(
      Y11_HHsize == 1 ~ "1 Person Household",
      Y11_HHsize == 2 ~ "2 Person Household",
      Y11_HHsize %in% 3:4 ~ "More Person Household"
    ),
    Y11_Q17 = case_when(
      Y11_Q17 == 1 ~ "1 Room",
      Y11_Q17 == 2 ~ "2 Rooms",
      Y11_Q17 == 3 ~ "3 Rooms",
      Y11_Q17 == 4 ~ "4 Rooms",
      Y11_Q17 == 5 ~ "5 Rooms",
      Y11_Q17 == 6 ~ "6 Rooms",
      Y11_Q17 == 7 ~ "7 Rooms",
      Y11_Q17 == 8 ~ "8 or More Rooms"
    ),
  ) %>% rename(
    Child_contact_frequency = Y11_Q33a,
    Community_affinity = Y11_Q29i,
    Personal_freedom = Y11_Q29c,
    Family_time_satisfaction = Y11_Q39a,
    Social_activity_participation_frequency = Y11_Q21d,
    Religion_participation_frequency = Y11_Q21a,
    Unpaid_voluntary_work_frequency = Y11_Q22d,
    Social_exclusion_feeling = Y11_Q29e,
    Marital_status = Y11_Q31,
    Number_of_children_own = Y11_Q32,
    Family_life_satisfaction = Y11_Q40e,
    Exercise_frequency = Y11_Q21c,
    Health_Satisfaction = Y11_Q40f,
    ltc_service_usage = Y11_Q54b_1,
    Ends_meet_ability = Y11_Q58,
    Social_meal_affordability = Y11_Q59f,
    Accommodation_payment = Y11_Q60a,
    Utility_bills_payment = Y11_Q60b,
    Gender = Y11_HH2a,
    Household_structure = Y11_HHstructure,
    Household_size = Y11_HHsize,
    Accommodation_rooms = Y11_Q17,
    Support_source = Y11_Q35a
  )

# Perform label encoding on all the variables ####
Main_data$Support_source <- ifelse(Main_data$Support_source == "Internal", 1, 0)

Main_data$Child_contact_frequency <- as.integer(as.factor(Main_data$Child_contact_frequency))

Main_data$Community_affinity <- as.integer(as.factor(Main_data$Community_affinity))

Main_data$Personal_freedom <- as.integer(as.factor(Main_data$Personal_freedom))

Main_data$Family_time_satisfaction <- as.integer(as.factor(Main_data$Family_time_satisfaction))

Main_data$Social_activity_participation_frequency <- as.integer(as.factor(Main_data$Social_activity_participation_frequency))

Main_data$Religion_participation_frequency <- as.integer(as.factor(Main_data$Religion_participation_frequency))

Main_data$Unpaid_voluntary_work_frequency <- as.integer(as.factor(Main_data$Unpaid_voluntary_work_frequency))

Main_data$Social_exclusion_feeling <- as.integer(as.factor(Main_data$Social_exclusion_feeling))

Main_data$Marital_status <- as.integer(as.factor(Main_data$Marital_status))

Main_data$Number_of_children_own <- as.integer(as.factor(Main_data$Number_of_children_own))

Main_data$Family_life_satisfaction <- as.integer(as.factor(Main_data$Family_life_satisfaction))

Main_data$Exercise_frequency <- as.integer(as.factor(Main_data$Exercise_frequency))

Main_data$Health_Satisfaction <- as.integer(as.factor(Main_data$Health_Satisfaction))

Main_data$ltc_service_usage <- as.integer(as.factor(Main_data$ltc_service_usage))

Main_data$Ends_meet_ability <- as.integer(as.factor(Main_data$Ends_meet_ability))

Main_data$Social_meal_affordability <- as.integer(as.factor(Main_data$Social_meal_affordability))

Main_data$Accommodation_payment <- as.integer(as.factor(Main_data$Accommodation_payment))

Main_data$Utility_bills_payment <- as.integer(as.factor(Main_data$Utility_bills_payment))

Main_data$Gender <- as.integer(as.factor(Main_data$Gender))

Main_data$Household_structure <- as.integer(as.factor(Main_data$Household_structure))

Main_data$Household_size <- as.integer(as.factor(Main_data$Household_size))

Main_data$Accommodation_rooms <- as.integer(as.factor(Main_data$Accommodation_rooms))

Main_data$Accommodation_rooms <- as.integer(as.factor(Main_data$Accommodation_rooms))

# Logistic Regression Analysis ####

# data a logit split

set.seed(123)
train <- Main_data %>% sample_frac(0.8)
test  <- anti_join(Main_data, train, by = "cid")

myformula <- formula(Support_source ~ Child_contact_frequency + Community_affinity + Personal_freedom + Family_time_satisfaction + Social_activity_participation_frequency + Religion_participation_frequency + Unpaid_voluntary_work_frequency + Social_exclusion_feeling + Marital_status + Number_of_children_own + Family_life_satisfaction + Exercise_frequency + Health_Satisfaction + ltc_service_usage + Ends_meet_ability + Social_meal_affordability + Accommodation_payment + Utility_bills_payment + Gender + Household_structure + Household_size + Accommodation_rooms)

# Fit a logit model

logit <- glm(data = train, myformula, family = binomial(link = "logit"))
summary(logit)

# Predict on the test dataset

logit_pred <- predict(logit, newdata = test, type = "response")
summary(logit_pred)

# Convert predicted probabilities to binary predictions

logit_pred_binary <- ifelse(logit_pred > 0.5, 1, 0)

# Compare predicted values with actual values

accuracy <- sum(logit_pred_binary == test$Support_source) / length(test$Support_source) * 100
print(accuracy)

# Display accuracy
#accuracy = 94.78%


# To check for most important variables

glm(data = Main_data, formula = Support_source ~ Child_contact_frequency, family = binomial(link = "logit"))

glm(data = Main_data, formula = Support_source ~ Community_affinity, family = binomial(link = "logit"))

glm(data = Main_data, formula = Support_source ~ Household_size, family = binomial(link = "logit"))


#Another prediction using caret

# Convert outcome variable to factor with two levels and valid names
Main_data$Support_source <- factor(Main_data$Support_source, levels = c(0, 1))
levels(Main_data$Support_source) <- make.names(levels(Main_data$Support_source))

# Fit logistic regression model using caret
library(caret)

logit.caret <- train(myformula,
                     data = Main_data,
                     metric = "Accuracy",
                     method = "glm",
                     family = "binomial",
                     trControl = trainControl(method = "cv", number = 10, classProbs = TRUE))

# Print the results
print(logit.caret)

pred_logit.caret <- predict(logit.caret, Main_data)



Main_data <- Main_data %>% mutate(Y11_Q35a =  case_when(
  Y11_Q35a == 1 ~ "family", 
  Y11_Q35a == 2 ~ "friend",
  Y11_Q35a == 3 ~ "Service Provider",
  TRUE ~ "Nobody"
))
