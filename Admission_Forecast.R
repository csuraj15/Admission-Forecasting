#Loading necessary libraries
library(forecast) #Modeling
library(dplyr) #Manipulating data
library(ggplot2) #Plotting

#Setting working directory
setwd("~/Documents/UNH/Semester3/Business Forecasting/Assignment Files")

###Part 1: Data Preparation
admission.data<- read.csv("ECON_6635 Assignment 3.csv")

#Exploring the data
head(admission.data) #To understand the columns we are working with
str(admission.data) #To see data types and sizing
colSums(is.na(admission.data)) #To check for missing values

#Using boxplots for visualisation
ggplot(admission.data, aes(x = gre, y = as.factor(admit))) + 
  geom_boxplot(fill = "lightblue") + 
  labs(title = "Boxplot of Admission Status across GRE scores", 
       x = "GRE", y = "Admission Status")
ggplot(admission.data, aes(x = gpa, y = as.factor(admit))) + 
  geom_boxplot(fill = "lightgreen") + 
  labs(title = "Boxplot of Admission Status across GPA Levels", 
       x = "GPA", y = "Admission Status")

###Part 2: Dataset Splitting
set.seed(123)

#Generating an ID column to help in splitting
admission.df = admission.data %>% mutate( ID = row_number())

#Assigning 70% of dataset for training and 30% towards model validation
train = admission.df %>% sample_frac(0.7)
test = admission.df %>% anti_join(train, by = "ID")

###Part 3: Model Development
#Logistic regression model fitting with training data
admission_glm <- glm(admit ~ gre+gpa+rank, family = "binomial", data = train)

#Model performance with training data
summary(admission_glm)

#To print statistics along with confusion matrix in Console
mod_fit = ifelse(admission_glm$fitted.values >= 0.5, 1,0)
caret::confusionMatrix(as.factor(mod_fit), as.factor(train$admit))

#For Visualizing the Confusion matrix
cm_data = data.frame(obs = as.factor(train$admit), pred = as.factor(mod_fit))
cm <-  yardstick::conf_mat(cm_data, obs, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "yellow", high = "cyan")

###Part 4: Model Evaluation

#Predicting and evaluating the model with our test data
test$mod_fit <- ifelse(predict(admission_glm, newdata = test, 
                               type = "response") >= 0.5, 1, 0)

# Model performance on the test set
accuracy_test <- mean(test$mod_fit == test$admit)

#To print statistics along with confusion matrix in Console
caret::confusionMatrix(as.factor(test$mod_fit), as.factor(test$admit))

#For Visualizing the Confusion matrix
cm_data_test <- data.frame(obs = as.factor(test$admit), 
                           pred = as.factor(test$mod_fit))
cm_test <- yardstick::conf_mat(cm_data_test, obs, pred)
autoplot(cm_test, type = "heatmap") +
  scale_fill_gradient(low = "yellow", high = "cyan")

###Part 5: Apply the Model to New Data (20 points)
new_data <- read.csv2("new_data.csv", sep = ",")
new_data$gpa <- as.numeric(new_data$gpa) #Explicit mention to prevent errors
str(new_data)

#Predictions using our constructed Model
new_data$predicted_admit <- ifelse(predict(admission_glm, newdata = new_data, 
                                           type = "response") >= 0.5, 1, 0)

# New data set with Predictions
head(new_data)

