library(C50)

# Read the dataset
data <- read.csv("/home/ds-da-09/saniya/decision.csv")

# Convert categorical columns to factors
data$Age <- factor(data$Age, levels = c("Young", "Middle", "Old"))
data$Gender <- factor(data$Gender, levels = c("Male", "Female"))
data$Income <- factor(data$Income, levels = c("Low", "Medium", "High"))
data$MaritalStatus <- factor(data$MaritalStatus, levels = c("Single", "Married", "Divorced"))
data$Children <- factor(data$Children, levels = c("No", "Yes"))
data$ViewedProduct <- factor(data$ViewedProduct, levels = c("No", "Yes"))
data$Purchase <- factor(data$Purchase, levels = c("No", "Yes"))

# Train the C5.0 decision tree model
model <- C5.0(Purchase ~ Age + Gender + Income + MaritalStatus + Children + TimeSpentOnSite + ViewedProduct, data = data)

# Visualize the decision tree
plot(model, main = "Decision Tree Visualization", type = "simple")

# Function to get user input
get_user_input <- function() {
  cat("Enter the following options:\n")
  
  cat("1. Age (1: Young, 2: Middle, 3: Old): ")
  age <- as.integer(readline())
  age <- c("Young", "Middle", "Old")[age]
  
  cat("2. Gender (1: Male, 2: Female): ")
  gender <- as.integer(readline())
  gender <- c("Male", "Female")[gender]
  
  cat("3. Income (1: Low, 2: Medium, 3: High): ")
  income <- as.integer(readline())
  income <- c("Low", "Medium", "High")[income]
  
  cat("4. Marital Status (1: Single, 2: Married, 3: Divorced): ")
  marital_status <- as.integer(readline())
  marital_status <- c("Single", "Married", "Divorced")[marital_status]
  
  cat("5. Children (1: No, 2: Yes): ")
  children <- as.integer(readline())
  children <- c("No", "Yes")[children]
  
  cat("6. Time spent on site (numeric value in minutes): ")
  time_spent <- as.numeric(readline())
  
  cat("7. Viewed Product (1: No, 2: Yes): ")
  viewed_product <- as.integer(readline())
  viewed_product <- c("No", "Yes")[viewed_product]
  
  return(data.frame(
    Age = factor(age, levels = c("Young", "Middle", "Old")),
    Gender = factor(gender, levels = c("Male", "Female")),
    Income = factor(income, levels = c("Low", "Medium", "High")),
    MaritalStatus = factor(marital_status, levels = c("Single", "Married", "Divorced")),
    Children = factor(children, levels = c("No", "Yes")),
    TimeSpentOnSite = time_spent,
    ViewedProduct = factor(viewed_product, levels = c("No", "Yes"))
  ))
}

# Get user input and make a prediction
cat("Provide input values for prediction:\n")
test_data <- get_user_input()
prediction <- predict(model, test_data)
cat(paste("Predicted Class: ", prediction, "\n"))
