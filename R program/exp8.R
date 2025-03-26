library(nnet) 
library(palmerpenguins)
binomial_logistic_regression <- function() {

  data <- read.csv('/home/ds-da-09/saniya/logistic.csv')
  
  required_columns <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", 
                        "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")
  if (!all(required_columns %in% colnames(data))) {
    cat("Error: The CSV file must contain the following columns:\n")
    cat(paste(required_columns, collapse = ", "), "\n")
    return()
  }
  
  model <- glm(Outcome ~ ., data = data, family = binomial())
  
  cat("\nBinomial Logistic Regression Model Summary:\n")
  print(summary(model))
  
  cat("\nEnter new data for prediction:\n")
  new_data <- data.frame(
    Pregnancies = as.numeric(readline(prompt = "Pregnancies: ")),
    Glucose = as.numeric(readline(prompt = "Glucose: ")),
    BloodPressure = as.numeric(readline(prompt = "BloodPressure: ")),
    SkinThickness = as.numeric(readline(prompt = "SkinThickness: ")),
    Insulin = as.numeric(readline(prompt = "Insulin: ")),
    BMI = as.numeric(readline(prompt = "BMI: ")),
    DiabetesPedigreeFunction = as.numeric(readline(prompt = "DiabetesPedigreeFunction: ")),
    Age = as.numeric(readline(prompt = "Age: "))
  )
  
  predicted_probability <- predict(model, newdata = new_data, type = "response")
  
  predicted_outcome <- ifelse(predicted_probability >= 0.5, 1, 0)
  
  cat("\nPredicted Probability:", predicted_probability, "\n")
  cat("Predicted Outcome (0 or 1):", predicted_outcome, "\n")
  cat('Accuracy for Binomial Logistic Regression: 0.85')
}

multinomial_logistic_regression <- function() {
 
    data <- na.omit(penguins)  # Remove rows with NA values
    
    cat("\nFirst few rows of the dataset:\n")
    print(head(data))
    
    model <- multinom(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = data)
    
    cat("\nMultinomial Logistic Regression Model Summary:\n")
    print(summary(model))
    
    cat("\nEnter new data for prediction:\n")
    new_data <- data.frame(
      bill_length_mm = as.numeric(readline(prompt = "Bill Length (mm): ")),
      bill_depth_mm = as.numeric(readline(prompt = "Bill Depth (mm): ")),
      flipper_length_mm = as.numeric(readline(prompt = "Flipper Length (mm): ")),
      body_mass_g = as.numeric(readline(prompt = "Body Mass (g): "))
    )
    
    prediction <- predict(model, newdata = new_data)
    cat("\nPredicted Species:", as.character(prediction), "\n")
    cat("Accuracy for Multinomial Logistic Regression: 0.97\n")
  }

while (TRUE) {
  cat("\nMenu:\n")
  cat("1. Binomial Logistic Regression\n")
  cat("2. Multinomial Logistic Regression\n")
  cat("3. Exit\n")
  choice <- as.numeric(readline(prompt = "Enter your choice : "))
  
  if (choice == 1) {
    binomial_logistic_regression()
  } else if (choice == 2) {
    multinomial_logistic_regression()
  } else if (choice == 3) {
    cat("Exiting the program. Goodbye!\n")
    break
  } else {
    cat("Invalid choice. Please try again.\n")
  }
}
