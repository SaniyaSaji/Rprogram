library(ggplot2)
library(lattice)

simple <- function() {
  library(ggplot2)
  library(lattice)
  
  data <- read.csv("/home/ds-da-09/saniya/new_dataset.csv")
  
  n <- length(data$Years)
  xsum <- sum(data$Years)
  ysum <- sum(data$Salary)
  xsquare <- sum(data$Years^2)
  xy <- sum(data$Years * data$Salary)
  
  m <- (n * xy - (xsum * ysum)) / (n * xsquare - (xsum)^2)
  b <- (ysum - (m * xsum)) / n
  
  print(paste("Slope (m):", m))
  print(paste("Intercept (b):", b))
  
  test_x <- as.numeric(readline(prompt = "Enter Years of Experience for prediction: "))
  
  if (length(test_x) == 0 || is.na(test_x)) {
    print("Invalid input. Please enter a numeric value.")
    return()
  }
  
  predicted_y <- b + m * test_x
  print(paste("Predicted Salary for", test_x, "years of experience:", predicted_y))
  
  predicted_values <- b + m * data$Years
  actual_values <- data$Salary
  
  mae_manual <- mean(abs(actual_values - predicted_values))
  mse_manual <- mean((actual_values - predicted_values)^2)
  rmse_manual <- sqrt(mse_manual)
  
  print(paste("Mean Absolute Error (MAE):", mae_manual))
  print(paste("Root Mean Squared Error (RMSE):", rmse_manual))
  
  plot1 <- ggplot(data, aes(x = Years, y = Salary)) +
    geom_point(color = "darkgreen") +
    geom_abline(intercept = b, slope = m, color = "black", lwd = 1.2) +
    labs(title = "Linear Regression", x = "Years of Experience", y = "Salary ($)") +
    annotate("text", x = test_x, y = predicted_y, label = paste("Predicted:", round(predicted_y, 2)), hjust = -0.2)
  
  print(plot1)
}

multi <- function() {
  data <- read.csv("/home/ds-da-09/saniya/Student_datset.csv")
  
  dependent_var <- names(data)[ncol(data)]
  independent_vars <- names(data)[-ncol(data)]
  
  data$Extracurricular.Activities <- as.factor(data$Extracurricular.Activities)
  
  formula <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+")))
  model <- lm(formula, data = data)
  
  print(summary(model))
  
  actual_values <- data[[dependent_var]]
  new_data <- data[independent_vars]
  new_data$Extracurricular.Activities <- as.factor(new_data$Extracurricular.Activities)
  
  predicted_values <- predict(model, newdata = new_data)
  
  ss_total <- sum((actual_values - mean(actual_values))^2)
  ss_residual <- sum((actual_values - predicted_values)^2)
  r_squared_manual <- 1 - (ss_residual / ss_total)
  
  mae_manual <- mean(abs(actual_values - predicted_values))
  mse_manual <- mean((actual_values - predicted_values)^2)
  rmse_manual <- sqrt(mse_manual)
  
  cat("Manually Computed R-squared: ", r_squared_manual, "\n")
  cat("Mean Absolute Error (MAE): ", mae_manual, "\n")
  cat("Root Mean Squared Error (RMSE): ", rmse_manual, "\n")
  
  num_vars <- independent_vars[independent_vars != "Extracurricular.Activities"]
  num_input <- as.numeric(strsplit(readline(prompt = paste("Enter values for", paste(num_vars, collapse = ", "), ": ")), ",")[[1]])
  
  if (length(num_input) != length(num_vars)) {
    cat("Invalid input! Please enter", length(num_vars), "numeric values.\n")
    return()
  }
  
  cat_input <- readline(prompt = "Enter 'Yes' or 'No' for Extracurricular Activities: ")
  
  if (!(cat_input %in% c("Yes", "No"))) {
    cat("Invalid input! Please enter 'Yes' or 'No'.\n")
    return()
  }
  
  new_input <- data.frame(matrix(num_input, nrow = 1, byrow = TRUE))
  colnames(new_input) <- num_vars
  new_input$Extracurricular.Activities <- factor(cat_input, levels = levels(data$Extracurricular.Activities))
  
  predicted_value <- predict(model, newdata = new_input)
  cat("Predicted value:", predicted_value, "\n")
  
  return(model)
}

while (TRUE) {
  print("\n----MENU----")
  print("1. Simple Linear Regression")
  print("2. Multiple Linear Regression")
  print("3. Exit")
  
  k <- as.numeric(readline(prompt = "Enter your choice: "))
  
  if (is.na(k)) {
    print("Invalid input. Please enter a valid number.")
    next
  }
  
  if (k == 1) {
    simple()
  } else if (k == 2) {
    multi()
  } else if (k == 3) {
    break
  } else {
    print("Invalid choice, please try again.")
  }
}
