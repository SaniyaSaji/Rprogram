library(e1071)  
library(ggplot2)  
library(C50) 

dataset_path_svm <- "/home/ds-da-09/saniya/svm.csv"
banknote_data <- read.csv(dataset_path_svm)

colnames(banknote_data) <- c("variance", "skewness", "kurtosis", "entropy", "class")
banknote_data$class <- factor(banknote_data$class, levels = c(0, 1), labels = c("Authentic", "Forged"))

set.seed(123)  
train_indices <- sample(1:nrow(banknote_data), 0.8 * nrow(banknote_data))
train_data <- banknote_data[train_indices, ]
test_data <- banknote_data[-train_indices, ]

svm_model <- svm(class ~ variance + skewness, data = train_data, kernel = "radial")

data <- read.csv("/home/ds-da-09/saniya/decision.csv", header = TRUE, stringsAsFactors = TRUE)
tree_model <- C5.0(PlayTennis ~ Outlook + Temperature + Humidity + Wind, data = data)

plot(tree_model, type = "simple")

summary_text <- capture.output(summary(tree_model))
rule_start <- grep("Decision tree:", summary_text) + 1
rule_end <- grep("Evaluation on training data", summary_text) - 1
rules <- summary_text[rule_start:rule_end]
cat(rules, sep = "\n")

get_user_input_svm <- function() {
  cat("Enter values for the features:\n")
  variance <- as.numeric(readline(prompt = "Variance: "))
  skewness <- as.numeric(readline(prompt = "Skewness: "))
  kurtosis <- as.numeric(readline(prompt = "Kurtosis: "))
  entropy <- as.numeric(readline(prompt = "Entropy: "))
  
  user_input <- data.frame(
    variance = variance,
    skewness = skewness,
    kurtosis = kurtosis,
    entropy = entropy
  )
  
  x_range <- seq(min(train_data$variance) - 1, max(train_data$variance) + 1, length.out = 200)
  y_range <- seq(min(train_data$skewness) - 1, max(train_data$skewness) + 1, length.out = 200)
  grid <- expand.grid(variance = x_range, skewness = y_range)
  grid_predictions <- predict(svm_model, grid)
  
  grid$class <- grid_predictions 
  print(ggplot() +
          geom_tile(data = grid, aes(x = variance, y = skewness, fill = class), alpha = 0.3) +
          geom_contour(data = grid, aes(x = variance, y = skewness, z = as.numeric(class)), color = "black", binwidth = 0.5) +
          geom_point(data = train_data, aes(x = variance, y = skewness, color = class), size = 2) +
          scale_fill_manual(values = c("lightblue", "lightcoral")) +
          scale_color_manual(values = c("blue", "brown")) +
          labs(
            title = "SVM Decision Boundary ",
            x = "Variance",
            y = "Skewness",
            fill = "Class",
            color = "Class"
          ) +
          theme_minimal())
  
  return(user_input)
}

get_user_input_dt <- function() {
  cat("\nEnter values for prediction:\n")
  
  outlook <- readline(prompt = "Outlook (Sunny/Overcast/Rain): ")
  temperature <- readline(prompt = "Temperature (Hot/Mild/Cool): ")
  humidity <- readline(prompt = "Humidity (High/Normal): ")
  wind <- readline(prompt = "Wind (Weak/Strong): ")
  
  new_data <- data.frame(
    Outlook = factor(outlook, levels = levels(data$Outlook)),
    Temperature = factor(temperature, levels = levels(data$Temperature)),
    Humidity = factor(humidity, levels = levels(data$Humidity)),
    Wind = factor(wind, levels = levels(data$Wind))
  )
  
  return(new_data)
}

while (TRUE) {
  cat("\nMenu:\n")
  cat("1. Make a prediction using SVM\n")
  cat("2. Make a prediction using Decision Tree\n")
  cat("3. Exit\n")
  choice <- as.integer(readline(prompt = "Enter your choice : "))
  
  if (choice == 1) {
    cat("Provide input values for SVM prediction:\n")
    user_input <- get_user_input_svm()
    
    prediction <- predict(svm_model, user_input)
    cat("Predicted Class (SVM)(0:Authentic, 1:Forged)", as.character(prediction), "\n")
    
  } else if (choice == 2) {
    cat("Provide input values for Decision Tree prediction:\n")
    new_data <- get_user_input_dt()
    
    prediction <- predict(tree_model, new_data)
    
    cat("\nPredicted PlayTennis outcome (1: No, 2: Yes): ", prediction, "\n")
    
  } else if (choice == 3) {
    cat("Exiting the program.\n")
    break  
  } else {
    cat("Invalid choice. Please enter 1, 2, or 3.\n")
  }
}