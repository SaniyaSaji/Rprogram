library(C50)

data <- read.csv("/home/ds-da-09/saniya/decision.csv", header = TRUE, stringsAsFactors = TRUE)

tree_model <- C5.0(PlayTennis ~ Outlook + Temperature + Humidity + Wind, data = data)

plot(tree_model, type = "simple")

summary_text <- capture.output(summary(tree_model))

rule_start <- grep("Decision tree:", summary_text) + 1
rule_end <- grep("Evaluation on training data", summary_text) - 1

rules <- summary_text[rule_start:rule_end]

cat(rules, sep = "\n")

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

prediction <- predict(tree_model, new_data)

cat("\nPredicted PlayTennis outcome:(1 No, 2 Yes) ", prediction, "\n")
