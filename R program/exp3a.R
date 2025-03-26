data <- read.csv("/home/ds-da-09/saniya/dataset.csv")

head(data)

numeric_variance1 <- var(data$numeric_data1)
numeric_variance2 <- var(data$numeric_data2)
numeric_covariance <- cov(data$numeric_data1, data$numeric_data2)
numeric_correlation <- cor(data$numeric_data1, data$numeric_data2)

cat("Numeric vs Numeric - Variance 1:", numeric_variance1, "\n")
cat("Numeric vs Numeric - Variance 2:", numeric_variance2, "\n")
cat("Numeric vs Numeric - Covariance:", numeric_covariance, "\n")
cat("Numeric vs Numeric - Correlation:", numeric_correlation, "\n")

nominal_data1 <- factor(data$nominal_data1)
nominal_data2 <- factor(data$nominal_data2)

nominal_covariance <- cov(as.numeric(nominal_data1), as.numeric(nominal_data2))
nominal_correlation <- cor(as.numeric(nominal_data1), as.numeric(nominal_data2))

cat("Nominal vs Nominal - Covariance:", nominal_covariance, "\n")
cat("Nominal vs Nominal - Correlation:", nominal_correlation, "\n")

ordinal_data1 <- factor(data$ordinal_data1, levels = c("Low", "Medium", "High"), ordered = TRUE)
ordinal_data2 <- factor(data$ordinal_data2, levels = c("Low", "Medium", "High"), ordered = TRUE)

ordinal_covariance <- cov(as.numeric(ordinal_data1), as.numeric(ordinal_data2))
ordinal_correlation <- cor(as.numeric(ordinal_data1), as.numeric(ordinal_data2))

cat("Ordinal vs Ordinal - Covariance:", ordinal_covariance, "\n")
cat("Ordinal vs Ordinal - Correlation:", ordinal_correlation, "\n")

binary_covariance <- cov(data$binary_data1, data$binary_data2)
binary_correlation <- cor(data$binary_data1, data$binary_data2)

cat("Binary vs Binary - Covariance:", binary_covariance, "\n")
cat("Binary vs Binary - Correlation:", binary_correlation, "\n")

nominal_table <- table(nominal_data1, nominal_data2)
chi_square_nominal <- chisq.test(nominal_table)
cat("Chi-Square Test for Nominal vs Nominal\n")
cat("Chi-Square Statistic:", chi_square_nominal$statistic, "\n")

ordinal_table <- table(ordinal_data1, ordinal_data2)
chi_square_ordinal <- chisq.test(ordinal_table)
cat("Chi-Square Test for Ordinal vs Ordinal\n")
cat("Chi-Square Statistic:", chi_square_ordinal$statistic, "\n")


binary_table <- table(data$binary_data1, data$binary_data2)
chi_square_binary <- chisq.test(binary_table)
cat("Chi-Square Test for Binary vs Binary\n")
cat("Chi-Square Statistic:", chi_square_binary$statistic, "\n")
plot(data$numeric_data1, data$numeric_data2, 
     main="Line Plot: Numeric Data", 
     xlab="Numeric Data 1", ylab="Numeric Data 2", 
     type="l", 
     col="blue", 
     lwd=2, 
     pch=19, 
     cex=1.5)


