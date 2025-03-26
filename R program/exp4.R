load<-read.csv("/home/ds-da-09/saniya/dataset.csv")
x<-load$col1
y<-load$col2
cat("Variance : ",var(x))


barplot(table(data$ordinal_data1), main="Bar Plot: Ordinal Data 1",
        xlab="Ordinal Categories", ylab="Frequency")
barplot(table(data$nominal_data1), main="Bar Plot: Nominal Data 1",
        xlab="Nominal Categories", ylab="Frequency")
barplot(table(data$binary_data1), main="Bar Plot: Binary Data 1",
        xlab="Binary Values", ylab="Frequency")
