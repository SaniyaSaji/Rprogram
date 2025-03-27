library(nnet)

data(iris)

iris$Species=as.factor(iris$Species)

iris$binaryspecies<-ifelse(iris$Species=="setosa",1,0)

iris$binaryspecies<-as.factor(iris$binaryspecies)



while(TRUE){

  cat("\nMenu:\n")

  cat("1. Binomial Logistic Regression\n")

  cat("2. Multinomial Logistic Regression\n")

  cat("3. Exit\n")

  cat("Enter your choice: ")

  choice=as.integer(readline())

  if (choice==1){

    model<-glm(binaryspecies~Sepal.Width+Sepal.Length,data=iris,family="binomial")

    pred_prob <- predict(model, type = "response")

    pred_class <- ifelse(pred_prob > 0.5, 1, 0)

    accuracy_model<-mean(pred_class==iris$binaryspecies)

    cat("accuracy:",accuracy_model,"\n")

    cat("Enter Sepal Length: ")

    sep_len <- as.numeric(readline())

    cat("Enter Sepal Width: ")

    sep_wid <- as.numeric(readline())

    new_data <- data.frame(Sepal.Length = sep_len, Sepal.Width = sep_wid)

    pred_prob <- predict(model, newdata=new_data,type = "response")

    pred_class <- ifelse(pred_prob > 0.5, 1, 0)

    cat("prediction:",pred_class,"\n")

  }

  if (choice==2){

    model<-multinom(Species~Sepal.Width+Sepal.Length,data=iris)

    predicted<-predict(model,newdata=iris)

    accuracy_model<-mean(predicted==iris$Species)

    cat("accuracy:",accuracy_model,"\n")

    cat("Enter Sepal Length: ")

    sep_len <- as.numeric(readline())

    cat("Enter Sepal Width: ")

    sep_wid <- as.numeric(readline())

    new_data <- data.frame(Sepal.Length = sep_len, Sepal.Width = sep_wid)

    prediction<-predict(model,newdata=new_data,type="class")

    cat("Predicted Species:", as.character( prediction), "\n")

    

  }

  else if (choice==3){

    break

  }

  }
