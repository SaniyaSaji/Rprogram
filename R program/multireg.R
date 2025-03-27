library(Metrics)

while(TRUE){

  cat("1.linear regression\n2.multiple linear regression\n3.exit\n")

  cat("enter your choice:")

  choice<-as.integer(readline())

  if (choice==1){

    

    cat("performing linear regression on mtcars dataset\n")

    model<-lm(mpg~hp,data=mtcars)

    predictions<-predict(model,data=mtcars)

    rmse_val<-rmse(mtcars$mpg,predictions)

    cat("rmse:",rmse_val,"\n")

    cat("enter value of horsepower:")

    print(coef(model))

    hp_val<-as.numeric(readline())

    newdf<-data.frame(hp=hp_val)

    predicted_mpg<-predict(model,newdata=newdf)

    cat("prediction for mpg:",predicted_mpg,"\n")

    plot(mtcars$hp,mtcars$mpg,col="black",main = "Linear regression",xlab="hp",ylab = "mpg")

    abline(model,col="red",lwd=2)

    

  }else if(choice==2){

    

    cat("performing multiple regression on mtcars dataset\n")

    model<-lm(mpg~hp+wt,data=mtcars)

    predictions<-predict(model,data=mtcars)

    rmse_val<-rmse(mtcars$mpg,predictions)

    cat("rmse:",rmse_val,"\n")

    print(coef(model))

    cat("enter value of horsepower:")

    hp_val<-as.numeric(readline())

    cat("enter value of weight:")

    wt_val<-as.numeric(readline())

    newdf<-data.frame(hp=hp_val,wt=wt_val)

    predicted_mpg<-predict(model,newdata=newdf)

    cat("prediction for mpg:",predicted_mpg,"\n")

    

  }else if(choice==3){

    break

  }else{

    cat("invalid input")

  }
}
