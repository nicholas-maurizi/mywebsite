#raw script


exp <- c("n2high","zig4high","n2low","zig4low") #experimental labels, just as a group of four to be used later
for (j in 1:5) {
  for (i in 1:4){#four total sets of data, each unique
    if(i %% 2==1){#two must be closer to zero and two closer to one to simulate significant results
      varname <- paste0("x_",i) #creating a unique name for each vector
      assign(varname,rnorm(50,mean=runif(1,min=0.4,max=1),sd=runif(1,min=0.05,max=0.5)))#the data has a normal distribution and each new vector has a random mean in a desired range and a random standard deviation within a reasonable range
    } else {
      varname <- paste0("x_",i)
      assign(varname,rnorm(50,mean=runif(1,min=0,max=0.6),sd=runif(1,min=0.05,max=0.5)))
    }
    if (i==4){ #two of the vectors (low salt treatment) have to be negative so I waited till the last loop and multiplied the vectors by -1
      x_3 <- x_3*-1
      x_4 <- x_4*-1
      experiment <- rep(exp, each = 50) #scaling experimental labels to fit the amount of data
      worm_index <- c(x_1,x_2,x_3,x_4) # combining data into a single vector
      fakewormdata <- data.frame(worm_index,experiment) #data frame of whole dataset (just preference index and experimental labels)
      anova1 <- aov(fakewormdata$worm_index~ fakewormdata$experiment)

      sum <- summary(anova1) #quick initial anova before adjusting data parameters
      cat("anova number",j,"\n")
      print(sum)
      cat("\n\n\n")
    }
  }
}

exp <- c("n2high","zig4high","n2low","zig4low")
experiment <- rep(exp, each = 50)
worm_index <- c(x_1,x_2,x_3,x_4)
fakewormdata <- data.frame(worm_index,experiment)

for (j in 2:5) {
  y_1 <- x_1[1:(j*10)] #resampling the data
  y_2 <- x_2[1:(j*10)]
  y_3 <- x_3[1:(j*10)]
  y_4 <- x_4[1:(j*10)]
  index <- c(y_1,y_2,y_3,y_4) #recombining the data
  exp <- rep(exp,each=(j*10))
  data <- data.frame(index,exp) #turning it into a data frame
  anova <- aov(data$index ~ data$exp)
  turkey <- TukeyHSD(anova) #post hoc test
  high <- turkey$data[2,4] #these two lines isolate the desired parts of the post hoc test, one comparing the high salt conditioned groups to each other and one comparing the low salt conditioned ones.
  low <- turkey$data[5,4]
  cat("When n =",j*40,"and each strain x treatment has a random standard deviation between 0.05 and 0.5: \n Analysis: \n zig-4 (mean =",mean(y_2),") x wild type (mean =",mean(y_1),"), with high salt conditioning: \n p =",high,"\n Analysis: \n zig-4 (mean =",mean(y_4),") x wild type (mean =",mean(y_3),"), with low salt conditioning: \n p =",low,"\n\n\n")
}
