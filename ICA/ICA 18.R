# question 1 
setwd("C:\\Users\\Adi\\Desktop\\Fall-22 Study Material\\STT 810\\ICA")
getwd()

data <- read.csv('hypodata.csv', stringsAsFactors = FALSE, header = TRUE)
data <- data$x

# question 2
mn <-  mean(data)
sd <- sd(data)
serror  <- sd/sqrt(length(data))

paste(mn, sd, serror)

# question 3 

a3 <- mn + serror*qt(c(0.005, 0.995), length(data) - 1)
a3

# question 4

# regular bootstrap 
datamean1 <- rep(0,10000)
datasd1 <- rep(0,10000)
for(i in 1:10000){
  datasamp <- sample(data, length(data), replace = TRUE)
  datamean1[i] <- mean(datasamp)
  datasd1[i] <- sd(datasamp)
}
quantile(datamean1, c(0.005, 0.995))
quantile(datasd1, c(0.005, 0.995))

# question 5

# Bayesian bootstrap 
library(DirichletReg)
weight <- rep(0,length(data))
datamean2 <- rep(0,10000)
datasd2 <- rep(0,10000)
for(i in 1:10000){
  weight <- rdirichlet(1, rep(1,length(data)))
  datasamp <- sample(data, length(data), prob = weight, replace = TRUE)
  datamean2[i] <- mean(datasamp)
  datasd2[i] <- sd(datasamp)
}
quantile(datamean2, c(0.005, 0.995))
quantile(datasd2, c(0.005, 0.995))


# question 6 
hist(datamean1, breaks = 20)
hist(datamean2, breaks = 20)

# question 7 

# question 8
hist(datasd1, breaks = 20)
hist(datasd2, breaks = 20)
