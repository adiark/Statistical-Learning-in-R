# question 1 

# The null hypothesis is - The mean of mpg in the cars data set is equal to 20.09. The hypothesis is two sided.

trials <- 100000
sampmean <- 22.3
H0mean <- 20.09
sampsd <- 5
n <- 25
p_value <- 0
sampse <- sampsd/sqrt(n)
simmean <- rep(0,trials)
for(i in 1:trials){
  simsamp <- H0mean + sampsd*rt(n, n - 1) 
  simmean[i] <- mean(simsamp)
}
p_value <- sum(abs(simmean - H0mean) > abs(sampmean - H0mean))/trials
p_value 


# The null hypothesis is falsified as p value is less than 0.05, The p-value is approximately equal to what we got last time.


# question 2

# The null hypothesis is - The mean value is less than or equal to 28. The hypothesis is one sided.

trials <- 10000
sampmean <- 30
n <- 750
H0mean <- 28
simmean <- rep(0,trials)
for(i in 1:trials){
  simsamp <- rexp(n, 1/H0mean)
  simmean[i] <- mean(simsamp)
}
p_value <- sum(simmean >= sampmean)/trials
p_value

# The null hypothesis is falsified as p value is less than 0.05
