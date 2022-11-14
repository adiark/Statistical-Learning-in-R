setwd("C:\\Users\\Adi\\Desktop\\Fall-22 Study Material\\STT 810\\HW\\HW5")

# question 1
cars_multi <- read.csv('cars_multi.csv', header = TRUE)

mean_acc <- mean(cars_multi$acceleration)

std_acc <- sd(cars_multi$acceleration)

serror <- std_acc/sqrt(length(cars_multi$acceleration))

quantile_range <- quantile(cars_multi$acceleration, c(0.05,0.95))

# question 2

a1 <- mean_acc + (std_acc/sqrt(length(cars_multi$acceleration)))*qt(0.05,length(cars_multi$acceleration)-1)
paste(a1)



a2 <- pt((15.2- mean_acc)/(std_acc/sqrt(length(cars_multi$acceleration))),length(cars_multi$acceleration)-1)
a2


trials <- 10000
sampmean <- mean_acc
n <- length(cars_multi$acceleration)
H0mean <- 15.2
simmean <- rep(0,trials)
for(i in 1:trials){
  simsamp <- H0mean + std_acc*rt(n, n - 1) 
  simmean[i] <- mean(simsamp)
}
p_value <- sum(simmean >= sampmean)/trials
p_value


# question 3
trials <- 10000
sampmean <- 2
n <- 10
H0mean <- 1.1
simmean <- rep(0,trials)
for(i in 1:trials){
  simsamp <- rpois(n, H0mean)
  simmean[i] <- mean(simsamp)
}
p_value <- sum(simmean >= sampmean)/trials
p_value
