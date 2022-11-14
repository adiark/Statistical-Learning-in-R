setwd("C:\\Users\\Adi\\Downloads")
# question 1 

x1 = read.csv('cars_multi.csv',header = TRUE, stringsAsFactors = FALSE)
data("mtcars")
paste(mean(mtcars$qsec), sd(mtcars$qsec))


# question 2
x2 = sd(mtcars$qsec)/sqrt(length(mtcars$qsec))
x2

# question 3

mn <- mean(mtcars$qsec)
sdev <- sd(mtcars$qsec)
n <- length(mtcars$qsec)
serror <- sdev/sqrt(n)
CI <- mn + serror*qt(c(0.025,0.975), length(mtcars$qsec) - 1)
CI

LCL <- mn + serror*qt(0.01, length(mtcars$qsec) - 1)
LCL

LCLN <- mn + serror*qnorm(0.01,mn,sdev)
LCLN

# question 4
UCL95 <- mn + serror*qt(0.95, length(mtcars$qsec) - 1)
UCL95

LCL95 <- mn + serror*qt(0.05, length(mtcars$qsec) - 1)
LCL95


# question 5
serror <- sdev/sqrt(200000)
Ci = mn +serror*qt(c(0.005,0.995),200000)
Ci
Ci[2]-Ci[1]
