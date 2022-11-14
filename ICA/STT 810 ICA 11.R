library(MASS)

# question 1 
a.
exp_1 <- rexp(1000000,1/3)
exp_2 <- rexp(1000000,1/3)

su <- exp_1 + exp_2

a <- mean(exp_1) + mean(exp_2)
b <- mean(su) 

a==b

b.

a <- var(exp_1) + var(exp_2)
b <- var(su) 

a==b
paste(a,b)

# question 2
a.

library(mvtnorm)
min <- -3
max <- 3
var_x <- 1.5
var_y <- 1.5
cor_xy <- -0.8
Sigma <- cbind(c(var_x,cor_xy*sqrt(var_x)*sqrt(var_y)),c(cor_xy*sqrt(var_x)*sqrt(var_y),var_y))
mu <- c(1, 3)
mvn <- mvrnorm(1000000, mu, Sigma)

x <- mvn[,1]
y <- mvn[,2]

z= x + y
cov(x,y)

b.

a <- mean(x) + mean(y)
b <- mean(z)
paste(a,b)
a==b

c.
a <- var(x) + var(y) + 2*cov(x,y)
b <- var(z)
a==b
paste(a,b)

# question 3
x3 <- lapply(1:20000, function(x) rexp(10000,1/3))

x3m <- c()

for (i in 1:20000){
    x3m[i] <- mean(x3[[i]])
}

hist(x3m)
