library(MASS)
library(dplyr)

data <- read.csv('C:\\Users\\Adi\\Desktop\\Fall-22 Study Material\\STT 810\\ICA\\Pizza.csv')
data <- data.frame(data,stringsAsFactors = TRUE)
data

apply(data, 2, as.integer)

# question 1
a
for (columns in col(data))
  print(cov(col))
cov(data)

cov(data$mois, data$cal)
cor(data$mois, data$cal)

b
x2 <- mean(data$mois*data$cal) - mean(data$mois)*mean(data$cal)
x2

c
cov(data$mois, data$cal)/(sd(data$mois)*sd(data$cal))

d

cor(data$cal,data$mois)
    
# question 2
a.

library(mvtnorm)
min <- -3
max <- 3
var_x <- 2
var_y <- 1
cor_xy <- -0.5
Sigma <- cbind(c(var_x,cor_xy*sqrt(var_x)*sqrt(var_y)),c(cor_xy*sqrt(var_x)*sqrt(var_y),var_y))
x <- seq(min, max, by = 0.1)
y <- seq(min, max, by = 0.1)
z <- matrix(nrow=length(x), ncol=length(y))
co_df <- data.frame('x', 'y', 'z')
for (i in 1:length(x)){
  for (j in 1:length(y)){
    z[i,j] <- dmvnorm(c(x[i],y[j]),c(0,0),Sigma)
  }
}
contour(x, y, z)

b.
library(MASS)
library(ggplot2)

co_df <- data.frame('x' = x, 'y' = y)
ggplot(co_df, aes(x = x, y = y, z = z)) + geom_contour()
df.grad <- expand.grid(x = seq(-4,4, by = 0.1),y = seq(-4,4, by = 0.1))
dens <- cbind(df.grad, z = dmvnorm(df.grad,c(0,0), Sigma))
ggplot(dens, aes(x = x, y = y, z = z)) + geom_contour_filled()
plot(mvrnorm(1000, mu = c(1,2), Sigma))
