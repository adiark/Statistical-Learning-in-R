# question 1
library(ggplot2)
library(tidyverse)

data = diamonds

a.
hist(data$price)

# yes it is exponential

b.
samp <- data$price
f <- function (lamb) length(samp)*log(lamb)-1*lamb*sum(samp)
optimize(f,c(0,5000),maximum=TRUE)
optimize(f,c(0,100),maximum=TRUE)
optimize(f,c(0,10),maximum=TRUE)
optimize(f,c(0,1),maximum=TRUE)
optimize(f,c(0,0.1),maximum=TRUE)

1/0.0002423274
1/0.0002683747
1/0.0002525061
1/0.0002800336
1/0.0002698719

mean(data$price)
best estimate comes in (0,10)
# question 2

hist(data$z)


samp <- data$z
para <- c(mean(samp),sd(samp))
log_lik <- function(para) -1*sum(log(dnorm(samp,para[1],para[2])))
optim(c(5, 2), log_lik, samp)

# question 3

x <- c(0.1, 0.2, 0.5, 0.7, 0.8, 0.9, 0.95)
f <- function (alpha) length(x)*log(alpha + 1) + alpha*sum(log(x))
optimize(f,c(0,1),maximum=TRUE)


