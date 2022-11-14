## Question 1

# a 

xa <- dpois(x = 3, lambda = 0.25*15)
xa

# b
xb <- 1 - ppois(q = 11,lambda = 0.25*60)
xb

# c
xc <- qpois(p = c(0.5,0.9),lambda = 0.25*10) 
xc

# d
xd <- rpois(n = 10, lambda = 0.25*120)
xd

# e
xe <- seq(1:15)
ye <- dpois(x = xe, lambda = 0.25*20)
plot(xe,ye)

## Question 2

# a
xa <- pnbinom(q = 9,size = 5,prob = 0.4)
xa

# b
xb <- 1 - pnbinom(q = 25, size = 5, prob = 0.4)
xb

# c
xc <- rnbinom(n = 100, size = 5,prob = 0.4)
xc
mean(xc)

# d
xd <- seq(1:30)
yd <- dnbinom(x = xd, size = 5, prob = 0.4)
plot(xd,yd)


