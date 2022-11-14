

# Question 2

p_seq <- c(0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1)
x2_m <- sum(seq(1:8)*p_seq)
x2_m

x2_ms <- sum(seq(1:8)^2*p_seq)
x2_v <- x2_ms - x2_m^2
x2_v


# question 3

a.

x3_a <- mean(rpois(5,8))
x3_a
x3_a1 <- mean(rpois(10,8))
x3_a1
x3_a2 <- mean(rpois(15,8))
x3_a2

b. 

x3_b <- mean(rpois(n = 1000 ,lambda = 8))
x3_b

# Question 4

a. 

x4_a <- mean(rbinom(n = 1000000,size = 24,prob = 0.3))
x4_a

x4_av <- var(rbinom(n = 1000000,size = 24,prob = 0.3))
x4_av

b. 

x4_b <- mean(rbinom(n = 1000000,size = 150,prob = 0.8))
x4_b

x4_bv <- var(rbinom(n = 1000000,size = 150,prob = 0.8))
x4_bv

d. 

x4_d <- mean(rexp(n = 1000000,rate = 0.3))
x4_d

x4_dv <- var(rexp(n = 1000000,rate = 0.3))
x4_dv

e. 

x4_e <- mean(rexp(n = 1000000,rate = 3))
x4_e

x4_ev <- var(rexp(n = 1000000,rate = 3))
x4_ev

f. 

x4_f <- mean(rnorm(n = 1000000,mean = 4, sd = 1.4))
x4_f

x4_fv <- var(rnorm(n = 1000000,mean = 4, sd = 1.4))
x4_fv

