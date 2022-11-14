setwd("C:\\Users\\Adi\\Desktop\\Fall-22 Study Material\\STT 810\\HW\\HW4")
# question 1

func_1 <- function(x) x[1]**2 - 3*x[1] + 
  x[2]**4 -3*x[2] + x[3]**2 + 10*x[3] + cos(x[1]*x[2]*x[3])
optim( c(0,0,0),func_1)

optim(c(6,7,12),func_1)


optim(c(-6,19,32),func_1)


optim(c(-124,197,382),func_1)


# question 2
x <- c(2,3,2.5,3,1.6,1.4,1.3,1.8, 1.9, 2.4, 4.6)
f <- function (alpha) length(x)*log(alpha) - (alpha+1)*sum(log(x))
optimize(f,c(1,3),maximum=TRUE)

# question 3
betadata = read.csv('betasample.csv', header = TRUE)
y <- betadata$x
f <- function (x) -1*sum(log(dbeta(y,x[1],x[2])))

a = optim(par = c(2,4),fn = f)

y2 <- dbeta(seq(0.1,0.99,0.01), a$par[1],a$par[2])

plot(y2)


# question 4
penguins = read.csv('penguins.csv', header = TRUE)



mn <- mean(penguins$body_mass_g)
sdev <- sd(penguins$body_mass_g)
n <- length(penguins$body_mass_g)
serror <- sdev/sqrt(n)
CI <- mn + serror*qt(c(0.005,0.995), length(penguins$body_mass_g) - 1)
CI

mn_spe <- aggregate(penguins$body_mass_g, list(penguins$species), FUN = mean)
sdev_spe <- aggregate(penguins$body_mass_g, list(penguins$species), FUN = sd)
n_spe <- aggregate(penguins$body_mass_g, list(penguins$species), FUN = length)
serror_spe <- sdev_spe$x/sqrt(n_spe$x)

for (x in c(1:3)) {
  CI <- mn_spe$x[x] + serror_spe[x]*qt(c(0.005,0.995),n_spe$x[x] - 1)
  print(CI)
}
    
