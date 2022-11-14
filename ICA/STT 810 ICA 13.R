# question 1
f <- function(x) x^2 + 3*sin(x)
a.

plot(seq(-8,8, by = 0.1), f(seq(-8,8, by = 0.1)))

b.
optimize(f, c(-3,3))

c.
optimize(f, c(-3,3),maximum = TRUE)

# question 2

#3-d plot of z 
a.
fm <- function(x) x[1]^2 + x[2]^2 - 3*x[1] + 2*x[2] + sin(x[1]*x[2])
x_min <- -5
x_max <- 5
y_min <- -5
y_max <- 5
x <- seq(-5, 5, by = 0.05)
y <- seq(-5, 5, by = 0.05)
z <- matrix(nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j] <- fm(c(x[i],y[j]))
  }
}
contour(x,y,z)

# Multi-variable optimization
b.
optim(c(0,0),fm)
