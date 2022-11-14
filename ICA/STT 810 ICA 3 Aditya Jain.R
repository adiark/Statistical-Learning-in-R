#2 Question 2

# a 

x1 <- dbinom(x = 6,size = 12,prob = 0.5)
x1

# b
x2 <- pbinom(q= 3, size = 12, prob = 0.5)
x2

# c
x3 <- sample(0:1,size = 12,replace = TRUE)
x3

# question 3

# a
y1 <- pbinom(q= 2, size = 6, prob = 0.2)
y1

# b
y2 <- pbinom(q= 0, size = 6, prob = 0.2)
y2

# c

game <- function(nd) return(sample(0:1,nd,replace = TRUE,prob = c(0.8 ,0.2)))


probtotk <- function(nd,nreps){
  win <- replicate(nreps , sum(game(nd)))
  return(mean(win==0))
}


win <- probtotk(6,1000)
win
