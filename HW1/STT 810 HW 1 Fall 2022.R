library(dplyr)

# Question 1

# a 
a1 <- sample(x = seq(1,6),size = 100, replace = TRUE)
a1

# b 
mean(a1)

# comes out to be 3.31

#c
c1 <- sample(x = seq(1,6),size = 1000, replace = TRUE)
c2 <- sample(x = seq(1,6),size = 10000, replace = TRUE)
c3 <- sample(x = seq(1,6),size = 100000, replace = TRUE)
c4 <- sample(x = seq(1,6),size = 1000000, replace = TRUE)
c5 <- sample(x = seq(1,6),size = 10000000, replace = TRUE)
c6 <- sample(x = seq(1,6),size = 100000000, replace = TRUE)

for (i in list(c1,c2,c3,c4,c5,c6)){
  print(mean(i))
}

# c1 mean 3.53
# c2 mean 3.5111
# c3 mean 3.50528
# c4 mean 3.502325
# c5 mean 3.4996
# c6 mean 3.500192
# the mean is not approching the mean value

# d
d0 <- sample(x = c(0,1),size = 100, replace = TRUE)
d1 <- sample(x = c(0,1),size = 1000, replace = TRUE)
d2 <- sample(x = c(0,1),size = 10000, replace = TRUE)
d3 <- sample(x = c(0,1),size = 100000, replace = TRUE)
d4 <- sample(x = c(0,1),size = 1000000, replace = TRUE)
d5 <- sample(x = c(0,1),size = 10000000, replace = TRUE)
d6 <- sample(x = c(0,1),size = 100000000, replace = TRUE)

for (i in list(d0,d1,d2,d3,d4,d5,d6)){
  print(mean(i))
}

# d0 mean 0.49
# d1 mean 0.494
# d2 mean 0.4974
# d3 mean 0.49776
# d4 mean 0.499349
# d5 mean 0.5000382
# d6 mean 0.4999478
# the mean is approaching the mean value

# e 
e0 <- sample(x = c(0,1),size = 100, replace = TRUE,prob = c(1/3,2/3))
e1 <- sample(x = c(0,1),size = 1000, replace = TRUE,prob = c(1/3,2/3))
e2 <- sample(x = c(0,1),size = 10000, replace = TRUE,prob = c(1/3,2/3))
e3 <- sample(x = c(0,1),size = 100000, replace = TRUE,prob = c(1/3,2/3))
e4 <- sample(x = c(0,1),size = 1000000, replace = TRUE,prob = c(1/3,2/3))
e5 <- sample(x = c(0,1),size = 10000000, replace = TRUE,prob = c(1/3,2/3))
e6 <- sample(x = c(0,1),size = 100000000, replace = TRUE,prob = c(1/3,2/3))

for (i in list(e0,e1,e2,e3,e4,e5,e6)){
  print(mean(i))
}

# e0 mean 0.57
# e1 mean 0.663
# e2 mean 0.6711
# e3 mean 0.66954
# e4 mean 0.666707
# e5 mean 0.6666379
# e6 mean 0.6666686
# the mean is approching the input probability of the sample size

# Question 2 

# a
sample_space <- c(0,1,2,3,4,5)

#b

nreps <- 10000
nstops <- 10
count <- 0
for ( i in 1: nreps ) {
  passengers <- 0
  newpass <- sample (0:2 ,1 , prob = c (0.5 ,0.4 ,0.1))
  passengers <- passengers + newpass
  for ( j in 2: nstops ) {
    if (passengers == 0){
      count <- count + 1
      break
    }
    if (passengers > 0){ 
      for ( k in 1: passengers )
        if ( runif (1) < 0.2){
          passengers <- passengers - 1
        }
    }
    
    newpass <- sample (0:2 ,1 , prob = c (0.5 ,0.4 ,0.1))
    passengers <- passengers + newpass
    
  }  
}
print(nreps)
print(count)



minpiece <- function ( k ) {
  breakpts <- sort (runif(sample(2:k,size = 1,prob = c(0.3,0.3,0.4)) -1))
  lengths <- diff ( c (0 , breakpts ,1))
  min ( lengths )
}

bkrod <- function ( nreps ,k , q ) {
  minpieces <- replicate ( nreps , minpiece ( k ))
  mean ( minpieces < q )
}


bkrod (10000 ,4 ,0.02)

bkrod (10000 ,4 ,0.02)
