# questions 

# 1 
A.data <- c(5,-1,5,-1,10,1,5,1,-3)
A = matrix(A.data, nrow = 3, ncol = 3, byrow = TRUE)
A

B.data = c(5,1,3,1,-2,0)
B = matrix(B.data, nrow = 2, ncol = 3, byrow = TRUE)
B

C.data = c(2,1,-3,4)
C = matrix(C.data, nrow = 2, ncol = 2, byrow = TRUE)
C

X = as.matrix(c(5,1,4))
X

# 2
two = B%*%A
two

# 3
three = C%*%B
three

# 4 
four = det(C)
four

# 5
five = X%*%t(X)
five

# 6
six = solve(A)%*%X
six

# 7
seven = eigen(A)
seven

# 8
eight = eigen(A%*%A)
eight

# 9 
z = as.matrix(c(1,-3,4))
z1 = solve(seven$vectors)%*%z
z1
