mat1 = matrix(1,3,3)
mat1[upper.tri(mat1)] = 0
B = matrix(0,3,3)
# B[,1] = rnorm(5)
# B[,3] = rnorm(5)
# B[,5] = rnorm(5)
#B[3,1] = rnorm(1)
#B[3,3] = rnorm(1)
#B[3,5] = rnorm(1)

# B[1,1] = rnorm(1)
# B[1,3] = rnorm(1)

B[,1] = rnorm(3)
B[,3] = rnorm(3)


mat1 %*%B %*% t(mat1)

mat1 %x% mat1

mat2 = mat1 %x% mat1
mat2 %*% c(B)



###### not equal dimension matrix ? #####

mat1 = matrix(1,4,4)
mat2 = matrix(1,3,3)
mat1[upper.tri(mat1)] = 0
mat2[upper.tri(mat2)] = 0
B = matrix(0,4,3)


B[1,1] = rnorm(1)
B[1,2] = rnorm(1)
B[2,1] = rnorm(1)
B[2,2] = rnorm(1)
B[4,1] = rnorm(1)
B[4,2] = rnorm(1)


mat1 %*%B %*%  t(mat2)

max(abs((mat2 %x% mat1) %*% c(B) - c(mat1 %*%B %*%  t(mat2))))

