# Matrices are an extension of numeric or character vectors.
# They are not a separate type of object, but simply a vector with dimensions
# we have a number of rows and columns
# the elements must be of the same data type

m <- matrix(nrow=2, ncol=3)
m

#different ways to create matrices:
x2=matrix(1:30, nrow=5, ncol=6)
x2

x3 = matrix(seq(1, by=0.5, length=30), nrow=5, ncol=6, byrow=TRUE)
x3
#retrieving elements from matrices x[row, column]
x3[3, 4]
x3[3,]
x3[,4]