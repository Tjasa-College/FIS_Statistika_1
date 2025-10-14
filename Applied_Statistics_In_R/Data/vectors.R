#vector is a collection of elements of the same type: character, logical, integer or numeric.
#we can create vectors in some different ways

#as an empty vector:
vector()

#as a sequence from 1 to 10
x=1:10
x

#using function seq(), to create a vector from 1 to 10 with a 0.5 distance
x=seq(1, 10, by=0.5)
x

#using function seq(), create vector from 1, with a 0.5 distance, having 10 elements
x=seq(1, by=0.5, length.out=10)
x

#if we want to create a vector by specifying it's values, we use command c()
#Remember that vectors in R must be homogenus, meaning all elements must be of the same type
x = c(1,2,3,4,5,6,7,8,9,10)
y = c("Milan", "Albert", "Teja", "Nika", "Joze")
z = c(TRUE, FALSE, TRUE, TRUE)
x[3]
y[4]
z[1:3]
z[5]

#we can also do some mathematical operations with 2 or more vectors. The operations are applied element-wise
x=c(3,5,2,7,9,4)
y=c(9,8,7,6,5)
x[1] + y[1]
x[1:3] + y[1]