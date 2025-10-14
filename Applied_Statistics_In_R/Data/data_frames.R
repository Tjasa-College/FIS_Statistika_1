# One of the most important data types in R
# it is like a collection of vectors (every column of data frame is a vector)
name = c("Marko","Joze","Ana","Robert","Teja")
age = c(29,30,31,30,32)
average_grade = c(8.5,8.6,9,7,8.1)
passed_exam = c(TRUE,TRUE,FALSE,FALSE,TRUE)
#create data frame from 4 vectors above:
students = data.frame(name,age,average_grade,passed_exam)
students

# every column in data frame is one statistical variable, and every row is one unit of the statistical sample
# this is how we retrieve data

students[1, 2] # row, column
students[1,] # whole row
students$age # whole column
students[['age']] # whole row